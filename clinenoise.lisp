;;;; clinenoise.lisp

(in-package #:clinenoise)

;;; "clinenoise" goes here. Hacks and glory await!


(defconstant +linenoise-default-history-max-len+ 100)
(defconstant +linenoise-max-line+ 4096)
(defparameter +unsupported-term+ '("dumb" "cons25"))

(defparameter *mlmode* nil)
(defparameter *history-len* +linenoise-default-history-max-len+)
(defparameter *history* nil)
(defparameter *completion-callback* (lambda (x) (declare (ignore x)) nil)
  "Function to call for completions.
  Takes the current line as a parameter, and returns a list of possible completions
  Does not currently indicate the position of the cursor when the completion was requested")

(defstruct (linenoise-state (:conc-name ls-))
  fd
  buf
  prompt
  plen
  pos
  oldpos
  cols
  maxrows
  history-index)

(defparameter *unread-char* nil)

(defun my-read-char (fd)
  (if *unread-char*
      (pop *unread-char*)
      (with-foreign-object (ch :char)
	(loop for len = (cread fd ch 1)
	     if (= len 1) return (code-char (mem-aref ch :char))
	     if (< len 0)  do (error 'eof-error)))))

(defun my-unread-char (ch fd)
  (declare (ignore fd))
  (push ch *unread-char*))
    

;;; Low level terminal handling ;;;

(defun linenoise-set-multiline (ml)
  (setf *mlmode* ml))

(defun is-unsupported-term ()
  (let ((term (getenv "TERM")))
    (member term +unsupported-term+ :test #'equalp)))

(defmacro with-raw-mode ((fd) &body b)
  (with-gensyms (orig-termios)
    (once-only (fd)
      `(with-foreign-object (,orig-termios '(:struct termios))
	 (tcgetattr ,fd ,orig-termios)
	 (enable-raw-mode ,fd)
	 (unwind-protect
	      (progn ,@b)
	   (restore-tty ,fd ,orig-termios)
	   (format *standard-output* "~%"))))))

(defun enable-raw-mode (fd)
  (unless (isatty fd)
    (error "Not a TTY"))
  (with-foreign-object (raw '(:struct termios))
    (tcgetattr fd raw)
    (with-foreign-slots ((iflag oflag cflag lflag cc) raw (:struct termios))
      (setf iflag (logand iflag (lognot (logior +brkint+ +icrnl+ +inpck+ +istrip+ +ixon+)))
	    oflag (logand oflag (lognot +opost+))
	    cflag (logior cflag +cs8+)
	    lflag (logand lflag
			  (lognot (logior +echo+ +icanon+ +iexten+ +isig+)))
	    (mem-aref cc 'cc-t +vmin+) 1
	    (mem-aref cc 'cc-t +vtime+) 0))
    (tcsetattr fd +tcsaflush+ raw)))

(defun restore-tty (fd orig-termios)
  (tcsetattr fd +tcsaflush+ orig-termios))
      
(defun get-columns ()
  (with-foreign-object (ws '(:struct winsize))
    (if (or (not (zerop (ioctl 1 +tiocgwinsz+ (:pointer (:struct winsize)) ws)))
	    (zerop (foreign-slot-value ws '(:struct winsize) 'col)))
	80
	(foreign-slot-value ws '(:struct winsize) 'col))))

(defun linenoise-clear-screen ()
  (format *standard-output*
	  "~C[H~C[2J" #\Escape #\Escape)
  (finish-output *standard-output*))

(defun linenoise-beep ()
  (write-char (code-char #x7) *standard-output*)
  (finish-output *standard-output*))

(defun complete-line (ls)
  (let ((completions (funcall *completion-callback* (ls-buf ls))))
    (when (emptyp completions)
      (linenoise-beep)
      (return-from complete-line nil))
    (loop with stop = nil and i = 0 and char = #\Nul
	 with saved = (copy-structure ls)
	 until stop
	 when (< i (length  completions))
	 do
	 (setf
	  (ls-pos ls) (length (elt completions i))
	  (ls-buf ls) (elt completions i))
	 else do
	   (setf (ls-pos ls) (ls-pos saved)
		 (ls-buf ls) (ls-buf saved))
	 do (refresh-line ls)

	 do (setf char (my-read-char 0))
	 (case (char-code char)
	   ((9)
	    (setf i (mod (1+ i) (1+ (length completions))))
	    (when (= i (length completions)) (linenoise-beep)))
	   ((27)
	    (when (< i (length completions))
	      (refresh-line ls))
	    (return-from complete-line nil))
	   (t
	    (when (< i (length completions))
	      (setf (ls-buf ls)
		    (with-output-to-string (s (make-array 0 :element-type 'character
							  :fill-pointer t))
		      (write-string (elt completions i) s)))
	      (my-unread-char char 0)
	      (return-from complete-line nil)))))))

(defun set-completion-callback (fn)
  (setf *completion-callback* fn))

(defun refresh-single-line (l)
  (let* ((pos (+ (ls-pos l) (ls-plen l)))
	 (lineno (floor pos (ls-cols l)))
	 (linestart (* lineno (ls-cols l)))
	 (lineend (min (length (ls-buf l))
		       (* (1+ lineno) (ls-cols l))))
	 (linebuf (subseq (ls-buf l) linestart lineend))
	 (linepos (mod pos (ls-cols l))))

    (format *standard-output* "~C[0G" #\Escape) ; cursor to left-edge
    (when (= lineno 0)
      (write-string (ls-prompt l) *standard-output*))
    (write-string linebuf *standard-output*)
    (format *standard-output* "~C[0K" #\Escape) ; erase to right
    ;move cursor to position
    (format *standard-output* "~C[0G~C[~DC" #\Escape #\Escape linepos)))

(defun refresh-multi-line (l)
    (let* ((pos (+ (ls-pos l) (ls-plen l)))
	   (lineno (floor pos (ls-cols l)))
	   ;(linestart (* lineno (ls-cols l)))
	   ;(lineend (min (length (ls-buf l))
			 ;(* (1+ lineno) (ls-cols l))))
	   ;(linebuf (subseq (ls-buf l) linestart lineend))
	   (linepos (mod pos (ls-cols l)))
	   (rows (ceiling (+ (ls-plen l) (length (ls-buf l))) (ls-cols l)))
	   (oldrows (ls-maxrows l)))
      
      (when (> rows oldrows)
	  (setf (ls-maxrows l) rows))

      (when (> oldrows lineno)
					;go down
	(format *standard-output* "~C[~DB" #\Escape (- oldrows lineno)))

      (loop repeat (1- oldrows)
					;clear and move-up
	   do (format *standard-output* "~C[0G~C[0K~C[1A" #\Escape #\Escape #\Escape))

					;clear a line
      (format *standard-output* "~C[0G~C[0K" #\Escape #\Escape)

      (write-string (ls-prompt l) *standard-output*)

      (write-string (ls-buf l))

      (when (zerop (mod (+ (length (ls-buf l)) (ls-plen l)) (ls-cols l)))
	(write-char #\Newline *standard-output*)
	(incf rows)
	(when (> rows (ls-maxrows l))
	  (setf (ls-maxrows l) rows)))
	  
      (format *standard-output* "~C[0G" #\Escape)

      (when (> rows lineno)
	(format *standard-output* "~C[~DA" #\Escape (- rows lineno)))

      (unless (zerop linepos)
	(format *standard-output* "~C[~DG" #\Escape linepos))))

(defun refresh-line (l)
  (if *mlmode*
      (refresh-multi-line l)
      (refresh-single-line l))
  (finish-output *standard-output*))

(defun edit-insert (l c)
  (setf (ls-buf l)
	(concatenate 'string (subseq (ls-buf l) 0 (ls-pos l))
		     (list c)
		     (when (< (ls-pos l) (length (ls-buf l)))
		       (subseq (ls-buf l) (ls-pos l)))))
  (if (and (not *mlmode*) (< (+ (ls-plen l) (length (ls-buf l))) (ls-cols l))
	   (= (incf (ls-pos l)) (length (ls-buf l))))
      (progn
	(write-char c *standard-output*)
	(finish-output *standard-output*))
      (refresh-line l)))

(defun edit-move-left (l)
  (when (> (ls-pos l) 0)
    (decf (ls-pos l))
    (refresh-line l)))

(defun edit-move-right (l)
  (when (< (ls-pos l) (length (ls-buf l)))
    (incf (ls-pos l))
    (refresh-line l)))

(defun edit-history-next (l dir)
  (when (> (length *history*) 1)
    (setf (elt *history* (ls-history-index l)) (ls-buf l))
    (incf (ls-history-index l) (if (eql dir :prev) 1 -1))
    (when (< (ls-history-index l) 0)
      (setf (ls-history-index l) 0))
    (when (>= (ls-history-index l) (length *history*))
      (setf (ls-history-index l) (1- (length *history*))))
    (setf (ls-buf l) (elt *history* (ls-history-index l)))
    (refresh-line l)))

(defun edit-delete (l)
  (if (and (> (length (ls-buf l)) 0)
	   (< (ls-pos l) (length (ls-buf l))))
      (setf (ls-buf l)
	    (concatenate 'string
			 (subseq (ls-buf l) 0 (ls-pos l))
			 (when (< (ls-pos l) (1- (length (ls-buf l))))
			   (subseq (ls-buf l) (1+ (ls-pos l)))))))
  (refresh-line l))

(defun edit-backspace (l)
  (when (> (ls-pos l) 0)
    (decf (ls-pos l))
    (edit-delete l)))

(defun edit-delete-prev-word (l)
  (let* ((end
	  (loop for index downfrom  (ls-pos l)
	     when (or (= index 0)
		      (char/= #\Space (elt (ls-buf l) (1- index))))
	     return index))
	 (start
	  (loop for index downfrom (max (1- end) 0)
	     when (or (= index 0)
		      (char= #\Space (elt (ls-buf l) index))
		      )
	       return index)))
    (setf (ls-buf l)
	  (concatenate 'string
		       (subseq (ls-buf l) 0 start)
		       (subseq (ls-buf l) end)))
  (setf (ls-pos l) start))
  (refresh-line l))

(defun edit (fd prompt plen)
  (let ((l
	 (make-linenoise-state
	  :fd fd
	  :buf ""
	  :prompt prompt
	  :plen plen
	  :pos 0
	  :oldpos 0
	  :cols (get-columns)
	  :maxrows 0
	  :history-index 0)))
    (history-add "")
    (refresh-line l)
    (handler-case
	(loop
	   (let* ((c (my-read-char 0))
		  (code (char-code c)))
	     (cond
	       ((and (char= c #\Tab) *completion-callback*)
		(complete-line l))
	       ((member c '(#\Newline #\Return))
		(pop *history*)
		(return-from edit (ls-buf l)))
	       ((char= c (code-char 3))
		(return-from edit :interrupted))
	       ((member (char-code c) '(127 8) :test #'=)
		(edit-backspace l))
	       ((= (char-code c) 4)
		(if (> (length (ls-buf l)) 0)
		    (edit-delete l)
		    (progn
		      (pop *history*)
		      :eof)))
	       ((and (= (char-code c) 20)
		     (> (ls-pos l) 0)
		     (< (ls-pos l) (length (ls-buf l))))
		(rotatef
		 (elt (ls-buf l) (ls-pos l))
		 (elt (ls-buf l) (1- (ls-pos l)))))
	       ((= code 2)
		(edit-move-left l))
	       ((= code 6)
		(edit-move-right l))
	       ((= (char-code c) 16)
		(edit-history-next l :prev))
	       ((= (char-code c) 14)
		(edit-history-next l :next))
	       ((char= c #\Escape)
		(let ((seq (coerce (list (my-read-char 0) (my-read-char 0)) 'string)))
		  ;(read-sequence seq *standard-input*)
		  (cond
		    ((string= seq "[A")
		     (edit-history-next l :prev))
		    ((string= seq "[B")
		     (edit-history-next l :next))
		    ((string= seq "[C")
		     (edit-move-right l))
		    ((string= seq "[D")
		     (edit-move-left l))
		    ((and (char= (elt seq 0)) #\[
			  (> (char-code (elt seq 1)) 48)
			  (< (char-code (elt seq 1)) 55))
		     (let ((code
			    (with-output-to-string (s)
			      (write-char (elt seq 1) s)
			      (loop for char = (my-read-char 0)
				 do (write-char char s)
				 until
				   (and (>= (char-code char) 64)
					(<= (char-code char) 126))))))
		       (when (string= code "3~")
			 (edit-delete l)))))))
	       ((= code 21)
		(setf (ls-buf l) ""
		      (ls-pos l) 0)
		(refresh-line l))
	       ((= code 11)
		(setf (ls-buf l)
		      (subseq (ls-buf l) 0 (ls-pos l))))
	       ((= code 1)
		(setf (ls-pos l) 0)
		(refresh-line l))
	       ((= code 5)
		(setf (ls-pos l) (length (ls-buf l)))
		(refresh-line l))
	       ((= code 12)
		(clear-screen)
		(refresh-line l))
	       ((= code 23)
		(edit-delete-prev-word l))
	       (t
		(edit-insert l c))))))))

(defun linenoise-raw (prompt plen)
  (if (not (isatty 0))
      (read-line *standard-input*)
      (with-raw-mode (0)
	(edit 0 prompt plen))))

(defun linenoise (prompt &optional (plen (length prompt)))
  "Display PROMPT and wait for a line to be entered with basic editing support
  PLEN can be specified for prompts that have a display width different from
  their character count (e.g. combining characters, escape codes)"
  (cond
    ((is-unsupported-term)
     (format *standard-output* "~A" prompt)
     (finish-output *standard-output*)
     (remove-if (lambda (x) (member x '(#\Newline #\Return)))
		(read-line *standard-input*)))
    (t
     (linenoise-raw prompt plen))))

(defun history-add (line)
  "Add LINE to the prompt history"
  (push line *history*)
  (when (> (length *history*) *history-len* )
    (setf *history* (subseq *history* 0 *history-len*))))

(defun history-save (fn)
  "Save history to pathname FN"
  (with-open-file (s fn :if-exists :supersede
		     :if-does-not-exist :create
		     :direction :output)
    (format s "~{~A~%~}" *history*)))

(defun history-load (fn)
  "Load history from pathname FN"
  (ignore-errors
    (with-open-file (s fn)
      (setf *history*
	    (loop for line = (read-line s nil)
	       while line
	       collect line)))))
(defun clear-screen ()
  (format *standard-output* "~C[H~C[2J" #\Escape #\Escape))

