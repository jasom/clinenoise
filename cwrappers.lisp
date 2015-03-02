(in-package :clinenoise)
(defcfun (getenv "getenv") :string
    (name :string))

(defcfun (isatty "isatty") :boolean
  (fd :int))


(defun zero-or-error-from-c (value) (if (zerop value) t (error "Error")))

(defcfun tcgetattr (:wrapper :int :from-c zero-or-error-from-c)
  (fd :int)
  (termios-p (:pointer (:struct termios))))

(defcfun tcsetattr (:wrapper :int :from-c zero-or-error-from-c)
  (fd :int)
  (optional-actions :int)
  (termios-p (:pointer (:struct termios))))
  
(defcfun ioctl :int
  (d :int)
  (request :ulong)
  &rest)

(defcfun (cread "read") ssize_t
  (fd :int)
  (buf (:pointer :void))
  (nbyte size_t))
