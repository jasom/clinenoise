;;;; clinenoise.asd
(cl:eval-when (:load-toplevel :execute)
  (asdf:operate 'asdf:load-op 'cffi-grovel))

(asdf:defsystem #:clinenoise
  :serial t
  :description "Describe clinenoise here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:cffi
               #:alexandria
               #:split-sequence
	       #:cffi-grovel)
  :components ((:file "package")
	       (cffi-grovel:grovel-file "termios-grovel")
	       (:file "cwrappers")
               (:file "clinenoise")))

