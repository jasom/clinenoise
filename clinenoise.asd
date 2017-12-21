;;;; clinenoise.asd
(asdf:defsystem #:clinenoise
  :serial t
  :description "Describe clinenoise here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :defsystem-depends-on ("cffi-grovel")
  :depends-on (#:cffi
               #:alexandria
               #:split-sequence
	       #:cffi-grovel)
  :components ((:file "package")
	       (:cffi-grovel-file "termios-grovel")
	       (:file "cwrappers")
               (:file "clinenoise")))

