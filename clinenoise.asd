;;;; clinenoise.asd
(asdf:defsystem #:clinenoise
  :serial t
  :description "A trivial line-input library for VT-like terminals"
  :author "Jason Miller <aidenn0@geocities.com"
  :license "BSD-2"
  :defsystem-depends-on ("cffi-grovel")
  :depends-on (#:cffi
               #:alexandria
               #:split-sequence
	       #:cffi-grovel)
  :components ((:file "package")
	       (:cffi-grovel-file "termios-grovel")
	       (:file "cwrappers")
               (:file "clinenoise")))

