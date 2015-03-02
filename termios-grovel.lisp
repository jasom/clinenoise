(in-package :clinenoise)

(include "sys/ioctl.h" "termios.h" "unistd.h")

(constant (+nccs+ "NCCS"))
(constant (+brkint+ "BRKINT"))
(constant (+icrnl+  "ICRNL"))
(constant (+inpck+  "INPCK"))
(constant (+istrip+ "ISTRIP"))
(constant (+opost+ "OPOST"))
(constant (+ixon+   "IXON"))
(constant (+cs8+ "CS8"))
(constant (+echo+ "ECHO"))
(constant (+icanon+ "ICANON"))
(constant (+iexten+ "IEXTEN"))
(constant (+isig+ "ISIG"))
(constant (+vmin+ "VMIN"))
(constant (+vtime+ "VTIME"))
(constant (+tiocgwinsz+ "TIOCGWINSZ"))
(constant (+tcsaflush+ "TCSAFLUSH"))

(ctype tcflag "tcflag_t")
(ctype cc-t "cc_t")
(ctype size_t "size_t")
(ctype ssize_t "size_t")

(cstruct termios "struct termios"
	 (iflag "c_iflag" :type tcflag)
	 (oflag "c_oflag" :type tcflag)
	 (cflag "c_cflag" :type tcflag)
	 (lflag "c_lflag" :type tcflag)
	 (cc    "c_cc"    :type cc-t :count nccs))

(cstruct winsize "struct winsize"
	 (row "ws_row" :type :short)
	 (col "ws_col" :type :short)
	 (xpixel "ws_xpixel" :type :short)
	 (ypixel "ws_ypixel" :type :short))
