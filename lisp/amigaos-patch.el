;; AmigaOS patches for ELISP packages that are loaded with loadup.el.
;; 
;; Please note: It would be also possible to patch the LISP packages
;; (*.el) directly, but I don't want to have byte compiled LISP code
;; (*.elc) in the diff distribution.  The diffs should contain source
;; code but not compiled code. -bw/18-Aug-97

;; Improve Emacs-19 with Emacs-20 macros
(if (fboundp 'when) nil
  (defmacro when (condition &rest body)
    (cons 'if (cons condition body)))
  (defmacro unless (condition &rest body)
    (cons 'if (cons condition (cons nil body)))))

;; lazy-lock-stealth does not work so good in Emacs-20.3 (at least
;; with the default configuration).  I'll better switch it off here
;; -bw/21-May-98.
(setq-default lazy-lock-stealth-time nil)

;; patch for files.el::file-truename()  -bw/18-Aug-97
;; - expand volume assign if any (needed by 'find-file-existing-other-name).
;; - accept Amiga path syntax (for compatibility with previous Emacs ports)
;; (note: file-truename works also for remote paths (ange-ftp).)
;;

(if (fboundp 'old-file-truename) nil
  (fset 'old-file-truename (symbol-function 'file-truename))
  (defun file-truename (filename &optional counter prev-dirs)
    "Return the truename of FILENAME, which should be absolute.
The truename of a file name is found by chasing symbolic links
both at the level of the file and at the level of the directories
containing it, until no links are left at any level.
On AmigaOS we patched this function in file `amigaos-patch.el' 
to expand also volume assigns (and allow mixed path syntax).

The arguments COUNTER and PREV-DIRS are used only in recursive calls.
Do not specify them in other calls."
    (interactive)
    (cond
     (prev-dirs (old-file-truename filename counter prev-dirs))
     ;; Expand assigns on outermost call: FIXME-bw: add a special
     ;; function for that. Don't abuse `substitute-in-file-name'.
     (t 
      (let ((old-amiga-expand-path amiga-expand-path) result)
	(setq amiga-expand-path t
	      result (substitute-in-file-name (old-file-truename 
					       (expand-file-name filename)))
	      amiga-expand-path old-amiga-expand-path)
	result))
     )))

(if t nil
;; Fix too long filename in `auto-save-list-file-name' 
(add-hook 'emacs-startup-hook
(lambda ()
  ;; Truncate file path to (pathlen<=255 &&  filenamelen<=30)
  (setq auto-save-list-file-name
	(if (and (string-match "\\(^.*\\)/\\([^/]+\\)$" auto-save-list-file-name)
		 (< (match-end 2) 255))
	    (concat
	     (substring auto-save-list-file-name (match-beginning 1) (match-end 1))
	     "/"
	     (substring auto-save-list-file-name 
			(match-beginning 2) 
			(min (+ 255 (match-beginning 1))
			     (+ 30 (match-beginning 2))
			     (match-end 2))))
	  nil))
  ))
)


;; utility functions for workbench support (MOVEME-XXX-bw/17-Nov-97)
(defun amiga--frame-window-by-coord (frame x y) 
  "used by workbench app-window requests to select
the window in which the icon was dropped"
  (let ((sel-win (frame-selected-window frame))
	(win (frame-selected-window frame))
	(edges nil)
	)
    (while (progn
	     (setq edges (window-edges win))
	     (cond
	      ((and (<= (nth 0 edges) x)
		    (<= (nth 1 edges) y)
		    (<= x (nth 2 edges))
		    (<= y (nth 3 edges))) nil)
	      ((eq (setq win (next-window win)) sel-win) nil)
	      (t t))))
    win))

(defun amiga--wb-drop-frame (file frame x y)
  "used by workbench app-window requests"
  (if (frame-live-p frame)
      (select-window (amiga--frame-window-by-coord frame x y)))
  (raise-frame)
  (find-file file) nil)

(defun amiga--wb-drop-icon (file frame)
  "used by workbench app-icon requests"
  (if (frame-live-p frame) (select-frame frame))
  (raise-frame)
  (find-file file) nil)

(defun amiga--unix-to-dos-path (dir)
  "used for ASL file requesters"
  (if (not (eq (aref dir 0) ?/)) nil
    (setq dir (substring dir 1))
    (let ((i 0) (len (length dir)))
      (while (< i len)
	(if (not (eq (aref dir i) ?/)) nil
	  (aset dir i ?:)
	  (setq i len))
	(setq i (1+ i)))))
    dir)

(defun amiga--dos-to-unix-dir (s)
  ;; converts "home://" ==> "home:../../"
  (let* ((suffix "/")
	 (len (length s))
	 (sub-len (1- len))
	 (idx sub-len))
    (while (and (> (setq idx (1- idx)) 0)
		(eq ?/ (aref s idx)))
      (setq suffix (concat suffix "../"))
      (setq sub-len (1- sub-len)))
    (concat (substring s 0 sub-len) suffix))
;; TODO: "x:" ==> "/x/"
  )

(defun amiga--dos-to-unix-path (s)
  "unused experimental function"
  (expand-file-name
   (let* ((file (file-name-nondirectory s))
	  (file-len (length file)))
     (concat
      (amiga--dos-to-unix-dir (substring s 0 (- (length s) file-len)))
      file))))


;; ARexx stuff (TODO-bw/16-Mar-98: Add original copyright)

;;; This function needs to be re-written to handle rexx returned results.
;;;
(setq amiga-arexx-processing nil)
(setq amiga-arexx-errors nil)

(defvar amiga-arexx-failat 5
  "Return level from which arexx commands returns cause errors")

;;
;; process incoming rexx messages
;;
(defun amiga-arexx-process ()
  (interactive)
  (if (not amiga-arexx-processing)
      (progn
	(setq amiga-arexx-processing t)
	(condition-case nil		; Avoid blocking of processing in case of bugs
	    (let (arexxcmd)
	      (while (setq arexxcmd (amiga-arexx-get-next-msg))
		(let ((rc 0) result)
		  (condition-case err	; detect errors in arexx command
		      (let ((expr (car (read-from-string arexxcmd))))
			(setq result (prin1-to-string (eval expr))))
		    (error (progn
			     (setq rc 20)
			     (setq result (prin1-to-string err)))))
		  (amiga-arexx-reply rc result))))
	  (error nil)
	  ;;(quit (amiga-arexx-reply 20 "quit")))
	  (quit nil))
	(setq amiga-arexx-processing nil))))

(defun amiga-arexx-wait-command (id)
  "Waits for a pending ARexx commands (MSGID) to complete.
Also processes any pending ARexx requests during this interval.
returns the result list associated with this id, which takes the
form: (msgid result-code error-or-string)
``error-or-string'' depends on ``result-code''.
if ``result-code'' is 0 the command finished successfully and
``error-or-string'' will be a string or nil, otherwise the command
returned with an error and ``error-or-string'' will be an interger
that is the secondary error code of the arexx command."
  (amiga-arexx-process)
  (while (not (amiga-arexx-check-command id))
    (amiga-arexx-wait)
    (amiga-arexx-process))
  (amiga-arexx-get-msg-results id))

(defconst amiga-arexx-error-messages
["No cause"
"Program not found"
"Execution halted"
"Insufficient memory"
"Invalid character"
"Unmatched quote"
"Unterminated comment"
"Clause too long"
"Invalid token"
"Symbol or string too long"
"Invalid message packet"
"Command string error"
"Error return from function"
"Host environment not found"
"Requested library not found"
"Function not found"
"Function did not return value"
"Wrong number of arguments"
"Invalid argument to function"
"Invalid PROCEDURE"
"Unexpected THEN or WHEN"
"Unexpected ELSE or OTHERWISE"
"Unexpected BREAK, LEAVE or ITERATE"
"Invalid statement in SELECT"
"Missing or multiple THEN"
"Missing OTHERWISE"
"Missing or unexpected END"
"Symbol mismatch"
"Invalid DO syntax"
"Incomplete IF or SELECT"
"Label not found"
"Symbol expected"
"Symbol or string expected"
"Invalid keyword"
"Required keyword missing"
"Extraneous characters"
"Keyword conflict"
"Invalid template"
"Invalid TRACE request"
"Unitialized variable"
"Invalid variable name"
"Invalid expression"
"Unbalanced parentheses"
"Nesting limit exceeded"
"Invalid expression result"
"Expression required"
"Boolean value not 0 or 1"
"Arithmetic conversion error"
"Invalid operand"
]
"The arexx error messages, sorted by number")

(defun amiga-arexx-do-command (str as-file)
  "Sends ARexx command STR (like amiga-arexx-send-command).
If AS-FILE is true, STR is an arexx command, otherwise it is a file name.
Waits for the command to return.  If the arexx command fails an error will
be caused.

If you would like to get result strings and errors (ie. not cause
a lisp error) use: (amiga-arexx-do-command-with-results)"
  (interactive "sARexx command:
P")
  (let ((id (amiga-arexx-send-command str as-file))
	results res1 res2)
    (if (not id)
	(error "Failed to send arexx command.")
      (setq results (amiga-arexx-wait-command id)
	    res1 (nth 1 results)
	    res2 (nth 2 results))
      (if (<= res1 0)
	  res2
	(error "Arexx command failed, level %d, cause %s" res1
	       (if (< res2 (length amiga-arexx-error-messages))
		   (aref amiga-arexx-error-messages res2)
		 (format nil "Unknown error %d" res2)))
	results))))


(defun amiga-arexx-do-command-with-results (str as-file)
  "Sends ARexx command STR (like amiga-arexx-do-command).
If AS-FILE is true, STR is an arexx command, otherwise it is a file name.
Waits for the command to return.

The return value is one of three things:
 - the command executed succesfully: nil or a result string.
 - the command failed: a list of the form (RC ERROR-CODE)
   where RC is the severity and ERROR-CODE is the secondary error."
  (interactive "sARexx command:
P")
  (let ((id (amiga-arexx-send-command str as-file))
	results res1 res2)
    (if (not id)
	(error "Failed to send arexx command.")
      (setq results (amiga-arexx-wait-command id)
	    res1 (nth 1 results)
	    res2 (nth 2 results))
      (if (and rc (> rc 0))
	  (list res1 res2)
	res2))))

;; Code for compatibility with old AREXX scripts
(defalias 'amiga-cut 'amiga-set-clipboard-data)
(defalias 'amiga-paste 'amiga-get-clipboard-data)
;;;; (defun amiga-set-foreground-color (pen) ...)
;;;; (defun amiga-set-background-color (pen) ...)

(defun amiga-set-geometry (left top width height &optional screen backdrop)
  "Set Emacs window geometry"
  (interactive)
  (modify-frame-parameters (selected-frame) 
			   (list (cons 'top top)
				 (cons 'left left)
				 (cons 'width (/ width (frame-char-width)))
				 (cons 'height (/ height (frame-char-height)))))
  (redraw-frame (selected-frame)))

(defun amiga-get-window-geometry ()
       "Get Emacs window geometry.
a list returned is of the form:  (iconified x y width height backdrop)
where x, y, width, height are integers, backdrop is t or nil and iconified
is t if the window is iconified and nil otherwise"
       (list (eq (frame-visible-p (selected-frame)) 'icon)
	     (frame-parameter nil 'left)
	     (frame-parameter nil 'top)
	     (* (frame-width) (frame-char-width))
	     (* (frame-height) (frame-char-height))
	     nil))


;; FIXME--bw/11-Mar-98: Could implemented as builtin function only.
(defun amiga-get-screen-geometry ()
       "Get geometry of the screen emacs window resides on.
a list returned is of the form:  (name x y width height)
where name is a string, x, y, width, height are integers.
Only the public screen name is returned if the window is not currently open.
In this last case, the name may be nil if the window will be opened on the
default public screen."
 (list "Workbench" 0 0 640 400)
)

(defalias 'amiga-iconify 'iconify-or-deiconify-frame)
(defalias 'amiga-window-to-front  'raise-frame)

(defun amiga-set-icon-pos (left top)
  "Set the X Y position of the icon for emacs when iconified."
  nil)

;; FIXME-bw/11-Mar-98:
(defun  amiga-activate-window ()
  "Makes emacs window the currently active one."
  (if (frame-visible-p (selected-frame))
      (raise-frame)
    (error "No window to make active.")))

;; FIXME-bw/11-Mar-98:
(if t nil
(defun  amiga-window-to-front ()
  "Pulls the emacs window to the front (including screen)"
  (if (frame-visible-p (selected-frame))
      (raise-frame)
    (error "No window to pull to the front."))))

(defun  amiga-window-to-back ()
  "Pushes the emacs window to the back (including screen)"
  (if (frame-visible-p (selected-frame))
      (lower-frame)
    (error "No window to push back.")))

(if (fboundp 'amiga-popup-font-request) ()
  (defun amiga-popup-font-request ()
    "Open an ASL Font Requester and return the value as cons
of font name and font size."
    '("courier" . 13)))


;; customize support
;; Emacs-19 compatibility
(if (fboundp 'defcustom) nil
  (defmacro defcustom (sym initvalue docstring &optional &rest ignore)
    (list 'defvar sym initvalue docstring))
  (defmacro defgroup (&optional &rest ignore) nil)

  (defun custom-set-faces (&rest ARGS)
    "compatibility function to allow sharing ~/.emacs with emacs-20"
    (mapcar 
     '(lambda (SPECS)
	(let ((face (car SPECS))
	      (attribs (car (cdr (car (car (cdr SPECS))))))
	      (frame (car (car (car (cdr SPECS)))))
	      (a "") (font) (bold) (italic))
	  (setplist 'a attribs)
	  (setq frame nil)
	  (make-face face)
	  (set-face-foreground face (get 'a ':foreground) frame)
	  (set-face-background face (get 'a ':background) frame)
	  (set-face-underline-p face (get 'a ':underlined) frame)
    
	  (setq font (get 'a ':font))
	  (setq bold (get 'a ':bold))
	  (setq italic (get 'a ':italic))
	  (if (or font bold italic)
	      (progn
		(or font
		    (setq font (face-font face))
		    (setq font (cdr (assoc 'font (frame-parameters)))))
		(if bold (setq font (x-frob-font-weight font "bold")))
		(if italic (setq font (x-frob-font-slant font "i")))
		(set-face-font face font)))
	  (if (get 'a ':inverse-video) (invert-face face frame))))
     ARGS) nil)
  )

(defcustom amiga-expand-path nil
  "*Non-nil causes expanding drives to volumes and
assigns to absolute paths by `substitute-in-file-name'"
  :type 'boolean
  :options '(nil t)
  ;;:version "19.28"
  :group 'amiga)

(defcustom amiga-paths t
 "*Non-nil allows use of amiga path syntax."
  :type 'boolean
  :options '(nil t)
  ;;:version "19.28"
  :group 'amiga)

(defcustom amiga-working-jobctrl t
  "*Enable this if SIGTSTP is implemented by both the current ixemul
and the used shell.
Setting it to nil causes `suspend-emacs' to spawn a subshell"
  :type 'boolean
  :options '(nil t)
  ;;:version "19.28"
  :group 'amiga)

(defcustom amiga-working-killpg t
  "*Experimental debug variable: nil means changes sign of gid for kill()"
  :type 'boolean
  :options '(nil t)
  ;;:version "19.28"
  :group 'amiga)

;; The following items are only available when configured with
;; option "--have-intui" (Amiga-OS Intuition support).
(if (not (boundp 'amiga-i-windows)) nil
  (defcustom amiga--nk-numlock 'nil
    "*To control meaning of keys on numeric keypad"
    :type '(choice (const :tag "function keys" nil)
		   (const :tag "numeric keys" t))
    ;;:version "20.3"
    :group 'amiga)

  (defcustom amiga-menu-mouse-submenu 'mouse-button-up
    "*how to open an Intuition submenu using the mouse"
    :type '(choice (const :tag "Automatic" nil)
		   (const :tag "Mouse button up" mouse-button-up)
		   (const :tag "Mouse button down" mouse-button-down))
    ;;:version "20.3"
    :group 'amiga)

  (defcustom amiga-menu-font nil
    "*NAME and SIZE of font used by Intuition menu text.\n\
Allowed values are: nil (for using the Intuition screen font)\n\
or a cons cell FONT-NAME FONT-HEIGHT (example: '(\"topaz\" . 8))"
    :type '(cons string integer)
;;    :type '(restricted-sexp :match-alternatives (stringp 'nil))
    ;;:version "20.3"
    :group 'amiga)

  (defcustom amiga-menu-font-style 'bold
    "*Which font style to use in Intuition menu text.\n\
Allowed values are: nil, bold, underline."
    :type '(choice (const :tag "Normal" nil)
		   (const :tag "Bold" bold)
		   (const :tag "Underlined" underline))
    ;;:version "20.3"
    :group 'amiga)
  ) ;; amiga-i-windows

(defgroup amiga-group nil "Amiga-OS specific options."
  ;;  :prefix "amiga"
  ;;:version "19.28"
  )


;; deactivate MULE (code merged from MBSK)

(if (not (boundp 'amiga--no-mule)) nil

  ;; speed up insert-file-contents (mainly for tags-search) -bw/11-May-98
  (setq-default set-auto-coding-function nil)

  ;; this will tested in lisp/term/x-win.el
  (fmakunbound 'new-fontset)

  ;; from MBSK
  (setq-default enable-multibyte-characters nil)
  (setq-default mode-line-mule-info "")
  (setq load-source-file-function nil)
  (setq after-insert-file-functions nil)
  (setq enable-kinsoku nil)
  (setq file-name-coding-system 'no-conversion)
  (setq clipboard-coding-system 'no-conversion)
  (setq sendmail-coding-system 'no-conversion)
  (setq rmail-file-coding-system 'no-conversion)
  (setq default-buffer-file-coding-system 'no-conversion)
  (setq default-keyboard-coding-system 'no-conversion)
  (setq default-process-coding-system 'no-conversion)
  (setq default-terminal-coding-system 'no-conversion)
  (setq nnmail-file-coding-system 'no-conversion)
  (setq nnmail-pathname-coding-system 'no-conversion)
  (setq nnmail-active-file-coding-system 'no-conversion)
  (setq nnheader-pathname-coding-system 'no-conversion)
  (setq nnheader-file-coding-system 'no-conversion)
  (setq nnspool-file-coding-system 'no-conversion)
  (setq nntp-coding-system-for-read 'no-conversion)
  (setq pop3-movemail-file-coding-system 'no-conversion)
  ;; disabling mule menu would also done by standard-european-display
  ;; in Emacs-20.3 -bw/20-May-98
  (global-set-key [menu-bar mule] 'undefined) ;thanks to Albert L. Ting
  (defun generate-fontset-menu () '("" "")) ;thanks to Dave Love

;; from MBSK version of "disp-table.el"
(cond
  ((eq emacs-major-version 19) nil) ;; do nothing
  ((string= "20.2.1" emacs-version)
  ;; for Emacs-20.2
  (defun standard-display-european (arg &optional auto)
    "Toggle display of European characters encoded with ISO 8859.
  When enabled, characters in the range of 160 to 255 display not
  as octal escapes, but as accented characters.  Codes 146 and 160
  display as apostrophe and space, even though they are not the ASCII
  codes for apostrophe and space.

  With prefix argument, enable European character display iff arg is positive.

  Normally, this function turns off `enable-multibyte-characters'
  for all Emacs buffers, because users who call this function
  probably want to edit European characters in single-byte mode.

  However, if the optional argument AUTO is non-nil, this function
  does not alter `enable-multibyte-characters'.
  AUTO also specifies, in this case, the coding system for terminal output."
    (interactive "P")
    (if (or (<= (prefix-numeric-value arg) 0)
	    (and (null arg)
		 (char-table-p standard-display-table)
		 ;; Test 161, because 160 displays as a space.
		 (equal (aref standard-display-table 161) [161])))
	(progn
	  (standard-display-default 160 255)
	  (unless (eq window-system 'x)
	    (set-terminal-coding-system nil)))
      ;; If the user does this explicitly,
      ;; turn off multibyte chars for more compatibility.
      (or auto
	  (setq-default enable-multibyte-characters nil))
      (standard-display-8bit 160 255)
      (unless (or noninteractive (eq window-system 'x))
	;; Send those codes literally to a non-X terminal.
	;; If AUTO is nil, we are using single-byte characters,
	;; so it doesn't matter which one we use.
	(set-terminal-coding-system
	 (cond ((eq auto t) 'no-conversion)  ;;! <---no-mule
	       ((symbolp auto) (or auto 'no-conversion)) ;;! <---no-mule
	       ((stringp auto) (intern auto)))))
      ;; Make non-line-break space display as a plain space.
      ;; Most X fonts do the wrong thing for code 160.
      (aset standard-display-table 160 [32])
      ;; Most Windows programs send out apostrophe's as \222.  Most X fonts
      ;; don't contain a character at that position.  Map it to the ASCII
      ;; apostrophe.
      (aset standard-display-table 146 [39])
      ))
  ) ;; Emacs-20.2

  ((string< "20.2.1" emacs-version) ;; for Emacs-20.2.92
  (defun standard-display-european (arg &optional auto)
    "Toggle display of European characters encoded with ISO 8859.
  When enabled, characters in the range of 160 to 255 display not
  as octal escapes, but as accented characters.  Codes 146 and 160
  display as apostrophe and space, even though they are not the ASCII
  codes for apostrophe and space.

  With prefix argument, enable European character display iff arg is positive.

  Normally, this function turns off `enable-multibyte-characters'
  for subsequently created Emacs buffers, and for `*scratch*.
  This is because users who call this function
  probably want to edit European characters in single-byte mode."

    ;; If the optional argument AUTO is non-nil, this function
    ;; does not alter `enable-multibyte-characters'.
    ;; AUTO also specifies, in this case, the coding system for terminal output.
    ;; The AUTO argument is meant for use by startup.el only.
    ;; which is why it is not in the doc string.
    (interactive "P")
    (if (or (<= (prefix-numeric-value arg) 0)
	    (and (null arg)
		 (char-table-p standard-display-table)
		 ;; Test 161, because 160 displays as a space.
		 (equal (aref standard-display-table 161) [161])))
	(progn
	  (standard-display-default 160 255)
	  (unless (eq window-system 'x)
	    (set-terminal-coding-system nil)))
      ;; If the user does this explicitly,
      ;; turn off multibyte chars for more compatibility.
      (unless auto
	(setq-default enable-multibyte-characters nil)
	(if (get-buffer "*scratch*")
	    (with-current-buffer "*scratch*"
	      (set-buffer-multibyte nil)
	      (load "latin-1"))))
      (standard-display-8bit 160 255)
      (unless (or noninteractive (eq window-system 'x))
	;; Send those codes literally to a non-X terminal.
	;; If AUTO is nil, we are using single-byte characters,
	;; so it doesn't matter which one we use.
	(set-terminal-coding-system
	 (cond ((eq auto t) 'no-conversion)  ;;! <---no-mule
	       ((symbolp auto) (or auto 'no-conversion)) ;;! <---no-mule
	       ((stringp auto) (intern auto)))))
      ;; Make non-line-break space display as a plain space.
      ;; Most X fonts do the wrong thing for code 160.
      (aset standard-display-table 160 [32])
      ;; Most Windows programs send out apostrophe's as \222.  Most X fonts
      ;; don't contain a character at that position.  Map it to the ASCII
      ;; apostrophe.
      (aset standard-display-table 146 [39])
    ))
)) ;; cond

;; from MBSK version of "faces.el"
(defun set-face-font-auto (face font &optional frame)
  "Change the font of face FACE to FONT (a string), for an automatic change.
An automatic change means that we don't change the \"explicit\" flag;
if the font was derived from the frame font before, it is now.
If the optional FRAME argument is provided, change only
in that frame; otherwise change each frame."
  (interactive (internal-face-interactive "font"))
  (if (stringp font)
      (setq font (or (query-fontset font)
		     (x-resolve-font-name font 'default frame))))
  (internal-set-face-1 face 'font font 3 frame))

) ;; amiga--no-mule
