
;;programming-support:
(defun cxx-support-load ()
   (if (not (fboundp 'cxx-new-class))
       (load "cxx-support"))
)

(add-hook
 'c-mode-hook 
 (function 
  (lambda ()
    (cxx-support-load)
    (define-key c-mode-map "" 
      '(lambda () "compile-file-of-current-buffer"  (interactive)
	 (compile (concat "gcc -c " (buffer-name)))))
    (define-key c-mode-map "" 'manual-entry)
    (define-key c-mode-map "" 
      '(lambda () "compile by executing file of buffer"
	 (interactive)
	 (compile (concat "/c/execute " (buffer-name)))))
    (define-key c-mode-map "" '(lambda () (interactive) 
				     (view-file "~/dev/doc/cxx_cd2")))

    (define-key c-mode-map [kp-down] 
      '(lambda () "Narrow to next or current C function" (interactive)
	 (widen)
	 (if (not (numberp current-prefix-arg))
	     (if (not (numberp (re-search-forward "^}" nil 0 2)))
	     (progn (beginning-of-buffer)
		    (re-search-forward "^}"))))
	 (mark-defun)
	 (re-search-backward "^[ 	]*$" nil 0 2)
	 (narrow-to-region (mark) (point))
	 (re-search-forward "^{")))


    (define-key c-mode-map [kp-up] 
      '(lambda () "Narrow to previous C function" (interactive)
	 (widen)
	 (if (not (numberp (re-search-backward "^}" nil 0)))
	     (progn (end-of-buffer)
		    (re-search-backward "^}")))
	 (mark-defun)
	 (re-search-backward "^[ 	]*$" nil 0 2)
	 (narrow-to-region (mark) (point))
	 (re-search-forward "^{")))

    (defun amiga-autodoc () "load Amiga header-file" (interactive)
      (amiga-arexx-do-command 
       (concat "emacs-autodoc " 
	       (read-from-minibuffer "Tag: "
				     (buffer-substring  
				      (progn (backward-word 1) (point))
				      (progn (forward-word 1) (point)))
				     )) nil)
      ))))

(global-set-key "%" 'match-paren)
(defun match-paren (arg)
  "Go to the matching parenthesis if on parenthesis otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
	((looking-at "\\s\)") (forward-char 1) (backward-list 1))
	(t (self-insert-command (or arg 1)))))

;;obsolete!?;;(autoload 'perl-mode "perl-mode" "Perl Editing Mode" t)
(global-set-key "~" 'perldb-next-error)

(defun c-style-gnu-2-amiga ()
  "convert styles old_style to NewStyle" (interactive)
  (let ((flag t)(terminate nil)(old (point)) (cur (point)))
    (while (not terminate)
      (if (= 119 (char-syntax (following-char)))
	  ;;a letter
	  (progn
	    (capitalize-word 1)
	    )
	(progn (if (= 95 (following-char)) 
		   ;;an underscore
		   (progn
		     (delete-char 1 nil)
		     (capitalize-word 1)
		     )
		 ;;another
		 (setq terminate t)
		 ))))))

;;-c++ compilation
(defun c++-template-typedef () (interactive)
  (beginning-of-buffer)
  (replace-string "basic_string<char,string_char_traits<char> >" "string")
  )


(defun see-chars ()
  "Displays characters typed, terminated by a 3-second timeout."
  (interactive)
  (let ((chars "")
	(inhibit-quit t))
    (message "Enter characters, terminated by 3-second timeout.")
    (while (not (sit-for 3))
      (setq chars (concat chars (list (read-char)))
	    quit-flag nil))		; quit-flag maybe set by C-g
    (message "Characters entered: %s" (key-description chars))))

(defun translate (s1 s2 s3) 
  "perform char translation on s3 according to char in strings s1 s2"
  (interactive)
  (if (/= (length s1) (length s2)) 
      (error "argstrings \"%s\" \"%s\" must have same length" s1 s2))
  (let ((tbl (make-vector 256 0)) i (s (concat s3)))
    (setq i 0) (while (< i 256) (progn (aset tbl i i) (setq i (1+ i))))
    (setq i (length s1))
    (while (> i 0) (progn (setq i (1- i)) (aset tbl (aref s1 i) (aref s2 i))))
    (setq i (length s))
    (while (> i 0) (progn (setq i (1- i)) 
			  (aset s i (aref tbl (aref s i)))))
    s))

(defun ted () "translate english to german (aka deutsch)
"
 (interactive)
(save-excursion 
  (forward-char 1)
  (forward-word -1)
  (let ((b (point)) (s) (buf) (old-buf (current-buffer)))
    (forward-word 1)
    (setq s (downcase (buffer-substring b (point)))
	  buf (switch-to-buffer-other-window "ted.out"))
    (beginning-of-buffer)
    (princ (concat "<" s  ">: ") (current-buffer))
    (call-process "ted" nil buf t s)
    (if (not (string-equal "\C-j" (buffer-substring (1- (point)) (point))))
	(princ " =???= \n" (current-buffer)))	
    (switch-to-buffer-other-window old-buf)
    )))

(defun vera () "expand abbrevation using VERA list
"
 (interactive)
(save-excursion 
  (forward-char 1)
  (forward-word -1)
  (let ((b (point)) (s) (buf) (old-buf (current-buffer)))
    (forward-word 1)
    (setq s (upcase (buffer-substring b (point)))
	  buf (switch-to-buffer-other-window "ted.out"))
    (beginning-of-buffer)
    (call-process "/bin/grep" "/txt/lists/vera.list" buf t s)
    (switch-to-buffer-other-window old-buf)
    )))
