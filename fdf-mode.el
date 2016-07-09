;;; fdf-mode.el --- Major mode for editing FDF files

;; Author: Oliver Strickson <ots22@cam.ac.uk>
;; Created: 14 Sep 2012
;; Keywords: languages

;; Part of FDF Mode for Emacs

(require 'cl)

(defvar fdf-ignore-regexp "\\w\\([-_.]+\\)")

(defun fdf-string-remove-ignored-chars (str)
  "Remove matches of 'fdf-ignore-regexp' from STR."
  ;; replace subexpression 1 in match
  (replace-regexp-in-string fdf-ignore-regexp "" str nil nil 1))

(defun fdf-matcher (keyword-regexp)
  "Helper function for defining matchers that ignore -,_ and . tokens."
  (lexical-let ((keyword-regexp keyword-regexp))
    (lambda (&optional search-limit)
      ;; copy point to search-limit in current buffer
      (let ((buf (current-buffer))
	    (buf-point (point))
	    (ignored-char-locations nil)
	    (match-point nil))
	(with-temp-buffer 
	  (insert-buffer-substring buf buf-point search-limit)
	  ;; replace -_ and ., building a list of places where they occur
	  (while (re-search-backward fdf-ignore-regexp nil 'no-error-on-fail)
					; only replace the subexpression in the match above
	    (push (cons (match-beginning 1) (match-end 1)) ignored-char-locations)
	    (replace-match "" nil nil nil 1))
	  (when (re-search-forward keyword-regexp nil 'no-error-on-fail)
	    ;; restore the 'ignored' characters
	    (save-match-data
	      (map 'list (lambda (x) 
			   (goto-char (car x))
			   (insert-char ?_ (- (cdr x) (car x))))
	      ignored-char-locations))
	    (setq match-point (+ (1- buf-point) (match-end 0)))
	    (set-match-data (list (+ (1- buf-point) (match-beginning 0))
				  match-point
				  buf))
	    t))
	(when match-point (goto-char match-point))))))

(defvar fdf-constants-regexp
  (regexp-opt '("T" "true" ".true." "yes" "F" "false" ".false." "no") 'words))

(defvar fdf-comment-regexp "[#;!].*")

(defvar fdf-special-forms-regexp 
  (regexp-opt '("%block" "%endblock" "%dump") 'words))

(defvar fdf-include-regexp
  (regexp-quote "%include"))

(defvar fdf-font-lock-defaults
  `((,fdf-comment-regexp          . font-lock-comment-face)
    (,fdf-include-regexp          . font-lock-preprocessor-face)
    (,fdf-special-forms-regexp    . font-lock-keyword-face)    
    (,fdf-constants-regexp        . font-lock-constant-face)))

;;; Indentation

;; This is necessary to define here because 'save-excursion' does not
;; have quite the correct behaviour if some indentation is inserted at
;; or after the point.
(defmacro save-excursion* (&rest forms)
  "Like save-excursion, but point is restored to *after* text
inserted there (uses marker insertation type t)"
  (let ((old-pt (make-symbol "old-pt")))
    `(let ((,old-pt (copy-marker (point-marker) t)))
       (unwind-protect (progn ,@forms)
	(goto-char ,old-pt)))))

(defun fdf-indent-line ()
  "Indent the current line according to fdf '%block' keywords"
  (interactive)
  (save-excursion*
    (beginning-of-line)
    (if (bobp)
	(indent-line-to 0)
      (let ((not-indented t)
	    (cur-indent 0))
	(if (looking-at "^[ \t]*%endblock")
	    (progn
	      (save-excursion
		(forward-line -1)
		(setq cur-indent (- (current-indentation) default-tab-width)))
	      (if (< cur-indent 0)
		  (setq cur-indent 0)))
	  (save-excursion
	    (while not-indented
	      (forward-line -1)
	      (if (looking-at "^[ \t]*%endblock")
		  (progn
		    (setq cur-indent (current-indentation))
		    (setq not-indented nil))
		(if (looking-at "^[ \t]*%block[^<]*$")
		    (progn
		      (setq cur-indent (+ (current-indentation) fdf-indent))
		      (setq not-indented nil))
		  (if (bobp)
		      (setq not-indented nil)))))))
	(indent-line-to cur-indent)))))

(defun fdf-last-unclosed-block ()
  "Send point to the start of the last unclosed block."
  (while (and (re-search-backward "\\(%block[^<]*$\\)\\|\\(%endblock\\)")
              (looking-at "%endblock"))
    (fdf-last-unclosed-block)))

(defun fdf-close-block ()
  "Close an open '%block' with a corresponding '%endblock'."
  (interactive)
  (save-excursion 
    (condition-case nil 
	(fdf-last-unclosed-block)
      (error (error "No block to close")))
    (re-search-forward "%block\\s *\\([^\n]*\\)")
    (setq block-name (buffer-substring (match-beginning 1) (match-end 1))))
  (unless (bolp) (insert ?\n))
  (insert "%endblock " block-name)
  (fdf-indent-line))

;;; Completion with 'hippie-expand'

(defun he-fdf-keyword-beg ()
  "Find the beginning of the current keyword.  Helper function
for 'try-expand-fdf-keyword'."
  (let ((p))
    (save-excursion
      (backward-word 1)
      (setq p (point)))
    p))

(defun try-expand-fdf-keyword (old)
  "Completion function to use for FDF keywords with 'hippie-expand'. 
Keywords from 'fdf-keywords'."
  (unless old
    (he-init-string (he-fdf-keyword-beg) (point))
    (setq he-expand-list 
	  (sort
	   ;; filter the search string to remove -_. ignored in fdf
	   (all-completions 
	    (fdf-string-remove-ignored-chars he-search-string)
	    (lambda (string predicate tt)
	      (remove-if-not (lambda (str) 
			       (string-prefix-p string 
						(fdf-string-remove-ignored-chars str) 
						'ignore-case))
	       fdf-keywords)))
	   'string-lessp)))
  (while (and he-expand-list
	      (he-string-member (car he-expand-list) he-tried-table))
    (setq he-expand-list (cdr he-expand-list)))
  (if (null he-expand-list)
      (progn
	(when old (he-reset-string))
	nil)
    (he-substitute-string (car he-expand-list) t) ; non-nil second arg: transfer case
    (setq he-tried-table (cons (car he-expand-list) (cdr he-tried-table)))
    (setq he-expand-list (cdr he-expand-list))
    t))

;;; Mode definition

(define-derived-mode fdf-mode fundamental-mode "FDF"
  "Major mode for editing FDF files"
  (set (make-local-variable 'font-lock-defaults) '((fdf-font-lock-defaults) 
						   nil 'case-insensitive))
  (set (make-local-variable 'indent-line-function) 'fdf-indent-line)
  (set (make-local-variable 'comment-start) "#")
  (set (make-local-variable 'comment-start-skip) "[#!;]")
  (setq indent-tabs-mode nil)
  (set (make-local-variable 'fdf-indent) 2)
  (set (make-local-variable 'fdf-keywords) nil)
  (set (make-local-variable 'completion-ignore-case) t)
  (set (make-local-variable 'hippie-expand-try-functions-list)
       (cons 'try-expand-fdf-keyword hippie-expand-try-functions-list))
  (modify-syntax-entry ?. "w")
  (modify-syntax-entry ?_ "w")
  (modify-syntax-entry ?- "w")
  (define-key fdf-mode-map "\C-c\C-e" 'fdf-close-block))

(provide 'fdf-mode)

;; fdf-mode.el ends here
