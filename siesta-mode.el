;;; siesta-mode.el --- Major mode for editing SIESTA FDF files

;; Author: Oliver Strickson <ots22@cam.ac.uk>
;; Created: 14 Sep 2012
;; Keywords: languages

;; Part of FDF Mode for Emacs

(require 'fdf-mode)
(require 'siesta-keywords)
(require 'cl)

(defvar siesta-keywords-regexp
  (regexp-opt (mapcar #'fdf-string-remove-ignored-chars siesta-keywords) 
	      'words))

(defvar siesta-block-keywords-regexp
  (regexp-opt (mapcar #'fdf-string-remove-ignored-chars siesta-block-keywords)
	      'words))

(defvar transiesta-keywords-regexp
  (regexp-opt (mapcar #'fdf-string-remove-ignored-chars transiesta-keywords)
	      'words))

(defvar siesta-units-regexp
  (regexp-opt siesta-units 'words))

(defvar siesta-strings-regexp
  (regexp-opt siesta-strings 'words))

(defun siesta-keyword-matcher (&optional search-limit)
  (funcall (fdf-matcher siesta-keywords-regexp) 
	   search-limit))

(defun siesta-block-keyword-matcher (&optional search-limit)
  (funcall (fdf-matcher siesta-block-keywords-regexp)
	   search-limit))

(defun transiesta-keyword-matcher (&optional search-limit)
  (funcall (fdf-matcher transiesta-keywords-regexp)
	   search-limit))

(defvar siesta-font-lock-defaults
  `((,siesta-units-regexp         . font-lock-type-face)
    (,siesta-strings-regexp       . font-lock-string-face)
    (siesta-block-keyword-matcher . font-lock-function-name-face)
    (siesta-keyword-matcher       . font-lock-variable-name-face)
    (transiesta-keyword-matcher   . font-lock-variable-name-face)))

(defvar siesta-keywords-all (append siesta-keywords 
				    siesta-block-keywords 
				    transiesta-keywords
				    siesta-units
				    siesta-strings))

;; define new derived mode based on fdf mode - provide font-locking
;; for SIESTA keywords and completion (for hippie-expand).
(define-derived-mode siesta-mode fdf-mode
  "Siesta FDF"
  "Mode for editing SIESTA FDF files"
  (font-lock-add-keywords nil siesta-font-lock-defaults t)
  (set (make-local-variable 'fdf-keywords)
       (append fdf-keywords siesta-keywords-all)))

(provide 'siesta-mode)

;; siesta-mode.el ends here
