;; Emacs mode for rholang 

(defvar rholang-mode-hook nil)
(defvar rholang-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-j" 'newline-and-indent)
    map)
  "Keymap for rholang major mode")

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.rho\\'" . rholang-mode))

(defconst rholang-keywords
  (list
   '("\\<\\(contract\\|for\\|in\\|match\\|new\\|with\\)\\>"
     . font-lock-keyword-face))
  "Keywords in rholang.")


(defvar rholang-identifier-regexp
  "\\([a-zA-z0-9]\\|_\\)+")

(defvar rholang-identifiers-regexp
  (concat "\\(" rholang-identifier-regexp ", \\)*" rholang-identifier-regexp)
  )

(defun rholang-match-regexp (re limit)
  "Generic regular expression matching wrapper for RE with a given LIMIT."
  (re-search-forward re
                     limit ; search bound
                     t     ; no error, return nil
                     nil   ; do not repeat
                     )
  )

;;When a contract is introduced, the contract name 
(defun rholang-match-contract-decl (limit)
  "Search the buffer forward until LIMIT matching a 'contract' declarations.
   First match should be a keyword and second an identifier."
  (rholang-match-regexp
   (concat
    " *\\(contract\\) +\\(" rholang-identifier-regexp "\\)")
   limit))

(defun rholang-match-new-decl (limit)
  "Search the buffer forward until LIMIT matching a 'contract' declarations.
   First match should be a keyword and second an identifier."
  (rholang-match-regexp
   (concat
    " *\\(new\\) +\\(" rholang-identifiers-regexp "\\)")
   limit))

(defun rholang-match-for-decl (limit)
  "Search the buffer forward until LIMIT matching a 'for' construct.
   First match should be a keyword and second an identifier."
  (rholang-match-regexp
   (concat
    " *\\(for\\) +( *+\\(" rholang-identifier-regexp "\\)")
   limit)
  )


(defconst rholang-font-lock-highlight
  (append rholang-keywords
	  (list
;;	   '("\\<\\(|\\)\\>" . font-lock-constant-face)
	   '("\\<\\(true\\|false\\)\\>" . font-lock-builtin-face)
	  '(rholang-match-contract-decl (1 font-lock-keyword-face)
					(2 font-lock-type-face)
					(3 font-lock-variable-name-face)
					)
	  '(rholang-match-new-decl (1 font-lock-keyword-face)
					(2 font-lock-variable-name-face))
	  '(rholang-match-for-decl (1 font-lock-keyword-face)
				   (2 font-lock-variable-name-face))
	  )
	  )
  
  "Highlighting in rholang mode")


(defvar rholang-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?/ ". 124b" st) ;;rholang comments are of the form //
    (modify-syntax-entry ?* ". 23" st)   ;;or /*
    (modify-syntax-entry ?| "." st)      ;;make | a punctuation char (doesn't really do much)
    (modify-syntax-entry ?\n "> b" st)
    st)
  "Syntax table for rholang-mode")        ;;finish this!

;; (defun rholang-indent-line ()
;;   "Indent current line as rholang code"
;;   (interactive)
;;   (beginning-of-line)
;;   ) ;;TODO!

(define-derived-mode rholang-mode c-mode "rholang"
  "Major mode for editing Rholang files"
  (set-syntax-table rholang-mode-syntax-table)
  (setq font-lock-defaults '(rholang-font-lock-highlight))
  ;;Inherit indentation from c-indent-line
  (set (make-local-variable 'indent-line-function) 'c-indent-line)
  (set (make-local-variable 'indent-region-function) 'c-indent-region)
  (run-hooks 'rholang-mode-hook))

(provide 'rholang-mode)
;; rholang-mode.el ends here
