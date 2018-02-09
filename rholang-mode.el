;;Emacs mode for rholang
;;Following this guide; https://www.emacswiki.org/emacs/ModeTutorial#toc3

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
     . font-lock-variable-name-face))
  "Keywords in rholang.")

(defconst rholang-keywords-2
  (append rholang-keywords
	  (list
	   '("\\<\\(|\\)\\>" . font-lock-constant-face)
	   '("\\<\\(true\\|false\\)\\>" . font-lock-constant-face)))
  "Additional Keywords to highlight in rholang mode")


(defvar rholang-font-lock-keywords rholang-keywords-2
  "just the keywords for now")


(defvar rholang-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?/ ". 124b" st) ;;rholang comments are of the form //
    (modify-syntax-entry ?* ". 23" st)   ;;or /*
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
  (setq font-lock-defaults '(rholang-font-lock-keywords))
  ;;Inherit indentation from c-indent-line
  (set (make-local-variable 'indent-line-function) 'c-indent-line)
  (set (make-local-variable 'indent-region-function) 'c-indent-region)  
  (run-hooks 'rholang-mode-hook))

(provide 'rholang-mode)
;; rholang-mode.el ends here
