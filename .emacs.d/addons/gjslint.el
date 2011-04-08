;;
;; A way to use Google's gjslint from Emacs
;; Copyright 2010 (c) Valeriy Zamarayev
;;
;; Put this into your .emacs
;; (require 'gjslint)
;; (add-hook 'js-mode-hook
;; 	  (lambda () (flymake-mode t)))


(require 'flymake)

(defun flymake-gjslint-init ()
  "Initialize flymake for gjslint"
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
		     'flymake-create-temp-inplace)))
    (list "gjslint" (list "--nojsdoc" "--strict" temp-file))))

(add-to-list 'flymake-allowed-file-name-masks
	     '(".+\\.js$"
	       flymake-gjslint-init
	       flymake-simple-cleanup
	       flymake-get-real-file-name))

(add-to-list 'flymake-err-line-patterns
 	     '("^Line \\([[:digit:]]+\\), E:[[:digit:]]+: "
 	      nil 1 nil))

(provide 'gjslint)