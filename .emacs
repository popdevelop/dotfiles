;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Popdevelop made functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ---- Dynamicly load paths from var set in users .emacs file
(defun add-path (path)
 "Add new path with loadable files using popdevelop path"
 (concat popdevelop-packages-path path))

;;; ---- Check if system is Darwin/Mac OS X
(defun system-type-is-darwin ()
  "Return true if system is darwin-based (Mac OS X)"
  (string-equal system-type "darwin"))

;;;; ---- Check if system is GNU/Linux
(defun system-type-is-gnu ()
  "Return true if system is GNU/Linux-based"
  (string-equal system-type "gnu/linux"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Load packages files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq user-emacs-directory (add-path ".emacs.d"))
(add-to-list 'load-path (add-path ".emacs.d/addons/"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Fonts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (system-type-is-gnu) (set-default-font "-misc-fixed-medium-r-semicondensed-*-*-120-*-*-c-*-iso8859-1"))
(if (system-type-is-darwin) (set-default-font "-apple-monaco-medium-r-normal--12-0-72-72-m-0-iso10646-1"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Auto complete config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ---- auto-complete
(add-to-list 'load-path (add-path ".emacs.d/addons/auto-complete-1.3/"))
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories (add-path ".emacs.d/addons/auto-complete-1.3/dict"))
(ac-config-default)
(add-hook 'objc-mode-common-hook 'ac-cc-mode-setup)

(define-key ac-completing-map "\M-n" 'ac-next)
(define-key ac-completing-map "\M-p" 'ac-previous)
(define-key ac-completing-map [down] [down])
(define-key ac-completing-map [up] [up])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Text input
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ---- Misc

;; C-x 8 for entering non-ASCII Latin-1
(require 'iso-transl)

;; Avoid jumpy scolling
(setq scroll-step 1)

;; Show trailing whitespaces
(setq-default show-trailing-whitespace t)

;; Used for fill-paragraph
(setq-default fill-column 80)

;; Pending delete (typing erases selected region)
(delete-selection-mode t)

;; Show column-number in the mode line
(column-number-mode 1)

;; Do not indent with tabs.
(setq-default indent-tabs-mode nil)

;;; ---- Mini buffer

;; Interactive completion in minibuffer
(require 'icomplete)

;; Make the y or n suffice for a yes or no question
(fset 'yes-or-no-p 'y-or-n-p)

;;; ---- Move lines with M-p (up) and M-n (down)
(global-set-key "\M-p" 'move-line-up)
(global-set-key "\M-n" 'move-line-down)

(defun move-line (&optional n)
   "Move current line N (1) lines up/down leaving point in place."
   (interactive "p")
   (when (null n)
     (setq n 1))
   (let ((col (current-column)))
     (beginning-of-line)
     (next-line 1)
     (transpose-lines n)
     (previous-line 1)
     (forward-char col)))

(defun move-line-up (n)
   "Moves current line N (1) lines up leaving point in place."
   (interactive "p")
   (move-line (if (null n) -1 (- n))))

(defun move-line-down (n)
   "Moves current line N (1) lines down leaving point in place."
   (interactive "p")
   (move-line (if (null n) 1 n)))

;;; ---- dabbrev
;; Advanced abbreviation completion M-/ or M-C-/
;; This lets you autocomplete words that exist anywhere in the file by just
;; typing a part of it and pressing M-/
(require 'dabbrev)
(global-set-key [?\M--]    'dabbrev-expand)
(global-set-key [?\M-\C--] 'dabbrev-completion)

;;; ---- lineker
(require 'lineker)
(add-hook 'c-mode-hook 'lineker-mode)

;; Do NOT warn for long lines when saving (very annoying feature)
(setq lineker-check-on-save nil)

;;; ---- grep command
;; When using 'grep'
;; '-i'   Ignore case distinctions
;; '-n'   Prefix each line of output with line number
;; '-H'   Print the filename for each match.
;; '-e'   Protect patterns beginning with a hyphen character, '-'
(setq grep-command "grep -i -nH -e ")

;;; ---- Key-bindings
(define-key global-map [(control f3)]  'cscope-set-initial-directory)
(define-key global-map [(control f4)]  'cscope-unset-initial-directory)
(define-key global-map [(control f5)]  'cscope-find-this-symbol)
(define-key global-map [(control f6)]  'cscope-find-global-definition)
(define-key global-map [(control f7)]  'cscope-find-global-definition-no-prompting)
(define-key global-map [(control f8)]  'cscope-pop-mark)
(define-key global-map [(control f9)]  'cscope-next-symbol)
(define-key global-map [(control f10)] 'cscope-next-file)
(define-key global-map [(control f11)] 'cscope-prev-symbol)
(define-key global-map [(control f12)] 'cscope-prev-file)
(define-key global-map [(meta f9)]  'cscope-display-buffer)
(define-key global-map [(meta f10)] 'cscope-display-buffer-toggle)

;;; ---- Global Keyboard shortcuts
(global-set-key "\M-." 'find-tag-other-window)
(global-set-key "\C-h" 'backward-delete-char)
(global-set-key "\C-x\ ?" 'help)
(global-set-key "\C-c\ g" 'goto-line)
(global-set-key (kbd "C-c i") 'indent-region)
(global-set-key (kbd "C-c c") 'comment-region)
(global-set-key (kbd "C-c u") 'uncomment-region)

;;; ---- Pager
;; More sane scrolling. Return to same line when paging up, down and back up again.
(require 'pager)
(global-set-key "\C-v"	   'pager-page-down)
(global-set-key [next]	   'pager-page-down)
(global-set-key "\ev"	   'pager-page-up)
(global-set-key [prior]	   'pager-page-up)
(global-set-key '[M-up]    'pager-row-up)
(global-set-key '[M-kp-8]  'pager-row-up)
(global-set-key '[M-down]  'pager-row-down)
(global-set-key '[M-kp-2]  'pager-row-down)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Emacs look and feel
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ---- djcb
;; Change cursor color according to mode; inspired by
;; http://www.emacswiki.org/emacs/ChangingCursorDynamically
(setq djcb-read-only-color       "gray")
;; valid values are t, nil, box, hollow, bar, (bar . WIDTH), hbar,
;; (hbar. HEIGHT); see the docs for set-cursor-type
(setq djcb-read-only-cursor-type 'hbar)
(setq djcb-overwrite-color       "red")
(setq djcb-overwrite-cursor-type 'box)
(setq djcb-normal-color          "yellow")
(setq djcb-normal-cursor-type    'bar)

(defun djcb-set-cursor-according-to-mode ()
  "change cursor color and type according to some minor modes."

  (cond
    (buffer-read-only
      (set-cursor-color djcb-read-only-color)
      (setq cursor-type djcb-read-only-cursor-type))
    (overwrite-mode
      (set-cursor-color djcb-overwrite-color)
      (setq cursor-type djcb-overwrite-cursor-type))
    (t
      (set-cursor-color djcb-normal-color)
      (setq cursor-type djcb-normal-cursor-type))))

(add-hook 'post-command-hook 'djcb-set-cursor-according-to-mode)

(add-to-list 'load-path (add-path ".emacs.d/addons/yasnippet-0.6.1c"))
(require 'yasnippet)
(yas/initialize)
(yas/load-directory (add-path ".emacs.d/addons/yasnippet-0.6.1c/snippets"))
(setq yas/prompt-functions '(yas/dropdown-prompt))

;;; ---- Color theme
(defun popdevelop-theme ()
  (interactive)
  (color-theme-install
   '(popdevelop-theme
      ((background-color . "#2b2b2b")
      (background-mode . light)
      (border-color . "#2e2e2e")
      (cursor-color . "#525252")
      (foreground-color . "#e5e5e5")
      (mouse-color . "black"))
     (fringe ((t (:background "#2e2e2e"))))
     (mode-line ((t (:foreground "#8f8f8f" :background "#303030"))))
     (region ((t (:background "#424242"))))
     (flymake-errline ((((class color)) (:background "LightPink" :foreground "black"))))
     (flymake-warnline ((((class color)) (:background "LightBlue2" :foreground "black"))))
     (font-lock-builtin-face ((t (:foreground "#ffb885"))))
     (font-lock-comment-face ((t (:foreground "#525252"))))
     (font-lock-function-name-face ((t (:foreground "#f359a0"))))
     (font-lock-keyword-face ((t (:foreground "#fdda08"))))
     (font-lock-string-face ((t (:foreground "#9edd4b"))))
     (font-lock-type-face ((t (:foreground"#ff8fad"))))
     (font-lock-variable-name-face ((t (:foreground "#5dc0f4"))))
     (minibuffer-prompt ((t (:foreground "#adadad" :bold t))))
     (font-lock-warning-face ((t (:foreground "Red" :bold t))))
     )))
(provide 'popdevelop-theme)
(require 'color-theme)
(popdevelop-theme)

;;; ---- Bell
;; Flash window instead of annoying beep
(setq visible-bell t)

;;; ---- Decorations
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
;; Don't show any startup message
(setq inhibit-startup-message t)
;; Set a better title
(setq frame-title-format '("%b" (buffer-file-name ": %f")))

;;; ---- Mouse
(mouse-wheel-mode t)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; File handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'gitsum)
(require 'git)

;; Customize buffer name when identical to another
(require 'uniquify)

;;; ---- iswitchb
(iswitchb-mode 1)

;; ignore * files
(setq iswitchb-buffer-ignore '("^\\*"))
(setq iswitchb-buffer-ignore '("\*"))

(defun iswitchb-local-keys ()
  (mapc (lambda (K)
	  (let* ((key (car K)) (fun (cdr K)))
	    (define-key iswitchb-mode-map (edmacro-parse-keys key) fun)))
	'(("C-p" . iswitchb-next-match)
	  ("C-n"  . iswitchb-prev-match)
	  ("<up>"    . ignore             )
	  ("<down>"  . ignore             ))))
(add-hook 'iswitchb-define-mode-map-hook 'iswitchb-local-keys)

;;; ---- temporary files
;; Put autosave files (ie #foo#) in one place, *not* scattered all over the
;; file system! (The make-autosave-file-name function is invoked to determine
;; the filename of an autosave file.)
(defvar autosave-dir (concat "/tmp/emacs_autosaves/" (user-login-name) "/"))
(make-directory autosave-dir t)
(defun auto-save-file-name-p (filename) (string-match "^#.*#$" (file-name-nondirectory filename)))
(defun make-auto-save-file-name () (concat autosave-dir (if buffer-file-name (concat "#" (file-name-nondirectory buffer-file-name) "#") (expand-file-name (concat "#%" (buffer-name) "#")))))

(setq backup-directory-alist (quote ((".*" . "~/.emacs.d/backups/"))))

(setq version-control nil)

;;; ---- Open old opened files when emacs is closed and reopened
(desktop-save-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Language add-ons
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ---- Coffee script

;; coffe-mode
(require 'coffee-mode)
(defun coffee-custom ()
  "coffee-mode-hook"
 (set (make-local-variable 'tab-width) 2))
(add-hook 'coffee-mode-hook
  '(lambda() (coffee-custom)))

;;; ---- C

; Highlight the matching parentheses surrounding point.
(require 'highlight-parentheses)
(add-hook 'c-mode-hook 'highlight-parentheses-mode)

; Set different coding styles depending on directory
(setq c-style-variables-are-local-p t)
(defun my-c-mode-hooks ()
  (let ((bname (buffer-file-name)))
    (cond
     ((string-match "util/jm" bname) (setq c-basic-offset 2))
     ((string-match "mve6" bname) (setq c-basic-offset 4))
     ((string-match "os/linux-2.6" bname) (c-set-style "linux"))
     ((string-match "modules/" bname) (c-set-style "linux"))
     ((string-match "\\.[ch]$" bname) (c-set-style "gnu"))
     )))
(add-hook 'c-mode-hook 'my-c-mode-hooks)

;;; ---- Java

;; This removes the default extra indent on Java methods.
(lambda nil (c-set-offset 'inline-open 0))

;;; --- Python

; Python indentation
(setq-default py-indent-offset 4)

; Static analysis
(when (load "flymake" t)
         (defun flymake-pyflakes-init ()
           (let* ((temp-file (flymake-init-create-temp-buffer-copy
                              'flymake-create-temp-inplace))
              (local-file (file-relative-name
                           temp-file
                           (file-name-directory buffer-file-name))))
             (list "pyflakes" (list local-file))))

         (add-to-list 'flymake-allowed-file-name-masks
                  '("\\.py\\'" flymake-pyflakes-init)))

   (add-hook 'find-file-hook 'flymake-find-file-hook)

; Code standard
;;(autoload 'python-pep8 "python-pep8")
;;(autoload 'pep8 "python-pep8")

; Pymacs can run Python code from Emacs
; Download: http://pymacs.progiciels-bpi.ca/archives/Pymacs.tar.gz
; Or check packages directory in our dotfiles. .el-file is for version 0.24
; sudo python setup.py install
(require 'pymacs)
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)

(pymacs-load "ropemacs" "rope-")
(setq ropemacs-enable-autoimport t)
(add-hook 'python-mode-hook
          '(lambda () (ropemacs-mode 1)) t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Auto-completion
;;;  Integrates:
;;;   1) Rope
;;;   2) Yasnippet
;;;   all with AutoComplete.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(defun prefix-list-elements (list prefix)
;  (let (value)
;    (nreverse
;     (dolist (element list value)
;       (setq value (cons (format "%s%s" prefix element) value))))))
;(defvar ac-source-rope
;  '((candidates
;     . (lambda ()
;         (prefix-list-elements (rope-completions) ac-target))))
;  "Source for Rope")

(defun ac-ropemacs-candidate ()
  (print "ponny")
  (print rope-completions)
  (list rope-completions)
;  (list (current-time-string) "seb" "kolloil"))
  )

(ac-define-source popdevelop
;  '((candidates . (lambda () 
;         (ac-ropemacs-candidate (list (current-time-string) "seb" "kolloil"))))
;  '((candidates . (rope-completions))
;  '((candidates . (ac-ropemacs-candidate))
  '((candidates . (ac-ropemacs-candidate))
;  '((candidates . (prefix-list-elements (list "klas" "mas") ac-prefix))
;  '((candidates . (lambda ()
;         (prefix-list-elements (rope-completions) ac-prefix)))
      ;(prefix-list-elements (list "klasen" "masen") ac-prefix)))
    (prefix . ac-prefix-c-dot)
    (requires . 0)
    (symbol . "p")
    (action . ac-start)
    (limit . nil)))

(add-hook 'python-mode-hook
          (lambda ()
            (auto-complete-mode 1)
            (set (make-local-variable 'ac-sources)
                 (append ac-sources '(ac-source-popdevelop) '(ac-source-yasnippet)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End Auto Completion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(c-default-style (quote ((c-mode . "linux") (java-mode . "java") (other . "gnu"))))
 '(case-fold-search t)
 '(current-language-environment "English")
 '(ecb-options-version "2.32")
 '(fill-column 80)
 '(global-font-lock-mode t nil (font-lock))
 '(show-paren-mode t nil (paren))
 '(tab-always-indent nil)
 '(transient-mark-mode t)
 '(uniquify-buffer-name-style (quote post-forward-angle-brackets) nil (uniquify)))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(cursor ((t nil)))
 '(diff-added ((t (:inherit diff-changed :foreground "#9edd4b"))))
 '(lineker-warning-face ((((type x)) (:background "#202020" :foreground "#dddddd"))))
 '(trailing-whitespace ((((class color) (background dark)) (:strike-through "#222222" :underline "#383838"))))
 '(vertical-border ((nil (:foreground "#444444")))))