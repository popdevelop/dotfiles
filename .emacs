;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Popdevelop made functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ---- Dynamicly load paths from var set in users .emacs file
(defun add-path (path)
 "Add new path with loadable files using popdevelop path"
 (concat popdevelop-packages-path path))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Load packages files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq user-emacs-directory (add-path ".emacs.d")) 
(add-to-list 'load-path (add-path ".emacs.d/addons/"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Auto complete config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ---- auto-complete
(add-to-list 'load-path (add-path ".emacs.d/addons/auto-complete-1.3/"))
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories (add-path ".emacs.d/addons/auto-complete-1.3/dict"))
(ac-config-default)
(add-hook 'objc-mode-common-hook 'ac-cc-mode-setup)

(setq ac-use-menu-map t)
(define-key ac-menu-map "\C-n" 'ac-next)
(define-key ac-menu-map "\C-p" 'ac-previous)

(desktop-save-mode 1)

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

;;; ---- dabrev
;; Advanced abbreviation completion M-/ or M-C-/
;; This lets you autocomplete words that exist anywhere in the file by just
;; typing a part of it and pressing M-/
(require 'dabbrev)
(global-set-key [?\M--]    'dabbrev-expand)
(global-set-key [?\M-\C--] 'dabbrev-completion)

;;; ---- lineker
(require 'lineker)
(add-hook 'c-mode-hook 'lineker-mode)

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
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 121 :width normal :foundry "bitstream" :family "Bitstream Vera Sans Mono"))))
 '(cursor ((t nil)))
 '(diff-added ((t (:inherit diff-changed :foreground "#9edd4b"))))
 '(lineker-warning-face ((((type x)) (:background "#202020" :foreground "#dddddd"))))
 '(trailing-whitespace ((((class color) (background dark)) (:strike-through "#222222" :underline "#383838"))))
 '(vertical-border ((nil (:foreground "#444444")))))