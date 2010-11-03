; Place all addons here

; Popdevelop made functions
(defun add-path (path)
 "Add new path with loadable files using popdevelop path"
 (concat popdevelop-packages-path path))

; Load packages files
(setq user-emacs-directory (add-path ".emacs.d")) 
(add-to-list 'load-path (add-path ".emacs.d/addons/"))
(add-to-list 'load-path (add-path ".emacs.d/addons/auto-complete-1.3/"))

(require 'coffee-mode)
(defun coffee-custom ()
  "coffee-mode-hook"
 (set (make-local-variable 'tab-width) 2))
(add-hook 'coffee-mode-hook
  '(lambda() (coffee-custom)))

(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories (add-path ".emacs.d/addons/auto-complete-1.3/dict"))
(ac-config-default)
(add-hook 'objc-mode-common-hook 'ac-cc-mode-setup)

(setq ac-use-menu-map t)
;; Default settings
(define-key ac-menu-map "\C-n" 'ac-next)
(define-key ac-menu-map "\C-p" 'ac-previous)

(add-hook 'c-mode-common-hook
  (lambda() 
    (local-set-key  (kbd "C-c o") 'ff-find-other-file)))
(add-hook 'objc-mode-hook
          (lambda ()
            (set (make-local-variable 'cc-other-file-alist)  '(("\\.m\\'" (".h")) ("\\.h\\'" (".m" ".c" ".cpp"))))))


(add-to-list
 'magic-mode-alist
 '("\\(.\\|\n\\)*@interface" . objc-mode))



(desktop-save-mode 1)
;(require 'midnight)
(require 'iso-transl)

;; Check if system is Darwin/Mac OS X
(defun system-type-is-darwin ()
(interactive)
"Return true if system is darwin-based (Mac OS X)"
(string-equal system-type "darwin")
)

;; Check if system is GNU/Linux
(defun system-type-is-gnu ()
(interactive)
"Return true if system is GNU/Linux-based"
(string-equal system-type "gnu/linux")
)

;; Check if the system is my Kubuntu GNU/Linux at work
(defun system-is-my-workpc ()
(interactive)
"Return true if the system we are running on is my PC at work"
(string-equal system-name "dev002.workplace.com")
)

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

(add-to-list 'load-path (add-path ".emacs.d/addons/yasnippet-0.6.1c"))
(require 'yasnippet)
(yas/initialize)
(yas/load-directory (add-path ".emacs.d/addons/yasnippet-0.6.1c/snippets"))
(setq yas/prompt-functions '(yas/dropdown-prompt))

; Customize buffer name when identical to another
(require 'uniquify)

(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)
;; displayed vertically, instead of horizontally
(setq ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))
;;Did you ever want to use bookmarks from within ido? I just did a little mashup of bookmark and ido code, just M-C-b from your ido file selection. – AnselmHelbig
    (setq enable-recursive-minibuffers t)
    (define-key ido-file-dir-completion-map [(meta control ?b)] 'ido-goto-bookmark)
    (defun ido-goto-bookmark (bookmark)
      (interactive
       (list (bookmark-completing-read "Jump to bookmark"
    				   bookmark-current-bookmark)))
      (unless bookmark
        (error "No bookmark specified"))
      (let ((filename (bookmark-get-filename bookmark)))
        (ido-set-current-directory
         (if (file-directory-p filename)
             filename
           (file-name-directory filename)))
        (setq ido-exit        'refresh
              ido-text-init   ido-text
              ido-rotate-temp t)
        (exit-minibuffer)))

;; When using 'grep'
;; '-i'   Ignore case distinctions
;; '-n'   Prefix each line of output with line number
;; '-H'   Print the filename for each match.
;; '-e'   Protect patterns beginning with a hyphen character, '-'
(setq grep-command "grep -i -nH -e ")

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

; Highlight the matching parentheses surrounding point.
(require 'highlight-parentheses)
(add-hook 'c-mode-hook 'highlight-parentheses-mode)

; This removes the default extra indent on Java methods.
(lambda nil (c-set-offset 'inline-open 0))

; Python indentation
(setq-default py-indent-offset 4)

; Avoid jumpy scolling
(setq scroll-step 1)

; Show trailing whitespaces
(setq-default show-trailing-whitespace t)

; Remove some noobie stuff :)
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

; Disable control-Z from backgrounding emacs
;; TODO: only when GUI
;(global-set-key "\C-z" nil)

; Don't show any startup message
(setq inhibit-startup-message t)

; Used for fill-paragraph
(setq-default fill-column 80)

; Pending delete (typing erases selected region)
(delete-selection-mode t)

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

; Set a better title
(setq frame-title-format '("%b" (buffer-file-name ": %f")))

; Enable mouse scrolling
(mouse-wheel-mode t)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

;; Show column-number in the mode line
(column-number-mode 1)

; Make the y or n suffice for a yes or no question
(fset 'yes-or-no-p 'y-or-n-p)

(setq-default indent-tabs-mode nil)

; Interactive completion in minibuffer
(require 'icomplete)

; Keyboard shortcuts
(global-set-key "\M-." 'find-tag-other-window)
(global-set-key "\C-h" 'backward-delete-char)
(global-set-key "\C-x\ ?" 'help)
(global-set-key "\C-c\ g" 'goto-line)

(global-set-key (kbd "C-c i") 'indent-region)
(global-set-key (kbd "C-c c") 'comment-region)
(global-set-key (kbd "C-c u") 'uncomment-region)

;; Put autosave files (ie #foo#) in one place, *not* scattered all over the
;; file system! (The make-autosave-file-name function is invoked to determine
;; the filename of an autosave file.)
(defvar autosave-dir (concat "/tmp/emacs_autosaves/" (user-login-name) "/"))
(make-directory autosave-dir t)
(defun auto-save-file-name-p (filename) (string-match "^#.*#$" (file-name-nondirectory filename)))
(defun make-auto-save-file-name () (concat autosave-dir (if buffer-file-name (concat "#" (file-name-nondirectory buffer-file-name) "#") (expand-file-name (concat "#%" (buffer-name) "#")))))

;(setq make-backup-files nil)
(setq backup-directory-alist (quote ((".*" . "~/.emacs.d/backups/"))))

(setq version-control nil)

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

; More sane scrolling. Return to same line when paging up, down and back up again.
(require 'pager)
(global-set-key "\C-v"	   'pager-page-down)
(global-set-key [next]	   'pager-page-down)
(global-set-key "\ev"	   'pager-page-up)
(global-set-key [prior]	   'pager-page-up)
(global-set-key '[M-up]    'pager-row-up)
(global-set-key '[M-kp-8]  'pager-row-up)
(global-set-key '[M-down]  'pager-row-down)
(global-set-key '[M-kp-2]  'pager-row-down)

; Flash window instead of annoying beep
(setq visible-bell t)

;; Advanced abbreviation completion M-/ or M-C-/
;; This lets you autocomplete words that exist anywhere in the file by just
;; typing a part of it and pressing M-/
(require 'dabbrev)
(global-set-key [?\M--]    'dabbrev-expand)
(global-set-key [?\M-\C--] 'dabbrev-completion)

; Check for too long lines
; TODO: this had to be hacked to 80. could it be done from here??
(require 'lineker)
(add-hook 'c-mode-hook 'lineker-mode)
;; TODO: In Emacs 23 this is integrated in whitespace-mode