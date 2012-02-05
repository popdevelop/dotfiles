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

;;;; ---- Check if system is Windows
(defun system-type-is-win ()
  "Return true if system is Windows based"
  (string-equal system-type "windows-nt"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Load packages files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq user-emacs-directory (add-path ".emacs.d"))
(add-to-list 'load-path (add-path ".emacs.d/addons/"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Fonts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (system-type-is-gnu) (set-default-font "-misc-fixed-medium-r-semicondensed-*-*-120-*-*-c-*-iso8859-1"))
(if (system-type-is-darwin)
    (if (> (x-display-pixel-width) 1280)
        (set-default-font
         "-apple-monaco-medium-r-normal--12-0-72-72-m-0-iso10646-1")
      (set-default-font
       "-apple-monaco-medium-r-normal--10-0-72-72-m-0-iso10646-1")))
(if (system-type-is-win) (set-default-font "-bitstream-bitstream vera sans mono-medium-r-*--*-90-*--*--*-"))

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

; Make Emacs UTF-8 compatible for both display and editing:
(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

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

;; Reload buffer automatically if changed elsewhere
(global-auto-revert-mode 1)

;;; ---- Mini buffer

;; Interactive completion in minibuffer
(require 'icomplete)

;; Make the y or n suffice for a yes or no question
(fset 'yes-or-no-p 'y-or-n-p)

;;; ---- Move lines with M-p (up) and M-n (down)
(global-set-key "\M-p" 'move-text-up)
(global-set-key "\M-n" 'move-text-down)

(defun move-text-internal (arg)
   (cond
    ((and mark-active transient-mark-mode)
     (if (> (point) (mark))
        (exchange-point-and-mark))
     (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
       (forward-line arg)
       (move-to-column column t)
       (set-mark (point))
       (insert text)
       (exchange-point-and-mark)
       (setq deactivate-mark nil)))
    (t
     (beginning-of-line)
     (when (or (> arg 0) (not (bobp)))
       (forward-line)
       (when (or (< arg 0) (not (eobp)))
        (transpose-lines arg))
       (forward-line -1)))))
(defun move-text-down (arg)
   "Move region (transient-mark-mode active) or current line
  arg lines down."
   (interactive "*p")
   (move-text-internal arg))
(defun move-text-up (arg)
   "Move region (transient-mark-mode active) or current line
  arg lines up."
   (interactive "*p")
   (move-text-internal (- arg)))

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
(add-hook 'python-mode-hook 'lineker-mode)

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
(global-set-key (kbd "C-c b") 'revert-buffer)

;;; -- Flymake
(global-set-key "\C-c\ l" 'flymake-display-err-menu-for-current-line)

;;; -- Rope documemtation
(global-set-key "\C-c\ d" 'rope-show-doc)

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

;;; ---- Mini map
(require 'minimap)
(defun minimap-toggle ()
  "Show minimap if hidden, hide if present."
  (interactive)
  (if (and minimap-bufname
	       (get-buffer minimap-bufname)
	       (get-buffer-window (get-buffer minimap-bufname)))
      (minimap-kill)
    (minimap-create))
  )
(global-set-key (kbd "s-l") 'minimap-toggle)

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

;;; ---- Php
(require 'php-mode)

;;; ---- Markdown

(autoload 'markdown-mode "markdown-mode.el"
   "Major mode for editing Markdown files" t)
(setq auto-mode-alist
   (cons '("\\.md" . markdown-mode) auto-mode-alist))

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

;;; ---- CSS

;; CSS autocomplete inifinite loop hacks
(add-to-list 'ac-css-value-classes
             '(border-width "thin" "medium" "thick" "inherit"))

(require 'sass-mode)
(setq auto-mode-alist
      (cons '("\\.scss$" . css-mode) auto-mode-alist))

;;; --- JST templates
(setq auto-mode-alist
      (cons '("\\.jst$" . html-mode) auto-mode-alist))

;;; ---- HTML/HAML
(require 'haml-mode)

;;; ---- XML / KML
(setq auto-mode-alist
      (cons '("\\kml$" . xml-mode) auto-mode-alist))


;;; ---- Python

; Python indentation
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
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

;         (setq flymake-allowed-file-name-masks '("\\.py\\'" flymake-pyflakes-init)))
         (add-to-list 'flymake-allowed-file-name-masks
                      '("\\.py\\'" flymake-pyflakes-init)))

(add-hook 'find-file-hook 'flymake-find-file-hook)

;;(load "flymake-php.el")

(defun flymake-jslint-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
		     'flymake-create-temp-inplace))
         (local-file (file-relative-name
		      temp-file
		      (file-name-directory buffer-file-name))))
    (list "jshint" (list local-file))))

(setq flymake-allowed-file-name-masks
      (cons '(".+\\.js$"
	      flymake-jslint-init
	      flymake-simple-cleanup
	      flymake-get-real-file-name)
	    flymake-allowed-file-name-masks))

(setq flymake-err-line-patterns
      (cons '("^\\(.+\\). \(line: \\([[:digit:]]+\\), character: \\([[:digit:]]+\\)\)$"
	      nil 2 3 1)
	    flymake-err-line-patterns))

;;(require 'gjslint)

(add-hook 'javascript-mode-hook
	  (lambda () (flymake-mode 1)))

; Use 2 spaces for indents in javascript-mode
(setq js-indent-level 2)

; Code standard
(autoload 'python-pep8 "python-pep8")
(autoload 'pep8 "python-pep8")
(require 'python-pep8)
(require 'tramp)
(setq tramp-default-method "ssh")
(defvar find-file-root-prefix
  (if (featurep 'xemacs) "/[sudo/root@localhost]" "/sudo:root@localhost:")
  "*The filename prefix used to open a file with `find-file-root'.")

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

(defun prefix-list-elements (list prefix)
  (let (value)
    (nreverse
     (dolist (element list value)
       (setq value (cons (format "%s%s" prefix element) value))))))

(defun ac-ropemacs-candidate ()
  (prefix-list-elements (rope-completions) ac-prefix))

(ac-define-source popdevelop
  '((candidates . (ac-ropemacs-candidate))
    (prefix . ac-prefix-c-dot)
    (requires . 1)
    (symbol . "f")
    (action . ac-start)
    (limit . nil)))

(add-hook 'python-mode-hook
          (lambda ()
            (auto-complete-mode 1)
            (set (make-local-variable 'ac-sources)
                 (append ac-sources '(ac-source-popdevelop)))))

;;; --- Ruby
(require 'ruby-mode)
;; Invoke ruby with '-c' to get syntax checking
(defun flymake-ruby-init ()
  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
	 (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
    (list "ruby" (list "-c" local-file))))

(push '(".+\\.rb$" flymake-ruby-init) flymake-allowed-file-name-masks)
(push '("Rakefile$" flymake-ruby-init) flymake-allowed-file-name-masks)

(push '("^\\(.*\\):\\([0-9]+\\): \\(.*\\)$" 1 2 nil 3) flymake-err-line-patterns)

(add-hook 'ruby-mode-hook
          '(lambda ()

	     ;; Don't want flymake mode for ruby regions in rhtml files and also on read only files
	     (if (and (not (null buffer-file-name)) (file-writable-p buffer-file-name))
		 (flymake-mode))
	     ))

(setq auto-mode-alist
      (cons '("\\Gemfile$" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist
      (cons '("\\Gemfile.lock$" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist
      (cons '("\\Capfile$" . ruby-mode) auto-mode-alist))

(defun kill-other-buffers ()
    "Kill all other buffers."
    (interactive)
    (mapc 'kill-buffer
          (delq (current-buffer)
                (remove-if-not 'buffer-file-name (buffer-list)))))


(require 'csharp-mode)
(setq auto-mode-alist
      (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))
;; (defun my-csharp-mode-fn ()
;;   (setq c-basic-offset 4)
;;   )
;; (add-hook  'csharp-mode-hook 'my-csharp-mode-fn t)

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