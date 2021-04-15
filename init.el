(defun tangle-init ()
  "If 'init.org' tangle it and compile."
  (when (equal (buffer-file-name)
               (expand-file-name 
	       (concat user-emacs-directory "init.org")))
    (let ((prog-mode-hook nil))
      (org-babel-tangle)
      (byte-compile-file (concat user-emacs-directory "init.el")))))

(add-hook 'after-save-hook 'tangle-init)

(add-hook
 'after-init-hook
 (lambda ()
 (let ((private-file (concat user-emacs-directory "priv.el")))
 (when (file-exists-p private-file)
 (load-file private-file)))))

(setq auto-revert-interval 1            ; Refresh buffers fast
      custom-file (make-temp-file "")   ; Discard customization's
      echo-keystrokes 0.1               ; Show keystrokes asap
      inhibit-startup-message t         ; No splash screen please
      initial-scratch-message nil       ; Clean scratch buffer
       use-dialog-box nil                ; Without annoying beeps
      ring-bell-function 'ignore        ; 
       visible-bell t                    ;
      select-enable-clipboard t         ; Fix clipbboard
      show-paren-delay 0                ; disable delay for paren mode
      make-backup-files nil             ; stop ~files
      auto-save-default nil             ; stop #files
      create-lockfiles nil              ; stop symlinks
      savehist-file "~/.emacs.d/savehist"
      history-length t
      history-delete-duplicates t
      savehist-save-minibuffer-history 1
      sentence-end-double-space nil     ; No double space
                   ; indentation
      compilation-scroll-output 'first-error
      require-final-newline t
      ripgrep-arguments (quote ("--ignore-file ~/.rgignore" "--smart-case")))
      (savehist-mode 1)

(defconst custom-style
'(
  (indent-tabs-mode . nil)                   
  (c-basic-offset . 4)         
  (c-default-style . "bsd")              
  (c-offsets-alist . ((substatement-open . 0)                  
  (case-label . +)                         
  (inline-open . 0)                        
  (block-open . 0)                         
  (statement-cont . +)                     
  (inextern-lang . 0)                      
  (innamespace . 0))))
  "My style")
  (c-add-style "PERSONAL" custom-style)
  ;; Customizations for all modes in CC Mode.
  (defun my-c-mode-common-hook ()
  ;; set my personal style for the current buffer
  (c-set-style "PERSONAL")
  ;; other customizations
  (setq tab-width 2
  ;; this will make sure spaces are used instead of tabs
  indent-tabs-mode nil
  c-default-style "bsd"))
  (add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

(setq-default fill-column 120                   ; Maximum line width
              truncate-lines t                  ; Don't fold lines
	      cursor-type 'box                  ; Change cursor
              indent-tabs-mode nil)             ; Spaces

(let ((default-directory (concat user-emacs-directory "extra/")))
  (when (file-exists-p default-directory)
    (setq load-path
          (append
           (let ((load-path (copy-sequence load-path)))
             (normal-top-level-add-subdirs-to-load-path)) load-path))))

(fset 'yes-or-no-p 'y-or-n-p)

(dolist (mode'(
              menu-bar-mode            ; no menu
              tool-bar-mode            ; disable toolbars
              scroll-bar-mode          ; disable scroll bars 
              global-hl-line-mode      ; disable scroll bars 
              blink-cursor-mode))      ; disable cursor blinking
  (funcall mode 0))

(dolist (mode'(
              delete-selection-mode            ; fix deleting
              transient-mark-mode              ; better marking
              column-number-mode               ; show columnt number
              show-paren-mode                  ; show parents 
              winner-mode))                    ; do/undo window configuration
  (funcall mode t))

(windmove-default-keybindings)   
(global-set-key (kbd "M-[ d")    'windmove-left)
(global-set-key (kbd "M-[ c")   'windmove-right)
(global-set-key (kbd "M-[ b")    'windmove-down)
(global-set-key (kbd "M-[ a")      'windmove-up)

(setq whitespace-style '(trailing lines space-before-tab
  indentation space-after-tab)
  whitespace-line-column 80)

(require 'ansi-color)
(defun colorize-compilation-buffer ()
(let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(setq url-proxy-services
     '(("no_proxy" . "^\\(localhost\\|10.*\\)")
       ("http" . "10.144.1.10:8080")
       ("https" . "10.144.1.10:8080")))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(defun configuration/apply-keyboard-bindings (pair)
"Apply keyboard-bindings for supplied list of key-pair values"
(global-set-key (kbd (car pair))
               (cdr pair)))


(defvar configuration/my-keyboard-bindings 
       '(("<f6>" . proxy-toggle)))

(mapc 'configuration/apply-keyboard-bindings
configuration/my-keyboard-bindings)

(unless (package-installed-p 'use-package)
(package-refresh-contents)
(package-install 'use-package))
(require 'bind-key)



(use-package expand-region      ;; ;;
:ensure t                       ;; ;;
:init                           ;; ;;
:bind                           ;; ;;
("C-M-z" . er/expand-region)    ;; ;;
("C-M-x" . er/contract-region)) ;; ;;

(defalias 'save-mark-and-excursion 'save-excursion)

(use-package projectile
:ensure t
:config
(projectile-mode)
(setq projectile-completion-system 'ivy))
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(use-package helm
:diminish helm-mode
:init
(progn
  (require 'helm-config)
  (setq helm-candidate-number-limit 100)
  (setq helm-idle-delay 0.0 ; update fast sources immediately (doesn't).
       helm-input-idle-delay 0.01  ; this actually updates things
                                     ; reeeelatively quickly.
       helm-yas-display-key-on-candidate t
       helm-quick-update t
       helm-M-x-requires-pattern nil
       helm-ff-skip-boring-files t)
  (helm-mode))
:bind (("C-c h" . helm-mini)
       ("C-h a" . helm-apropos)
       ("C-x C-b" . helm-buffers-list)
       ("C-x b" . helm-buffers-list)
       ("M-y" . helm-show-kill-ring)
       ("M-x" . helm-M-x)
       ("C-x c o" . helm-occur)
       ("C-x c s" . helm-swoop)
       ("C-x c SPC" . helm-all-mark-rings)))
(ido-mode -1) ;; Turn off ido mode in case I enabled it accidentally

(use-package helm-config
:init
:bind
("C-x b" . helm-buffers-list)
("C-x r b" . helm-bookmarks)
("C-x m" . helm-M-x)
("M-y" . helm-show-kill-ring)
("C-x C-f" . helm-find-files)
("M-<up>"  . helm-all-mark-rings)
)

;; for helm-find-files
(customize-set-variable 'helm-ff-lynx-style-map t)

;; for helm-imenu
(customize-set-variable 'helm-imenu-lynx-style-map t)

;; for semantic
(customize-set-variable 'helm-semantic-lynx-style-map t)

;; for helm-occur
(customize-set-variable 'helm-occur-use-ioccur-style-keys t)

;; for helm-grep
(customize-set-variable 'helm-grep-use-ioccur-style-keys t)
(define-key helm-map (kbd "<left>") 'helm-previous-source)
(define-key helm-map (kbd "<right>") 'helm-next-source)

(use-package helm-projectile
:ensure t
:init
:config
(helm-projectile-on)
(setq projectile-enable-caching t)
(add-to-list 'projectile-globally-ignored-directories ".tmp_svn_workspace ")
)

(use-package helm-swoop
:ensure t
:bind
(("M-s s" . helm-swoop)
)
:config
(progn
(define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
(define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop))
)

(use-package autopair
:ensure t
:config (autopair-global-mode))

(use-package xcscope
:ensure t
:config (cscope-setup))

(use-package helm-cscope
:ensure t
:config
;; Enable helm-cscope-mode
(add-hook 'c-mode-hook 'helm-cscope-mode)
(add-hook 'c++-mode-hook 'helm-cscope-mode))

(use-package helm-rg
:ensure t
:init
:bind(
("M-s M-s" . helm-rg)))
()

(use-package magit
:ensure t)

(use-package ace-jump-mode
:ensure t
:commands ace-jump-mode
:init
:bind(
("C-c SPC" . ace-jump-mode)))

(use-package rainbow-delimiters
:ensure t
:init
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package undo-tree
:diminish undo-tree-mode
:config
(progn
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t))
:bind
 ("M-/" . undo-tree-visualize))

(use-package which-key
:ensure t
:config
(which-key-mode))

(use-package ripgrep
:ensure t)

(use-package projectile-ripgrep
:ensure t
:bind
("M-s M-s" . projectile-ripgrep))

(use-package ws-butler
:ensure t
:init
(add-hook 'prog-mode-hook #'ws-butler-mode))

(use-package diff-hl
:ensure t
:config
(diff-hl-margin-mode)
:bind
("<f11>" . diff-hl-mode))

(defvar customs-map (make-keymap)
  "Custom")

(require 'multiple-cursors)
(use-package expand-region      ;; ;;
:ensure t                       ;; ;;
:init                           ;; ;;
:bind                           ;; ;;
("C-c m c" . mc/edit-lines)    ;; ;;
("C-M-a" . mc/mark-all-like-this) ;; ;;
("C-M-m" . mc/mark-next-like-this) ;; ;;
("C-M-n" . mc/mark-previous-like-this)) ;; ;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (global-set-key (kbd "C-c m c") 'mc/edit-lines)          ;;
;; (global-set-key (kbd "C->") 'mc/mark-next-like-this)     ;;
;; (global-set-key (kbd "C-<") 'mc/mark-previous-like-this) ;;
;; (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load-theme 'wombat t)
