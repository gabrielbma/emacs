;; Starts emacs server. It should be used with an additional bash script in order to avoid starting server everytime a new Emacs instance is created.
;; For more information check out terdon's answear on:
;;  https://superuser.com/questions/462451/how-to-open-a-file-from-bash-command-line-in-the-already-open-emacs-instead-of-a
;; Note: I ammend his answear with the correct path for emacs and by adding -c options to emacsclient. I placed the script inside the zsh custom configuration script.
;;(server-start)

;; Disable GUI components
(tooltip-mode      -1)
(menu-bar-mode     -1)
(tool-bar-mode     -1)
(menu-bar-mode     -1)
(scroll-bar-mode   -1)
(setq use-dialog-box     nil)
(setq ring-bell-function 'ignore)

;; Inhibit startup/splash screen
(setq inhibit-splash-screen   t)
(setq inhibit-startup-message t)

;; Cursor
(blink-cursor-mode 0)
(setq-default cursor-type 'box)
;; (set-cursor-color "#BE81F7")
(set-cursor-color "#FFFFFF")

;; Narrowing
(put 'narrow-to-region 'disabled nil)

;; Short messages
(defalias 'yes-or-no-p 'y-or-n-p)

;; Show paren
(setq show-paren-delay 0)
(setq show-paren-style 'parenthesis)
(show-paren-mode 1)

;; Menu line
(column-number-mode t)

;; Auto revert buffer
(global-auto-revert-mode 1)

;; Cycle through amounts of spacing
(global-set-key (kbd "s-SPC") 'cycle-spacing)

;; winner-mode to restory window position
;; C-c <left> to restore the previous window configuration
;; C-c <right> to return to the most recent configuration
(winner-mode 1)

;; Recent files
(recentf-mode 1)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

;; window management
(global-set-key (kbd "M-P") 'windmove-up)
(global-set-key (kbd "M-N") 'windmove-down)
(global-set-key (kbd "M-F") 'windmove-right)
(global-set-key (kbd "M-B") 'windmove-left)

;; dired
(setq dired-dwim-target t)
(add-hook 'dired-mode-hook
      (lambda ()
        (dired-hide-details-mode)))

;; show line show
(defun goto-line-show ()
  "Show line numbers temporarily, while prompting for the line number input."
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (call-interactively #'goto-line))
    (linum-mode -1)))
(global-set-key (kbd "M-g M-g") 'goto-line-show)

;; Toggle comments for regions and lines
(global-set-key (kbd "M-;") 'comment-line)

;; Indent settings
(setq-default indent-tabs-mode nil)
(setq-default tab-width          4)
(setq tab-width                  4)
(setq-default tab-always-indent nil)
(setq-default c-basic-offset     4)
(setq-default standart-indent    4)
(setq-default lisp-body-indent   4)
(setq indent-line-function  'insert-tab)


;; set keys for Apple keyboard, for emacs in OS X;
;; See: http://ergoemacs.org/emacs/emacs_hyper_super_keys.html
(setq mac-command-modifier 'meta) ; make cmd key do Meta
(setq mac-option-modifier 'super) ; make opt key do Super
(setq mac-control-modifier 'control) ; make Control key do Control
(setq ns-function-modifier 'hyper)  ; make Fn key do Hyper

;; That keybind is useful with multi-cursor mode
(global-set-key (kbd "H-SPC") 'set-rectangular-region-anchor)

;; Collapse lines
(global-set-key (kbd "M-j")
            (lambda ()
                  (interactive)
                  (join-line -1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)

;; Add melpa to your packages repositories
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

;; Install use-package if not already installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;; Enable defer and ensure by default for use-package
;; Keep auto-save/backup files separate from source code:  https://github.com/scalameta/metals/issues/1027
(setq use-package-always-defer t
      use-package-always-ensure t
      backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(use-package zenburn-theme
  :demand t
  :config
  (load-theme 'zenburn t))

(use-package windswap
  :demand
  :bind
  (("H-n" . windswap-down)
   ("H-p" . windswap-up)
   ("H-b" . windswap-left)
   ("H-f" . windswap-right))
  :config
  (windswap-default-keybindings))

(use-package which-key
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.3)
  (which-key-setup-minibuffer))

(use-package nyan-mode
  :if window-system
  :demand t
  :config
  (nyan-mode 1))

(use-package flyspell
  :init (flyspell-mode +1))

(use-package flycheck
  :init (global-flycheck-mode)
  :config (global-flycheck-mode))

(use-package yasnippet
    :demand t
    :config
    (add-to-list 'yas-snippet-dirs (getenv "YASNIPPET_SNIPPETS_REPO"))
    (yas-global-mode 1))

(use-package yasnippet-snippets)

(use-package real-auto-save
    :hook (prog-mode . real-auto-save-mode)
    :config (setq real-auto-save-interval 1))

(use-package magit
    :bind (("C-x g" . magit-status)))

(use-package git-messenger
  :bind ("C-x G" . git-messenger:popup-message)
  :config
  (setq git-messenger:show-detail t
        git-messenger:use-magit-popup t))

(use-package markdown-mode
    :config
    (add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
    (add-to-list 'auto-mode-alist '("\\.txt\\'" . markdown-mode))
    (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
    (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
    (add-to-list 'auto-mode-alist '("\\.Rmd\\'" . markdown-mode)))

(use-package vlf)

(use-package expand-region
    :bind (("C-=" . er/expand-region)))

(use-package multiple-cursors
    :bind
    (("C-S-c C-S-c" . mc/edit-lines)
    ("C->" . mc/mark-next-like-this)
    ("C-<" . mc/mark-previous-like-this)
    ("C-c C-<" . mc/mark-all-like-this)))

(use-package move-text
    :demand t
    :config
    (move-text-default-bindings))

(use-package smartparens
    :demand t
    :bind (:map smartparens-mode-map
                ("C-M-f" . sp-forward-sexp)
                ("C-M-b" . sp-backward-sexp)
                ("C-M-d" . sp-down-sexp)
                ("C-M-a" . sp-backward-down-sexp)
                ("C-S-d" . sp-beginning-of-sexp)
                ("C-S-a" . sp-end-of-sexp)
                ("C-M-e" . sp-up-sexp)
                ("C-M-u" . sp-backward-up-sexp)
                ("C-M-t" . sp-transpose-sexp)
                ("C-M-n" . sp-forward-hybrid-sexp)
                ("C-M-p" . sp-backward-hybrid-sexp)
                ("C-M-k" . sp-kill-sexp)
                ("C-M-w" . sp-copy-sexp)
                ("C-M-<delete>" . sp-unwrap-sexp)
                ("C-M-<backspace>" . sp-backward-unwrap-sexp))
    :config
    (require 'smartparens-config)
    (smartparens-global-mode t))


(use-package org-superstar
    :hook (org-mode . (lambda ()
                        (org-superstar-mode 1))))

(use-package duplicate-thing
    :bind ("M-c" . duplicate-thing))

(use-package pip-requirements)

(use-package dockerfile-mode
    :config
    (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

(use-package docker-compose-mode)

(use-package diredfl
    :config
    (diredfl-global-mode))

(use-package dired-git-info
    :after dired
    :bind (:map dired-mode-map (")" . dired-git-info-mode)))

(use-package shell-pop
    :init
    (setq shell-pop-full-span t)
    :bind (("C-c s" . shell-pop)))

(use-package vterm
    :ensure t)

(use-package vterm-toggle
    :bind (("H-z" . vterm-toggle)
           ("H-F" . vterm-toggle-forward)
           ("H-B" . vterm-toggle-backward)))

(use-package multi-vterm :ensure t)

(use-package projectile
    :bind (:map projectile-mode-map
                ("s-p" . projectile-command-map)
                ("C-c p" . projectile-command-map))
    :config
        (projectile-mode +1)
        (add-to-list 'projectile-other-file-alist '("html" "js"))
        (add-to-list 'projectile-other-file-alist '("js" "html")))


;; begin company-mode setup ;;;

(defun company-yasnippet-or-completion ()
    "Solve company yasnippet conflicts."
    (interactive)
    (let ((yas-fallback-behavior
           (apply 'company-complete-common nil)))
        (yas-expand)))

(defun company-mode/backend-with-yas (backend)
  (if (or (not company-mode/enable-yas) 
          (and (listp backend)    
               (member 'company-yasnippet backend)))
          backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))



;; (use-package company
;;     :demand t
;;     :config
;;     (setq company-idle-delay 0)
;;     (setq company-show-numbers t)
;;     (setq company-minimum-prefix-length 2)
;;     (setq company-dabbrev-downcase nil)
;;     (setq company-dabbrev-other-buffers t)
;;     (setq company-auto-complete nil)
;;     (setq company-dabbrev-code-other-buffers 'all)
;;     (setq company-dabbrev-code-everywhere t)
;;     (setq company-dabbrev-code-ignore-case t)
;;     (defvar company-mode/enable-yas t "Enable yasnippet for all backends.")
;;     (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))
;;     :hook
;;     (after-init . global-company-mode)
;;     (company-mode . (lambda ()
;;                         (substitute-key-definition
;;                          'company-complete-common
;;                          'company-yasnippet-or-completion
;;                          company-active-map))))


(defvar company-mode/enable-yas t "Enable yasnippet for all backends.")
(use-package company
    :demand t
    :config
    (setq company-idle-delay 0)
    (setq company-show-numbers t)
    (setq company-minimum-prefix-length 2)
    (setq company-dabbrev-downcase nil)
    (setq company-dabbrev-other-buffers t)
    (setq company-auto-complete nil)
    (setq company-dabbrev-code-other-buffers 'all)
    (setq company-dabbrev-code-everywhere t)
    (setq company-dabbrev-code-ignore-case t)
    (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))
    (setq company-tooltip-align-annotations t
          company-tooltip-flip-when-above t
          ;; Easy navigation to candidates with M-<n>
          company-show-numbers t)
    :hook
    (after-init . global-company-mode))

(add-hook 'company-mode-hook
          (lambda ()
            (substitute-key-definition
             'company-complete-common
             'company-yasnippet-or-completion
             company-active-map)))
(setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))
;;end company-mode setup

(use-package helm
    :config
    (helm-autoresize-mode 1)
    (setq helm-M-x-fuzzy-match t)
    (setq helm-buffers-fuzzy-matching t
          helm-recentf-fuzzy-match    t)
    :bind 
    (("M-x" . helm-M-x)
    ("C-x b" . helm-multi-files)
    ("C-x C-f" . helm-find-files)
    ("C-x C-r" . helm-recentf)
    ("C-h a" . helm-apropos)
    ("M-y" . helm-show-kill-ring)))

(use-package helm-projectile
    :demand t
    :config
    (helm-projectile-on)
    ;; helm-projectile: fix a bug as explained in: https://github.com/bbatsov/projectile/issues/1302
    (setq projectile-git-submodule-command nil))


(use-package helm-swoop
    :bind
    (("M-i" . helm-swoop)
     ("M-I" . helm-swoop-back-to-last-point)
     ("C-c M-i" . helm-multi-swoop)
     ("C-x M-i" . helm-multi-swoop-all)
     :map isearch-mode-map 
     ;; When doing isearch, hand the word over to helm-swoop
     ("M-i" . helm-swoop-from-isearch)
     :map helm-swoop-map 
     ;; From helm-swoop to helm-multi-swoop-all
     ("M-i" . helm-multi-swoop-all-from-helm-swoop)
     ;; Move up and down like isearch
     ("C-r" . helm-previous-line)
     ("C-s" . helm-next-line)
     :map helm-multi-swoop-map
     ;; Move up and down like isearch
     ("C-r" . helm-previous-line)
     ("C-s" . helm-next-line))
    :config
    ;; If you prefer fuzzy matching
    ;; (setq helm-swoop-use-fuzzy-match t)
    ;; Optional face for line numbers
    ;; Face name is `helm-swoop-line-number-face`
    ;; (setq helm-swoop-use-line-number-face t)
    ;; If this value is t, split window inside the current window
    (setq helm-swoop-split-with-multiple-windows t)
    ;; Split direcion. 'split-window-vertically or 'split-window-horizontally
    (setq helm-swoop-split-direction 'split-window-vertically)
    ;; If nil, you can slightly boost invoke speed in exchange for text color
    (setq helm-swoop-speed-or-color t))

(use-package helm-ag)

(use-package helm-rg)

;; Enable scala-mode for highlighting, indentation and motion commands
(use-package scala-mode
  :interpreter
    ("scala" . scala-mode))

;; Enable sbt mode for executing sbt commands
(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
   ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
  (setq sbt:program-options '("-Dsbt.supershell=false")))

(use-package lsp-mode
  ;; Optional - enable lsp-mode automatically in scala files
  :hook  (scala-mode . lsp)
         (lsp-mode . lsp-lens-mode)
  :config (setq lsp-prefer-flymake nil))

;; Add metals backend for lsp-mode
(use-package lsp-metals
  :config (setq lsp-metals-treeview-show-when-views-received t))

;; Enable nice rendering of documentation on hover
(use-package lsp-ui)

;; Use the Debug Adapter Protocol for running tests and debugging
(use-package posframe
  ;; Posframe is a pop-up tool that must be manually installed for dap-mode
  )

(use-package dap-mode
    :hook
    (lsp-mode . dap-mode)
    (lsp-mode . dap-ui-mode))


(use-package js2-mode 
    :mode
    (("\\.js\\'" . js2-mode)
     ("\\.avsc\\'" . js2-mode)))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("e6df46d5085fde0ad56a46ef69ebb388193080cc9819e2d6024c9c6e27388ba9" default))
 '(package-selected-packages
   '(js2-mode dired dired-git-info dired-git-info-mode docker-compose-mode dockerfile-mode org-superstart helm-swoop helm-projectile projectile buffer-move nyan-mode which-key git-messenger multi-vterm vterm-toggle shell-pop vterm company-mode helm company move-text smartparens multiple-cursors expand-region vlf yasnippet-snippets magit real-auto-save use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
