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

;;
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

;; enable lingature in Mac port of Emacs
(if (fboundp 'mac-auto-operator-composition-mode)
        (mac-auto-operator-composition-mode))
;(set-frame-font "Fira Code" nil t)

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

;; preserve scroll position
(setq scroll-preserve-screen-position t)

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

;; disable backup-files and set up auto-save mode
(setq make-backup-files nil)
;;(setq backup-directory-alist '(("." . "~/MyEmacsBackups")))
;;(setq auto-save-file-name-transforms '(("." . "~/MyEmacsBackups")))
(auto-save-visited-mode 1)
(setq auto-save-interval 10
      auto-save-timeout 10)

;; C-u C-SPC to jump to last mark and to pop the mark repeatedly you can press C-u C-SPC C-SPC C-SPC ...
(setq set-mark-command-repeat-pop t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
        (with-current-buffer
                (url-retrieve-synchronously
                 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
                 'silent 'inhibit-cookies)
            (goto-char (point-max))
            (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))


(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
;; example to add my own repos
;; (use-package <project_name>
;;     :straight (:repo "https://github.com/gabrielbma/<project_name>.git")
;; )

(use-package guix)

(use-package geiser-guile)

;; Set up Tramp to use SSH
(use-package tramp
    :straight (tramp :build t :pre-build (("make" "autoloads")))
    :config
    ;; (setq tramp-debug-buffer t) ;; debug
    ;; (setq tramp-verbose 10) ;; debug
    (with-eval-after-load 'tramp-cache
        (setq tramp-persistency-file-name "~/.emacs.d/tramp"))
    (custom-set-variables
     '(tramp-default-method "ssh")
     '(tramp-default-user "gabriel.armelin"))
    (setq tramp-default-method "ssh"
          tramp-chunksize 500
          tramp-terminal-type "tramp"
          tramp-adb-program "adb"
          ;; use the settings in ~/.ssh/config instead of Tramp's
          tramp-use-ssh-controlmaster-options nil
          ;; don't generate backups for remote files opened as root (security hazzard)
          backup-enable-predicate
          (lambda (name)
              (and (normal-backup-enable-predicate name)
                   (not (let ((method (file-remote-p name 'method)))
                            (when (stringp method)
                                (member method '("su" "sudo")))))))))

(use-package ediff
    :custom
    ((ediff-window-setup-function 'ediff-setup-windows-plain)
     (ediff-diff-options "-w")
     (ediff-split-window-function 'split-window-horizontally)))

(use-package zenburn-theme
    :demand t
    :config
    (load-theme 'zenburn t))

(use-package windswap
    :demand t
    :bind
    (("H-n" . windswap-down)
     ("H-p" . windswap-up)
     ("H-b" . windswap-left)
     ("H-f" . windswap-right))
    :config
    (windswap-default-keybindings))

(use-package which-key
    :demand t
    :config
    (which-key-mode)
    (setq which-key-idle-delay 1)
    (which-key-setup-minibuffer))

(use-package kubernetes
    :ensure t
    :commands (kubernetes-overview)
    :config
    (setq kubernetes-poll-frequency 3600
          kubernetes-redraw-frequency 3600))

(use-package terraform-mode
    :ensure t
    :config
    (custom-set-variables
     '(terraform-indent-level 4)))

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

(use-package flycheck-pos-tip
    :ensure t
    :config
    (use-package flycheck
        :config
        (setq flycheck-display-errors-function 'flycheck-pos-tip-error-messages)))

(use-package yasnippet
    :demand t
    :config
    (add-to-list 'yas-snippet-dirs (concat (getenv "PROJECTS_DIR") "/" "yasnippet-snippets"))
    (yas-global-mode 1))

(use-package yasnippet-snippets
    :after yasnippet
    :demand t)

(use-package ag)

(use-package real-auto-save
    :disabled ;; temporarily disabled to test TRAMP
    :hook (prog-mode . real-auto-save-mode)
    :config (setq real-auto-save-interval 1))


;; fix issue with Magit
;; See: https://libredd.it/r/emacs/comments/po9cfj/magit_commands_broken/?sort=new
(use-package project 
    :demand t
    :config
    (setq project-switch-commands t))

(use-package magit
    :bind (("C-x g" . magit-status))
    :config
    (setq magit-diff-refine-hunk 'all
          magit-diff-refine-ignore-whitespace nil
          magit-diff-paint-whitespace t
          magit-diff-paint-whitespace-lines 'all
          magit-diff-highlight-trailing t))

;; manually disable that mode if not using Gerrit.
(use-package magit-gerrit
    :disabled t
    :after magit
    :demand t
    :config
    (setq-default magit-gerrit-ssh-creds "gabriel.armelin@airties.com@gerrit.tooling.wifi-doctor.org:29418"
                  magit-gerrit-remote "origin"))

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

(use-package parinfer-rust-mode
    :hook emacs-lisp-mode
    :init
    (setq parinfer-rust-auto-download t))

(use-package org
    :straight
    (org :type git
         :repo "https://git.savannah.gnu.org/git/emacs/org-mode.git"
         :local-repo "org"
         :depth full
         :pre-build (straight-recipes-org-elpa--build)
         :build (:not autoloads)
         :files (:defaults "lisp/*.el" ("etc/styles/" "etc/styles/*")))
    :config
    (setq org-image-actual-width nil)
    (setq org-startup-with-inline-images t)
    (setq org-confirm-babel-evaluate nil)
    (setq org-hide-emphasis-markers t)
    (setq org-log-done 'time)
    (setq org-startup-with-inline-images t)
    (setq org-todo-keywords
          '((sequence "TODO(t)" "IN-PROGRESS(p)" "PAUSED(u)" "|" "DONE(d)" "CANCELLED(c)")))    
    (org-babel-do-load-languages 'org-babel-load-languages
                                 (append org-babel-load-languages
                                         '((shell     . t)
                                           (js . t))))
    :hook
    (org-mode . visual-line-mode))
    ;; (org-mode . variable-pitch-mode))

;; (custom-theme-set-faces
;;  'user
;;  '(variable-pitch ((t (:family "ETBembo" :height 180 :weight thin))))
;;  '(fixed-pitch ((t ( :family "Fira Code" :height 160)))))

;; (custom-theme-set-faces
;;  'user
;;  '(org-block ((t (:inherit fixed-pitch))))
;;  '(org-code ((t (:inherit (shadow fixed-pitch)))))
;;  '(org-document-info ((t (:foreground "dark orange"))))
;;  '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
;;  '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
;;  '(org-link ((t (:foreground "royal blue" :underline t))))
;;  '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
;;  '(org-property-value ((t (:inherit fixed-pitch))) t)
;;  '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
;;  '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
;;  '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
;;  '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))

(use-package org-superstar
    :after org
    :hook (org-mode . org-superstar-mode)
    :config
    (org-superstar-configure-like-org-bullets))

;; OSX: please install pngpaste from Homebrew
(use-package org-download
    :after org
    :demand t
    :bind
    (:map org-mode-map
          (("s-Y" . org-download-screenshot)
           ("s-y" . org-download-yank)
           ("s-C" . org-download-clipboard))))

;; (let* ((variable-tuple
;;         (cond ((x-list-fonts "ETBembo")         '(:font "ETBembo"))
;;               ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
;;               ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
;;               ((x-list-fonts "Verdana")         '(:font "Verdana"))
;;               ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
;;               (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
;;        (base-font-color     (face-foreground 'default nil 'default))
;;        (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

;;     (custom-theme-set-faces
;;      'user
;;      `(org-level-8 ((t (,@headline ,@variable-tuple))))
;;      `(org-level-7 ((t (,@headline ,@variable-tuple))))
;;      `(org-level-6 ((t (,@headline ,@variable-tuple))))
;;      `(org-level-5 ((t (,@headline ,@variable-tuple))))
;;      `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
;;      `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
;;      `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
;;      `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75))))
;;      `(org-document-title ((t (,@headline ,@variable-tuple :height 2.0 :underline nil))))))

(use-package org-roam
    :after org
    :custom
    (org-roam-directory (concat (getenv "PROJECTS_DIR") "/" "knowledge-base"))
    :config
    (setq org-return-follows-link t)
    (org-roam-setup)
    :bind (("C-c n f" . org-roam-node-find)
           ("C-c n r" . org-roam-node-random)		    
           (:map org-mode-map
                 (("C-c n i" . org-roam-node-insert)
                  ("C-c n o" . org-id-get-create)
                  ("C-c n t" . org-roam-tag-add)
                  ("C-c n a" . org-roam-alias-add)
                  ("C-c n l" . org-roam-buffer-toggle)))))

(use-package deft
    :after org
    :bind
    ("C-c n d" . deft)
    :custom
    (deft-recursive t)
    (deft-use-filter-string-for-filename t)
    (deft-default-extension "org")
    (deft-directory org-roam-directory))

(use-package duplicate-thing
    :bind ("M-c" . duplicate-thing))

(use-package pip-requirements)

(use-package dockerfile-mode
    :config
    (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

(use-package docker-compose-mode)

(use-package docker
    :ensure t
    :bind ("C-c d" . docker))

(use-package symbol-overlay
    :bind
    (("C-c h" . symbol-overlay-put)
     ("M-n" . symbol-overlay-switch-forward)
     ("M-p" . symbol-overlay-switch-backward)
     ("<f7>" . symbol-overlay-mode)
     ("<f8>" . symbol-overlay-remove-all))
    :config
    (set-face-background 'symbol-overlay-default-face "#606060"))

(use-package dired-filter)

(use-package dired-narrow
    :after dired
    :demand t
    )

(use-package dired-collapse
    :after dired
    :demand t
    )

(use-package dired-rainbow
    :after dired
    :demand t
    :config
    (progn
        (dired-rainbow-define-chmod directory "#6cb2eb" "d.*")
        (dired-rainbow-define html "#eb5286" ("css" "less" "sass" "scss" "htm" "html" "jhtm" "mht" "eml" "mustache" "xhtml"))
        (dired-rainbow-define xml "#f2d024" ("xml" "xsd" "xsl" "xslt" "wsdl" "bib" "json" "msg" "pgn" "rss" "yaml" "yml" "rdata"))
        (dired-rainbow-define document "#9561e2" ("docm" "doc" "docx" "odb" "odt" "pdb" "pdf" "ps" "rtf" "djvu" "epub" "odp" "ppt" "pptx"))
        (dired-rainbow-define markdown "#ffed4a" ("org" "etx" "info" "markdown" "md" "mkd" "nfo" "pod" "rst" "tex" "textfile" "txt"))
        (dired-rainbow-define database "#6574cd" ("xlsx" "xls" "csv" "accdb" "db" "mdb" "sqlite" "nc"))
        (dired-rainbow-define media "#de751f" ("mp3" "mp4" "MP3" "MP4" "avi" "mpeg" "mpg" "flv" "ogg" "mov" "mid" "midi" "wav" "aiff" "flac"))
        (dired-rainbow-define image "#f66d9b" ("tiff" "tif" "cdr" "gif" "ico" "jpeg" "jpg" "png" "psd" "eps" "svg"))
        (dired-rainbow-define log "#c17d11" ("log"))
        (dired-rainbow-define shell "#f6993f" ("awk" "bash" "bat" "sed" "sh" "zsh" "vim"))
        (dired-rainbow-define interpreted "#38c172" ("py" "ipynb" "rb" "pl" "t" "msql" "mysql" "pgsql" "sql" "r" "clj" "cljs" "scala" "js"))
        (dired-rainbow-define compiled "#4dc0b5" ("asm" "cl" "lisp" "el" "c" "h" "c++" "h++" "hpp" "hxx" "m" "cc" "cs" "cp" "cpp" "go" "f" "for" "ftn" "f90" "f95" "f03" "f08" "s" "rs" "hi" "hs" "pyc" ".java"))
        (dired-rainbow-define executable "#8cc4ff" ("exe" "msi"))
        (dired-rainbow-define compressed "#51d88a" ("7z" "zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar"))
        (dired-rainbow-define packaged "#faad63" ("deb" "rpm" "apk" "jad" "jar" "cab" "pak" "pk3" "vdf" "vpk" "bsp"))
        (dired-rainbow-define encrypted "#ffed4a" ("gpg" "pgp" "asc" "bfe" "enc" "signature" "sig" "p12" "pem"))
        (dired-rainbow-define fonts "#6cb2eb" ("afm" "fon" "fnt" "pfb" "pfm" "ttf" "otf"))
        (dired-rainbow-define partition "#e3342f" ("dmg" "iso" "bin" "nrg" "qcow" "toast" "vcd" "vmdk" "bak"))
        (dired-rainbow-define vc "#0074d9" ("git" "gitignore" "gitattributes" "gitmodules"))
        (dired-rainbow-define-chmod executable-unix "#38c172" "-.*x.*")
        ))

(use-package diredfl
    :config
    (diredfl-global-mode))

(use-package peep-dired
    :after dired
    :demand t
    :bind (:map dired-mode-map
                ("P" . peep-dired))
    :config
    (setq peep-dired-cleanup-on-disable t))

(use-package dired-git-info
    :after dired
    :bind (:map dired-mode-map (")" . dired-git-info-mode)))

(use-package shell-pop
    :init
    (setq shell-pop-full-span t)
    :bind (("C-c s" . shell-pop)))

(use-package puppet-mode)

(use-package web-mode
    :config
    (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode)))

(use-package lsp-tailwindcss
    :straight (:type git :host github :repo "merrickluo/lsp-tailwindcss"))

(use-package vterm
    :bind (:map vterm-mode-map
                ("M-P" . 'windmove-up)
                ("M-N" . 'windmove-down)
                ("M-F" . 'windmove-right)
                ("M-B" . 'windmove-left)))

;; Evaluation whether the keybind set up to vterm is working or not.
;; In case that keybinding works well then this mode can be removed.
(use-package vterm-toggle
    :bind (("H-z" . vterm-toggle)
           ("H-F" . vterm-toggle-forward)
           ("H-B" . vterm-toggle-backward)))

(use-package multi-vterm)

(use-package projectile
    :disabled
    :init
    (projectile-mode +1)
    :bind (:map projectile-mode-map
                ("s-p" . projectile-command-map)
                ("C-c p" . projectile-command-map)))

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

(use-package helm
    :demand t
    :config
    (helm-autoresize-mode 1)
    (setq helm-M-x-fuzzy-match t)
    (setq helm-buffers-fuzzy-matching t
          helm-recentf-fuzzy-match    t)
    (setq helm-buffer-max-length 60)
    :bind 
    (("M-x" . helm-M-x)
     ("C-x b" . helm-multi-files)
     ("C-x C-f" . helm-find-files)
     ("C-x C-r" . helm-recentf)
     ("C-h a" . helm-apropos)
     ("M-y" . helm-show-kill-ring)
     ("C-x f" . helm-find)))

(use-package helm-fd
    :straight
    (helm-df :type git :host github :repo "lerouxrgd/helm-fd")
    :bind (:map helm-command-map
                ("/" . helm-fd)))

(use-package helm-projectile
    :after projectile
    :demand t
    :config
    (helm-projectile-on)
    (setq projectile-completion-system 'helm)
    ;; helm-projectile: fix a bug as explained in: https://github.com/bbatsov/projectile/issues/1302
    (setq projectile-git-submodule-command nil))


(use-package helm-swoop
    :disabled t
    :after (helm)
    ;; :bind
    ;; (("C-i" . helm-swoop)
    ;;  ("M-I" . helm-swoop-back-to-last-point)
    ;;  ("C-c C-s" . helm-multi-swoop)
    ;;  ;; ("C-x C-s" . helm-multi-swoop-all)
    ;;  :map isearch-mode-map 
    ;;  ;; When doing isearch, hand the word over to helm-swoop
    ;;  ("M-i" . helm-swoop-from-isearch)
    ;;  :map helm-swoop-map 
    ;;  ;; From helm-swoop to helm-multi-swoop-all
    ;;  ("M-i" . helm-multi-swoop-all-from-helm-swoop)
    ;;  ;; Move up and down like isearch
    ;;  ("C-p" . helm-previous-line)
    ;;  ("C-n" . helm-next-line)
    ;;  :map helm-multi-swoop-map
    ;;  ;; Move up and down like isearch
    ;;  ("C-p" . helm-previous-line)
    ;;  ("C-n" . helm-next-line))
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

(use-package helm-ag
    :config
    (setq helm-boring-buffer-regexp-list '("\\` " "\\`\\*Echo Area" "\\`\\*Minibuf")))

(use-package helm-rg
    :demand t
    :config
    (setq helm-rg-default-directory 'git-root)
    :bind
    ("C-x l" . helm-rg)) 

(use-package typescript-mode
    :mode "\\.ts\\'"
    :hook (typescript-mode . lsp))

(use-package restclient
    :demand t
    :mode 
    ("\\.restclient\\'" . restclient-mode))

(use-package ob-restclient
    :demand t
    :after org-mode
    :config
    (org-babel-do-load-languages 'org-babel-load-languages
                                 (append org-babel-load-languages
                                         '((restclient     . t)))))

(use-package jq-mode
    :config
    (org-babel-do-load-languages 'org-babel-load-languages
                                 (append org-babel-load-languages
                                         '((jq . t)))))

(use-package rainbow-delimiters
    :hook (prog-mode . rainbow-delimiters-mode))

(use-package  highlight-indent-guides
    :hook (prog-mode . highlight-indent-guides-mode)
    :custom 
    (highlight-indent-guides-method 'character)
    (highlight-indent-guides-responsive 'top))

;; Enable scala-mode for highlighting, indentation and motion commands
(use-package scala-mode
    :interpreter
    ("scala" . scala-mode)
    :mode   
    (("\\.sc\\'" . scala-mode)))


;;; End: Enable Scala-mode 

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

(use-package js2-mode
    :hook  (js2-mode . (lambda ()
                           (progn (setq js-indent-level 2)
                                  (setq standart-indent 2)
                                  (setq tag-width 2))))
    :mode
    (("\\.js\\'" . js2-mode)
     ("\\.avsc\\'" . js2-mode)))

(use-package json-mode
    :hook (json-mode . (lambda ()
                           (progn (setq js-indent-level 2)
                                  (setq standart-indent 2)
                                  (setq tag-width 2))))
    :mode
    (("\\.template\\'" . json-mode)))

(use-package nodejs-repl
    :bind (:map js-mode-map
                ("C-x C-e" . nodejs-repl-send-last-expression)
                ("C-c C-j" . nodejs-repl-send-line)
                ("C-c C-r" . nodejs-repl-send-region)
                ("C-c C-c" . nodejs-repl-send-buffer)
                ("C-c C-l" . nodejs-repl-load-file)
                ("C-c C-z" . nodejs-repl-switch-to-repl)))

(use-package cider
    :demand t
    :after org
    :init
    (require 'ob-clojure)
    (setq org-babel-clojure-backend 'cider))

(use-package helm-cider
    :after (helm cider))

(use-package clojure-snippets
    :after cider)

(use-package flycheck-clj-kondo
    :after (clojure-mode))

(use-package flycheck-clojure
    :ensure t
    :init
    (add-hook 'after-init-hook 'global-flycheck-mode)
    :config
    (use-package flycheck
        :config
        (flycheck-clojure-setup)))


(use-package groovy-mode
    :mode
    (("\\.gradle\\'" . groovy-mode)))

(use-package elpy
    :init
    (elpy-enable)
    :custom
    (elpy-rpc-virtualenv-path 'current))

(use-package ein
    :init
    (setq ein:output-area-inlined-images t))

(use-package lsp-mode
    :demand t
    ;; Optional - enable lsp-mode automatically in scala files
    :hook  (scala-mode . lsp)
    (lsp-mode . lsp-lens-mode)
    :config
    ;; Uncomment following section if you would like to tune lsp-mode performance according to
    ;; https://emacs-lsp.github.io/lsp-mode/page/performance/
    ;;       (setq gc-cons-threshold 100000000) ;; 100mb
    ;;       (setq read-process-output-max (* 1024 1024)) ;; 1mb
    ;;       (setq lsp-idle-delay 0.500)
    ;;       (setq lsp-log-io nil)
    ;;       (setq lsp-completion-provider :capf)
    (setq lsp-prefer-flymake nil))

;; Add metals backend for lsp-mode
;; Note: metals should be download and compiled manually
(use-package lsp-metals
    :custom
    ;; Metals claims to support range formatting by default but it supports range
    ;; formatting of multiline strings only. You might want to disable it so that
    ;; emacs can use indentation provided by scala-mode.
    (lsp-metals-server-args '("-J-Dmetals.allow-multiline-string-formatting=off"))
    :hook (scala-mode . lsp))

;; (use-package lsp-python-ms
;;   :ensure t
;;   :init (setq lsp-python-ms-auto-install-server t)
;;   :hook (python-mode . (lambda ()
;;                           (require 'lsp-python-ms)
;;                           (lsp))))  ; or lsp-deferred

;; Enable nice rendering of documentation on hover
;;   Warning: on some systems this package can reduce your emacs responsiveness significally.
;;   (See: https://emacs-lsp.github.io/lsp-mode/page/performance/)
;;   In that case you have to not only disable this but also remove from the packages since
;;   lsp-mode can activate it automatically.
(use-package lsp-ui)

(use-package lsp-treemacs
    :after lsp-mode
    :bind (:map lsp-mode-map
                ("C-<f8>" . lsp-treemacs-errors-list)
                ("M-<f8>" . lsp-treemacs-symbols)
                ("s-<f8>" . lsp-treemacs-java-deps-list))
    :init (lsp-treemacs-sync-mode 1))

(use-package dap-mode
    :hook
    ((lsp-mode . dap-mode)
     (lsp-mode . dap-ui-mode)
     (clojure-mode . lsp)
     (clojurec-mode . lsp)
     (clojurescript-mode . lsp)))

;; Use the Debug Adapter Protocol for running tests and debugging
(use-package posframe
    ;; Posframe is a pop-up tool that must be manually installed for dap-mode
    )

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
    (setq lsp-completion-provider :capf)
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

;;; Airties custom scripts
(defun send-to-vterm (beg end)
    (interactive "r")
    (process-send-region "vterm" beg end))

(global-set-key (kbd "C-c C-v") 'send-to-vterm)


;;; My list of Toggles
(defun toggle-line-numbers-mode ()
    "Toggle 'line-numbers-mode'."
    (interactive)
    (global-display-line-numbers-mode (if (bound-and-true-p global-display-line-numbers-mode) -1 1)))

(defun toggle-fill-column-indicator ()
    "Toggle 'display-fill-column-indicator'."
    (interactive)
    (global-display-fill-column-indicator-mode (if (bound-and-true-p global-display-fill-column-indicator-mode) -1 1)))

(defun toggle-whitespace-mode-style ()
    "Toggle 'whitespace-mode' style."
    (interactive)
    ;; (let* ((min '(tabs tab-mark face trailing))
    ;;        (max '(tabs tab-mark spaces space-mark lines lines-tail newline newline-mark empty face trailing))
    ;;        (style (if (equal whitespace-active-style min) max min)))
    ;;     (setq whitespace-style style))
    (whitespace-mode (if (bound-and-true-p whitespace-mode) -1 1)))

(defun toggle-highlight-indentation ()
    "Toggle 'highlight-indentation'."
    (interactive)
    (highlight-indent-guides-mode (if (bound-and-true-p highlight-indent-guides-mode) -1 1)))

(defun toggle-show-paren-mode-style ()
    "Toggle 'show-paren-mode' style."
    (interactive)
    (let* ((min 'parenthesis)
           (max 'expression)
           (style (if (equal show-paren-style min) max min)))
        (setq show-paren-style style)))

(defun toggle-rainbow-delimiters ()
    "Toggle 'rainbow-delimiters-mode'."
    (interactive)
    (rainbow-delimiters-mode (if (bound-and-true-p rainbow-delimiters-mode) -1 1)))

(progn
    (define-prefix-command 'toggles-keymap)
    (define-key toggles-keymap (kbd "<f1>") 'toggle-line-numbers-mode)
    (define-key toggles-keymap (kbd "<f2>") 'toggle-fill-column-indicator)
    (define-key toggles-keymap (kbd "<f3>") 'toggle-whitespace-mode-style)
    (define-key toggles-keymap (kbd "<f4>") 'toggle-show-paren-mode-style)
    (define-key toggles-keymap (kbd "<f5>") 'toggle-highlight-indentation)
    (define-key toggles-keymap (kbd "<f6>") 'toggle-rainbow-delimiters)
    (define-key toggles-keymap (kbd "<f7>") 'symbol-overlay-mode)
    (define-key toggles-keymap (kbd "<f8>") 'toggle-truncate-lines)
    )

(global-set-key (kbd "<f12>") toggles-keymap)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("e6df46d5085fde0ad56a46ef69ebb388193080cc9819e2d6024c9c6e27388ba9" default))
 '(package-selected-packages
   '(indent-guide aggressive-indent rainbow-delimiters-mode rainbow-delimiters ein elpy cider lsp-python-ms python-mode magit-gerrit ag auto-save-visited auto-save-visited-mode symbol-overlay dired-rainbow dired-collapse dired-narrow dired-filter peep-dired groovy-mode js2-mode dired dired-git-info dired-git-info-mode docker-compose-mode dockerfile-mode org-superstart helm-swoop helm-projectile projectile buffer-move nyan-mode which-key git-messenger multi-vterm vterm-toggle shell-pop vterm company-mode helm company move-text smartparens multiple-cursors expand-region vlf yasnippet-snippets magit real-auto-save use-package))
 '(tramp-default-method "ssh")
 '(tramp-default-user "gabriel.armelin")
 '(warning-suppress-types '((comp) (comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
