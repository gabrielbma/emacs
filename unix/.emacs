;; Starts emacs server. It should be used with an additional bash script in order to avoid starting server everytime a new Emacs instance is created.
;; For more information check out terdon's answear on:
;;  https://superuser.com/questions/462451/how-to-open-a-file-from-bash-command-line-in-the-already-open-emacs-instead-of-a
;; Note: I ammend his answear with the correct path for emacs and by adding -c options to emacsclient. I placed the script inside the zsh custom configuration script.
(server-start)

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

;; ;; runs emacs in client mode
;; (require 'server)
;; (unless (server-running-p)
;;   (server-start))

;;;;;;  Add Ons ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)
(add-to-list 'package-archives
             ; warning not using the MELPA stable. To use MELPA stable repo, replace melpa with melpa-stable
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

;; enable elpy
(elpy-enable)
(setq elpy-rpc-virtualenv-path 'current)

;; Explicitly add company-mode with yasnippet support to company-backends
;; This is a workaround which makes company mode exhibis
;; a list of yasnippet's snippets:
;; https://github.com/jorgenschaefer/elpy/issues/530
(setq elpy-modules (delq 'elpy-module-company elpy-modules))
(add-hook 'python-mode-hook
          (lambda ()
            ;; explicitly load company for the occasion when the deferred
            ;; loading with use-package hasn't kicked in yet
            (company-mode)
            (add-to-list 'company-backends
                         (company-mode/backend-with-yas 'elpy-company-backend))))


;;; Use flycheck instead of flymake
(when (load "flycheck" t t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules)) (add-hook 'elpy-mode-hook 'flycheck-mode))

;;; Auto-format code on save using Black
(add-hook 'elpy-mode-hook (lambda ()
                            (add-hook 'before-save-hook
                                      'elpy-black-fix-code nil t)))

;; real-auto-save
(require 'real-auto-save)
(add-hook 'prog-mode-hook 'real-auto-save-mode)
(setq real-auto-save-interval 1)

;; magit mode
(global-set-key (kbd "C-x g") 'magit-status)

;;AucTex and Preview-Latex
;;(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
;;(setq reftex-plug-into-AUCTeX t)
;;(setq-default ispell-program-name "aspell")  
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)
(setq TeX-PDF-mode t)
;; (add-to-list 'exec-path "/Library/TeX/texbin")

(setenv "PATH" (concat (getenv "PATH") ":/Library/TeX/texbin"))
(setq exec-path (append exec-path '("/Library/TeX/texbin")))
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin/"))
(setq exec-path (append exec-path '("/usr/local/bin/")))

;; 
;; (getenv "PATH")
;;  (setenv "PATH"
;;          (concat
;;           "/Library/TeX/texbin/" ":"
;; (getenv "PATH")))


;; use Skim as default pdf viewer
;; Skim's displayline is used for forward search (from .tex to .pdf)
;; option -b highlights the current line; option -g opens Skim in the background  
(setq TeX-view-program-selection '((output-pdf "PDF Viewer")))
(setq TeX-view-program-list
      '(("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))


;; To RefTex work properly with AUCTex
(setq reftex-plug-into-AUCTeX t)

(require 'zenburn-theme)
(load-theme 'zenburn t)

(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js$". js2-mode))

(require 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook  'emmet-mode)
(define-key emmet-mode-keymap (kbd "C-j") nil)
(define-key emmet-mode-keymap (kbd "M-e") 'emmet-expand-line)

(require 'flycheck)
(global-flycheck-mode)
;; (exec-path-from-shell-initialize)
;; (add-hook 'after-init-hook #'global-flycheck-mode)
                                        ;enable running flycheck from Finder
;; (when (memq window-system '(mac ns))
;;   (exec-path-from-shell-initialize))
(add-hook 'js-mode-hook
          (lambda () (flycheck-mode t)))


(require 'yasnippet)
;; (require 'yasnippet-snippets)
;; (setq yas-snippet-dirs '("/Users/gabrielbma/Projects/yasnippet-snippets"))
(add-to-list 'yas-snippet-dirs '"/Users/gabrielbma/Projects/yasnippet-snippets")
(yas-global-mode 1)

(require 'yasnippet-snippets)

;; markdown-mode
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.txt\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.Rmd\\'" . markdown-mode))


;; ;; ess
;; (add-to-list 'load-path "/Users/gabrielbma/Projects/ess/lisp/")
;; (load "ess-site")
;; (setenv "PATH"
;;         (concat
;;          (getenv "PATH") ":"         
;;          "/usr/local/bin" ":" 
;;          "/usr/texbin"))

;; (setq exec-path (append exec-path '("/usr/local/bin" "/usr/texbin")))


;; projectile
(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(add-to-list 'projectile-other-file-alist '("html" "js")) ;; switch from html -> js
(add-to-list 'projectile-other-file-alist '("js" "html")) ;; switch from js -> html

(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(global-company-mode)
(setq company-idle-delay 0)
(setq company-show-numbers t)
(setq company-minimum-prefix-length 2)
(setq company-dabbrev-downcase nil)
(setq company-dabbrev-other-buffers t)
(setq company-auto-complete nil)
(setq company-dabbrev-code-other-buffers 'all)
(setq company-dabbrev-code-everywhere t)
(setq company-dabbrev-code-ignore-case t)

;; company-math
;; require company-math: global activation of the unicode symbol completion 
;; (add-to-list 'company-backends 'company-math-symbols-unicode) temp-disabled

;; company-auctex
;; (company-auctex-init) temp-disabled                  ;

;; ;; yasnippet-temp-disabled
;; company-yasnippet
;; Add yasnippet support for all company backends
;; https://github.com/syl20bnr/spacemacs/pull/179
(defvar company-mode/enable-yas t "Enable yasnippet for all backends.")
(defun company-yasnippet-or-completion ()
  "Solve company yasnippet conflicts."
  (interactive)
  (let ((yas-fallback-behavior
         (apply 'company-complete-common nil)))
    (yas-expand)))

(add-hook 'company-mode-hook
          (lambda ()
            (substitute-key-definition
             'company-complete-common
             'company-yasnippet-or-completion
             company-active-map)))
(defun company-mode/backend-with-yas (backend)
  (if (or (not company-mode/enable-yas) (and (listp backend)    (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))
(setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))

;; temp-disabled
;; (require 'company-statistics)
;; (company-statistics-mode)

;; temp-disabled
;; (company-quickhelp-mode 1)

;; (setq-local company-backends 
;;     (append '((company-yasnippet company-R-args company-R-objects)) 
;;              company-backends))


(require 'helm-config)
(helm-mode 1)
(global-set-key (kbd "C-x b") 'helm-multi-files)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-r") 'helm-recentf)
(global-set-key (kbd "C-h a") 'helm-apropos)
(global-set-key (kbd "M-y") 'helm-show-kill-ring) ;; replaces emacs' kill ring

(helm-autoresize-mode 1)
(setq helm-M-x-fuzzy-match t)
(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t)

;; helm-company temp-disabled
;; (autoload 'helm-company "helm-company") ;; Not necessary if using ELPA package
;; (eval-after-load 'company
;;   '(progn
;;      (define-key company-mode-map (kbd "C-:") 'helm-company)
;;      (define-key company-active-map (kbd "C-:") 'helm-company)))

;; ;; yasnippet-temp-disabled
;; ;; helm-c-yasnippet
;; (require 'helm-c-yasnippet)
;; (setq helm-yas-space-match-any-greedy t)
;; (global-set-key (kbd "C-c y") 'helm-yas-complete)

;; helm-projectile
(require 'helm-projectile)
(helm-projectile-on)

;; helm-projectile: fix a bug as explained in: https://github.com/bbatsov/projectile/issues/1302
(setq projectile-git-submodule-command nil)
;; (global-set-key (kbd "C-x C-f") 'helm-projectile)

;; helm-swoop
(require 'helm-swoop)
(global-set-key (kbd "M-i") 'helm-swoop)
(global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)
(global-set-key (kbd "C-c M-i") 'helm-multi-swoop)
(global-set-key (kbd "C-x M-i") 'helm-multi-swoop-all)

;; When doing isearch, hand the word over to helm-swoop
(define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
;; From helm-swoop to helm-multi-swoop-all
(define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)
;; Move up and down like isearch
(define-key helm-swoop-map (kbd "C-r") 'helm-previous-line)
(define-key helm-swoop-map (kbd "C-s") 'helm-next-line)
(define-key helm-multi-swoop-map (kbd "C-r") 'helm-previous-line)
(define-key helm-multi-swoop-map (kbd "C-s") 'helm-next-line)
;; If you prefer fuzzy matching
(setq helm-swoop-use-fuzzy-match t)
;; Optional face for line numbers
;; Face name is `helm-swoop-line-number-face`
;; (setq helm-swoop-use-line-number-face t)
;; If this value is t, split window inside the current window
(setq helm-swoop-split-with-multiple-windows t)
;; Split direcion. 'split-window-vertically or 'split-window-horizontally
(setq helm-swoop-split-direction 'split-window-vertically)
;; If nil, you can slightly boost invoke speed in exchange for text color
(setq helm-swoop-speed-or-color t)

(require 'helm-ag)
(require 'helm-rg)


;; autolad octave mode for *.m-files
       (autoload 'octave-mode "octave-mod" nil t)
       (setq auto-mode-alist (cons '("\\.m$" . octave-mode) auto-mode-alist))

(require 'vlf-setup)

(require 'web-beautify)
(eval-after-load 'js2-mode
  '(define-key js2-mode-map (kbd "C-c b") 'web-beautify-js))
;; Or if you're using 'js-mode' (a.k.a 'javascript-mode')
(eval-after-load 'js
  '(define-key js-mode-map (kbd "C-c b") 'web-beautify-js))

(eval-after-load 'json-mode
  '(define-key json-mode-map (kbd "C-c b") 'web-beautify-js))

(eval-after-load 'sgml-mode
  '(define-key html-mode-map (kbd "C-c b") 'web-beautify-html))

(eval-after-load 'web-mode
  '(define-key web-mode-map (kbd "C-c b") 'web-beautify-html))

(eval-after-load 'css-mode
  '(define-key css-mode-map (kbd "C-c b") 'web-beautify-css))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(nyan-mode t)
 '(nyan-wavy-trail t)
 '(package-selected-packages
   '(magit yasnippet-snippets pip-requirements duplicate-thing org-superstar which-key helm-descbinds wgrep helm-rg helm-ag ag smart-jump nyan-mode dired-git-info diredfl docker-compose-mode dockerfile-mode matlab-mode projectile flycheck real-auto-save move-text engine-mode zenburn-theme smartparens python-mode markdown-mode js2-mode helm-swoop helm-projectile)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; (require 'iy-go-to-char)
;; (add-to-list 'mc/cursor-specific-vars 'iy-go-to-char-start-pos)
;; (global-set-key (kbd "C-c f") 'iy-go-to-char)
;; (global-set-key (kbd "C-c F") 'iy-go-to-char-backward)
;; (global-set-key (kbd "C-c ;") 'iy-go-to-or-up-to-continue)
;; (global-set-key (kbd "C-c ,") 'iy-go-to-or-up-to-continue-backward)

(require 'engine-mode)
(engine-mode t)
(defengine amazon
  "http://www.amazon.com/s/ref=nb_sb_noss?url=search-alias%3Daps&field-keywords=%s")

(defengine duckduckgo
  "https://duckduckgo.com/?q=%s"
  :keybinding "d")

(defengine github
  "https://github.com/search?ref=simplesearch&q=%s")

(defengine google
  "http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s"
  :keybinding "g")

(defengine google-images
  "http://www.google.com/images?hl=en&source=hp&biw=1440&bih=795&gbv=2&aq=f&aqi=&aql=&oq=&q=%s")

(defengine google-maps
  "http://maps.google.com/maps?q=%s"
  :docstring "Mappin' it up.")

(defengine project-gutenberg
  "http://www.gutenberg.org/ebooks/search/?query=%s")

(defengine rfcs
  "http://pretty-rfc.herokuapp.com/search?q=%s")

(defengine stack-overflow
  "https://stackoverflow.com/search?q=%s")

(defengine twitter
  "https://twitter.com/search?q=%s")

(defengine wikipedia
  "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s"
  :keybinding "w"
  :docstring "Searchin' the wikis.")

(defengine wiktionary
  "https://www.wikipedia.org/search-redirect.php?family=wiktionary&language=en&go=Go&search=%s")

(defengine wolfram-alpha
  "http://www.wolframalpha.com/input/?i=%s")

(defengine youtube
  "http://www.youtube.com/results?aq=f&oq=&search_query=%s")

;; smartparens
(require 'smartparens-config)
(smartparens-global-mode t)

;; keybinds used by Smartparens' author
(define-key smartparens-mode-map (kbd "C-M-f") 'sp-forward-sexp)
(define-key smartparens-mode-map (kbd "C-M-b") 'sp-backward-sexp)

(define-key smartparens-mode-map (kbd "C-M-d") 'sp-down-sexp)
(define-key smartparens-mode-map (kbd "C-M-a") 'sp-backward-down-sexp)
(define-key smartparens-mode-map (kbd "C-S-d") 'sp-beginning-of-sexp)
(define-key smartparens-mode-map (kbd "C-S-a") 'sp-end-of-sexp)

(define-key smartparens-mode-map (kbd "C-M-e") 'sp-up-sexp)
(define-key smartparens-mode-map (kbd "C-M-u") 'sp-backward-up-sexp)
(define-key smartparens-mode-map (kbd "C-M-t") 'sp-transpose-sexp)

(define-key smartparens-mode-map (kbd "C-M-n") 'sp-forward-hybrid-sexp)
(define-key smartparens-mode-map (kbd "C-M-p") 'sp-backward-hybrid-sexp)

(define-key smartparens-mode-map (kbd "C-M-k") 'sp-kill-sexp)
(define-key smartparens-mode-map (kbd "C-M-w") 'sp-copy-sexp)

(define-key smartparens-mode-map (kbd "C-M-<delete>") 'sp-unwrap-sexp)
(define-key smartparens-mode-map (kbd "C-M-<backspace>") 'sp-backward-unwrap-sexp)


;; (eval-after-load 'smartparens-mode
;;   '(progn 
;;      (define-key smartparens-mode-map (kbd "C-M-k") 'sp-kill-sexp)
;;      (define-key smartparens-mode-map (kbd "C-M-a") 'sp-beginning-of-sexp)
;;      (define-key smartparens-mode-map (kbd "C-M-f") 'sp-forward-sexp)
;;      (define-key smartparens-mode-map (kbd "C-M-b") 'sp-backward-sexp)
;;      (define-key smartparens-mode-map (kbd "C-M-n") 'sp-next-sexp)
;;      (define-key smartparens-mode-map (kbd "C-M-p") 'sp-previous-sexp)
;;      (define-key smartparens-mode-map (kbd "M-[") 'sp-unwrap-sexp)
;;      (define-key smartparens-mode-map (kbd "C-M-w") 'sp-copy-sexp)))

;; move-text mode
(move-text-default-bindings)

;; dockerfile-mode
(require 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

;; docker-compose-mode
(require 'docker-compose-mode)

;; diredfl
(require 'diredfl)
(diredfl-global-mode)

;; dired-git-info-mode
(with-eval-after-load 'dired
  (define-key dired-mode-map ")" 'dired-git-info-mode))
;;; enable autmomatically in every Dired buffer
;; (add-hook 'dired-after-readin-hook 'dired-git-info-auto-enable)


;; dumb-jump 
(setq dumb-jump-prefer-searcher 'rg)
(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
(eval-after-load 'dumb-jump-mode
  '(progn 
     (define-key dumb-jump-mode-map (kbd "M-g o") 'dumb-jump-go-other-window)
     (define-key dumb-jump-mode-map (kbd "M-g j") 'dumb-jump-go)
     (define-key dumb-jump-mode-map (kbd "M-g q") 'dumb-jump-quick-look)
     (setq dumb-jump-selector 'helm)
     (setq dumb-jump-prefer-searcher 'ag)))


;; smart-jump (disable because it changes the keybind M-P)
;; (require 'smart-jump)
;; (smart-jump-setup-default-registers)

;; I am going to use rp for searching but I need to investigate which mode 
;; is the best at the moment.
;; This article discuss the searching problem very well:
;; https://with-emacs.com/posts/tutorials/search-and-replacement-techniques/
;; and this is a good thread to have an impression of what people are doing:
;; https://www.reddit.com/r/emacs/comments/dgglux/search_and_replace_options/
;; Options for rg are: deadgrep, helm-grep, helm-rg, rg.el

;; nyan-mode
(require 'nyan-mode)
(setq nyan-bar-length 15)

;; which-key
(require 'which-key)
(which-key-mode)
(setq which-key-idle-delay 0.3)
(which-key-setup-minibuffer)
;;; .emacs ends here

;; org-superstar
(require 'org-superstar)
(add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))

;; duplicate-thing
(require 'duplicate-thing)
(global-set-key (kbd "M-c") 'duplicate-thing)

(require 'pip-requirements)


