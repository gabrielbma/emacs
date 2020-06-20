
;; Starts emacs server. It should be used with an additional bash script in order to avoid starting server everytime a new Emacs instance is created.
;; For more information check out terdon's answear on:
;;  https://superuser.com/questions/462451/how-to-open-a-file-from-bash-command-line-in-the-already-open-emacs-instead-of-a
;; Note: I ammend his answear with the correct path for emacs and by adding -c options to emacsclient. I placed the script inside the zsh custom configuration script.
(server-start)

;; Look and feel:
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(setq inhibit-startup-message t)
(blink-cursor-mode 0)
(show-paren-mode 1)
(column-number-mode 1)
(put 'narrow-to-region 'disabled nil)
(defalias 'yes-or-no-p 'y-or-n-p)

;;(prefer-coding-system 'utf-8-unix)

;; winner-mode to restory window position
;; C-c <left> to restore the previous window configuration
;; C-c <right> to return to the most recent configuration
(winner-mode 1)

;; Recent files
(recentf-mode 1)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

;; Prevent tab identation
;; Ref: http://stackoverflow.com/questions/45861/how-do-i-get-js2-mode-to-use-spaces-instead-of-tabs-in-emacs

(setq-default indent-tabs-mode nil)
;; (setq js2-mode-hook
;;   '(lambda () (progn
;;     (set-variable 'indent-tabs-mode nil))))


;; set keys for Apple keyboard, for emacs in OS X;
;; See: http://ergoemacs.org/emacs/emacs_hyper_super_keys.html
(setq mac-command-modifier 'meta) ; make cmd key do Meta
(setq mac-option-modifier 'super) ; make opt key do Super
(setq mac-control-modifier 'control) ; make Control key do Control
(setq ns-function-modifier 'hyper)  ; make Fn key do Hyper

;; window management
(global-set-key (kbd "M-P") 'windmove-up)
(global-set-key (kbd "M-N") 'windmove-down)
(global-set-key (kbd "M-F") 'windmove-right)
(global-set-key (kbd "M-B") 'windmove-left)

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

;;;;;;  Add Ons
(require 'package)
(add-to-list 'package-archives
             ; warning not using the MELPA stable. To use MELPA stable repo, replace melpa with melpa-stable
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

;; enable elpy
(elpy-enable)
(setq elpy-rpc-virtualenv-path 'current)

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

;; disable to use Helm
;; (require 'ido)
;; (ido-mode t)

(require 'zenburn-theme)
(load-theme 'zenburn t)

;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js$". js2-mode))

(require 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook  'emmet-mode)
(define-key emmet-mode-keymap (kbd "C-j") nil)
(define-key emmet-mode-keymap (kbd "M-e") 'emmet-expand-line)

;; disable to switch to company-mode
;;(require 'ac-emmet)
;;(add-hook 'sgml-mode-hook 'ac-emmet-html-setup)
;;(add-hook 'css-mode-hook 'ac-emmet-css-setup)

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

(setq yas-snippet-dirs '("/Users/gabrielbma/Projects/bitbucket/yasnippet-snippets"))

(yas-global-mode 1)

;;;;;;;;; completion key
;; (define-key yas-minor-mode-map [(tab)]        nil)
;; (define-key yas-minor-mode-map (kbd "TAB")    nil)
;; (define-key yas-minor-mode-map (kbd "<tab>")  nil)

;; (defun try-flyspell (arg)
;;   (if (nth 4 (syntax-ppss))
;;       (call-interactively 'flyspell-correct-word-before-point)
;;   nil))

;; (setq hippie-expand-try-functions-list
;;       '(try-flyspell
;;         yas-hippie-try-expand 
;;         try-expand-dabbrev-visible 
;;         (lambda (arg) (call-interactively 'company-complete))

;;         ))

;; (global-set-key (kbd "<tab>")  'hippie-expand)
;; (global-set-key (kbd "TAB")  'hippie-expand)
;;;;;;;;;;

;; markdown-mode
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.txt\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.Rmd\\'" . markdown-mode))

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

(define-key smartparens-mode-map (kbd "M-<delete>") 'sp-unwrap-sexp)
(define-key smartparens-mode-map (kbd "M-<backspace>") 'sp-backward-unwrap-sexp)
(define-key smartparens-mode-map (kbd "C-M-f") 'sp-forward-sexp)
(define-key smartparens-mode-map (kbd "C-M-b") 'sp-backward-sexp)


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

;; ;; ess
;; (add-to-list 'load-path "/Users/gabrielbma/Projects/ess/lisp/")
;; (load "ess-site")
;; (setenv "PATH"
;;         (concat
;;          (getenv "PATH") ":"         
;;          "/usr/local/bin" ":" 
;;          "/usr/texbin"))

;; (setq exec-path (append exec-path '("/usr/local/bin" "/usr/texbin")))


;; python-mode is using elpy to be set, don't forget to enable pyvenv-activate


;; projectile
(projectile-mode +1)
;; (setq projectile-enable-caching t)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(add-to-list 'projectile-other-file-alist '("html" "js")) ;; switch from html -> js
(add-to-list 'projectile-other-file-alist '("js" "html")) ;; switch from js -> html

;; helm-projectile
(require 'helm-projectile)
(helm-projectile-on)

;; helm-projectile: fix a bug as explained in: https://github.com/bbatsov/projectile/issues/1302
(setq projectile-git-submodule-command nil)

;; Disable to use Helm
;; flx
;; (require 'flx-ido)
;; (ido-mode 1)
;; (ido-everywhere 1)
;; (flx-ido-mode 1)
;; ;; disable ido faces to see flx highlights.
;; (setq ido-enable-flex-matching t)
;; (setq ido-use-faces nil)


;; company
;; NOTE: I cound not make company-yasnippet work with 
;; ess-mode. So, to evaluate a yasnippet snippet
;; one must use M-x company-yasnippet.
(require 'company)
;(add-hook 'after-init-hook 'global-company-mode)
(global-company-mode 1)

(setq company-idle-delay 0)
(setq company-show-numbers t)
(setq company-minimum-prefix-length 2)
(setq company-dabbrev-downcase nil)
(setq company-dabbrev-other-buffers t)
(setq company-auto-complete nil)
(setq company-dabbrev-code-other-buffers 'all)
(setq company-dabbrev-code-everywhere t)
(setq company-dabbrev-code-ignore-case t)

(add-to-list 'company-backends 'company-files)

(require 'helm-config)
(helm-mode 1)
(global-set-key (kbd "C-x b") 'helm-multi-files)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-r") 'helm-recentf)

(helm-autoresize-mode 1)
(setq helm-M-x-fuzzy-match t)
(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t)

;; helm-company
(autoload 'helm-company "helm-company") ;; Not necessary if using ELPA package
(eval-after-load 'company
  '(progn
     (define-key company-mode-map (kbd "C-:") 'helm-company)
     (define-key company-active-map (kbd "C-:") 'helm-company)))

;; helm-c-yasnippet
(require 'helm-c-yasnippet)
(setq helm-yas-space-match-any-greedy t)
(global-set-key (kbd "C-c y") 'helm-yas-complete)

;; helm-projectile
(require 'helm-projectile)
(setq projectile-completion-system 'helm)
(setq projectile-switch-project-action 'helm-projectile)
(helm-projectile-on)


;; helm-swoop
(require 'helm-swoop)
(global-set-key (kbd "M-i") 'helm-swoop)
(global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)
(global-set-key (kbd "C-c M-i") 'helm-multi-swoop)
(global-set-key (kbd "C-x M-i") 'helm-multi-swoop-all)

;; company-math
;; require company-math: global activation of the unicode symbol completion 
(add-to-list 'company-backends 'company-math-symbols-unicode)

;; company-auctex
(company-auctex-init)

;; company-yasnippet
;; Add yasnippet support for all company backends
;; https://github.com/syl20bnr/spacemacs/pull/179
(defvar company-mode/enable-yas t "Enable yasnippet for all backends.")

(defun company-mode/backend-with-yas (backend)
  (if (or (not company-mode/enable-yas) (and (listp backend)    (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))

(setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))

(require 'company-statistics)
(company-statistics-mode)

(company-quickhelp-mode 1)

(setq-local company-backends 
    (append '((company-yasnippet company-R-args company-R-objects)) 
             company-backends))

(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

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
 '(package-selected-packages
   (quote
    (projectile flycheck real-auto-save move-text smart-shift engine-mode iy-go-to-char multiple-cursors expand-region web-beautify zenburn-theme vlf smartparens python-mode markdown-mode js2-mode helm-swoop helm-projectile helm-company helm-c-yasnippet ensime emmet-mode company-statistics company-quickhelp company-auctex))))
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

(require 'iy-go-to-char)
(add-to-list 'mc/cursor-specific-vars 'iy-go-to-char-start-pos)
(global-set-key (kbd "C-c f") 'iy-go-to-char)
(global-set-key (kbd "C-c F") 'iy-go-to-char-backward)
(global-set-key (kbd "C-c ;") 'iy-go-to-or-up-to-continue)
(global-set-key (kbd "C-c ,") 'iy-go-to-or-up-to-continue-backward)

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

;; move-text mode
(move-text-default-bindings)
