;; Look and fell:
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

;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; Recent files
(recentf-mode 1)
;; replace for helm-recentf
;;(global-set-key (kbd "C-x C-r") 'recentf-open-files)

(winner-mode 1)

;; Prevent tab identation
;; Ref: http://stackoverflow.com/questions/45861/how-do-i-get-js2-mode-to-use-spaces-instead-of-tabs-in-emacs

(setq-default indent-tabs-mode nil)
;; (setq js2-mode-hook
;;   '(lambda () (progn
;;     (set-variable 'indent-tabs-mode nil))))

;; keybinds for special keys


;; set keys for Apple keyboard, for emacs in OS X;
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


;; magit mode
(global-set-key (kbd "C-x g") 'magit-status)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (rainbow-mode web-mode tide flycheck smart-shift engine-mode iy-go-to-char multiple-cursors expand-region web-beautify zenburn-theme vlf smartparens python-mode markdown-mode js2-mode helm-swoop helm-projectile helm-company helm-c-yasnippet ensime emmet-mode company-statistics company-quickhelp company-auctex))))
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

(require 'smart-shift)
(global-smart-shift-mode 1)

;; Tide mode

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))
;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)
;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)
(add-hook 'typescript-mode-hook #'setup-tide-mode)

; web-mode

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode)) ; html files?

