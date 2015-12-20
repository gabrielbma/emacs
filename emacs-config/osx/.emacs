;; Look and fell:
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(setq inhibit-startup-message t)
(blink-cursor-mode 0)
;; (show-paren-mode 1) disable in order to use the same functionality from smartparens-mode
(column-number-mode 1)
(put 'narrow-to-region 'disabled nil)
(defalias 'yes-or-no-p 'y-or-n-p)

;;(prefer-coding-system 'utf-8-unix)

;; Recent files
(recentf-mode 1)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

(winner-mode 1)

;; Prevent tab identation
;; Ref: http://stackoverflow.com/questions/45861/how-do-i-get-js2-mode-to-use-spaces-instead-of-tabs-in-emacs

(setq-default indent-tabs-mode nil)
;; (setq js2-mode-hook
;;   '(lambda () (progn
;;     (set-variable 'indent-tabs-mode nil))))

;; window management
(global-set-key (kbd "M-P") 'windmove-up)
(global-set-key (kbd "M-N") 'windmove-down)
(global-set-key (kbd "M-F") 'windmove-right)
(global-set-key (kbd "M-B") 'windmove-left)

                                        ; Add Ons
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

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

;; disable to switch to company-mode
;; (require 'auto-complete-config)
;;(ac-config-default)

; Disable to use Helm
;; (require 'smex)
;; (smex-initialize)
;; (global-set-key (kbd "M-x") 'smex)
;; (global-set-key (kbd "M-X") 'smex-major-mode-commands)

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

;; flycheck does not officially support Windows.
;; (require 'flycheck)
;; (add-hook 'after-init-hook #'global-flycheck-mode)
                                        ;enable running flycheck from Finder
;; (when (memq window-system '(mac ns))
;;   (exec-path-from-shell-initialize))

(add-hook 'js-mode-hook
          (lambda () (flycheck-mode t)))

(require 'yasnippet)

(setq yas-snippet-dirs '("/Users/gabrielbma/Projects/bitbucket/yasnippet-snippets"
                         "/Users/gabrielbma/Projects/capitaomorte-repo-yasnippet/snippets"
                         "/Users/gabrielbma/Projects/capitaomorte-repo-yasnippet/yasmate"
                         ))

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

(smartparens-global-mode t)

;; ess
(add-to-list 'load-path "/Users/gabrielbma/Projects/ess/lisp/")
(load "ess-site")
(setenv "PATH"
        (concat
         (getenv "PATH") ":"         
         "/usr/local/bin" ":" 
         "/usr/texbin"))

(setq exec-path (append exec-path '("/usr/local/bin" "/usr/texbin")))


;; python-mode
(setq python-shell-interpreter "/usr/local/opt/python3/bin/python3")

;; projectile
(projectile-global-mode)
(setq projectile-enable-caching t)
(add-to-list 'projectile-other-file-alist '("html" "js")) ;; switch from html -> js
(add-to-list 'projectile-other-file-alist '("js" "html")) ;; switch from js -> html

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

(require 'helm-config)
(helm-mode 1)
(global-set-key (kbd "C-x b") 'helm-multi-files)
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
