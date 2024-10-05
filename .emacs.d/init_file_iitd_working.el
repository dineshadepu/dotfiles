;;; package --- Summary
;;; Commentary:


;; -------------- normal functions -------------------
;;; code:
(require 'package)
(setq package-enable-at-startup nil)

(setq package-archives
      '(("elpa" . "http://elpa.gnu.org/packages/")
        ("melpa-stable" . "http://stable.melpa.org/packages/")
        ("melpa" . "http://melpa.org/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;; UI

(setq inhibit-startup-message t)
(tool-bar-mode -1)
(menu-bar-mode -1)
;; (global-display-line-numbers-mode t)
(global-hl-line-mode t)
(setenv "PATH" "/usr/local/bin:/Library/TeX/texbin/:$PATH" t)
(setenv "PATH" "~/.cargo/" t)
(setq exec-path (append exec-path '("/Library/TeX/texbin")))
;; (setq mac-command-modifier 'meta)
(setq ns-function-modifier 'control)
(global-auto-revert-mode 1)
(set-cursor-color "#ffffff")
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
(line-number-mode)
(column-number-mode)
(size-indication-mode)
(fset 'yes-or-no-p #'y-or-n-p)
(setq x-gtk-use-system-tooltips nil)
(setq-default indent-tabs-mode nil)
(global-whitespace-mode -1)
(global-set-key (kbd "M-x") 'helm-M-x)
(define-key global-map [?\s-s] 'save-buffer)
(setq package-check-signature nil)
;; (global-set-key [(super s)] 'save-buffer)

;; backup files nil
(setq make-backup-files nil)

;; open recently closed files
(desktop-save-mode 1)

;; hash or pound key
(global-set-key (kbd "M-3") '(lambda () (interactive) (insert "#")))

;; disable backup
(setq backup-inhibited t)

;; disable auto save
(setq auto-save-default nil)


;; Electric auto pair
(electric-pair-mode t)

(setq temporary-file-directory "~/.emacs.d/tmp/")

;; White space astropy
;; Remove trailing whitespace manually by typing C-t C-w.
(add-hook 'python-mode-hook
          (lambda ()
            (local-set-key (kbd "C-t C-w")
                           'delete-trailing-whitespace)))

;; Automatically remove trailing whitespace when file is saved.
(add-hook 'python-mode-hook
          (lambda()
            (add-hook 'local-write-file-hooks
                      '(lambda()
                         (save-excursion
                           (delete-trailing-whitespace))))))
;; whitespace clean up mode
(add-hook 'before-save-hook 'whitespace-cleanup)

;; Copy to clipboard
;; (defun copy-from-osx ()
;;   (shell-command-to-string "pbpaste"))

;; (defun paste-to-osx (text &optional push)
;;   (let ((process-connection-type nil))
;;     (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
;;       (process-send-string proc text)
;;       (process-send-eof proc))))

;; (setq interprogram-cut-function 'paste-to-osx)
;; (setq interprogram-paste-function 'copy-from-osx)

;; https://github.com/belak/emacs-monokai-pro-theme
(load-theme 'monokai-pro t)

(eval-when-compile
  (require 'use-package))
;; (require 'diminish)
(require 'bind-key)

(use-package exec-path-from-shell
  :ensure t
  ;; :load-path "~/.emacs.d/elisp/exec-path-from-shell/"
  :config
  (push "HISTFILE" exec-path-from-shell-variables)
  (setq exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-initialize))

(use-package rainbow-delimiters
  :ensure t
  :init
  (progn
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
    (add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)))

;; Mouse disable globally

;; -------------------------------------------
;; -------------------------------------------
(mouse-wheel-mode -1)

(global-set-key [wheel-up] 'ignore)
(global-set-key [wheel-down] 'ignore)
(global-set-key [double-wheel-up] 'ignore)
(global-set-key [double-wheel-down] 'ignore)
(global-set-key [triple-wheel-up] 'ignore)
(global-set-key [triple-wheel-down] 'ignore)

;; -------------------------------------------
;; -------------------------------------------

;; Aliases
(defalias 'yes-or-no-p 'y-or-n-p)

;; (use-package whitespace-mode
;;   :disabled t)

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config (which-key-mode)
  )


(setq evil-want-keybinding nil)
(use-package evil
  :ensure t
  :diminish evil
  :init (evil-mode 1)
  (setq evil-want-integration nil)
  (setq evil-insert-state-cursor '((bar . 1) "white")
        evil-visual-state-cursor '(box "dark orange")
        evil-normal-state-cursor '(box "white"))
  :bind (:map
         evil-insert-state-map
         ;; ([left]     . windmove-left)
         ;; ([right]    . windmove-right)
         ;; ([up]       . windmove-up)
         ;; ([down]     . windmove-down)
         ("SPC" . nil)
         :map
         evil-normal-state-map
         (";" . evil-ex)
         (":"	.	evil-repeat-find-char)
         :map    evil-motion-state-map
         ;; ([left]     . windmove-left)
         ;; ([right]    . windmove-right)
         ;; ([up]       . windmove-up)
         ;; ([down]     . windmove-down)
         ))
(fset 'evil-visual-update-x-selection 'ignore)
(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))



(use-package evil-escape
  :ensure t
  :diminish evil-escape
  :init (evil-escape-mode 1))
(setq-default evil-escape-key-sequence "jk")


(use-package evil-leader
  :ensure t
  :diminish evil-leader
  :init (global-evil-leader-mode)
  :config (progn
            (evil-leader/set-leader ",")
              (evil-leader/set-key "r" 'projectile-ripgrep)
              (evil-leader/set-key "b" 'helm-buffers-list)
              (evil-leader/set-key "e" 'helm-find-files)
              (evil-leader/set-key "f" 'helm-projectile-find-file)
              (evil-leader/set-key "t" 'org-babel-tangle)
              (evil-leader/set-key "h" 'helm-mini)
              (evil-leader/set-key "1" 'delete-other-windows)
              (evil-leader/set-key "x" 'helm-filtered-bookmarks)
              (evil-leader/set-key "0" 'delete-window)
              (evil-leader/set-key "3" 'split-window-right)
              (evil-leader/set-key "2" 'split-window-below)
              (evil-leader/set-key "." 'elpy-goto-definition-other-window)
              (evil-leader/set-key "," 'elpy-goto-definition)
              (evil-leader/set-key "i" 'org-ref-insert-ref-link)
              (evil-leader/set-key "l" 'org-ref-helm-insert-label-link)
              (evil-leader/set-key "w" 'ispell-word)
              (evil-leader/set-key "g" 'magit-status)
              (evil-leader/set-key "n" 'windmove-left)
              (evil-leader/set-key "m" 'windmove-right)
              (evil-leader/set-key "p" 'windmove-up)
              (evil-leader/set-key "<SPC>" 'windmove-down)
              (evil-leader/set-key "v" 'pdf-view-goto-page)
              (evil-leader/set-key "k" 'kill-this-buffer)))

;; evil cursor terminal
(use-package evil-terminal-cursor-changer
             :ensure t)
(unless (display-graphic-p)
  (require 'evil-terminal-cursor-changer)
  (evil-terminal-cursor-changer-activate) ; or (etcc-on)
  )

(setq evil-motion-state-cursor 'box)  ; █
(setq evil-visual-state-cursor 'box)  ; █
(setq evil-normal-state-cursor 'box)  ; █
(setq evil-insert-state-cursor 'bar)  ; ⎸
(setq evil-emacs-state-cursor  'hbar) ; _

;;; esc quits
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
  In Delete Selection mode, if the mark is active, just deactivate it;
  then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
    (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)


(use-package avy
  :ensure t
  :config
  (evil-leader/set-key
    "q" 'avy-goto-char-2))

(use-package magit
  :ensure t)

(use-package evil-nerd-commenter
  :ensure t
  :config(progn
           (evilnc-default-hotkeys)))

(use-package restart-emacs
  :ensure t
  :bind (("C-x M-c" . restart-emacs)))

(use-package tex
  :ensure auctex
  :config
  ;; (setq TeX-show-compilation t)
  (add-hook 'LaTeX-mode-hook 'predictive-mode))


;; (use-package elpy
;;   :ensure t
;;   :diminish elpy-mode
;;   :config(progn
;;            ;; (defalias 'workon 'pyvenv-workon)
;;            ;; (elpy-use-cpython "/usr/local/bin/python3")
;;            ;; (setq elpy-rpc-python-command "python3")
;;            ;; (setq 'python-indent-offset 4)
;;            (setq company-minimum-prefix-length 1)
;;            ;; (elpy-use-ipython)
;;            ;; (elpy-clean-modeline)
;;            (elpy-enable)))


(use-package ido
  :ensure t
  :config(progn
           (setq ido-enable-flex-matching t)
           ;; (setq ido-everywhere t)
           (ido-mode 1)))


(use-package flx-ido
  :ensure t
  :init
  (progn
    (setq gc-cons-threshold (* 20 (expt 2 20)) ; megabytes
          ido-use-faces nil))
  :config
  (flx-ido-mode 1))


(use-package helm
  :ensure t
  :bind (("C-x C-f" . helm-find-files)))
;; :init
;; (progn
;;   (require 'helm-config)
;;   (helm-mode 1)
;;   (set-face-attribute 'helm-selection nil
;;                       )))

(use-package golden-ratio                 ; Auto resize windows
  :ensure t
  :diminish golden-ratio-mode
  :config
  (golden-ratio-mode 1)
  (setq golden-ratio-auto-scale t)
  (setq golden-ratio-extra-commands
        (append golden-ratio-extra-commands
                '(evil-window-left
                  evil-window-right
                  evil-window-up
                  evil-window-down))))



(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :config
  (progn
    (require 'smartparens-config)
    (smartparens-global-mode 1)))

(use-package recentf
  :ensure t
  :config
  (require 'recentf)
  (recentf-mode 1)
  (setq recentf-max-menu-items 25)
  (global-set-key "\C-x\ \C-r" 'recentf-open-files))


(use-package fzf
  :ensure t)


(use-package rainbow-delimiters
  :ensure t
  :init
  (progn
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
    (add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)))


(use-package projectile
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))
(projectile-global-mode)
 (use-package helm
     :diminish helm-mode
     :defer t
     :bind (("C-x C-f" . helm-find-files))
     :init
     (progn
        ;; (require 'helm-config)
        (helm-mode 1)
        (set-face-attribute 'helm-selection nil
                            )))
   ;; (global-set-key (kbd "M-x") #'helm-M-x)
   (global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
   (global-set-key (kbd "C-x C-f") #'helm-find-files)
   (helm-mode 1)
   (setq helm-M-x-fuzzy-match t)

   ;; helm mini
   (global-set-key (kbd "C-x b") 'helm-mini)

   (setq helm-buffers-fuzzy-matching t
          helm-recentf-fuzzy-match    t)

   (use-package helm-swoop
     :bind (("M-i" . helm-swoop)))

   ;;(use-package helm-fuzzier)
   ;;(require 'helm-fuzzier)

   ;;(helm-fuzzier-mode 1)
   (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
   (global-set-key (kbd "M-y") 'helm-show-kill-ring)

   ;; helm projectile
   (use-package helm-projectile)
   (helm-projectile-on)

   (setq projectile-completion-system 'helm)
   (setq projectile-switch-project-action 'helm-projectile)


;; kill all oher buffers
(defun nuke-all-buffers ()
  (interactive)
  (mapcar 'kill-buffer (buffer-list))
  (delete-other-windows))

(global-set-key (kbd "C-x K") 'nuke-all-buffers)

(defun kill-other-buffers ()
  (interactive)
  (dolist (buffer (buffer-list))
    (unless (or (eql buffer (current-buffer)) )
      (kill-buffer buffer))))
(global-set-key (kbd "C-x L") 'kill-other-buffers)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#2d2a2e" "#ff6188" "#a9dc76" "#ffd866" "#78dce8" "#ab9df2" "#a1efe4" "#fcfcfa"])
 '(ansi-term-color-vector
   [unspecified "#2d2a2e" "#ff6188" "#a9dc76" "#ffd866" "#78dce8" "#ab9df2" "#a1efe4" "#fcfcfa"])
 '(custom-safe-themes
   '("c8b83e7692e77f3e2e46c08177b673da6e41b307805cd1982da9e2ea2e90e6d7" "8dbbcb2b7ea7e7466ef575b60a92078359ac260c91fe908685b3983ab8e20e3f" "89c50e934a32921ed51da9fa883484a433f32fbc5cf9780860d13322e23edcde" default))
 '(helm-minibuffer-history-key "M-p")
 '(ispell-dictionary nil)
 '(package-selected-packages
   '(monokai-alt-theme yasnippet which-key use-package smartparens scheme-complete restart-emacs rainbow-delimiters racket-mode py-yapf monokai-theme markdown-mode magit helm golden-ratio gnu-elpa-keyring-update fzf flycheck flx-ido exec-path-from-shell evil-terminal-cursor-changer evil-nerd-commenter evil-leader company-statistics company-rtags company-c-headers color-theme avy auctex aggressive-indent)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
