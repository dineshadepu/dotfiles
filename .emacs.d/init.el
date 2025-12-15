(require 'package)

(setq package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

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

;; disable backup
(setq backup-inhibited t)

;; disable auto save
(setq auto-save-default nil)


;; Electric auto pair
(electric-pair-mode t)

(setq temporary-file-directory "~/.emacs.d/tmp/")

;; Automatically remove trailing whitespace when file is saved.
(add-hook 'python-mode-hook
          (lambda()
            (add-hook 'local-write-file-hooks
                      '(lambda()
                         (save-excursion
                           (delete-trailing-whitespace))))))
;; whitespace clean up mode
(add-hook 'before-save-hook 'whitespace-cleanup)



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


;; Aliases
(defalias 'yes-or-no-p 'y-or-n-p)


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


(use-package git-timemachine
  :ensure t)
(with-eval-after-load 'git-timemachine
  (evil-make-overriding-map git-timemachine-mode-map 'normal)
  ;; force update evil keymaps after git-timemachine-mode loaded
  (add-hook 'git-timemachine-mode-hook #'evil-normalize-keymaps))

(use-package evil-escape
  :ensure t
  :diminish evil-escape
  :init (evil-escape-mode 1))
(setq-default evil-escape-key-sequence "jk")
(setq-default evil-escape-delay 0.2)


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
              (evil-leader/set-key "." 'xref-find-definitions)
              (evil-leader/set-key "," 'xref-go-back)
              (evil-leader/set-key "/" 'xref-find-references)
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


;; (use-package ido
;;   :ensure t
;;   :config(progn
;;            (setq ido-enable-flex-matching t)
;;            ;; (setq ido-everywhere t)
;;            (ido-mode 1)))


;; (use-package flx-ido
;;   :ensure t
;;   :init
;;   (progn
;;     (setq gc-cons-threshold (* 20 (expt 2 20)) ; megabytes
;;           ido-use-faces nil))
;;   :config
;;   (flx-ido-mode 1))

(use-package helm
  :ensure t
  :diminish helm-mode
  :init
  (helm-mode 1)
  :bind
  (("M-x"     . helm-M-x)
   ("C-x C-f" . helm-find-files)
   ("C-x b"   . helm-mini)
   ("C-x r b" . helm-filtered-bookmarks)
   ("M-y"     . helm-show-kill-ring))
  :config
  (setq helm-M-x-fuzzy-match t
        helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match t)
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action))

(setq helm-rg-ripgrep-executable "/Users/dineshadepu/.cargo/bin/rg")

(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (setq projectile-completion-system 'helm))

(use-package helm-projectile
  :ensure t
  :after (helm projectile)
  :config
  (helm-projectile-on))

;; =========================
;; Helm swoop is not working
;; =========================
;; (use-package helm-swoop
;;   :straight (helm-swoop
;;              :type git
;;              :host github
;;              :repo "ShingoFukuyama/helm-swoop")
;;   :after helm
;;   :bind (("M-i" . helm-swoop)))
;; ;; (use-package helm-swoop
;; ;;   :vc (:url "https://github.com/ShingoFukuyama/helm-swoop"
;; ;;        :branch "master")
;; ;;   :after helm
;; ;;   :bind (("M-i" . helm-swoop)))
(add-to-list 'load-path "~/.emacs.d/local_git_repos/helm-swoop")
(require 'helm-swoop)
(global-set-key (kbd "M-i") 'helm-swoop)
;; =========================
;; Helm swoop is not working
;; =========================


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


;; ===================
;; Theme
;; ===================
;; (use-package doom-themes
;;   :ensure t
;;   :config
;;   ;; Enable doom-one
;;   (load-theme 'doom-one t)

;;   ;; Better visuals
;;   (doom-themes-visual-bell-config)
;;   (doom-themes-org-config))
;; ;; Slightly brighter current line
;; (set-face-attribute 'hl-line nil :background "#282c34")

;; ;; Better region selection
;; (set-face-attribute 'region nil :background "#3e4451")

;; (use-package gruvbox-theme
;;   :ensure t
;;   :config
;;   (load-theme 'gruvbox-dark-medium t))
(use-package modus-themes
  :ensure t
  :config
  (load-theme 'modus-vivendi t))
(setq evil-normal-state-cursor '(box "#51afef"))   ;; blue
(setq evil-insert-state-cursor '(bar "#98be65"))   ;; green
(setq evil-visual-state-cursor '(box "#c678dd"))   ;; purple
;; ===================
;; Theme ends
;; ===================


;; ======================
;; Rust + LSP (Eglot)
;; ======================

(use-package eglot
  :ensure t
  :hook ((rust-mode . eglot-ensure)))

(use-package rustic
  :ensure t
  :mode ("\\.rs\\'" . rustic-mode)
  :config
  ;; Use eglot instead of lsp-mode
  (setq rustic-lsp-client 'eglot)

  ;; Auto-format on save
  (setq rustic-format-on-save t)

  ;; Clippy instead of cargo check
  (setq rustic-default-test-arguments "--all")
  )
;; =========================
;; Rust + LSP (Eglot) ends
;; =========================

;; ======================
;; Completion UI (Corfu)
;; ======================

(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  :custom
  ;; Similar to company-minimum-prefix-length = 1
  (corfu-min-width 30)
  (corfu-max-width 80)
  (corfu-auto t)
  (corfu-auto-delay 0.0)
  (corfu-auto-prefix 2)
  (corfu-cycle t)
  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous)))

(use-package corfu-popupinfo
  :after corfu
  :init
  (corfu-popupinfo-mode)
  :custom
  (corfu-popupinfo-delay 0.2))

(use-package cape
  :ensure t
  :init
  ;; Add useful completion sources
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))


(setq completion-category-overrides
      '((eglot (styles orderless basic))))

;; Rust specific tuning
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(rust-mode . ("rust-analyzer")))
  (setq eglot-workspace-configuration
        '((:rust-analyzer
           (:completion
            (:autoimport t
             :callable (:snippets "add_parentheses")))
           :cargo (:allFeatures t)
           :procMacro (:enable t)
           :inlayHints
           (:bindingModeHints t
            :closureReturnTypeHints t
            :lifetimeElisionHints
            (:enable "always"))))))

;; ===========================
;; Completion UI (Corfu) ends
;; ===========================

;; ======================
;; C / C++ + LSP (Eglot)
;; ======================

(use-package eglot
  :ensure t
  :hook ((c-mode c++-mode) . eglot-ensure))

(with-eval-after-load 'eglot
  ;; Tell Eglot to use clangd
  (add-to-list 'eglot-server-programs
               '((c-mode c++-mode)
                 . ("clangd"
                    "--header-insertion=never"
                    "--completion-style=detailed"
                    "--clang-tidy"
                    "--cross-file-rename"))))

;; ;; Optional: format on save using clang-format
;; (defun my-cpp-format-on-save ()
;;   (when (and (derived-mode-p 'c-mode 'c++-mode)
;;              (executable-find "clang-format"))
;;     (clang-format-buffer)))

;; (add-hook 'before-save-hook #'my-cpp-format-on-save)

;; =========================
;; C / C++ + LSP (Eglot) ends
;; =========================


;; ======================
;; LaTeX + LSP (Eglot)
;; ======================

(use-package tex
  :ensure auctex
  :defer t
  :hook ((LaTeX-mode . eglot-ensure))
  :config
  ;; PDF viewer (macOS)
  (setq TeX-view-program-selection '((output-pdf "PDF Viewer")))
  (setq TeX-view-program-list
        '(("PDF Viewer" "open -a Preview.app %o")))
  (setq TeX-source-correlate-mode t)
  (setq TeX-source-correlate-start-server t))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(LaTeX-mode . ("texlab"))))

;; =========================
;; LaTeX + LSP (Eglot) ends
;; =========================

;; ======================
;; Python + LSP (Eglot)
;; ======================

(use-package python
  :ensure nil
  :hook ((python-mode . eglot-ensure)))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(python-mode . ("pylsp"))))

;; ;; Optional: format on save (black)
;; (defun my-python-format-on-save ()
;;   (when (and (derived-mode-p 'python-mode)
;;              (executable-find "black"))
;;     (call-process "black" nil nil nil buffer-file-name)))

;; (add-hook 'before-save-hook #'my-python-format-on-save)

;; =========================
;; Python + LSP (Eglot) ends
;; =========================




(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-minibuffer-history-key "M-p")
 '(package-selected-packages
   '(helm-rg modus-themes helm-swoop helm-projectile projectile which-key smartparens restart-emacs rainbow-delimiters magit helm golden-ratio git-timemachine fzf flx-ido exec-path-from-shell evil-terminal-cursor-changer evil-nerd-commenter evil-leader evil-escape evil-collection avy)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
