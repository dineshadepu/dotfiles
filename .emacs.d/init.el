;;; package --- Summary
;;; Commentary:


;; -------------- normal functions -------------------
;;; code:
(require 'package)
(setq package-enable-at-startup nil)

(cond
 ((>= 24 emacs-major-version)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives
               '("melpa-stable" . "http://stable.melpa.org/packages/") t)
  (package-refresh-contents)
  )
 )
(setq package-archives
      (append package-archives
              '(("melpa" . "http://melpa.milkbox.net/packages/"))))

;; (setq package-archives
;;       '(("elpa" . "http://elpa.gnu.org/packages/")
;;         ("melpa-stable" . "http://stable.melpa.org/packages/")
;;         ("melpa" . "http://melpa.org/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;; UI

(setq inhibit-startup-message t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(global-linum-mode t)
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

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

;; == Load Custom Theme ==
(use-package color-theme :ensure t)
(use-package monokai-theme
  :ensure t
  :init
  (load-theme 'monokai t))
;; (use-package zenburn-theme
;;   :ensure t
;;   :init
;;   (load-theme 'zenburn t))

(use-package exec-path-from-shell
  :ensure t
  ;; :load-path "~/.emacs.d/elisp/exec-path-from-shell/"
  :config
  (push "HISTFILE" exec-path-from-shell-variables)
  (setq exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-initialize))

;; Provides all the racket support
(use-package racket-mode
  :ensure t)

(use-package scheme-complete :ensure t)

;; (use-package rainbow-identifiers
;;   :ensure t
;;   :init
;;   ;; (rainbow-identifiers-mode 1) doesn't work. have to set it up as a hoook
;;   (progn
;;     (add-hook 'prog-mode-hook 'rainbow-identifiers-mode)
;;     (setq rainbow-identifiers-choose-face-function 'rainbow-identifiers-cie-l*a*b*-choose-face
;;           rainbow-identifiers-cie-l*a*b*-lightness 70
;;           rainbow-identifiers-cie-l*a*b*-saturation 30
;;           rainbow-identifiers-cie-l*a*b*-color-count 20
;;           ;; override theme faces
;;           rainbow-identifiers-faces-to-override '(highlight-quoted-symbol
;;                                                   font-lock-variable-name-face
;;                                                   font-lock-function-name-face
;;                                                   font-lock-type-face
;;                                                   js2-function-param
;;                                                   js2-external-variable
;;                                                   js2-instance-member
;;                                                   js2-private-function-call))))

(use-package rainbow-delimiters
  :ensure t
  :init
  (progn
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
    (add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)))

;; (use-package paredit
;;   :ensure t
;;   :config
;;   (add-hook 'racket-mode-hook #'enable-paredit-mode))

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


(use-package evil
  :ensure t
  :diminish evil
  :init (evil-mode 1)
  (setq evil-insert-state-cursor '((bar . 1) "white")
        evil-visual-state-cursor '(box "dark orange")
        evil-normal-state-cursor '(box "white"))
  :bind (:map
         evil-insert-state-map
         ([left]     . windmove-left)
         ([right]    . windmove-right)
         ([up]       . windmove-up)
         ([down]     . windmove-down)
         ("SPC" . nil)
         :map
         evil-normal-state-map
         (";" . evil-ex)
         (":"	.	evil-repeat-find-char)
         :map    evil-motion-state-map
         ([left]     . windmove-left)
         ([right]    . windmove-right)
         ([up]       . windmove-up)
         ([down]     . windmove-down)))
(fset 'evil-visual-update-x-selection 'ignore)


(use-package evil-leader
  :ensure t
  :diminish evil-leader
  :init (global-evil-leader-mode)
  :config (progn
            (evil-leader/set-leader ",")
            (evil-leader/set-key "b" 'switch-to-buffer)
            (evil-leader/set-key "s" 'save-buffer)
            (evil-leader/set-key "e" 'find-file)
            (evil-leader/set-key "1" 'delete-other-windows)
            (evil-leader/set-key "x" 'bookmark-jump)
            (evil-leader/set-key "0" 'delete-window)
            (evil-leader/set-key "3" 'split-window-right)
            (evil-leader/set-key "2" 'split-window-below)
            (evil-leader/set-key "." 'elpy-goto-definition-other-window)
            (evil-leader/set-key "," 'elpy-goto-definition)
            (evil-leader/set-key "f" 'ff-find-other-file)
            (evil-leader/set-key "r" 'recentf-open-files)
            (evil-leader/set-key "c" 'tramp-cleanup-all-connections)
            (evil-leader/set-key "w" 'ispell-word)
            (evil-leader/set-key "g" 'magit-status)
            (evil-leader/set-key "z" 'fzf)
            ;; (evil-leader/set-key "h" 'helm-M-x)
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


(use-package evil-magit
  :ensure t)

(use-package evil-nerd-commenter
  :ensure t
  :config(progn
           (evilnc-default-hotkeys)))

(use-package company
  :ensure t
  :defer 12
  :diminish company-mode
  :commands (compay-mode
             company-complete
             company-complete-common
             company-complete-common-or-cycle
             company-files
             company-dabbrev
             company-ispell
             company-c-headers
             company-jedi
             company-auctex
             company--auto-completion
             company-julia)
  :init
  (setq company-minimum-prefix-length 2
        company-require-match 1
        company-selection-wrap-around t
        company-dabbrev-downcase nil
        company-tooltip-limit 15
        company-tooltip-align-annotations t
        company-idle-delay 0.1
        company-begin-commands '(self-insert-command))
  (eval-after-load 'company
    '(add-to-list 'company-backends '(company-files
                                      company-capf)))
  :bind (("C-c f" . company-files)
         ("C-c a" . company-dabbrev)
         ("C-c d" . company-ispell)
         ("<tab>" . tab-indent-or-complete)
         ("TAB" . tab-indent-or-complete)
         ("M-t" . company-complete-common)
         :map company-active-map
         ("C-a" . company-abort)
         ("<tab>" . expand-snippet-or-complete-selection)
         ("TAB" . expand-snippet-or-complete-selection))
  :config
  (defun check-expansion ()
    (save-excursion
      (if (looking-at "\\_>") t
        (backward-char 1)
        (if (looking-at "\\.") t
          (backward-char 1)
          (if (looking-at "->") t nil)))))
  (defun do-yas-expand ()
    (let ((yas/fallback-behavior 'return-nil))
      (yas/expand)))
  (defun tab-indent-or-complete ()
    (interactive)
    (cond
     ((minibufferp)
      (minibuffer-complete))
     (t
      (indent-for-tab-command)
      (if (or (not yas-minor-mode)
              (null (do-yas-expand)))
          (if (check-expansion)
              (progn
                (company-manual-begin)
                (if (null company-candidates)
                    (progn
                      (company-abort)
                      (indent-for-tab-command)))))))))
  (defun expand-snippet-or-complete-selection ()
    (interactive)
    (if (or (not yas-minor-mode)
            (null (do-yas-expand))
            (company-abort))
        (company-complete-selection)))
  ;; Add yasnippet support for all company backends
  (defvar company-mode/enable-yas t
    "Enable yasnippet for every back-end")
  (defun company-mode/backend-with-yas (backend)
    (if (or (not company-mode/enable-yas)
            (and (listp backend) (member 'company-yasnippet backend)))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))
  (setq company-backends
        (mapcar #'company-mode/backend-with-yas company-backends))
  (global-company-mode))
;; (add-hook 'after-init-hook 'global-company-mode)

(defun add-pcomplete-to-capf ()
  (add-hook 'completion-at-point-functions 'pcomplete-completions-at-point nil t))

(add-hook 'org-mode-hook #'add-pcomplete-to-capf)


(use-package company-statistics
  :ensure t
  :config
  (company-statistics-mode))

(use-package company-c-headers
  :ensure t
  :config
  (add-to-list 'company-c-headers-path-system "/usr/include/c++/5/")
  (add-to-list 'company-backend 'company-c-headers))

(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :init
  (global-flycheck-mode t)
  ;; (setq flycheck-highlighting-mode 'columns)
  (setq flycheck-highlighting-mode 'symbols))



(use-package flyspell
  :ensure t
  :diminish flyspell-mode
  :defer t
  :init
  (progn
    (add-hook 'prog-mode-hook 'flyspell-prog-mode)
    (add-hook 'text-mode-hook 'flyspell-mode)
    )

  :config
  ;; Sets flyspell correction to use two-finger mouse click
  (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word))

;; ispell for spell check
(setq exec-path (append exec-path '("/usr/local/Cellar/")))
(setq-default ispell-program-name "aspell")


(use-package restart-emacs
             :ensure t
             :bind (("C-x M-c" . restart-emacs)))

(use-package aggressive-indent
  :ensure t
  :init (global-aggressive-indent-mode))


(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :init (yas-global-mode 1)
  :preface
  (defun abort-company-or-yas ()
    (interactive)
    (if (null company-candidates)
        (yas-abort-snippet)
      (company-abort)))
  (defun tab-complete-or-next-field ()
    (interactive)
    (if (or (not yas-minor-mode)
            (null (do-yas-expand)))
        (if company-candidates
            (company-complete-selection)
          (if (check-expansion)
              (progn
                (company-manual-begin)
                (if (null company-candidates)
                    (progn
                      (company-abort)
                      (yas-next-field))))
            (yas-next-field)))))
  :bind (:map yas-minor-mode-map
              ([tab] . nil)
              ("TAB" . nil)
              :map yas-keymap
              ([tab] . tab-complete-or-next-field)
              ("TAB" . tab-complete-or-next-field)
              ("M-n" . yas-next-field)
              ("C-g" . abort-company-or-yas)))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))
;; (use-package markdown-mode
;;   :ensure t
;;   ;; :load-path "~/elisp/markdown-mode"
;;   :commands markdown-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Section for Predictive mode
;;
(add-to-list 'load-path "~/.emacs.d/elisp/predictive/")
(autoload 'predictive-mode "predictive" "predictive" t)
(set-default 'predictive-auto-add-to-dict t)
(setq predictive-main-dict 'rpg-dictionary
      predictive-auto-learn t
      predictive-add-to-dict-ask nil
      predictive-use-auto-learn-cache nil
      predictive-which-dict t)

(when (and (featurep 'predictive) (featurep 'company))
  (defun company-predictive (command &optional arg &rest ignored)
    (case command
      (prefix (let* ((text (downcase (word-at-point))))
                (set-text-properties 0 (length text) nil text)
                text))
      (candidates (predictive-complete arg))))
  (load "dict-english")
  (add-to-list 'company-backends '(company-predictive)))
(add-hook 'org-mode-hook 'predictive-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package predictive
;;   :load-path "~/.emacs.d/elisp/predictive/"
;;   :config
;;   (autoload 'predictive-mode "predictive" "predictive" t)
;;   (set-default 'predictive-auto-add-to-dict t)
;;   (setq predictive-main-dict 'rpg-dictionary)
;;   (setq predictive-auto-learn t)
;;   (setq predictive-add-to-dict-ask nil)
;;   (setq predictive-use-auto-learn-cache nil)
;;   (setq predictive-which-dict t))



(use-package tex
  :ensure auctex
  :config
  ;; (setq TeX-show-compilation t)
  (add-hook 'LaTeX-mode-hook 'predictive-mode))


(use-package elpy
  :load-path "~/.emacs.d/elisp/elpy/"
  ;; :ensure t
  :diminish elpy-mode
  :config(progn
           (defalias 'workon 'pyvenv-workon)
           ;; (elpy-use-cpython "/usr/local/bin/python3")
           ;; (setq elpy-rpc-python-command "python3")
           ;; (setq 'python-indent-offset 4)
           (setq company-minimum-prefix-length 1)
           ;; (elpy-use-ipython)
           ;; (elpy-clean-modeline)
           (elpy-enable)))


(use-package py-yapf
  :ensure t
  :diminish py-yapf)
;; :config
;; (add-hook 'python-mode-hook 'py-yapf-enable-on-save))


(use-package ido
  :ensure t
  :config(progn
           (setq ido-enable-flex-matching t)
           (setq ido-everywhere t)
           (ido-mode 1)))


(use-package flx-ido
  :ensure t
  :init
  (progn
    (setq gc-cons-threshold (* 20 (expt 2 20)) ; megabytes
          ido-use-faces nil))
  :config
  (flx-ido-mode 1))



;; (use-package smex
;;   :ensure t
;;   :bind
;;   (([remap execute-extended-command] . smex)
;;    ("M-X" . smex-major-mode-commands)))

(use-package helm
  :ensure t
  :bind (("C-x C-f" . helm-find-files))
  :init
  (progn
    (require 'helm-config)
    (helm-mode 1)
    (set-face-attribute 'helm-selection nil
                        )))

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

;; Emacs c++ environment

;; ------------------------------------------------
;; ------------------------------------------------

;; c++ development irony starts here

;; tags for code navigation
;; (use-package ggtags
;;   :ensure t
;;   :init
;;   (setq path-to-ctags "/usr/local/bin/ctags")
;;   :bind
;;   (:map evil-normal-state-map
;;         ;; ("M-," . ggt)
;;         ("M-." .  ggtags-find-tag-dwim))
;;   :config
;;   (add-hook 'c-mode-common-hook
;;             (lambda ()
;;               (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
;;                 (ggtags-mode 1))))
;;   ;; :evil-leader ("," ggtags-find-tag-dwim)
;;   )


;; (defun setup-c-clang-options ()
;;   (setq irony-additional-clang-options (quote ("-std=c11"))))

;; (defun setup-cpp-clang-options ()
;;   (setq irony-additional-clang-options (quote ("-std=c++11" "-I/usr/local/include/Eigen/Eigen/")))
;;   (setq irony-additional-clang-options '("-std=c++11"))
;;   (setq irony-additional-clang-options (quote ("-std=c++14" "-stdlib=libc++"))))

;; (use-package irony
;;   :ensure t
;;   :init
;;   (progn
;;     (add-hook 'c++-mode-hook 'irony-mode)
;;     (add-hook 'c-mode-hook 'irony-mode)
;;     (add-hook 'objc-mode-hook 'irony-mode))
;;   :config
;;   (progn
;;     ;; (setq irony-libclang-additional-flags
;;     ;;       (append '("-I" "inc") irony-libclang-additional-flags))
;;     (add-hook 'c++-mode-hook 'setup-cpp-clang-options)
;;     (add-hook 'c-mode-hook 'setup-c-clang-options)))

;; (use-package company-irony
;;   :ensure t
;;   :config
;;   (progn
;;     (eval-after-load 'company '(add-to-list 'company-backends 'company-irony))
;;     (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)))

;; (use-package flycheck-irony
;; :ensure t
;; :config
;; (eval-after-load 'flycheck
;;   '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup)))

;; (use-package clang-format
;;   :ensure t
;;   :config
;;   (progn
;;     (setq clang-format-style "llvm")
;; (add-hook 'c++-mode-hook (lambda () (add-hook 'before-save-hook 'clang-format-buffer nil t)))
;; (add-hook 'c-mode-hook (lambda () (add-hook 'before-save-hook 'clang-format-buffer nil t)))))

;; (use-package google-c-style
;;   :ensure t
;;   :defer t
;;   :config
;;   (progn
;;     (add-hook 'c-mode-common-hook 'google-set-c-style)
;;     (add-hook 'c-mode-common-hook 'google-make-newline-indent)))

;; c++ development irony ends here

;; ------------------------------------------------
;; ------------------------------------------------




;; (use-package rust-mode
;;   :ensure t
;;   :defer t)

;; (use-package flycheck-rust
;;   :ensure t
;;   :defer t
;;   :init (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

;; (use-package flycheck-package
;;   :load-path "~/.emacs.d/elisp/flycheck-package/"
;;   :init (with-eval-after-load 'flycheck (flycheck-package-setup)))

;; rust mode stars here

;; (use-package racer
;; :ensure t
;; :load-path "~/.emacs.d/elisp/emacs-racer/"
;; :bind
;; (:map evil-normal-state-map
;;       ("M-," .  racer-find-definition))
;; :config
;; (add-hook 'rust-mode-hook #'racer-mode)
;; (add-hook 'racer-mode-hook #'eldoc-mode))

;; (use-package emacs-rustfmt
;;   :ensure t
;;   :load-path "~/.emacs.d/elisp/emacs-rustfmt/"
;;   :config
;;   (add-hook 'rust-mode-hook #'rustfmt-enable-on-save))

;; (defun my-rust-mode-hooks ()
;;   (add-hook 'before-save-hook 'rustfmt-format-buffer)
;;   )
;; (add-hook 'rust-mode-hook 'my-rust-mode-hooks)

;; (use-package rustfmt
;;   :config
;;   (define-key rust-mode-map (kbd "C-c C-f") #'rustfmt-format-buffer))

;; (use-package toml-mode
;;   :ensure t)

;; (use-package clang-format
;;   :ensure t)


(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :config
  (progn
    (require 'smartparens-config)
    (smartparens-global-mode 1)))

;; (use-package cython-mode
;;   :ensure t
;;   :config
;;   (require 'cython-mode))

;; (use-package sr-speedbar
;;   :load-path "~/.emacs.d/elisp/sr-speedbar.el"
;;   :config
;;   (require 'sr-speedbar))

(use-package recentf
  :ensure t
  :config
  (require 'recentf)
  (recentf-mode 1)
  (setq recentf-max-menu-items 25)
  (global-set-key "\C-x\ \C-r" 'recentf-open-files))


(use-package fzf
  :ensure t)

;; (use-package ensime-emacs
;;   :load-path "~/.emacs.d/elisp/ensime-emacs"
;;   )

;; ESS for statistics

;; (add-to-list 'load-path "~/.emacs.d/elisp/ESS/lisp/")
;; (load "ess-site")
;; auto-mode-alist (append (list '("\\.c$" . c-mode)
;;                               '("\\.tex$" . latex-mode)
;;                               '("\\.S$" . S-mode)
;;                               '("\\.s$" . S-mode)
;;                               '("\\.R$" . R-mode)
;;                               '("\\.r$" . R-mode)
;;                               '("\\.html$" . html-mode)
;;                               '("\\.emacs" . emacs-lisp-mode)
;;                               )
;;                         auto-mode-alist)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CPP rtags and irony combined unique development
(use-package rtags
  :ensure t)

(use-package company-rtags
  :ensure t)

(setq rtags-completions-enabled t)
(eval-after-load 'company
  '(add-to-list
    'company-backends 'company-rtags))
(setq rtags-autostart-diagnostics t)
(rtags-enable-standard-keybindings)

;; Install irony and keep this in init file
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))

(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;; Company irony for auto completion
(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
(setq company-backends (delete 'company-semantic company-backends))

(setq company-idle-delay 0)
(define-key c-mode-map [(tab)] 'company-complete)
(define-key c++-mode-map [(tab)] 'company-complete)


;; Header file completion with c headers
(require 'company-irony-c-headers)
(eval-after-load 'company
  '(add-to-list
    'company-backends '(company-irony-c-headers company-irony)))

;; flycheck for c and cpp
(add-hook 'c++-mode-hook 'flycheck-mode)
(add-hook 'c-mode-hook 'flycheck-mode)

(require 'flycheck-rtags)

(defun my-flycheck-rtags-setup ()
  (flycheck-select-checker 'rtags)
  (setq-local flycheck-highlighting-mode nil) ;; RTags creates more accurate overlays.
  (setq-local flycheck-check-syntax-automatically nil))
;; c-mode-common-hook is also called by c++-mode
(add-hook 'c-mode-common-hook #'my-flycheck-rtags-setup)

;; Integrating Irony with Flycheck
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))


;; CMake automation with cmake-ide
(cmake-ide-setup)

;; Automatically create .dir-locals.el
((nil . ((cmake-ide-build-dir . "<PATH_TO_PROJECT_BUILD_DIRECTORY>"))))



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (rtags fzf ensime-emacs ensime sr-speedbar cython-mode solarized-theme zenburn-theme processing-mode avy smartparens emacs-rustfmt evil-magit magit rainbow-delimiters scheme-complete paredit racket-mode company-quickhelp ggtags predictive-mode predictive markdown-mode meghanada meghananda-emacs meghananda jde-mode company-emacs-eclim eclim emacs-eclim rustfmt flycheck-package toml-mode clang-format racer exec-path-from-shell which-key use-package smex rich-minority restart-emacs py-yapf monokai-theme helm golden-ratio flycheck flx-ido evil-terminal-cursor-changer evil-surround evil-nerd-commenter evil-leader evil-exchange elpy company-statistics company-irony company-c-headers company-ansible color-theme auctex aggressive-indent))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
