;; Interesting variables: C-h v
;; ----------------------------
;; data-directory
;; emacs-version
;; emacs-build-system
;; emacs-repository-version
;; system-configuration
;; system-configuration-options
;; system-configuration-features

;; Customize per Os/Emacs version
;; https://www.emacswiki.org/emacs/CustomizingBoth

;; https://github.com/syl20bnr/spacemacs
;; https://github.com/bbatsov/prelude

;; https://www.reddit.com/r/emacs/comments/55ork0/is_emacs_251_noticeably_slower_than_245_on_windows/
;; -------------------------------------------------------------------------------------------------
;; Garbage collection, valori di default;
;; gc-cons-threshold  -> 800000
;; gc-cons-percentage -> 0.1
(setq gc-cons-threshold (* 64 1024 1024))
(setq gc-cons-percentage 0.5)
(run-with-idle-timer 5 t #'garbage-collect)
(setq garbage-collection-messages t)
;; Disattivo temporaneamente l'hook (lo riattivo in coda al file)
(if (>= emacs-major-version 25)
    (remove-hook 'find-file-hooks 'vc-refresh-state)
  (remove-hook 'find-file-hooks 'vc-find-file-hook))

;; Setup package.el
(message "Setup package.el")
(require 'package)
;; (setq package-enable-at-startup nil)
;; Manage package repositories
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(message "package-initialize")
(package-initialize)

;; Bootstrap `use-package'
(message "Bootstrapping use-package")
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

;; Color Themes (use (load-theme xxx) at the end
;; ---------------------------------------------
;; Color Theme
(use-package color-theme
  :init
  (message "use-package color-theme")
  :defer t
  :ensure t
  ;; :config
  )
(use-package aurora-theme
  :init
  (message "use-package aurora-theme")
  :ensure t
  :defer t
  )
(use-package badwolf-theme
  :init
  (message "use-package badwolf-theme")
  :ensure t
  :defer t
  )
(use-package darkokai-theme
  ;; A darker variant on Monokai
  :init
  (message "use-package darkokai-theme")
  :ensure t
  :defer t
  )
(use-package darktooth-theme
  ;; Color theme for Emacs, when soothe and gruvbox collide
  :init
  (message "use-pakcage darktooth-theme")
  :ensure t
  :defer t
  )
(use-package gruvbox-theme
  ;; A retro-groove colour theme for Emacs
  :init
  (message "use-pakcage gruvbox-theme")
  :ensure t
  :defer t
  )
(use-package jbeans-theme
  :init
  (message "use-package jbeans-theme")
  :ensure t
  :defer t
  )
(use-package material-theme
  ;; Material theme see https://realpython.com/blog/python/emacs-the-best-python-editor/
  :init
  (message "use-pakcage material-theme")
  :ensure t
  ;; :defer t
  )
(use-package moe-theme
  ;; A customizable colorful eye-candy theme for Emacser. Moe, moe, kyun!
  :init
  (message "use-pakcage moe-theme")
  :ensure t
  :defer t
  )
(use-package molokai-theme
  ;; Molokai theme with Emacs theme engine
  :init
  (message "use-package molokai-theme")
  :ensure t
  :defer t
  )
(use-package monokai-theme
  :init
  (message "use-pakcage monokai-theme")
  :ensure t
  :defer t
  )
(use-package mustang-theme
  ;; Port of vim's mustang theme
  :init
  (message "use-pakcage mustang-theme")
  :ensure t
  :defer t
  )
(use-package color-theme-solarized
  ;; The Solarized color theme, ported to Emacs
  :init
  (message "use-package color-theme-solarized")
  :ensure t
  :defer t
  :after
  color-theme
  )
(use-package zenburn-theme
  ;; Port of vim's mustang theme
  :init
  (message "use-pakcage zenburn-theme")
  :ensure t
  :defer t
  )

;; EMACS enhancements
;; ------------------
(use-package company
  ;; A modular text completion framework
  :init
  (message "use-package company")
  :disabled t
  :ensure t
  :defer t
  )
(use-package esup
  ;; Emacs Start Up Profiler https://github.com/jschaf/esup
  :init
  (message "use-package esup")
  :ensure t
  )
(use-package hl-line
  ;; Highlight Current Line
  :init
  (message "use-package hl-line")
  (global-hl-line-mode 1)
  :ensure t
  )

(use-package ibuffer
  ;; Shows a list of buffers
  ;; https://www.emacswiki.org/emacs/IbufferMode
  :init
  (message "use-package ibuffer")
  :ensure t
  :defer t
  :bind
  ("C-x C-b" . ibuffer)
  )
(use-package ido
  ;; ido-mode allows you to more easily navigate choices. For example,
  ;; when you want to switch buffers, ido presents you with a list
  ;; of buffers in the the mini-buffer. As you start to type a buffer's
  ;; name, ido will narrow down the list of buffers to match the text
  ;; you've typed in
  ;; http://www.emacswiki.org/emacs/InteractivelyDoThings
  :init
  ;; (ido-mode t)
  (message "use-package ido")
  :ensure t
  :config
  (ido-mode t)
  ;; This allows partial matches, e.g. "tl" will match "Tyrion Lannister"
  (setq ido-enable-flex-matching t)
  ;; Turn this behavior off because it's annoying
  (setq ido-use-filename-at-point nil)
  ;; Don't try to match file across all "work" directories; only match files
  ;; in the current directory displayed in the minibuffer
  (setq ido-auto-merge-work-directories-length -1)
  ;; Includes buffer names of recently open files, even if they're not
  ;; open now
  (setq ido-use-virtual-buffers t)
  )
(use-package ido-ubiquitous
  ;; Ido-ubiquitous
  ;; This enables ido in all contexts where it could be useful, not just
  ;; for selecting buffer and file names
  :init
  (message "use-package ido-ubiquitous")
  ;; (ido-ubiquitous-mode 1)
  :ensure t
  :after
  ido
  :config
  (ido-ubiquitous-mode 1)
  )
(use-package org
  :init
  (message "use-package org")
  :ensure org-plus-contrib
  :defer t
  ;; Non e' necessario impostare :mode
  ;; :mode "\\.org\\"
  ;; :mode (("\\.org$" . org-mode))
  )
;; (use-package org
;;   :mode (("\\.org$" . org-mode))
;;   :ensure org-plus-contrib
;;   :config
;;   (progn
;;     ;; config stuff
;;     ))
(use-package recentf
  ;; Turn on recent file mode so that you can more easily switch to
  ;; recently edited files when you first start emacs
  :init
  (message "use-package recentf")
  (recentf-mode 1)
  :ensure t
  :config
  (setq recentf-save-file (concat user-emacs-directory ".recentf"))
  (setq recentf-exclude '(".ido.last"))
  (setq recentf-max-menu-items 40)
  )
(use-package smex
  ;; Enhances M-x too allow easier execution of commands. Provides
  ;; a filterable list of possible commands in the minibuffer
  ;; http://www.emacswiki.org/emacs/Smex
  :init
  (message "use-package smex")
  ;; (smex-initialize)
  :ensure t
  :bind
  ("M-x" . smex)
  :config
  (smex-initialize)
  (setq smex-save-file (concat user-emacs-directory ".smex-items"))
  )
(use-package uniquify
  ;; When several buffers visit identically-named files,
  ;; Emacs must give the buffers distinct names. The usual method
  ;; for making buffer names unique adds '<2>', '<3>', etc. to the end
  ;; of the buffer names (all but one of them).
  ;; The forward naming method includes part of the file's directory
  ;; name at the beginning of the buffer name
  ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Uniquify.html
  :disabled t
  :init
  (message "use-package uniquify")
  :ensure t
  :defer t
  :config
  (setq uniquify-buffer-name-style 'forward)
  )
(use-package whitespace
  :init
  (message "use-package whitespace")
  :ensure t
  :commands (whitespace-mode)
  :config
  (setq whitespace-style '(face
                           tabs
                           spaces
                           newline
                           empty
                           trailing
                           tab-mark
                           newline-mark))
  )
(use-package whitespace-cleanup-mode
  :init
  (message "use-package whitespace-cleanup-mode")
  (global-whitespace-cleanup-mode)
  :ensure t
  :disabled t
  :defer t
  :diminish whitespace-cleanup-mode
  )
;; GIT
;; ---
(use-package magit
  ;; A GIT porcelain inside Emacs
  :init
  (message "use-package magit")
  ;; Disable built-in VC for Git when using magit
  (setq vc-handled-backends (delq 'Git vc-handled-backends))
  ;; (global-set-key (kbd "C-x g") 'magit-status)
  :bind
  ("C-x g" . magit-status)
  :ensure t
  :defer t
  :config
  ;; (magit-diff-use-overlays nil)
  )
(use-package gitconfig-mode
  :init
  (message "use-package gitconfig-mode")
  :ensure t
  :defer t
  :mode ("/\\.gitconfig\\'" "/\\.git/config\\'" "/git/config\\'" "/\\.gitmodules\\'")
  )
(use-package gitignore-mode
  :init
  (message "use-package gitignore-mode")
  :ensure t
  :defer t
  :mode ("/\\.gitignore\\'" "/\\.git/info/exclude\\'" "/git/ignore\\'")
  )
;; Development: generic
;; --------------------
(use-package flycheck
  :init
  (message "use-package flycheck")
  :ensure t
  :defer t
  )
(use-package indent-guide
  ;; Show vertical lines to guide indentation
  :init
  (message "use-package flycheck")
  :defer t
  :ensure t
  )
(use-package projectile
  ;; Project navigation
  :init
  (message "use-package projectile")
  ;; (projectile-global-mode t)
  :ensure t
  :defer t
  :config
  (projectile-global-mode t)
  )
(use-package rainbow-delimiters
  ;; Colorful parentesis matching
  :init
  (message "use-package rainwbow-delimiters")
  :ensure t
  :defer t
  )
(use-package yasnippet
  ;; Yet another snippet extension for Emacs
  :init
  (message "use-package yasnippet")
  :ensure t
  :defer t
  )
;; Development: C#
;; ---------------
(use-package csharp-mode
  ;; C# mode
  :init
  (message "use-package csharp-mode")
  :ensure t
  :defer t
  ;; Non e' necessario impostare :mode
  ;; :mode "\\.cs\\"
  )
;; Development: Powershell
;; -----------------------
(use-package powershell
  :init
  (message "use-package powershell")
  :ensure t
  :defer t
  ;; Non e' necessario impostare :mode
  ;; :mode ("\\.ps[dm]?1\\'" . powershell-mode)
  :config
  (add-hook 'powershell-mode-hook 'work-style)
  )
;; Develpment: Python
;; ------------------
(use-package elpy
  :init
  (message "use-package elpy")
  (with-eval-after-load 'python (elpy-enable))
  :ensure t
  :defer t
  ;; Non e' necessario impostare :mode
  ;; :mode "\\.py[w]?\\"
  :after
  flycheck
  py-autopep8
  :config
  (elpy-enable)
  (setq elpy-rpc-backend "jedi")
  ;; (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  ;; (add-hook 'elpy-mode-hook 'flycheck-mode)
  (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
  )
(use-package py-autopep8
  ;; Autopep8
  :init
  (message "use-package py-autopep8")
  :ensure t
  :defer t
  :config
  (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
  )







;; Customization (outside of "custom")
;; -----------------------------------
(message "Customization (outside of 'custom'")
(setq delete-selection-mode t)               ;; Editing basics - See delete-selection-mode command
(setq column-number-mode t)                  ;; Modeline - Display current column number (modeline)
(setq global-hl-line-mode t)                 ;; Hl line - See global-hl-line-mode command
(setq hscroll-step 1)                        ;; Windows - Number of column to scroll when points get too close to the edge
(setq scroll-step 1)                         ;; Windows - Number of lines to try scrolling a windows when point moves out
(setq inhibit-startup-screen t)              ;; Initialization - Inhibits the startup screen
(setq x-select-enable-clipboard t)           ;; makes killing/yanking interact with the clipboard
(setq x-select-enable-primary t)             ;;
(setq save-interprogram-paste-before-kill t) ;;
(setq apropos-do-all t)                      ;; Shows all options when running apropos.
(setq mouse-yank-at-point t)                 ;; Mouse yank commands yank at point instead of at click.
(setq ring-bell-function 'ignore)            ;; No bell

(set-scroll-bar-mode 'right)                 ;; Scroll bars - right

;; (setq cua-mode t nil)                        ;; CUA Mode
;; (setq-default frame-title-format "%b (%f)")  ;; full path in title bar

;; ;; don't pop up font menu
;; (global-set-key (kbd "s-t") '(lambda () (interactive)))

;; ;; No cursor blinking, it's distracting
;; (blink-cursor-mode 0)

;; Tabulation settings
;; -------------------
(setq-default tab-width 4)                      ;; Set tab width to 4 spaces
(setq-default indent-tabs-mode nil)             ;; Use spaces instead of tabs
(setq tab-stop-list (number-sequence 4 200 4))  ;; Create list of tab stops every 4 char

;; Printing
;; --------
(setq ps-lpr-command "C:/Program Files/gs/gs9.16/bin/gswin64c.exe")
(setq ps-lpr-switches
      '("-q"
        "-dNOPAUSE"
        "-dBATCH"
        "-IC:/Program Files/gs/gs9.16/lib"
        "-sFONTPATH=C:/Windows/Fonts"
        "-sDEVICE=mswinpr2"
        "-sOutputICCProfile=default_cmyk.icc"
        "-dBitsPerPixel=24"
        "-dEmbedAllFonts=true"))
(setq ps-paper-type 'a4)
(setq ps-print-color-p 'black-white)
(setq ps-printer-name t)
(setq doc-view-continuous t)
(setq doc-view-ghostscript-program "C:/Program Files/gs/gs9.16/bin/gswin64c.exe")

;; Start Emacs fullscreen mode
(add-hook 'emacs-startup-hook 'toggle-frame-maximized)

;; Emacs Server
;; (require 'server)
;; (unless (server-running-p) (server-start))

;; Custom
(message "Loading 'custom'")
(setq custom-file (locate-user-emacs-file "custom-set-settings.el"))
(load custom-file t)

;; Color theme setup
(message "Loading material theme")
(load-theme 'material)


;; https://www.reddit.com/r/emacs/comments/55ork0/is_emacs_251_noticeably_slower_than_245_on_windows/
;; -------------------------------------------------------------------------------------------------
;; Riattivo l'hook disattivato in testa al file
(if (>= emacs-major-version 25)
    (add-hook 'find-file-hooks 'vc-refresh-state)
  (add-hook 'find-file-hooks 'vc-find-file-hook))

;; ***************************************************************************
;; Local Variables:
;; mode: lisp
;; indent-tabs-mode: nil
;; tab-width: 4
;; End:
;; ***************************************************************************
