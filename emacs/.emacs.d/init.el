
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
;; -----------------
(setq gc-cons-threshold (* 64 1024 1024))
(setq gc-cons-percentage 0.5)
;; -----------------
(run-with-idle-timer 5 t #'garbage-collect)
;; (setq garbage-collection-messages t)
;; Disattivo temporaneamente l'hook (lo riattivo in coda al file)
(if (>= emacs-major-version 25)
    (remove-hook 'find-file-hooks 'vc-refresh-state)
  (remove-hook 'find-file-hooks 'vc-find-file-hook))

;; https://github.com/howardabrams/dot-files/blob/master/emacs.org
(setq gnutls-min-prime-bits 4096)

;; Setup package.el
;; Cancellami? (message "Setup package.el")
(require 'package)

;; Manage package repositories
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))

;; https://github.com/sondr3/dotfiles/blob/master/emacs.org
;; Then we ll make sure we always load newer files if they are available,
;; even if there s a byte compiled version and disable automatic requiring
;; of packages on start as it ll be handled by use-package.
(setq load-prefer-newer t)
(setq package-enable-at-startup nil)

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; Cancellami? (message "package-initialize")
(package-initialize)

;; =========================================================================
;; use-package custom setup
;; =========================================================================
;; (setq use-package-debug t)
;; (setq use-package-verbose t)
;; (setq use-package-enable-imenu-support t)
;; (setq use-package-minimum-reported-time 0.0001)
;; =========================================================================
;; Bootstrap `use-package'
;; =========================================================================
;; N.B.: Remove e reinstall "use-package" from "list-packages"
;;       if there are error processing .emacs file after a
;;       package upgrade (use-package.el may be empty!!!)
;; Utile da leggere: http://irreal.org/blog/?p=6442
;; Cancellami? (message "Bootstrapping use-package")
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Da testare: sembra non essere necessario
;; https://cestlaz.github.io/posts/using-emacs-1-setup/
;; Per questo commento la eval-when-compile
;; (eval-when-compile
;;   (require 'use-package))

;; Non servono???? (sono dipendenze di use-package???)
;; (require 'diminish)
;; (require 'bind-key)



;; =========================================================================
;; Benchmark-init
;; Se voglio fare benchmarking devo caricarlo prima posssibile
;; =========================================================================
(use-package benchmark-init
  :init
  ;; Cancellami? (message "use-package benchmark-init")
  :ensure t
  )

;; Color Themes (use (load-theme xxx) at the end
;; =========================================================================
;; Color Theme
;; =========================================================================
(use-package color-theme
  ;; install color themes
  :if (display-graphic-p)
  :init
  ;; Cancellami? (message "use-package color-theme")
  :defer t
  :ensure t
  ;; :config
  )

;; Non esiste piu' ?????????
;; (use-package aurora-theme
;;   :init
;;   ;; Cancellami? (message "use-package aurora-theme")
;;   :ensure t
;;   :defer t
;;   )

(use-package badwolf-theme
  ;; Bad Wolf color theme
  :if (display-graphic-p)
  :init
  ;; Cancellami? (message "use-package badwolf-theme")
  :ensure t
  :defer t
  )

(use-package darkokai-theme
  ;; A darker variant on Monokai
  :if (display-graphic-p)
  :init
  ;; Cancellami? (message "use-package darkokai-theme")
  :ensure t
  :defer t
  )

(use-package darktooth-theme
  ;; Color theme for Emacs, when soothe and gruvbox collide
  :if (display-graphic-p)
  :init
  ;; Cancellami? (message "use-pakcage darktooth-theme")
  :ensure t
  :defer t
  )

(use-package gruvbox-theme
  ;; A retro-groove colour theme for Emacs
  :if (display-graphic-p)
  :init
  ;; Cancellami? (message "use-pakcage gruvbox-theme")
  :ensure t
  :defer t
  )

;;
;; Rimosso perche' rallenta molto lo startup
;; e non puo' essere :defer t
;; perche' altrimenti incasina gli altri themes
;;(use-package jbeans-theme
;;  ;; Jbeans theme for GNU Emacs 24 (deftheme)
;;  :if (display-graphic-p)
;;  :init
;;  ;; Cancellami? (message "use-package jbeans-theme")
;;  :ensure t
;;  ;; :defer t
;;  )

(use-package material-theme
  ;; Material theme see https://realpython.com/blog/python/emacs-the-best-python-editor/
  :if (display-graphic-p)
  :init
  ;; Cancellami? (message "use-pakcage material-theme")
  :ensure t
  :defer t
  )

(use-package molokai-theme
  ;; Molokai theme with Emacs theme engine
  :if (display-graphic-p)
  :init
  ;; Cancellami? (message "use-package molokai-theme")
  :ensure t
  :defer t
  )

(use-package monokai-theme
  ;; A fruity color theme for Emacs.
  :if (display-graphic-p)
  :init
  ;; Cancellami? (message "use-pakcage monokai-theme")
  :ensure t
  :defer t
  )

(use-package mustang-theme
  ;; Port of vim's mustang theme
  :if (display-graphic-p)
  :init
  ;; Cancellami? (message "use-pakcage mustang-theme")
  :ensure t
  :defer t
  )

(use-package obsidian-theme
  ;; port of the eclipse obsidian theme
  :if (display-graphic-p)
  :init
  ;; Cancellami? (message "use-pakcage obsidian-theme")
  :ensure t
  :defer t
  )

(if (>= emacs-major-version 24)
    (use-package solarized-theme
      ;; Da non confondere con color-theme-solarized
      ;; The Solarized color theme, ported to Emacs
      :if (display-graphic-p)
      :init
      ;; Cancellami? (message "use-package color-theme-solarized")
      :config
      ;; (setq frame-background-mode 'dark)
      :ensure t
      :defer t
      :after
      ;; color-theme
      )
  )

(use-package zenburn-theme
  ;; Port of vim's mustang theme
  :if (display-graphic-p)


  :init
  ;; Cancellami? (message "use-pakcage zenburn-theme")
  :ensure t
  :defer t
  )

;; =========================================================================
;; EMACS enhancements
;; =========================================================================
(use-package company
  ;; A modular text completion framework
  :init
  ;; Cancellami? (message "use-package company")
  ;; (global-company-mode)
  ;; (add-hook 'after-init-hook 'global-company-mode)
  ;; -> Perche' disabled?????? :disabled t
  :ensure t
  :defer 2
  :config
  ;; (add-hook 'after-init-hook 'global-company-mode)
  (global-company-mode t)
  :diminish company-mode "CMP"
  )

(use-package esup
  ;; Emacs Start Up Profiler https://github.com/jschaf/esup
  :init
  ;; Cancellami? (message "use-package esup")
  :ensure t
  )

(use-package ibuffer
  ;; Shows a list of buffers
  ;; https://www.emacswiki.org/emacs/IbufferMode
  :init
  ;; Cancellami? (message "use-package ibuffer")
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
  ;; Cancellami? (message "use-package ido")
  :ensure t
  :config
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
  ;;
  (setq ido-everywhere t)
  ;;
  (ido-mode t)
  )


;; Warning (ido-ubiquitous): The ido-ubiquitous package is now redundant.
;; All functionality, including ido-ubiquitous-mode, has been merged into the ido-completing-read+ package.
;; You should replace ido-ubiquitous with ido-completing-read+ in your Emacs config.
;; (use-package ido-ubiquitous
;;   ;; Ido-ubiquitous
;;   ;; This enables ido in all contexts where it could be useful, not just
;;   ;; for selecting buffer and file names
;;   :init
;;   ;; Cancellami? (message "use-package ido-ubiquitous")
;;   ;; (ido-ubiquitous-mode 1)
;;   :ensure t
;;   :defer t
;;   :after
;;   ido
;;   :config
;;   (ido-ubiquitous-mode 1)
;;   )

(use-package ido-completing-read+
  ;; Ido-completing-read+
  ;; A completing-read-function using ido
  :init
  ;; Cancellami? (message "use-package ido-completing-read+")
  ;; (ido-ubiquitous-mode 1)
  :ensure t
  :defer t
  :after
  ido
  :config
  (ido-ubiquitous-mode 1)
  )

;;(use-package org-plus-contrib
(use-package org
  :init
  ;; Cancellami? (message "use-package org-plus-contrib :init")
  ;;:ensure t
  :ensure org-plus-contrib
  :defer t
  :config
  ;; Cancellami? (message "use-package org-plus-contrib :config")
  (add-to-list 'org-latex-packages-alist '("" "tabularx" nil))
  (add-to-list 'org-latex-packages-alist '("" "tabu" nil))
  )

(use-package org-bullets
  :init
  :ensure t
  :defer t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  :after
  org
  )

;;(use-package org
;;  :init
;;  ;; Cancellami? (message "use-package org")
;;  :ensure org-plus-contrib
;;  :defer t
;;  ;; Non e' necessario impostare :mode
;;  ;; :mode "\\.org\\"
;;  ;; :mode (("\\.org$" . org-mode))
;;  )
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
  ;; Cancellami? (message "use-package recentf")
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
  ;; Cancellami? (message "use-package smex")
  ;; (smex-initialize)
  :ensure t
  :bind
  ("M-x" . smex)
  ("M-X" . smex-major-mode-commands)
  ;; This is your old M-x.
  ("C-c C-c M-x" . execute-extended-command)
  :config
  (smex-initialize)
  (setq smex-save-file (concat user-emacs-directory ".smex-items"))
  )

(use-package tramp
  :init
  ;; Cancellami? (message "use-package tramp")
  :ensure t
  :defer t
  :config
  (cond ((eq system-type 'windows-nt)
         ;; Windows-specific code goes here.
         (setq tramp-default-method "pscp")
         )
        ((eq system-type 'gnu/linux)
         ;; Linux-specific code goes here
         (setq tramp-default-method "ssh")
         ))
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
  ;; Cancellami? (message "use-package uniquify")
  :ensure t
  :defer t
  :config
  (setq uniquify-buffer-name-style 'forward)
  )

(use-package whitespace
  :init
  ;; Cancellami? (message "use-package whitespace")
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

(use-package switch-window
  ;; A *visual* way to choose a window to switch to
  :init
  ;; Cancellami? (message "use-package switch-window")
  ;; (global-set-key (kbd "C-x o") 'switch-window)
  ;; (global-set-key (kbd "C-x 1") 'switch-window-then-maximize)
  ;; (global-set-key (kbd "C-x 2") 'switch-window-then-split-below)
  ;; (global-set-key (kbd "C-x 3") 'switch-window-then-split-right)
  ;; (global-set-key (kbd "C-x 0") 'switch-window-then-delete)
  :ensure t
  :defer t
  :bind (("C-x o" . switch-window)
           ("C-x 1" . switch-window-then-maximize)
           ("C-x 2" . switch-window-then-split-below)
           ("C-x 3" . switch-window-then-split-right)
           ("C-x 0" . switch-window-then-delete))
  :config
  )

(use-package swap-buffers
  ;; A *visual* way to choose a window to switch to
  :init
  ;; Cancellami? (message "use-package swap-buffers")
  :ensure t
  :defer t
  )

;; (use-package buffer-move
;;   ;; easily swap buffers
;;   :init
;;   ;; Cancellami? (message "use-package buffer-move")
;;   :ensure t
;;   :defer t
;;   )

;; (use-package window-number
;;   ;; Select windows by numbers
;;   :init
;;   ;; Cancellami? (message "use-package window-number")
;;   :ensure t
;;   :defer t
;;   )

(use-package which-key
  ;; Display available keybindings in popup
  :init
  ;; Cancellami? (message "use-package which-key")
  :ensure t
  ;; Non si deve differire altrimenti non parte
  ;;:defer t
  :config
  (which-key-mode t)
  )

(use-package cursor-chg
  ;; Change cursor dynamically, depending on the context.
  :init
  ;; Cancellami? (message "use-package cursor-chg")
  :ensure t
  :config
  (change-cursor-mode 1) ; On for overwrite/read-only/input mode
  ;;(toggle-cursor-type-when-idle 1) ; On when idle
  
  )

(use-package undo-tree
  ;; Treat undo history as a tree
  :init
  ;; Cancellami? (message "use-package undo-tree")
  :ensure t
  :defer t
  :diminish undo-tree-mode
  :config
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t))
  )

(use-package fill-column-indicator
  ;; Graphically indicate the fill column
  :init
  :ensure t
  :defer t
  :config
  (setq fci-handle-truncate-lines nil)
  (setq fci-rule-width 1)
  )

(use-package try
  ;; Try out Emacs packages.
  :init
  :ensure t
  :defer t
  :config
  )

(use-package origami
  ;; Flexible text folding
  :init
  :ensure t
  :defer t
  :config
  )

;; =========================================================================
;; GIT
;; =========================================================================
(use-package magit
  ;; A GIT porcelain inside Emacs
  :init
  ;; Cancellami? (message "use-package magit")
  ;; Disable built-in VC for Git when using magit
  ;; (setq vc-handled-backends (delq 'Git vc-handled-backends))
  ;; (global-set-key (kbd "C-x g") 'magit-status)
  :bind
  ("C-x g" . magit-status)
  :ensure t
  :defer t
  :config
  ;; (magit-diff-use-overlays nil)
  ;; some packages already provide their own interfaces to ido, so
  ;; ido-completing-read+ specifically avoids interfering with these.
  ;; If you use any of the following packages, you need to enable ido for
  ;; each of them separately.
  (setq magit-completing-read-function 'magit-ido-completing-read)
  )

(use-package gitconfig-mode
  :init
  ;; Cancellami? (message "use-package gitconfig-mode")
  :ensure t
  :defer t
  :mode ("/\\.gitconfig\\'" "/\\.git/config\\'" "/git/config\\'" "/\\.gitmodules\\'")
  )

(use-package gitignore-mode
  :init
  ;; Cancellami? (message "use-package gitignore-mode")
  :ensure t
  :defer t
  :mode ("/\\.gitignore\\'" "/\\.git/info/exclude\\'" "/git/ignore\\'")
  )

;; =========================================================================
;; Development: generic
;; =========================================================================
(use-package company-quickhelp
  ;; Popup documentation for completion candidates
  :init
  ;; Cancellami? (message "use-package company-quickhelp")
  :ensure t
  :defer t
  :after
  company
  :config
  (company-quickhelp-mode t)
  )

(use-package flycheck
  :init
  ;; Cancellami? (message "use-package flycheck")
  :ensure t
  :defer t
  )

(use-package indent-guide
  ;; Show vertical lines to guide indentation
  :init
  ;; Cancellami? (message "use-package flycheck")
  :defer t
  :ensure t
  )

(use-package projectile
  ;; Project navigation
  :init
  ;; Cancellami? (message "use-package projectile")
  ;; (projectile-global-mode t)
  :ensure t
  :defer t
  :config
  (projectile-global-mode t)
  )

(use-package ibuffer-projectile
  ;;Group ibuffer's list by projectile root
  :init
  ;; Cancellami? (message "ibuffer-projectile")
  :ensure t
  :defer t
  :after
  ibuffer
  projectile
  )

(use-package project-explorer
  ;; A project explorer sidebar
  :init
  ;; Cancellami? (message "use-package project-explorer")
  :ensure t
  :defer t
  )

(use-package rainbow-delimiters
  ;; Colorful parentesis matching
  :init
  ;; Cancellami? (message "use-package rainwbow-delimiters")
  :ensure t
  :defer t
  )

(use-package yasnippet
  ;; Yet another snippet extension for Emacs
  :init
  ;; Cancellami? (message "use-package yasnippet")
  :ensure t
  :defer 2
  :config
  (yas-global-mode t)
  )

;; (use-package auto-complete
;;   ;; Auto Completion for GNU Emacs
;;   :init
;;   ;; Cancellami? (message "use-package auto-complete")
;;   :ensure t
;;   :defer t
;;   :config
;;   (ac-config-default)
;;   )

;; =========================================================================
;; Development: C#
;; =========================================================================
(use-package csharp-mode
  ;; C# mode
  :init
  ;; Cancellami? (message "use-package csharp-mode")
  :ensure t
  :defer t
  ;; Non e' necessario impostare :mode
  ;; :mode "\\.cs\\"
  )

;; =========================================================================
;; Development: Powershell
;; =========================================================================
(use-package powershell
  :init
  ;; Cancellami? (message "use-package powershell")
  :ensure t
  :defer t
  ;; Non e' necessario impostare :mode
  ;; :mode ("\\.ps[dm]?1\\'" . powershell-mode)
  :config
  (add-hook 'powershell-mode-hook 'work-style)
  )

;; =========================================================================
;; Develpment: Python
;; =========================================================================
(use-package elpy
  :init
  ;; Cancellami? (message "use-package elpy")
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
  ;; Non serve qui perche' che' nel config di py-autopep8 ?????
  ;; (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
  (cond ((eq system-type 'windows-nt)
         ;; Windows-specific code goes here.
         (setq python-shell-completion-native-enable nil)
         ))
  )

(use-package py-autopep8
  ;; Autopep8
  :init
  ;; Cancellami? (message "use-package py-autopep8")
  :ensure t
  :defer t
  :config
  (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
  )

(use-package company-jedi
  ;; company-mode completion back-end for Python JEDI
  :init
  ;; Cancellami? (message "use-package company-jedi")
  :ensure t
  :defer t
  :after
  company
  elpy
  )


;; =========================================================================
;; Development: Go language
;; =========================================================================
(use-package go-mode
  ;; Major mode for the Go programming language
  ;;
  ;; -> occorre installare i seguenti pacchetti:
  ;; go get -u golang.org/x/tools/cmd/goimports
  ;; ;; NO - Gia' presente?     ;;go get -u golang.org/x/tools/cmd/vet
  ;; ;; NO - Sostituito da GURU ;;go get -u golang.org/x/tools/cmd/oracle
  ;; go get -u golang.org/x/tools/cmd/guru
  ;; go get -u golang.org/x/tools/cmd/godoc
  ;; go get -u github.com/golang/lint/golint
  ;;
  ;; NonWindwos: go get -u github.com/nsf/gocode
  ;; Windows: go get -u -ldflags -H=windowsgui github.com/nsf/gocode
  ;; go get -u github.com/kisielk/errcheck
  ;;
  :init
  ;; Cancellami? (message "use-package go-mode")
  :ensure t
  :defer t
  :config
  (add-to-list 'load-path (concat (getenv "GOPATH") "/bin"))
  (setq compile-command "go build -v && go test -v && go vet && golint && errcheck")
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)
  )

(use-package go-eldoc
  ;; eldoc for go-mode
  :init
  ;; Cancellami? (message "use-package go-eldoc")
  :ensure t
  :defer t
  :after
  go-mode
  :config
  (go-eldoc-setup)
  )

(use-package company-go
  ;; company-mode backend for Go (using gocode)
  :init
  ;; Cancellami? (message "use-package company-go")
  ;;(add-to-list 'company-backends 'company-go)
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-go))
  :ensure t
  :defer t
  :after
  company
  go-mode
  :config
  )

(use-package golint
  ;; lint for the Go source code
  :init
  ;; Cancellami? (message "use-package golint")
  :ensure t
  :defer t
  :config
  (add-to-list 'load-path (concat (getenv "GOPATH") "/src/github.com/golang/lint/misc/emacs"))
  )

(use-package go-guru
  ;; Integration of the Go 'guru' analysis tool into Emacs.
  :init
  ;; Cancellami? (message "use-package go-guru")
  :ensure t
  :defer t
  )

(use-package go-errcheck
  ;; errcheck integration for go-mode
  :init
  ;; Cancellami? (message "use-package go-errcheck")
  :ensure t
  :defer t
  )





;; =========================================================================
;; Markdown
;; =========================================================================
(use-package markdown-mode
  ;; Major mode for Markdown-formatted text
  :init
  ;; Cancellami? (message "use-package markdown-mode")
  :ensure t
  :defer t
  )

;; =========================================================================
;; Json
;; =========================================================================
(use-package json-mode
  ;; Major mode for editing JSON files
  :init
  ;; Cancellami? (message "use-package json-mode")
  :ensure t
  :defer t
  )






;; =========================================================================
;; Customization (outside of "custom")
;; =========================================================================
;; Cancellami? (message "Customization (outside of 'custom'")
(setq column-number-mode t)                       ;; Modeline - Display current column number (modeline)
(setq hscroll-step 1)                             ;; Windows - Number of column to scroll when points get too close to the edge
(setq scroll-conservatively most-positive-fixnum) ;; Windows - Number of lines to try scrolling a windows when point moves out
(setq inhibit-startup-screen t)                   ;; Initialization - Inhibits the startup screen

(if (< emacs-major-version 25)
    ;; "x-" versions considered obsolete after
    ;; emacs 25.1
    (progn (setq x-select-enable-clipboard t)     ;; makes killing/yanking interact with the clipboard 
           (setq x-select-enable-primary t))      ;;
  (progn (setq select-enable-clipboard t)
         (setq select-enable-primary t)))

(setq save-interprogram-paste-before-kill t)      ;;
(setq apropos-do-all t)                           ;; Shows all options when running apropos.
(setq mouse-yank-at-point t)                      ;; Mouse yank commands yank at point instead of at click.
(setq ring-bell-function 'ignore)                 ;; No bell
(setq transient-mark-mode t)                      ;; Transient mark mode: mostra il testo selezionato come selezionato

(delete-selection-mode t)                         ;; Attiva delete-selection-mode
(set-scroll-bar-mode 'right)                      ;; Scrollbars - right
(global-hl-line-mode 1)                           ;; Highlight current line
(setq custom-safe-themes t)                       ;; Treat all themes as safe (warning: security issue!!!!)

;; (setq cua-mode t nil)                        ;; CUA Mode
;; (setq-default frame-title-format "%b (%f)")  ;; full path in title bar

;; ;; don't pop up font menu
;; (global-set-key (kbd "s-t") '(lambda () (interactive)))

;; ;; No cursor blinking, it's distracting
;; (blink-cursor-mode 0)

;; VC Auto Revert mode updates the version control status information
;; every "auto-revert-interval" seconds, even if the work file itself is unchanged
;; auto-revert-interval - Default = 5 sec
(setq auto-revert-check-vc-info t)

;; Impedisce lo split quando si aprono piu' files
;; NOTA BENE: Perche' la cosa funzioni correttamente e' necessario che
;;            inhibit-startup-screen sia impostato a t (vedi piu' sopra)
(add-hook 'emacs-startup-hook
          (lambda () (delete-other-windows)) t)


;; =========================================================================
;; Tabulation settings
;; =========================================================================
(setq-default tab-width 4)                      ;; Set tab width to 4 spaces
(setq-default indent-tabs-mode nil)             ;; Use spaces instead of tabs
(setq tab-stop-list (number-sequence 4 200 4))  ;; Create list of tab stops every 4 char

;; =========================================================================
;; Microsoft Windows settings
;; =========================================================================
(cond ((eq system-type 'windows-nt)
       ;; ;; Imposto Find e Grep
       ;; (setq find-program (concat invocation-directory "find.exe")
       ;;       grep-program (concat invocation-directory "grep.exe"))
       ;; (setenv "PATH" (concat "C:\\msys64\\usr\\bin;" (getenv "PATH")))
       ;; (setq exec-path (append exec-path '("C:/msys64/usr/bin")))
       (setq msys2-path "C:\\msys64\\usr\\bin")
       (setq find-program (concat msys2-path "\\" "find.exe")
             grep-program (concat msys2-path "\\" "grep.exe")
             diff-command (concat msys2-path "\\" "diff.exe")
             )
       (setq grep-use-null-device nil)
       ;; (setq grep-find-template (concat
       ;;                           (concat
       ;;                            (concat invocation-directory "find.exe")
       ;;                            " <D> <X> -type f <F> -exec ")
       ;;                           (concat
       ;;                            (concat invocation-directory "grep.exe")
       ;;                            " <C> -nH <R> {} \";\"")))
       (setq grep-find-template (concat
                                 (concat find-program " <D> <X> -type f <F> -exec ")
                                 (concat grep-program " <C> -nH <R> {} \";\"")))
       ;; "c:/Editors/emacs/emacs/bin/find.exe <D> <X> -type f <F> -exec c:/Editors/emacs/emacs/bin/grep.exe <C> -n <R> {} NUL \";\"")
       ))

;; =========================================================================
;; Printing
;; =========================================================================
(setq ps-paper-type 'a4)
;; (setq ps-print-color-p 'black-white)
(setq doc-view-continuous t)
(cond ((eq system-type 'windows-nt)
       ;; Windows-specific code goes here.
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
       (setq doc-view-ghostscript-program "C:/Program Files/gs/gs9.16/bin/gswin64c.exe")
       (setq ps-printer-name t)
       )
      ((eq system-type 'gnu/linux)
       ;; Linux-specific code goes here. 
       ))

;; Start Emacs fullscreen mode
;; (add-hook 'emacs-startup-hook 'toggle-frame-maximized)

;; Emacs Server
;; (require 'server)
;; (unless (server-running-p) (server-start))

;; =========================================================================
;; Custom
;; =========================================================================
;; Cancellami? (message "Loading 'custom'")
(setq custom-file (locate-user-emacs-file "custom-set-settings.el"))
(load custom-file t)

;; =========================================================================
;; Color theme setup
;; =========================================================================
(when (display-graphic-p)
  ;; Cancellami? (message "Loading material theme")
  ;; (load-theme 'material t)
  ;; (if (>= emacs-major-version 24)
  ;;     (load-theme 'solarized-dark t)
  ;;  )

  ;; Pre il momento torno alle origini!!!!
  ;; uso il tema di default!!!
  ;; (load-theme 'zenburn t)
  )


;; =========================================================================
;; Font setup
;; =========================================================================
;; from https://www.reddit.com/r/emacs/comments/1xe7vr/check_if_font_is_available_before_setting/
; Test char and monospace:
; 0123456789abcdefghijklmnopqrstuvwxyz [] () :;,. !@#$^&*
; 0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ {} <> "'`  ~-_/|\?
(cond 
 ((find-font (font-spec :name "DejaVu Sans Mono"))
  (set-frame-font "DejaVu Sans Mono-10"))
 ((find-font (font-spec :name "Consolas"))
  (set-frame-font "Consolas-10"))
 ((find-font (font-spec :name "Inconsolata"))
  (set-frame-font "Inconsolata-10"))
 ;; ((find-font (font-spec :name "Lucida Console"))
 ;;  (set-frame-font "Lucida Console-10"))
 ((find-font (font-spec :name "Courier New"))
  (set-frame-font "Courier New-10"))
 ;; ((find-font (font-spec :name "courier"))
 ;;  (set-frame-font "courier-10"))
 )

;; https://www.reddit.com/r/emacs/comments/55ork0/is_emacs_251_noticeably_slower_than_245_on_windows/
;; -------------------------------------------------------------------------------------------------
;; Riattivo l'hook disattivato in testa al file
(if (>= emacs-major-version 25)
    (add-hook 'find-file-hooks 'vc-refresh-state)
  (add-hook 'find-file-hooks 'vc-find-file-hook))

;; ***************************************************************************
;; Per una spiegazione sul coding dei caratteri in emacs vedere:
;; https://www.masteringemacs.org/article/working-coding-systems-unicode-emacs
;; ---------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8
;; mode: lisp
;; indent-tabs-mode: nil
;; tab-width: 4
;; End:
;; ***************************************************************************
