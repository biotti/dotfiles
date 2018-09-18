;;
;; Da verificare:
;; https://sam217pa.github.io/2016/08/30/how-to-make-your-own-spacemacs/
;; http://pragmaticemacs.com/emacs/get-that-spacemacs-look-without-spacemacs/
;;
;; https://github.com/syl20bnr/spacemacs
;; https://github.com/bbatsov/prelude
;; https://github.com/purcell/emacs.d
;; https://github.com/magnars/.emacs.d
;; https://github.com/eschulte/emacs24-starter-kit
;; https://github.com/xiaohanyu/oh-my-emacs
;; https://github.com/technomancy/better-defaults
;; https://github.com/rdallasgray/graphene
;; https://github.com/bodil/ohai-emacs
;; https://github.com/ergoemacs/ergoemacs-mode
;; https://github.com/technomancy/emacs-starter-kit
;; https://github.com/dimitri/emacs-kicker
;; https://github.com/kjhealy/emacs-starter-kit
;; https://github.com/jabranham/emacs-for-social-science
;; https://github.com/overtone/emacs-live
;; https://github.com/jkitchin/scimax
;;
;; Customize per Os/Emacs version
;; https://www.emacswiki.org/emacs/CustomizingBoth

;; Interesting variables: C-h v
;; ----------------------------
;; data-directory
;; emacs-version
;; emacs-build-system
;; emacs-repository-version
;; system-configuration
;; system-configuration-options
;; system-configuration-features

;; https://www.reddit.com/r/emacs/comments/55ork0/is_emacs_251_noticeably_slower_than_245_on_windows/
;; https://www.reddit.com/r/emacs/comments/7t4kxw/how_can_i_improve_startup_time_despite_many/
;; -------------------------------------------------------------------------------------------------
;; https://github.com/hlissner/doom-emacs/wiki/FAQ#how-is-dooms-startup-so-fast
;; -------------------------------------------------------------------------------------------------
;; Garbage collection, valori di default;
;; gc-cons-threshold  -> 800000
;; gc-cons-percentage -> 0.1
;; Li reimposto in coda al file
;; -----------------
;; 536870912 = 512MB
(setq gc-cons-threshold 536870912)
(setq gc-cons-percentage 0.6)
;; -----------------
;; ???? (run-with-idle-timer 5 t #'garbage-collect)
;; (setq garbage-collection-messages t)

;; https://github.com/hlissner/doom-emacs/wiki/FAQ#how-is-dooms-startup-so-fast
(defvar my/init-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(defun my/init-startup-hook ()
  "Funzione richiamata dall'hook emacs-startup-hook."
  (interactive)
  (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)
  ;; https://github.com/hlissner/doom-emacs/wiki/FAQ#how-is-dooms-startup-so-fast
  (setq file-name-handler-alist my/init-file-name-handler-alist))

;; https://blog.d46.us/advanced-emacs-startup/
(add-hook 'emacs-startup-hook 'my/init-startup-hook)

;; Set default coding system to UTF-8
(prefer-coding-system 'utf-8)

;; Disattivo temporaneamente l'hook (lo riattivo in coda al file)
(if (>= emacs-major-version 25)
    (remove-hook 'find-file-hooks 'vc-refresh-state)
  (remove-hook 'find-file-hooks 'vc-find-file-hook))

;; https://github.com/howardabrams/dot-files/blob/master/emacs.org
(setq gnutls-min-prime-bits 4096)

;; Setup package.el
(require 'package)
;; https://github.com/sondr3/dotfiles/blob/master/emacs.org
;; Then we ll make sure we always load newer files if they are available,
;; even if there s a byte compiled version and disable automatic requiring
;; of packages on start as it ll be handled by use-package.
(setq load-prefer-newer t)
;; (setq package-enable-at-startup nil)

;; =========================================================================
;; Manage package repositories
;; =========================================================================
;;
;; Org (org-mode)
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/"))
;;
;; MELPA - Stable
;;(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
;;                    (not (gnutls-available-p))))
;;       (url (concat (if no-ssl "http" "https") "://stable.melpa.org/packages/")))
;;  (add-to-list 'package-archives (cons "melpa-stable" url) t))
;;
;; MELPA - Unstable
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
  (add-to-list 'package-archives (cons "melpa" url) t))
;;
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
;;
;;(setq package-archive-priorities
;;      '(("melpa-stable" . 10)
;;        ("gnu"          . 5)
;;        ("melpa"        . 0)))

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; =========================================================================
;; use-package custom setup
;; =========================================================================
 (setq use-package-debug t)
 (setq use-package-verbose t)
;; (setq use-package-enable-imenu-support t)
 (setq use-package-minimum-reported-time 0.0001)
;; =========================================================================
;; Bootstrap `use-package'
;; =========================================================================
;; N.B.: Remove e reinstall "use-package" from "list-packages"
;;       if there are error processing .emacs file after a
;;       package upgrade (use-package.el may be empty!!!)
;; Utile da leggere: http://irreal.org/blog/?p=6442
(unless (package-installed-p 'use-package)
  (message "Refreshing package database...")
  (package-refresh-contents)
  (message "Done refreshing. Installing use-package")
  (package-install 'use-package)
  (message "Done installing.")
  )

;; Da testare: sembra non essere necessario
;; https://cestlaz.github.io/posts/using-emacs-1-setup/
;; Per questo commento la eval-when-compile
(eval-when-compile
  (require 'use-package))

;; Non servono???? (sono dipendenze di use-package???)
;; (require 'diminish)
;; (require 'bindkey)

;; =========================================================================
;; Benchmark-init
;; Se voglio fare benchmarking devo caricarlo prima posssibile
;; per quanto, necessariamente, dopo l'installazione di use-package
;; =========================================================================
(use-package benchmark-init
  :init
  :ensure t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate)
  )

;; =========================================================================
;; Diminish
;; Non piu' inserito come dipendenza in use-package, occorre installarlo
;; esplicitamente se si usa il :diminish
;; =========================================================================
(use-package diminish
  :init
  :ensure t
  )

;; Color Themes (use (load-theme xxx) at the end
;; =========================================================================
;; Color Theme
;; =========================================================================
(use-package spacemacs-common
  ;; Port of vim's mustang theme
  :if (display-graphic-p)
  :ensure spacemacs-theme
  ;;:defer t
  ;; :config
  ;; (load-theme 'spacemacs-dark t)
  )

(load-theme 'spacemacs-dark t)

;; =========================================================================
;; Color theme setup
;; =========================================================================
;; (when (display-graphic-p)
;;   ;; (load-theme 'material t)
;;   ;; (if (>= emacs-major-version 24)
;;   ;;     (load-theme 'solarized-dark t)
;;   ;;  )
;;   ;;(load-theme 'tango-dark t)
;;   
;;   )

;; =========================================================================
;; EMACS enhancements
;; =========================================================================
;; (use-package try
;;   :init
;;   :ensure t
;;   ;; Do not defer
;;   ;; defer t
;;   :config
;;   )

(use-package company
  ;; A modular text completion framework
  :init
  :ensure t
  :defer t
  :config
  ;; (global-company-mode t)
  (add-to-list 'company-backends 'company-restclient)
  :diminish company-mode "Cmp"
  )

(use-package esup
  ;; Emacs Start Up Profiler https://github.com/jschaf/esup
  :init
  :ensure t
  )

(use-package ibuffer
  ;; Shows a list of buffers
  ;; https://www.emacswiki.org/emacs/IbufferMode
  ;; - emacs internal -
  :init
  :ensure t
  :defer t
  :bind
  ("C-x C-b" . ibuffer)
  )

;; Disattivo IDO per provare?????
;; (use-package ido
;;   ;; ido-mode allows you to more easily navigate choices. For example,
;;   ;; when you want to switch buffers, ido presents you with a list
;;   ;; of buffers in the the mini-buffer. As you start to type a buffer's
;;   ;; name, ido will narrow down the list of buffers to match the text
;;   ;; you've typed in
;;   ;; http://www.emacswiki.org/emacs/InteractivelyDoThings
;;   ;; - emacs internal -
;;   :init
;;   ;; (ido-mode t)
;;   :ensure t
;;   :config
;;   ;; This allows partial matches, e.g. "tl" will match "Tyrion Lannister"
;;   (setq ido-enable-flex-matching t)
;;   ;; Turn this behavior off because it's annoying
;;   (setq ido-use-filename-at-point nil)
;;   ;; Don't try to match file across all "work" directories; only match files
;;   ;; in the current directory displayed in the minibuffer
;;   (setq ido-auto-merge-work-directories-length -1)
;;   ;; Includes buffer names of recently open files, even if they're not
;;   ;; open now
;;   (setq ido-use-virtual-buffers t)
;;   ;;
;;   (setq ido-everywhere t)
;;   ;;
;;   (ido-mode t)
;;   )


;; Disattivo IDO per provare?????
;; (use-package ido-completing-read+
;;   ;; Ido-completing-read+
;;   ;; A completing-read-function using ido
;;   :init
;;   ;; (ido-ubiquitous-mode 1)
;;   :ensure t
;;   :defer t
;;   :after
;;   ido
;;   :config
;;   (ido-ubiquitous-mode 1)
;;   )

;; Testing Ivy, Swiper & Counsel
(use-package ivy
  :init
  :ensure t
  :demand t
  :diminish ivy-mode
  :bind ("C-c C-r" . ivy-resume)
  :config
  (progn
    (ivy-mode t)
    (setq ivy-use-virtual-buffers t)
    (setq enable-recursive-minibuffers t)
    ;;(global-set-key (kbd "C-c C-r") 'ivy-resume)
    ;; (global-set-key (kbd "<f6>") 'ivy-resume)
    )
  )

(use-package swiper
  :init
  :ensure t
  :demand t
  :after (:all ivy)
  :bind ("\C-s" . swiper)
  :config
  ;; (global-set-key "\C-s" 'swiper)
  )

(use-package counsel
  :init
  :ensure t
  :demand t
  :diminish counsel-mode
  :after (:all ivy swiper)
  :bind (("<f2> u" . counsel-unicode-char)
         ("C-c g"  . counsel-git)
         ("C-c j"  . counsel-git-grep))
  :config
  (progn
    (counsel-mode t)
    ;; (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
    ;; (global-set-key (kbd "C-c g") 'counsel-git)
    ;; (global-set-key (kbd "C-c j") 'counsel-git-grep)
    ;; ;; (global-set-key (kbd "C-c k") 'counsel-ag)
    ;; ;; (global-set-key (kbd "C-x l") 'counsel-locate)
    ;; ;; (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
    (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
    )
  )

(use-package org
  :init
  ;;:ensure t
  :ensure org-plus-contrib
  :defer t
  :config
  (add-to-list 'org-latex-packages-alist '("" "tabularx" nil))
  (add-to-list 'org-latex-packages-alist '("" "tabu" nil))
  )

(use-package org-bullets
  :init
  :hook (org-mode . org-bullets-mode)
  :ensure t
  :defer t
  :config
  :after (:all org)
  )

;; (use-package recentf
;;   ;; Turn on recent file mode so that you can more easily switch to
;;   ;; recently edited files when you first start emacs
;;   ;; - emacs internal -
;;   :init
;;   :ensure t
;;   :config
;;   (setq recentf-save-file (concat user-emacs-directory ".recentf"))
;;   (setq recentf-exclude '(".ido.last"))
;;   (setq recentf-max-menu-items 40)
;;   (recentf-mode 1)
;;   )

;; (use-package smex
;;   ;; Enhances M-x too allow easier execution of commands. Provides
;;   ;; a filterable list of possible commands in the minibuffer
;;   ;; http://www.emacswiki.org/emacs/Smex
;;   :init
;;   ;; (smex-initialize)
;;   :ensure t
;;   ;; Using IVY counsel-M-x
;;   ;; :bind
;;   ;; ("M-x" . smex)
;;   ;; ("M-X" . smex-major-mode-commands)
;;   ;; ;; This is your old M-x.
;;   ;; ("C-c C-c M-x" . execute-extended-command)
;;   :config
;;   (smex-initialize)
;;   (setq smex-save-file (concat user-emacs-directory ".smex-items"))
;;   )

(use-package amx
  ;; Alternative M-x with extra features.
  :init
  :ensure t
  :defer t
  :after (:all counsel)
  ;; :bind (("M-X" . amx-major-mode-commands))
  :config
  (amx-mode t)
  )

(use-package tramp
  ;; - emacs internal -
  :init
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

(use-package counsel-tramp
  ;; Tramp ivy interface for ssh, docker, vagrant
  :init
  :ensure t
  :defer t
  :after (all: counsel tramp)
  :config
  )

(use-package uniquify
  ;; When several buffers visit identically-named files,
  ;; Emacs must give the buffers distinct names. The usual method
  ;; for making buffer names unique adds '<2>', '<3>', etc. to the end
  ;; of the buffer names (all but one of them).
  ;; The forward naming method includes part of the file's directory
  ;; name at the beginning of the buffer name
  ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Uniquify.html
  ;; - emacs internal -
  :disabled t
  :init
  :ensure t
  :defer t
  :config
  (setq uniquify-buffer-name-style 'forward)
  )

(use-package whitespace
  ;; - emacs internal -
  :init
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
  :ensure t
  :defer t
  )

(use-package winum
  ;; https://github.com/deb0ch/emacs-winum
  :init
  :ensure t
  ;; NON USARE DEFER!
  ;;:defer t
  :config
  ;; Per l'uso con spaceline
  ;; https://github.com/TheBB/spaceline#winum
  ;;(setq winum-auto-setup-mode-line nil)
  (winum-mode)
  )

;; (use-package buffer-move
;;   ;; easily swap buffers
;;   :init
;;   :ensure t
;;   :defer t
;;   )

;; (use-package window-number
;;   ;; Select windows by numbers
;;   :init
;;   :ensure t
;;   :defer t
;;   )

(use-package which-key
  ;; Display available keybindings in popup
  :init
  :ensure t
  ;; Non si deve differire altrimenti non parte
  ;;:defer t
  :config
  (which-key-mode t)
  )

;; Non piu' presente su melpa 2018-04-11
;; (use-package cursor-chg
;;   ;; Change cursor dynamically, depending on the context.
;;   :init
;;   :ensure t
;;   :config
;;   (change-cursor-mode 1) ; On for overwrite/read-only/input mode
;;   ;;(toggle-cursor-type-when-idle 1) ; On when idle
;;   
;;   )

(use-package undo-tree
  ;; Treat undo history as a tree
  :init
  :ensure t
  :defer t
  :diminish undo-tree-mode "Ut"
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

;; Sperimentazione modeline migliorate:
;; -> powerline
;; -> smart-mode-line
;; -> spaceline, magari con spaceline-all-the-icons
;;    se installato all-the-icons e i relativi fonts
;;    e magari spacemacs-theme
;; -> telephone-line
;; -> micgoline
;;(use-package smart-mode-line
;;  ;; https://github.com/Malabarba/smart-mode-line
;;  :init
;;  :ensure t
;;  ;;:defer t
;;  :config
;;  (setq sml/no-confirm-load-theme t)
;;  (sml/setup)
;;  )

;; (use-package powerline
;;   ;; https://github.com/milkypostman/powerline/
;;   :init
;;   ;; (setq powerline-default-separator 'alternate)
;;   ;; (setq powerline-default-separator 'arrow)
;;   ;; (setq powerline-default-separator 'arrow-fade)
;;   ;; (setq powerline-default-separator 'bar)
;;   ;; (setq powerline-default-separator 'box)
;;   ;; (setq powerline-default-separator 'brace)
;;   ;; (setq powerline-default-separator 'butt)
;;   ;; (setq powerline-default-separator 'chamfer)
;;   ;; (setq powerline-default-separator 'contour)
;;   ;; (setq powerline-default-separator 'curve)
;;   ;; (setq powerline-default-separator 'rounded)
;;   ;; (setq powerline-default-separator 'roundstub)
;;   (setq powerline-default-separator 'slant)
;;   ;; (setq powerline-default-separator 'wave)
;;   ;; (setq powerline-default-separator 'zigzag)
;;   ;; (setq powerline-default-separator 'nil)
;;   ;; (setq powerline-default-separator 'utf-8)
;;   :ensure t
;;   ;; NON USARE DEFER!
;;   ;; :defer t
;;   :after
;;   winum
;;   :config
;;   ;;(powerline-default-theme)
;;   ;;(powerline-center-theme)
;;   ;;(powerline-center-evil-theme)
;;   ;;(powerline-vim-theme)
;;   ;;(powerline-nano-theme)
;;   )
;; (use-package spaceline-config
;;   ;; https://github.com/TheBB/spaceline
;;   :init
;;   :ensure spaceline
;;   ;; NON USARE DEFER!
;;   ;; :defer t
;;   :after
;;   powerline
;;   :config
;;   (spaceline-spacemacs-theme)
;;   ;;(spaceline-emacs-theme)
;;   )
;;(use-package spaceline-all-the-icons 
;;  ;; https://github.com/domtronn/spaceline-all-the-icons.el
;;  :init
;;  ;;:defer t
;;  :after
;;  spaceline
;;  :config
;;  (spaceline-all-the-icons-theme)

(use-package highlight-indentation
  ;; https://github.com/antonj/Highlight-Indentation-for-Emacs
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
  (setenv "GIT_ASKPASS" "git-gui--askpass")
  )

(use-package gitconfig-mode
  :init
  :ensure t
  :defer t
  :mode ("/\\.gitconfig\\'" "/\\.git/config\\'" "/git/config\\'" "/\\.gitmodules\\'")
  )

(use-package gitignore-mode
  :init
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
  :ensure t
  :defer t
  :after (:all company)
  :config
  (company-quickhelp-mode t)
  )

(use-package flycheck
  :init
  :hook (prog-mode . flycheck-mode)
  :ensure t
  :defer t
  )

(use-package indent-guide
  ;; Show vertical lines to guide indentation
  :init
  :defer t
  :ensure t
  )

(use-package projectile
  ;; Project navigation
  :init
  ;; (projectile-global-mode t)
  ;; Attivo projectile soltanto per i "programmi"
  ;; Non lo voglio piu'!!!!!
  ;; :hook (prog-mode . projectile-mode)
  :ensure t
  :defer t
  :config
  (progn
    ;; L'attivazione di projectile in modalita' globale
    ;; e' disattivata in favore di quella impostata in :hook
    ;; (projectile-global-mode t)
    (setq projectile-mode-line
	      '(:eval (if (file-remote-p default-directory)
		              " Prj[*remote*]"
                    (format " Prj[%s]" (projectile-project-name)))))
    )
  )

(use-package ibuffer-projectile
  ;;Group ibuffer's list by projectile root
  :init
  :ensure t
  :defer t
  :after (:all ibuffer projectile)
  )

(use-package counsel-projectile
  ;; Ivy integration for Projectile
  :init
  :ensure t
  :defer t
  :after (:all counsel projectile)
  )

(use-package project-explorer
  ;; A project explorer sidebar
  :init
  :ensure t
  :defer t
  )

;; (use-package paren
;;   :init
;;   :ensure t
;;   :defer t
;;   :config
;;   (progn
;;     (setq show-paren-style 'parenthesis)
;;     (show-paren-mode t))
;;   )

(use-package smartparens
  :ensure t
  :defer t
  :diminish smartparens-mode
  :init
  (require 'smartparens-config)
  :config
  (progn
    ;; (smartparens-global-mode 1)
    (show-smartparens-global-mode 1))
  )

(use-package rainbow-delimiters
  ;; Colorful parentesis matching
  :init
  :ensure t
  :defer t
  )

(use-package yasnippet
  ;; Yet another snippet extension for Emacs
  :init
  :hook (prog-mode . yas-minor-mode)
  :ensure t
  ;;:defer 2
  :defer t
  :config
  ;; (yas-global-mode t)
  )

(use-package yasnippet-snippets
  ;; Collection of yasnippet snippets
  :init
  :ensure t
  :defer t
  :after (:all yasnippet)
  )

;; (use-package auto-complete
;;   ;; Auto Completion for GNU Emacs
;;   :init
;;   :ensure t
;;   :defer t
;;   :config
;;   (ac-config-default)
;;   )

;; =========================================================================
;; Development: Emacs Lisp
;; =========================================================================
(use-package emacs-lisp-mode
  :defer t
  :interpreter ("emacs" . emacs-lisp-mode)
  :diminish emacs-lisp-mode "El"
  :init
  (defun my/emacs-lisp-mode-hook ()
    "Funzione richiamata dall'hook emacs-lisp-mode-hook."
    (interactive)
    (company-mode)
    (yas-minor-mode))
  (add-hook 'emacs-lisp-mode-hook 'my/emacs-lisp-mode-hook)
  :after (:all yasnippet company)
  :config
  )

(use-package el-autoyas
  ;; Automatically create Emacs-Lisp Yasnippets
  :init
  :ensure t
  :defer t
  :after (:all yasnippet)
  )

;; =========================================================================
;; Development: C#
;; =========================================================================
(use-package csharp-mode
  ;; C# mode
  :init
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
  (add-hook 'powershell-mode-hook 'work-style)
  :ensure t
  :defer t
  ;; Non e' necessario impostare :mode
  ;; :mode ("\\.ps[dm]?1\\'" . powershell-mode)
  :config
  )

;; =========================================================================
;; Develpment: Python
;; =========================================================================
(use-package elpy
  :init
  (defun my/elpy-mode-hook ()
    "Funzione richiamata dall'hook elpy-mode-hook."
    (interactive)
    (company-mode)
    (yas-minor-mode))
  (add-hook 'elpy-mode-hook 'my/elpy-mode-hook)
  (with-eval-after-load 'python (elpy-enable))
  :ensure t
  :defer t
  :after (:all company highlight-indentation yasnippet)
  :config
  ;; (elpy-enable)
  (setq elpy-rpc-backend "jedi")
  ;; (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (cond ((eq system-type 'windows-nt)
         ;; Windows-specific code goes here.
         (setq python-shell-completion-native-enable nil)
         ))
  )

(use-package py-autopep8
  ;; Autopep8
  :init
  (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
  :ensure t
  :defer t
  :config
  )

(use-package company-jedi
  ;; company-mode completion back-end for Python JEDI
  :init
  :ensure t
  :defer t
  :after (:all company elpy)
  )


;; =========================================================================
;; Development: Go language
;; =========================================================================
(use-package go-mode
  ;; Major mode for the Go programming language
  ;; Installation:
  ;; 
  ;; go get -u golang.org/x/tools/cmd/goimports
  ;; go get -u golang.org/x/tools/cmd/godoc
  ;; ;; NO - Gia' presente?     ;;go get -u golang.org/x/tools/cmd/vet
  ;; ;; NO - Sostituito da GURU ;;go get -u golang.org/x/tools/cmd/oracle
  ;; -- https://tleyden.github.io/blog/2014/05/22/configure-emacs-as-a-go-editor-from-scratch/
  ;; go get github.com/rogpeppe/godef
  ;;
  ;; Documentazione utilizzata:
  ;; https://tleyden.github.io/blog/2014/05/22/configure-emacs-as-a-go-editor-from-scratch/
  ;; https://johnsogg.github.io/emacs-golang
  ;;
  :init
  (defun my/go-mode-hook ()
    "Funzione richiamata dall'hook go-lisp-mode-hook."
    (interactive)
    (set (make-local-variable 'company-backends) '(company-go))
                           (company-mode)
                           (yas-minor-mode)
                           (go-set-project)
                           (go-eldoc-setup)
                           (flycheck-gometalinter-setup)
                           (if (not (string-match "go" compile-command))
                               (if (eq system-type 'windows-nt)
                                   (setq compile-command "go build -v & go test -v & go vet & gometalinter -t --enable-gc & errcheck")
                                 (setq compile-command "go build -v && go test -v && go vet && gometalinter -t --enable-gc && errcheck")
                                 )
                             (set (make-local-variable 'compile-command)
                                  "go build -v && go test -v && go vet && gometalinter && errcheck")))
  :hook ((before-save-hook . gofmt-before-save)
         (go-mode-hook . my/go-mode-hook))
  :ensure t
  :defer t
  :after (:all flycheck-gometalinter company-mode)
  :config
  (add-to-list 'load-path (concat (getenv "GOPATH") "/bin"))
  (setq-default gofmt-command "goimports")
  )

(use-package company-go
  ;; company-mode backend for Go (using gocode)
  ;; Installation:
  ;;
  ;; Non Windwos: go get -u github.com/nsf/gocode
  ;; Windows    : go get -u -ldflags -H=windowsgui github.com/nsf/gocode
  ;;
  :init
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-go))
  :ensure t
  :defer t
  :after (:all company go-mode)
  :config
  )


(use-package go-rename
  ;; To install:
  ;; % go get golang.org/x/tools/cmd/gorename
  :init
  :ensure t
  :defer t
  :after (:all go-mode)
  :config
  )

(use-package go-eldoc
  ;; eldoc for go-mode
  ;; Installation:
  ;;
  ;; Non Windwos: go get -u github.com/nsf/gocode
  ;; Windows    : go get -u -ldflags -H=windowsgui github.com/nsf/gocode
  ;;
  :init
  :ensure t
  :defer t
  :after (:all go-mode)
  :config
  (go-eldoc-setup)
  )


(use-package golint
  ;; lint for the Go source code
  ;; Installation: Golint requires Go 1.6 or later.
  ;;
  ;; go get -u github.com/golang/lint/golint
  ;;
  :init
  :ensure t
  :defer t
  :config
  (add-to-list 'load-path (concat (getenv "GOPATH") "/src/github.com/golang/lint/misc/emacs"))
  :after (:all go-mode)
  )

(use-package go-guru
  ;; Integration of the Go 'guru' analysis tool into Emacs.
  :init
  :ensure t
  :defer t
  :after (:all go-mode)
  )

(use-package go-errcheck
  ;; errcheck integration for go-mode
  ;; Installation
  ;;
  ;; go get -u github.com/kisielk/errcheck
  ;;
  :init
  :ensure t
  :defer t
  :after (:all go-mode)
  )

(use-package go-snippets
  ;; Yasnippets for go
  :init
  :ensure t
  :defer t
  :after (:all yasnippet go-mode)
  )

(use-package flycheck-gometalinter
  ;; Flycheck checker for golang using gometalinter
  :init
  :ensure t
  :defer t
  :config
  (progn
    (flycheck-gometalinter-setup))
  ;; ;; skips 'vendor' directories and sets GO15VENDOREXPERIMENT=1
  ;; (setq flycheck-gometalinter-vendor t)
  ;; ;; only show errors
  ;; (setq flycheck-gometalinter-errors-only t)
  ;; ;; only run fast linters
  ;; (setq flycheck-gometalinter-fast t)
  ;; ;; use in tests files
  ;; (setq flycheck-gometalinter-test t)
  ;; ;; disable linters
  ;; (setq flycheck-gometalinter-disable-linters '("gotype" "gocyclo"))
  ;; ;; Only enable selected linters
  ;; (setq flycheck-gometalinter-disable-all t)
  ;; (setq flycheck-gometalinter-enable-linters '("golint"))
  ;; ;; Set different deadline (default: 5s)
  ;; (setq flycheck-gometalinter-deadline "10s")
  ;; ----
  :after (:all flycheck go-mode)
  )



;; =========================================================================
;; CSV
;; =========================================================================
(use-package csv-mode
  ;; Major mode for editing comma/char separated values
  :init
  :ensure t
  :defer t
  :config
  )

;; =========================================================================
;; Markdown
;; =========================================================================
(use-package markdown-mode
  ;; Major mode for Markdown-formatted text
  :init
  ;; (setq markdown-command "multimarkdown")
  :ensure t
  :defer t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  :after (:all flycheck)
  :diminish markdown-mode "MDown"
  )

;; =========================================================================
;; Json
;; =========================================================================
(use-package json-mode
  ;; Major mode for editing JSON files
  :init
  :ensure t
  :defer t
  :config
  :after (:all flycheck)
  )

;; =========================================================================
;; SQL
;; =========================================================================
(use-package sql
  ;; - emacs internal -
  :init
  ;;(setq sql-ms-program "sqlcmd")
  :ensure t
  :defer t
  :config
  )

(use-package sql-indent
  ;;
  :init
  :ensure t
  :defer t
  :config
  :after (:all sql)
  )

;; =========================================================================
;; REST
;; =========================================================================
(use-package restclient
  ;; An interactive HTTP client for Emacs
  :init
  :ensure t
  :defer t
  :config
  :after (:all json-mode)
  )

(use-package company-restclient
  ;; company-mode completion back-end for restclient-mode
  :init
  :ensure t
  :defer t
  :config
  :after (:all restclient)
  )


;; =========================================================================
;; Customization (outside of "custom")
;; =========================================================================
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
;; (unless (server-running-p)
;;   (server-start))

;; =========================================================================
;; Font setup
;; =========================================================================
;; from https://www.reddit.com/r/emacs/comments/1xe7vr/check_if_font_is_available_before_setting/
;; Test char and monospace:
;; 0123456789abcdefghijklmnopqrstuvwxyz [] () :;,. !@#$^&*
;; 0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ {} <> "'`  ~-_/|\?
;;
;; https://www.reddit.com/r/emacs/comments/6i55x3/emacs_serverclient_doesnt_respect_setfaceattribute/
;; If you run emacs as server in the background (--daemon or script),
;; it will not create any frame. Because of that, any frame settings wont
;; take any effect.
;; If interested, check 'after-make-frame-functions' hook
(cond 
 ((find-font (font-spec :name "DejaVu Sans Mono"))
  (set-frame-font "DejaVu Sans Mono-10" t t)
  (add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-10")))
 ((find-font (font-spec :name "Consolas"))
  (set-frame-font "Consolas-10" t t)
  (add-to-list 'default-frame-alist '(font . "Consolas-10")))
 ((find-font (font-spec :name "Inconsolata"))
  (set-frame-font "Inconsolata-10" t t)
  (add-to-list 'default-frame-alist '(font . "Incosolata-10")))
 ;; ((find-font (font-spec :name "Lucida Console"))
 ;;  (set-frame-font "Lucida Console-10")
 ;;  (add-to-list 'default-frame-alist '(font . "Lucida Console-10")))
 ((find-font (font-spec :name "Courier New"))
  (set-frame-font "Courier New-10" t t)
  (add-to-list 'default-frame-alist '(font . "Courier New-10")))
 ;; ((find-font (font-spec :name "courier"))
 ;;  (set-frame-font "courier-10")
 ;;  (add-to-list 'default-frame-alist '(font . "courier-10")))
 )

;; =========================================================================
;; Custom
;; =========================================================================
(setq custom-file (locate-user-emacs-file "custom-set-settings.el"))
(load custom-file 'noerror)

;; https://www.reddit.com/r/emacs/comments/55ork0/is_emacs_251_noticeably_slower_than_245_on_windows/
;; -------------------------------------------------------------------------------------------------
;; Riattivo l'hook disattivato in testa al file
(if (>= emacs-major-version 25)
    (add-hook 'find-file-hooks 'vc-refresh-state)
  (add-hook 'find-file-hooks 'vc-find-file-hook))

;; Reimposto i valori (quasi) di default per il garbage collector
;; il default di gc-cons-threshold e' in realta' 800000
;; (setq gc-cons-threshold 800000)
;; -------------------------------------------------------------------------------------------------
;; https://github.com/hlissner/doom-emacs/wiki/FAQ#how-is-dooms-startup-so-fast
;; -------------------------------------------------------------------------------------------------
;; 16777216 = 16MB
(setq gc-cons-threshold 16777216)
(setq gc-cons-percentage 0.1)

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
