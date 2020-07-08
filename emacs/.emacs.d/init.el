;; Nota bene per gli errori gpg;
;; M-x package-install RET gnu-elpa-keyring-update RET
;; This package updates the GPG keys used by the ELPA package manager
;; (a.k.a `package.el') to verify authenticity of packages downloaded
;; from the GNU ELPA archive.
;;
;; Those keys have a limited validity in time (for example, the first key was
;; valid until Sep 2019 only), so you need to install and keep this package up
;; to date to make sure signature verification does not spuriously fail when
;; installing packages.
;;
;; If your keys are already too old, causing signature verification errors when
;; installing packages, then in order to install this package you can do the
;; following:
;;
;; - Fetch the new key manually, e.g. with something like:
;;
;;       gpg --homedir ~/.emacs.d/elpa/gnupg --receive-keys 066DAFCB81E42C40
;;
;; - Modify the expiration date of the old key, e.g. with something like:
;;
;;       gpg --homedir ~/.emacs.d/elpa/gnupg \
;;           --quick-set-expire 474F05837FBDEF9B 1y
;;
;; - temporarily disable signature verification (see variable
;;   `package-check-signature').
;;

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


;;
;; Emacs Server
;;
(require 'server)
(setq server-log t)
(unless server-process
  ;; N.B.: (server-running-p) non funziona, usare server-process
  (progn
    ;;(interactive)
    (message "server-start")
    (server-start)))

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
;; https://stackoverflow.com/questions/3984730/emacs-gui-with-emacs-daemon-not-loading-fonts-correctly

;; (cond ((find-font (font-spec :name "DejaVu Sans Mono"))
;;        (set-frame-font "DejaVu Sans Mono-10" t t)
;;        (add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-10")))
;;       ((find-font (font-spec :name "Consolas"))
;;        (set-frame-font "Consolas-10" t t)
;;        (add-to-list 'default-frame-alist '(font . "Consolas-10")))
;;       ((find-font (font-spec :name "Inconsolata"))
;;        (set-frame-font "Inconsolata-10" t t)
;;        (add-to-list 'default-frame-alist '(font . "Incosolata-10")))
;;       ;; ((find-font (font-spec :name "Lucida Console"))
;;       ;;  (set-frame-font "Lucida Console-10")
;;       ;;  (add-to-list 'default-frame-alist '(font . "Lucida Console-10")))
;;       ((find-font (font-spec :name "Courier New"))
;;        (set-frame-font "Courier New-10" t t)
;;        (add-to-list 'default-frame-alist '(font . "Courier New-10")))
;;       ;; ((find-font (font-spec :name "courier"))
;;       ;;  (set-frame-font "courier-10")
;;       ;;  (add-to-list 'default-frame-alist '(font . "courier-10")))
;;       )


;; non sono sicuro che (require frame) sia necessario
;;(require 'frame)
(add-to-list 'initial-frame-alist
             (cond ((find-font (font-spec :name "DejaVu Sans mono"))
                    '(font . "DejaVu Sans Mono-10"))
                   ((find-font (font-spec :name "Consolas"))
                    '(font . "Consolas-10"))
                   ((find-font (font-spec :name "Inconsolata"))
                    '(font . "Inconsolata-10"))
                   ((find-font (font-spec :name "Courier New"))
                    '(font . "Courier New-10"))
                   )
             )

(message "DEBUG - add-to-list 'default-frame-alist")
(add-to-list 'default-frame-alist
             (cond ((find-font (font-spec :name "DejaVu Sans mono"))
                    '(font . "DejaVu Sans Mono-10"))
                   ((find-font (font-spec :name "Consolas"))
                    '(font . "Consolas-10"))
                   ((find-font (font-spec :name "Inconsolata"))
                    '(font . "Inconsolata-10"))
                   ((find-font (font-spec :name "Courier New"))
                    '(font . "Courier New-10"))
                   )
             )

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
           (format "%.2f seconds" (float-time
                                   (time-subtract
                                    after-init-time
                                    before-init-time)))
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

;; =========================================================================
;; Manage package repositories
;; =========================================================================
;;
;; Org (org-mode)
(add-to-list 'package-archives '("org-elpa" . "https://orgmode.org/elpa/") t)
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
       (proto (if no-ssl "http" "https")))
  (when no-ssl (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
  ;; and `package-pinned-packages`. Most users will not need or want to do this.
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  )
;;
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
;;
;;(setq package-archive-priorities
;;      '(("melpa-stable" . 10)
;;        ("gnu"          . 5)
;;        ("melpa"        . 0)))

;; (setq package-enable-at-startup nil)

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; https://github.com/sondr3/dotfiles/blob/master/emacs.org
;; Then we ll make sure we always load newer files if they are available,
;; even if there s a byte compiled version and disable automatic requiring
;; of packages on start as it ll be handled by use-package.
(setq load-prefer-newer t)


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
;; (unless (package-installed-p 'use-package)
;;   (message "use-package not present in package-installed-p")
;;   (message "Refreshing package database...")
;;   (package-refresh-contents)
;;   (message "Done refreshing. Installing use-package")
;;   (package-install 'use-package)
;;   (message "Done installing."))

(if (package-installed-p 'use-package)
    (message "Checking use-package: found")
  (progn
    (message "Checking use-package: not found in package-installed-p")
    (message "Refreshing package database...")
    (package-refresh-contents)
    (message "Done refreshing. Installing use-package")
    (message "Installing use-package")
    (package-install 'use-package)
    (message "Done installing."))
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
  :ensure t
  :config
  ;; To disable collection of benchmark data after init is done.
  ;; (add-hook 'after-init-hook 'benchmark-init/deactivate)
  :hook ((after-init) . benchmark-init/deactivate)
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

(use-package beacon
  :ensure t
  :diminish beacon-mode
  :config
  (progn (beacon-mode 1)
         (setq beacon-blink-when-focused t)
         (setq beacon-size 60))
  )

(use-package all-the-icons
  :ensure t
  :demand t
  ;;:init (require 'all-the-icons)
  )


;; Color Themes (use (load-theme xxx) at the end
;; =========================================================================
;; Color Themes
;; =========================================================================
(use-package spacemacs-common
  ;; Port of vim's mustang theme
  :if (display-graphic-p)
  :ensure spacemacs-theme
  :defer t
  ;; :config
  ;; (load-theme 'spacemacs-dark t)
  )

(use-package cloud-theme
  :ensure t
  :defer t
  )

(use-package moe-theme
  :ensure t
  :defer t
  )

(use-package zenburn-theme
  :ensure t
  :defer t
  )

(use-package monokai-theme
  :ensure t
  :defer t
  )

(use-package gruvbox-theme
  :ensure t
  :defer t
  )

(use-package ample-theme
  :ensure t
  :defer t
  )

(use-package ample-zen-theme
  :ensure t
  :defer t
  )

(use-package alect-themes
  :ensure t
  :defer t
  )

(use-package faff-theme
  :ensure t
  :defer t
  )

(use-package darktooth-theme
  :ensure t
  :defer t
  )

(use-package soothe-theme
  :ensure t
  :defer t
  )

(use-package doom-themes
  :ensure t
  ;; :demand t
  :defer t
  :config
  (progn
    ;; Global settings (defaults)
    ;; if nil, bold is universally disabled
    (setq doom-themes-enable-bold t)
    ;; if nil, italics is universally disabled
    (setq doom-themes-enable-italic t) 
  
    ;;(load-theme 'doom-one t)
    ;; Enable flashing mode-line on errors
    (doom-themes-visual-bell-config)
    ;; Enable custom neotree theme (all-the-icons must be installed!)
    (doom-themes-neotree-config)
  
    ;; or for treemacs users
    ;; use the colorful treemacs theme
    (setq doom-themes-treemacs-theme "doom-colors")
    (doom-themes-treemacs-config)
    ;; Corrects (and improves) org-mode's native fontification.
    (doom-themes-org-config))
  )

;; (load-theme 'gruvbox-light-soft t)
;;(load-theme 'spacemacs-dark t)
(require 'doom-themes)
(load-theme 'doom-one t)

(use-package doom-modeline
  :ensure t
  :hook ((after-init) . doom-modeline-mode)
  :init (message "DEBUG - use-package  doom-modeline")
  )

(use-package minions
  :ensure t
  :hook ((after-init) . minions-mode)
  :config
  (progn
    (setq doom-modeline-minor-modes t)
    ;; (minions-mode 1)
    )
  )

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
(use-package company
  ;; A modular text completion framework
  :ensure t
  :defer t
  :hook ((after-init) . global-company-mode)
  :config
  (progn
    ;; (global-company-mode t)
    ;; (add-to-list 'company-backends 'company-restclient)
    (setq company-idle-delay 0)
    (setq company-minimum-prefix-length 3))
  :diminish company-mode "Cmp"
  )

(use-package company-web
  :ensure t
  :defer t
  :after (:all company)
  :config
  (progn
    (add-to-list 'company-backends 'company-web-html)
    (add-to-list 'company-backends 'company-web-jade)
    (add-to-list 'company-backends 'company-web-slim))
  )

(use-package company-quickhelp
  ;; Popup documentation for completion candidates
  :ensure t
  :defer t
  :hook ((after-init) . company-quickhelp-mode)
  )

(use-package ibuffer
  ;; Shows a list of buffers
  ;; https://www.emacswiki.org/emacs/IbufferMode
  ;; - emacs internal -
  :ensure t
  :defer t
  :bind ("C-x C-b" . ibuffer)
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

;; Hydra
(use-package hydra
  :ensure t
  :demand t
  )

;; Testing Ivy, Swiper & Counsel
(use-package ivy
  :ensure t
  :demand t
  :hook ((after-init) . ivy-mode)
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
  :ensure t
  :demand t
  :after (:all ivy)
  ;;:bind ("\C-s" . swiper)
  ;; Provo swiper-isearch al posto di swiper "vanilla" perche'
  ;; consente in qualche modo di fare spostarsi su piu' occorrenze
  ;; del testo cercato sulla stessa riga (swieper "vani8lla" ragiona
  ;; sempre e soltanto in termini di "riga intera")
  ;; Da verificare se e' meglio o peggio
  :bind ("\C-s" . swiper-isearch)
  )

(use-package counsel
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
    (define-key read-expression-map (kbd "C-r") 'counsel-expression-history))
  )

(use-package avy
  :ensure t
  :demand t
  :after (:all swiper)
  )

(use-package counsel-css
  :ensure t
  :defer t
  :after (:all counsel)
  )

(use-package counsel-etags
  :ensure t
  :defer t
  :after (:all counsel)
  )

(use-package ivy-rich
  :ensure t
  ;;:defer t
  ;; :hook ((after-init) . ivy-rich-mode 1)
  :after (:all ivy counsel)
  ;;:config (ivy-rich-mode 1)
  :init (ivy-rich-mode 1)
  )

;; (use-package all-the-icons-ivy
;;   :ensure t
;;   ;;:demand t
;;   :defer t
;;   :hook ((after-init) . all-the-icons-ivy-setup)
;;   ;;:init (add-hook 'after-init-hook 'all-the-icons-ivy-setup)
;;   )

(use-package all-the-icons-ivy-rich
  :ensure t
  :defer t
  ;;:hook ((after-init) . (all-the-icons-ivy-rich-mode 1))
  :init (all-the-icons-ivy-rich-mode 1)
  )

(use-package org
  ;;:ensure org-plus-contrib
  :ensure t
  :pin org-elpa
  :init (setq org-export-backends '(ascii beamer html icalendar latex odt org))
  :defer t
  :config
  (progn
    (add-to-list 'org-latex-packages-alist '("" "tabularx" nil))
    (add-to-list 'org-latex-packages-alist '("" "tabu" nil)))
  )

;; (use-package ox-reveal
;;     :init
;;     ;; WorkAround per evitare problemi con Org successivo a 9.2 che ha
;;     ;; adottato una nuova metodica per org-structure-template-alist
;;     (setq org-reveal-note-key-char nil)
;;     :ensure t
;;     :config
;;     (progn
;;       (setq org-reveal-root
;;             (concat "file:///"
;;                     (expand-file-name
;;                      (concat user-emacs-directory
;;                              "reveal.js"))))
;;       ;; Obsoleto: org-reveal attiva mathjax quando rileva contenuto latex
;;       ;; (setq org-reveal-mathjax t)
;;       ;; WorkAround per evitare problemi con Org successivo a 9.2 che ha
;;       ;; adottato una nuova metodica per org-structure-template-alist
;;       (add-to-list 'org-structure-template-alist '("n" . "notes")))
;;     :after (:all org)
;;     )

(use-package org-re-reveal
  :init
  ;; Temporaneamente disabilitato
  :disabled
  ;; WorkAround per evitare problemi con Org successivo a 9.2 che ha
  ;; adottato una nuova metodica per org-structure-template-alist
  (setq org-re-reveal-note-key-char nil)
  :ensure t
  :config
  (progn
    (setq org-re-reveal-root
          (concat "file:///"
                  (expand-file-name
                   (concat user-emacs-directory
                           "reveal.js"))))
    ;; Obsoleto: org-reveal attiva mathjax quando rileva contenuto latex
    ;; (setq org-reveal-mathjax t)
    ;; WorkAround per evitare problemi con Org successivo a 9.2 che ha
    ;; adottato una nuova metodica per org-structure-template-alist
    (add-to-list 'org-structure-template-alist '("n" . "notes")))
  :after (:all org)
  )

(use-package htmlize
  :ensure t
  :defer t
  )

(use-package org-bullets
  ;; Ricordarsi che nel caso si voglia stampare in postscript
  ;; un file org e' necessario disattivare l'org-bullet-mode
  ;; altrimenti la stampa che si ottinene presenta degli
  ;; asterischi (tipo standard org) e dei punti interrogativi (?)
  ;; dovuti ai problemi di rendering dei bullets
  ;; Per quanto sopra disattivo l'hook. Se voglio usare i bullets
  ;; lo faro' a mano tramite comando
  ;; :hook (org-mode . org-bullets-mode)
  :ensure t
  :defer t
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

(use-package amx
  ;; Alternative M-x with extra features.
  :ensure t
  :defer t
  :after (:all counsel)
  ;; :bind (("M-X" . amx-major-mode-commands))
  :config (amx-mode t)
  )

;; (use-package tramp
;;   ;; - emacs internal -
;;   :defer t
;;   :config
;;   (cond ((eq system-type 'windows-nt)
;;          ;; Windows-specific code goes here.
;;          (setq tramp-default-method "pscp"))
;;         ((eq system-type 'gnu/linux)
;;          ;; Linux-specific code goes here
;;          (setq tramp-default-method "ssh")))
;;   )

;; (use-package counsel-tramp
;;   ;; Tramp ivy interface for ssh, docker, vagrant
;;   :ensure t
;;   :defer t
;;   :after (:all counsel tramp)
;;   )

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
  :defer t
  :config (setq uniquify-buffer-name-style 'forward)
  )

(use-package whitespace
  ;; - emacs internal -
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
  )

(use-package swap-buffers
  ;; A *visual* way to choose a window to switch to
  :ensure t
  :defer t
  )

(use-package winum
  ;; https://github.com/deb0ch/emacs-winum
  :ensure t
  ;; NON USARE DEFER!
  ;;:defer t
  :config
  (progn
    ;; Per l'uso con spaceline
    ;; https://github.com/TheBB/spaceline#winum
    ;;(setq winum-auto-setup-mode-line nil)
    (winum-mode))
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
  :ensure t
  ;; Non si deve differire altrimenti non parte
  ;;:defer t
  :config (which-key-mode t)
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
  :ensure t
  :defer t
  :config
  (progn
    (setq fci-handle-truncate-lines nil)
    (setq fci-rule-width 1))
  )

(use-package try
  ;; Try out Emacs packages.
  ;; -- do not defer --
  :ensure t
  :defer t
  )

(use-package origami
  ;; Flexible text folding
  :ensure t
  :defer t
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
  :ensure t
  :defer t
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
  :bind ("C-x g" . magit-status)
  :ensure t
  :defer t
  :config
  (progn
    ;; (magit-diff-use-overlays nil)
    ;; some packages already provide their own interfaces to ido, so
    ;; ido-completing-read+ specifically avoids interfering with these.
    ;; If you use any of the following packages, you need to enable ido for
    ;; each of them separately.
    ;; (setq magit-completing-read-function 'magit-ido-completing-read)
    (setq magit-completing-read-function 'ivy-completing-read)
    (setenv "GIT_ASKPASS" "git-gui--askpass"))
  )

(use-package forge
  :ensure t
  :defer t
  :after (:all magit markdown-mode)
  )

(use-package gitconfig-mode
  :ensure t
  :defer t
  :mode ("/\\.gitconfig\\'" "/\\.git/config\\'" "/git/config\\'" "/\\.gitmodules\\'")
  )

(use-package gitignore-mode
  :ensure t
  :defer t
  :mode ("/\\.gitignore\\'" "/\\.git/info/exclude\\'" "/git/ignore\\'")
  )

(use-package git-timemachine
  :ensure t
  :defer t
  )

;; =========================================================================
;; Development: generic
;; =========================================================================
(use-package eglot
  :ensure t
  :demand t)

(use-package eldoc
  ;; - emacs internal -
  :ensure t
  :defer t
  :diminish eldoc-mode
  )



(use-package flycheck
  :ensure t
  :defer t
  :hook (prog-mode . flycheck-mode)
  )

(use-package flycheck-pos-tip
  :ensure t
  :defer t
  ;;:defines flycheck-pos-tip-timeout
  :hook (flycheck-mode . flycheck-pos-tip-mode)
  :config (setq flycheck-pos-tip-timeout 30)
  )

(use-package indent-guide
  ;; Show vertical lines to guide indentation
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
                      " Prj[*remote*]" (format " Prj[%s]" (projectile-project-name))))))
  )

(use-package ibuffer-projectile
  ;;Group ibuffer's list by projectile root
  :ensure t
  :defer t
  :after (:all ibuffer projectile)
  )

(use-package counsel-projectile
  ;; Ivy integration for Projectile
  :ensure t
  :defer t
  :after (:all counsel projectile)
  )

(use-package project-explorer
  ;; A project explorer sidebar
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

(use-package smartparens-config
  :ensure smartparens
  :defer t
  :diminish smartparens-mode
  ;; :init (add-hook 'minibuffer-setup-hook #'turn-on-smartparens-strict-mode)
  :hook (;;(minibuffer-setup . turn-on-smartparens-strict-mode)
         (prog-mode . smartparens-mode))
  :config
  (progn
    ;; (show-smartparens-global-mode t)
    ;; (smartparens-global-mode t)
    (setq sp-show-pair-from-inside nil)
    ;;(require 'smartparens-config)
    (sp-use-smartparens-bindings)
    (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil))
  )


(use-package rainbow-delimiters
  ;; Colorful parentesis matching
  :ensure t
  :defer t
  :hook (prog-mode . rainbow-delimiters-mode)
  )

(use-package yasnippet
  ;; Yet another snippet extension for Emacs
  :hook (prog-mode . yas-minor-mode)
  :ensure t
  ;;:defer 2
  :defer t
  :config
  ;; (yas-global-mode t)
  )

(use-package yasnippet-snippets
  ;; Collection of yasnippet snippets
  :ensure t
  :defer t
  :after (:all yasnippet)
  )

(use-package aggressive-indent
  :ensure t
  :defer t
  :config
  (global-aggressive-indent-mode 1)
  ;;(add-to-list 'aggressive-indent-excluded-modes 'html-mode)
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
  :init (defun my/emacs-lisp-mode-hook ()
          "Funzione richiamata dall'hook emacs-lisp-mode-hook."
          (interactive)
          (company-mode)
          (yas-minor-mode))
  (add-hook 'emacs-lisp-mode-hook 'my/emacs-lisp-mode-hook)
  :after (:all yasnippet company)
  )

(use-package elisp-format
  :disabled t
  :ensure t
  :defer t
  )

;; =========================================================================
;; Development: C#
;; =========================================================================
(use-package csharp-mode
  ;; C# mode
  :ensure t
  :defer t
  ;; Non e' necessario impostare :mode
  ;; :mode "\\.cs\\"
  )

;; =========================================================================
;; Development: VB.NET
;; =========================================================================
(use-package vbnet-mode
  ;; VB.NET mode - Caricato localmente con clonazione del repository di
  ;; Dino Chiesa
  :load-path "vendor/dpchiesa-elisp"
  :mode "\\.\\(vb\\)$"
  :defer t
  )

;; =========================================================================
;; Development: VBScript
;; =========================================================================
(use-package vbs-mode
  ;; VB.NET mode - Caricato localmente con clonazione del repository di
  ;; Dino Chiesa
  :load-path "vendor/dpchiesa-elisp"
  ;; Non e' necessario impostare :mode
  ;; :mode "\\.cs\\"
  :defer t
  )


;; =========================================================================
;; Development: Powershell
;; =========================================================================
(use-package powershell
  :init (add-hook 'powershell-mode-hook 'work-style)
  :ensure t
  :defer t
  ;; Non e' necessario impostare :mode
  ;; :mode ("\\.ps[dm]?1\\'" . powershell-mode)
  )

;; =========================================================================
;; Develpment: Python
;; =========================================================================
(use-package python
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'python (elpy-enable))
  )

(use-package elpy
  :init
  (defun my/elpy-mode-hook ()
          "Funzione richiamata dall'hook elpy-mode-hook."
          (interactive)
          (company-mode)
          (yas-minor-mode))
  (add-hook 'elpy-mode-hook 'my/elpy-mode-hook)
  :ensure t
  :defer t
  :after (:all company
               highlight-indentation
               yasnippet)
  :config
  (progn
    (setq elpy-rpc-backend "jedi")
    (cond ((eq system-type 'windows-nt)
           ;; Windows-specific code goes here.
           (setq python-shell-completion-native-enable nil))))
  )

(use-package py-autopep8
  ;; Autopep8
  ;; :init (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
  :ensure t
  :defer t
  )

(use-package company-jedi
  ;; company-mode completion back-end for Python JEDI
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
  :init (defun my/go-mode-hook ()
          "Funzione richiamata dall'hook go-lisp-mode-hook."
          (interactive)
          (set (make-local-variable 'company-backends)
               '(company-go))
          (company-mode)
          (yas-minor-mode)
          (go-set-project)
          (go-eldoc-setup)
          (flycheck-gometalinter-setup)
          (if (not (string-match "go" compile-command))
              (if (eq system-type 'windows-nt)
                  (setq compile-command
                        "go build -v & go test -v & go vet & gometalinter -t --enable-gc & errcheck")
                (setq compile-command
                      "go build -v && go test -v && go vet && gometalinter -t --enable-gc && errcheck"))
            (set (make-local-variable 'compile-command)
                 "go build -v && go test -v && go vet && gometalinter && errcheck")))
  :hook ((before-save-hook . gofmt-before-save)
         (go-mode-hook . my/go-mode-hook))
  :ensure t
  :defer t
  :after (:all flycheck-gometalinter company-mode)
  :config
  (progn
    (add-to-list 'load-path (concat (getenv "GOPATH") "/bin"))
    (setq-default gofmt-command "goimports"))
  )

(use-package company-go
  ;; company-mode backend for Go (using gocode)
  ;; Installation:
  ;;
  ;; Non Windwos: go get -u github.com/nsf/gocode
  ;; Windows    : go get -u -ldflags -H=windowsgui github.com/nsf/gocode
  ;;
  :init (with-eval-after-load 'company (add-to-list 'company-backends 'company-go))
  :ensure t
  :defer t
  :after (:all company go-mode)
  )


(use-package go-rename
  ;; To install:
  ;; % go get golang.org/x/tools/cmd/gorename
  :init
  :ensure t
  :defer t
  :after (:all go-mode)
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
  :config (go-eldoc-setup)
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
  :config (add-to-list 'load-path (concat (getenv "GOPATH")
                                          "/src/github.com/golang/lint/misc/emacs"))
  :after (:all go-mode))

(use-package go-guru
  ;; Integration of the Go 'guru' analysis tool into Emacs.
  :init
  :ensure t
  :defer t
  :after (:all go-mode))

(use-package go-errcheck
  ;; errcheck integration for go-mode
  ;; Installation
  ;;
  ;; go get -u github.com/kisielk/errcheck
  ;;
  :init
  :ensure t
  :defer t
  :after (:all go-mode))

(use-package go-snippets
  ;; Yasnippets for go
  :init
  :ensure t
  :defer t
  :after (:all yasnippet
               go-mode))

(use-package flycheck-gometalinter
  ;; Flycheck checker for golang using gometalinter
  :init
  :ensure t
  :defer t
  :config (progn (flycheck-gometalinter-setup))
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
  :after (:all flycheck
               go-mode))

;; =========================================================================
;; rust
;; =========================================================================
(use-package flycheck-rust
  :ensure t
  :defer t)

(use-package rust-mode
  :ensure t
  :defer t
  :hook ((rust-mode . (lambda () (setq indent-tabs-mode nil)))
         (rust-mode . eglot-ensure)
         (rust-mode . flycheck-rust-setup)
         )
  :config (setq rust-format-on-save t)
  )



(when (executable-find "racer")
  (use-package racer
    :hook (racer-mode . eldoc-mode)
    :init (defun org-babel-edit-prep:rust (&optional _babel-info)
            "Run racer mode for Org Babel."
            (racer-mode 1))
    )
  )
(use-package toml-mode
  :ensure t
  :defer t
  )

(when (executable-find "cargo")
  (use-package cargo
    :ensure t
    :defer t
    :hook ((rust-mode . cargo-minor-mode)
           (toml-mode . cargo-minor-mode)
           )
    )
  )

;; =========================================================================
;; web development
;; =========================================================================
(use-package web-mode
  :init (defun my/web-mode-hook ()
          "Funzione richiamata dall'hook emacs-lisp-mode-hook."
          (interactive)
          (company-mode)
          (company-web)
          (yas-minor-mode))
  (add-hook 'web-mode-hook 'my/web-mode-hook)
  :after (:all yasnippet
               company
               company-web)
  :ensure t
  :defer t
  )

(use-package js2-mode
  :init
  :ensure t
  :defer t
  :mode ("\\.js\\'" . js2-mode)
  :config
  (setq js2-highlight-level 3)
  )

;; Skewer -- live web development minor mode.
;; Methods of launching:
;;   * M-x run-skewer
;;   * Use the Skewer bookmarklet to inject it into an existing page.
;; Keybindings resemble the Lisp ones:
;;   C-x C-e -- JS: eval form (with prefix: insert result); CSS: load declaration.
;;   C-M-x -- JS: eval top-level-form; CSS: load rule; HTML: load tag.
;;   C-c C-k -- JS, CSS: eval buffer.
;;   C-c C-z -- JS: switch to REPL (logging: "skewer.log()", like "console.log()").
;; Forms are sent to all attached clients simultaneously (use `list-skewer-clients' to show them).
;; If the browser disconnects, use "skewer()" in the browser console to reconnect.
(use-package skewer-mode
  :ensure t
  :defer t
  :init
  (skewer-setup)
  :config
  ) ; Integrate with js2-mode, html-mode and css-mode. (Don't worry about performance, this function is in a separate file.)

;; ;; Completion (also provides jump-to-definition).
;; (use-package ac-js2
;;   :ensure t
;;   :init
;;   (add-hook 'js2-mode-hook #'ac-js2-company)
;;   :config
;;   ;; C-c . -- jump to definition.
;;   (require 'conf/utils/keys) ; Used: clear-keymap.
;;   (clear-keymap ac-js2-mode-map)
;;   (bind-key "C-c ." #'ac-js2-jump-to-definition ac-js2-mode-map)
;;   (bind-key "C-c ," #'pop-tag-mark ac-js2-mode-map)
;;   ;; C-c C-c -- expand function arguments.
;;   (with-eval-after-load 'yasnippet
;;     (bind-key "C-c C-c" #'ac-js2-expand-function)))

;; Refactoring (and many common operations, e.g. kill expression).
;; Integrates nicely with `emacs-refactor' (select some code before invoking it).
(use-package js2-refactor
  :ensure t
  :defer t
  :init
  (add-hook 'js2-mode-hook #'js2-refactor-mode)
  :config
  (js2r-add-keybindings-with-prefix "C-c C-m")
  :after (:all j2s-mode)
  )
;; Doesn't work.
;; It seems that `smartparens' binds some characters to `sp--self-insert-command' in the menu buffer, which makes the menu items unselectable.
;; (use-package discover-js2-refactor
;;   :ensure t
;;   :init
;;   ;; Don't let `discover-js2-refactor's bind its default -- "C-c C-r". Instead, bind "C-c C-m" manually.
;;   (with-eval-after-load 'discover
;;     (with-eval-after-load 'js2-refactor
;;       (require 'discover-js2-refactor)
;;       (remove-hook 'js2-mode-hook #'js2-refactor-turn-on-discover)
;;       (with-eval-after-load 'js2-mode
;;         (bind-key "C-c C-m"
;;                   (discover-get-context-menu-command-name 'js2-refactor)
;;                   js2-mode-map)))))



;; =========================================================================
;; CSV
;; =========================================================================
(use-package csv-mode
  ;; Major mode for editing comma/char separated values
  :init
  :ensure t
  :defer t
  :config)

;; =========================================================================
;; Markdown
;; =========================================================================
(use-package markdown-mode
  ;; Major mode for Markdown-formatted text
  :init
  ;; (setq markdown-command "multimarkdown")
  :ensure
  t
  :defer t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  :after (:all flycheck)
  :diminish markdown-mode
  "MDown")

;; =========================================================================
;; Json
;; =========================================================================
(use-package json-mode
  ;; Major mode for editing JSON files
  :init
  :ensure t
  :defer t
  :config
  :after (:all flycheck))

;; =========================================================================
;; SQL
;; =========================================================================
(use-package sql
  ;; - emacs internal -
  :init
  ;;(setq sql-ms-program "sqlcmd")
  :ensure
  t
  :defer t
  :config)

(use-package sql-indent
  ;;
  :init
  :ensure t
  :defer t
  :config
  :after (:all sql))

;; =========================================================================
;; REST
;; =========================================================================
(use-package restclient
  ;; An interactive HTTP client for Emacs
  :init
  :ensure t
  :defer t
  :config
  :after (:all json-mode))

(use-package company-restclient
  ;; company-mode completion back-end for restclient-mode
  :init
  :ensure t
  :defer t
  :config
  (add-to-list 'company-backends 'company-restclient)
  :after (:all company
               restclient)
  )

;; =========================================================================
;; EMMS: Solo su sistemi GNU/Linux, su Windows ci sono problemi
;; =========================================================================
(cond ((eq system-type 'gnu/linux)
       (use-package emms
         ;;
         ;; sudo apt-get install mpg321
         ;; sudo apt-get install vorbis-tools
         ;; sudo apt-get install mplayer
         ;; sudo apt-get install mpv
         ;; sudo apt-get install vlc
         :ensure t
         :config
         (progn
           (emms-standard)
           (emms-default-players)
           (setq emms-playlist-buffer-name "Music-EMMS")
           (setq emms-source-file-default-directory "~/Music/"))
         ;;** EMMS
         ;; Autoload the id3-browser and bind it to F7.
         ;; You can change this to your favorite EMMS interface.
         (autoload 'emms-smart-browse "emms-browser.el" "Browse with EMMS" t)
         (global-set-key [(f7)] 'emms-smart-browse)

         (with-eval-after-load 'emms
           (emms-standard) ;; or (emms-devel) if you want all features
           (setq emms-source-file-default-directory "~/music"
                 emms-info-asynchronously t
                 emms-show-format "♪ %s")

           ;; Might want to check `emms-info-functions',
           ;; `emms-info-libtag-program-name',
           ;; `emms-source-file-directory-tree-function'
           ;; as well.

           ;; Determine which player to use.
           ;; If you don't have strong preferences or don't have
           ;; exotic files from the past (wma) `emms-default-players`
           ;; is probably all you need.
           (if (executable-find "mplayer")
               (setq emms-player-list '(emms-player-mplayer))
             (emms-default-players))

           ;; For libre.fm see `emms-librefm-scrobbler-username' and
           ;; `emms-librefm-scrobbler-password'.
           ;; Future versions will use .authoinfo.gpg.
           )
         )))

;; =========================================================================
;; Customization (outside of "custom")
;; =========================================================================
(setq column-number-mode t) ;; Modeline - Display current column number (modeline)
(setq hscroll-step 1) ;; Windows - Number of column to scroll when points get too close to the edge
(setq scroll-conservatively most-positive-fixnum) ;; Windows - Number of lines to try scrolling a windows when point moves out
(setq inhibit-startup-screen t) ;; Initialization - Inhibits the startup screen

(if (< emacs-major-version 25)
    ;; "x-" versions considered obsolete after
    ;; emacs 25.1
    (progn
      (setq x-select-enable-clipboard t) ;; makes killing/yanking interact with the clipboard
      (setq x-select-enable-primary t))  ;;
  (progn
    (setq select-enable-clipboard t)
    (setq select-enable-primary t)))

(setq save-interprogram-paste-before-kill t) ;;
(setq apropos-do-all t) ;; Shows all options when running apropos.
(setq mouse-yank-at-point t) ;; Mouse yank commands yank at point instead of at click.
(setq ring-bell-function 'ignore) ;; No bell
(setq transient-mark-mode t) ;; Transient mark mode: mostra il testo selezionato come selezionato

(delete-selection-mode t)    ;; Attiva delete-selection-mode

;; Solo se e' attivo un window-system
(if window-system
    (set-scroll-bar-mode 'right) ;; Scrollbars - right
  )
(global-hl-line-mode 1)      ;; Highlight current line
(setq custom-safe-themes t) ;; Treat all themes as safe (warning: security issue!!!!)

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
(add-hook 'emacs-startup-hook (lambda ()
                                (delete-other-windows)) t)


;; =========================================================================
;; Tabulation settings
;; =========================================================================
(setq-default tab-width 4)          ;; Set tab width to 4 spaces
(setq-default indent-tabs-mode nil) ;; Use spaces instead of tabs
(setq tab-stop-list (number-sequence 4 200 4)) ;; Create list of tab stops every 4 char

;; =========================================================================
;; Impostazioni dipendenti dal sistema operativo
;; =========================================================================
(cond ((eq system-type 'windows-nt)
       ;; =========================================================================
       ;; Microsoft Windows specific settings
       ;; =========================================================================
       ;; ;; Imposto Find e Grep
       ;; (setq find-program (concat invocation-directory "find.exe")
       ;;       grep-program (concat invocation-directory "grep.exe"))
       ;; (setenv "PATH" (concat "C:\\msys64\\usr\\bin;" (getenv "PATH")))
       ;; (setq exec-path (append exec-path '("C:/msys64/usr/bin")))
       (setq msys2-path "C:\\msys64\\usr\\bin")
       (setq find-program (concat msys2-path "\\" "find.exe") grep-program (concat msys2-path "\\"
                                                                                   "grep.exe")
             diff-command (concat msys2-path "\\" "diff.exe"))
       (setq grep-use-null-device nil)
       ;; (setq grep-find-template (concat
       ;;                           (concat
       ;;                            (concat invocation-directory "find.exe")
       ;;                            " <D> <X> -type f <F> -exec ")
       ;;                           (concat
       ;;                            (concat invocation-directory "grep.exe")
       ;;                            " <C> -nH <R> {} \";\"")))
       (setq grep-find-template (concat (concat find-program " <D> <X> -type f <F> -exec ")
                                        (concat grep-program " <C> -nH <R> {} \";\"")))
       ;; "c:/Editors/emacs/emacs/bin/find.exe <D> <X> -type f <F> -exec c:/Editors/emacs/emacs/bin/grep.exe <C> -n <R> {} NUL \";\"")
       )
      ((eq system-type 'gnu/linux)
       ;; =========================================================================
       ;; GNU/Linux specific settings
       ;; =========================================================================
       ))


;; =========================================================================
;; Printing
;; =========================================================================
(require 'ps-print)
(setq ps-paper-type 'a4)
;; (setq ps-print-color-p 'black-white)
(setq doc-view-continuous t)
(cond ((eq system-type 'windows-nt)
       ;; Windows-specific code goes here.
       (setq ps-lpr-command "C:/Program Files/gs/gs9.50/bin/gswin64c.exe")
       (setq ps-lpr-switches '("-q"
                               "-dNOPAUSE"
                               "-dBATCH"
                               "-dNoCancel"
                               "-sDEVICE=mswinpr2"
                               ;; "-IC:/Program Files/gs/gs9.50/lib"
                               ;; "-sFONTPATH=C:/Windows/Fonts"
                               ;; "-sOutputICCProfile=default_cmyk.icc"
                               ;; "-dBitsPerPixel=24"
                               ;; "-dEmbedAllFonts=true"
                               ))
       (setq doc-view-ghostscript-program "C:/Program Files/gs/gs9.50/bin/gswin64c.exe")
       (setq ps-printer-name t)
       (setq ps-printer-name-option nil)
       )
      ((eq system-type 'gnu/linux)
       ;; Linux-specific code goes here.
       ))

;; Start Emacs fullscreen mode
;; (add-hook 'emacs-startup-hook 'toggle-frame-maximized)




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
