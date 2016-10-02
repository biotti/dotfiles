;; Interesting variables: C-h v
;; ----------------------------
;; data-directory
;; emacs-version
;; emacs-build-system
;; emacs-repository-version
;; system-configuration
;; system-configuration-options
;; system-configuration-features

;; Manage package repositories
(require 'package)
(add-to-list 'package-archives
	     '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/"))


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; List of wanted packages
;; It's possible to install the packages manually using M-x pakcage-install
(defvar my-wanted-package-list
  '(
    ;; Themes
    ;; ------
    ;;
    ;; Aurora theme
    aurora-theme
    ;; Bad Wolf color theme
    badwolf-theme
    ;; A darker variant on Monokai
    darkokai-theme
    ;; Color theme for Emacs, when soothe and gruvbox collide
    darktooth-theme
    ;; A retro-groove colour theme for Emacs
    gruvbox-theme
    ;; Jbeans theme for GNU Emacs 24 (deftheme)
    jbeans-theme
    ;; Material theme see https://realpython.com/blog/python/emacs-the-best-python-editor/
    material-theme
    ;; Molokai theme with Emacs theme engine
    molokai-theme
    ;; A fruity color thene for Emacs
    monokai-theme
    ;; Port of vim's mustang theme
    mustang-theme
    ;; The Solarized color theme, ported to Emacs
    solarized-theme
    ;;
    ;; Emacs enhancements
    ;; ------------------
    ;;
    ;; A modular text completion framework
    ;;company
    ;; Ido Ubiquitous allow ido usage in as many context as possible
    ido-ubiquitous
    ;; Org-Mode - Outline-based notes management and organizer
    org
    ;; Enahnces M-x to allow easier execution of commands.
    ;; Provies a filterable list of possible commands in the minibuffer
    smex
    ;; A use-package declaration for simplifying your .emacs
    use-package
    ;;
    ;; Develpment - Generic
    ;; --------------------
    ;;
    ;; Flyceck is a modern on-the-fy syntax checking extension for GNU
    ;; Emacs, intended as replacement for the older Flymake extension
    ;; which is part of GNU Emacs.
    flycheck
    ;; show vertical lines to guide indentation
    indent-guide
    ;; A Git porcelain inside Emacs
    magit
    ;; Project navigation
    projectile
    ;; Colorful parentesis matching
    rainbow-delimiters
    ;; Edit html tags like sexps
    ;;tagedit
    ;; Yet another snippet extension for Emacs
    yasnippet
    ;;
    ;; Development - languages - C#
    ;; ----------------------------
    ;;
    ;; C# mode
    csharp-mode
    ;;
    ;; Development - languages - Python
    ;; see also: https://www.youtube.com/watch?v=0cZ7szFuz18
    ;; -----------------------------------------------------
    ;;
    ;; Elpy Python development
    elpy
    ;; Autopep8 - a tool that automatically formats Python
    ;; code to conform to the PEP 8 style guide
    py-autopep8
    ;; Emacs IPython Notebook
    ein
    ;;
    )
  )

;; On OS X an Emacs instance startd from the graphical user
;; interface will have a different environment than a shell
;; in a erminal window, bacause OS X does not run a shell
;; during the login.
;; Obviously this will lead to unexpected results when
;; calling exgrnal utilities like make from Emacs.
;; This library works around this problem by copying important
;; envionment variables from the user's shell
;; https://github/purcell/exec-path-from-shell
(if (eq system-type 'darwin)
    (add-to-list 'my-wanted-package-list 'exec-path-from-shell))

;; Check and install package from my wanted package list
(defun uninstalled-packages (packages)
  (delq nil
	(mapcar (lambda (p)
		  (if (package-installed-p p nil) nil p))
		packages)))
(let ((need-to-install
       (uninstalled-packages my-wanted-package-list)))
  (when need-to-install
    (package-refresh-contents)
    (progn
      (dolist (p need-to-install)
	(package-install p)))))

;; Color Theme
(use-package color-theme
  )
(use-package aurora-theme
  )
(use-package material-theme
  )

;; Highlight Current Line
(use-package hl-line
  :init (global-hl-line-mode 1))

;; uniquify
;; When several buffers visit identically-named files,
;; Emacs must give the buffers distinct names. The usual method
;; for making buffer names unique adds '<2>', '<3>', etc. to the end
;; of the buffer names (all but one of them).
;; The forward naming method includes part of the file's directory
;; name at the beginning of the buffer name
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Uniquify.html
(use-package uniquify
  :defer t
  :config
  (setq uniquify-buffer-name-style 'forward)
  )

;; recentf
;; Turn on recent file mode so that you can more easily switch to
;; recently edited files when you first start emacs
(use-package recentf
  :init
  (recentf-mode 1)
  :config
  (setq recentf-save-file (concat user-emacs-directory ".recentf"))
  (setq recentf-exclude '(".ido.last"))
  (setq recentf-max-menu-items 40)
  )

;; ido-mode allows you to more easily navigate choices. For example,
;; when you want to switch buffers, ido presents you with a list
;; of buffers in the the mini-buffer. As you start to type a buffer's
;; name, ido will narrow down the list of buffers to match the text
;; you've typed in
;; http://www.emacswiki.org/emacs/InteractivelyDoThings
(use-package ido
  :init
  ;; (ido-mode t)
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

;; Ido-ubiquitous
;; This enables ido in all contexts where it could be useful, not just
;; for selecting buffer and file names
(use-package ido-ubiquitous
  :after
  ido
  :init
  ;; (ido-ubiquitous-mode 1)
  :config
  (ido-ubiquitous-mode 1)
  )

;; Shows a list of buffers
;; https://www.emacswiki.org/emacs/IbufferMode
(use-package ibuffer
  :bind
  ("C-x C-b" . ibuffer)
  )

;; Enhances M-x too allow easier execution of commands. Provides
;; a filterable list of possible commands in the minibuffer
;; http://www.emacswiki.org/emacs/Smex
(use-package smex
  :init
  ;; (smex-initialize)
  :bind
  ("M-x" . smex)
  :config
  (smex-initialize)
  (setq smex-save-file (concat user-emacs-directory ".smex-items"))
  )

;; projectile everywhere! (disabilitato)
;;(projectile-global-mode)
(use-package projectile
  :defer t
  :init
  ;; (projectile-global-mode t)
  :config
  (projectile-global-mode t)
  )

;; Magit
(use-package magit
  :defer t
  )

;; FlyCheck
(use-package flycheck
  )

;; Autopep8
(use-package py-autopep8
  :config
  (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
  )

;; Python development
;; Elpy
(use-package elpy
  :after
  flycheck
  py-autopep8
  :config
  (elpy-enable)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode)
  (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
  )


;; Custom
;; Custom variables
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#839496"])
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-safe-themes
   (quote
    ("a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "6c0a087a4f49c04d4002393ffd149672f70e4ab38d69bbe8b39059b61682b61c" "a800120841da457aa2f86b98fb9fd8df8ba682cebde033d7dbf8077c1b7d677a" "b571f92c9bfaf4a28cb64ae4b4cdbda95241cd62cf07d942be44dc8f46c491f4" "5dc0ae2d193460de979a463b907b4b2c6d2c9c4657b2e9e66b8898d2592e3de5" "45712b65018922c9173439d9b1b193cb406f725f14d02c8c33e0d2cdad844613" "3fd0fda6c3842e59f3a307d01f105cce74e1981c6670bb17588557b4cebfe1a7" "5a0930a84612f861bb5e98999a50ec6ef7995676c7330aac9b8deda1aaa45f83" "70403e220d6d7100bae7775b3334eddeb340ba9c37f4b39c189c2c29d458543b" "7db4f811c922b96af34ed003edb27f976e19cfaabfeab11a5c54e3e0c27ba149" "790e74b900c074ac8f64fa0b610ad05bcfece9be44e8f5340d2d94c1e47538de" "98cc377af705c0f2133bb6d340bf0becd08944a588804ee655809da5d8140de6" default)))
 '(fci-rule-color "#eee8d5")
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#fdf6e3" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#586e75")
 '(highlight-tail-colors
   (quote
    (("#eee8d5" . 0)
     ("#B4C342" . 20)
     ("#69CABF" . 30)
     ("#69B7F0" . 50)
     ("#DEB542" . 60)
     ("#F2804F" . 70)
     ("#F771AC" . 85)
     ("#eee8d5" . 100))))
 '(hl-bg-colors
   (quote
    ("#DEB542" "#F2804F" "#FF6E64" "#F771AC" "#9EA0E5" "#69B7F0" "#69CABF" "#B4C342")))
 '(hl-fg-colors
   (quote
    ("#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3")))
 '(hl-sexp-background-color "#1c1f26")
 '(magit-diff-use-overlays nil)
 '(notmuch-search-line-faces
   (quote
    (("unread" :foreground "#aeee00")
     ("flagged" :foreground "#0a9dff")
     ("deleted" :foreground "#ff2c4b" :bold t))))
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(package-selected-packages
   (quote
    (powershell ein py-autopep8 elpy csharp-mode yasnippet projectile indent-guide flycheck use-package smex ido-ubiquitous material-theme jbeans-theme gruvbox-theme darktooth-theme badwolf-theme)))
 '(pos-tip-background-color "#eee8d5")
 '(pos-tip-foreground-color "#586e75")
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#eee8d5" 0.2))
 '(term-default-bg-color "#fdf6e3")
 '(term-default-fg-color "#657b83")
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#c85d17")
     (60 . "#be730b")
     (80 . "#b58900")
     (100 . "#a58e00")
     (120 . "#9d9100")
     (140 . "#959300")
     (160 . "#8d9600")
     (180 . "#859900")
     (200 . "#669b32")
     (220 . "#579d4c")
     (240 . "#489e65")
     (260 . "#399f7e")
     (280 . "#2aa198")
     (300 . "#2898af")
     (320 . "#2793ba")
     (340 . "#268fc6")
     (360 . "#268bd2"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
    (unspecified "#fdf6e3" "#eee8d5" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#657b83" "#839496")))
 '(xterm-color-names
   ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#073642"])
 '(xterm-color-names-bright
   ["#fdf6e3" "#cb4b16" "#93a1a1" "#839496" "#657b83" "#6c71c4" "#586e75" "#002b36"]))
;; -----------------------------------------------------------------------------
;; Custom: faces
;; ATTENZIONE
;; Quando si imposta un nuovo tema ricordarsi di:
;; M-x customize-group <ret>
;; faces <ret>
;; Basic Faces <ret>
;; Default face <ret>
;; qui togliere la spunta da:
;; Foreground e Background
;; Quindi salvare per future esecuzioni
;; -----------------------------------------------------------------------------
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; Customization (outside of "custom")
;; -----------------------------------
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

;; Emacs theme
(load-theme 'material)

;; Emacs Server
;;(server-start)



;; ***************************************************************************
;; Local Variables:
;; mode: lisp
;; indent-tabs-mode: nil
;; tab-width: 4
;; End:
;; ***************************************************************************
