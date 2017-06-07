;; Custom
;; Custom variables
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("67e998c3c23fe24ed0fb92b9de75011b92f35d3e89344157ae0d544d50a63a72" "8453c6ba2504874309bdfcda0a69236814cefb860a528eb978b5489422cb1791" "5dc0ae2d193460de979a463b907b4b2c6d2c9c4657b2e9e66b8898d2592e3de5" "64f2981274fd8740b794bce9feee7949ed87b88fc0b4654bd98594e1aa81abcd" "d29231b2550e0d30b7d0d7fc54a7fb2aa7f47d1b110ee625c1a56b30fea3be0f" "c9321e2db48a21fc656a907e97ee85d8cd86967855bf0bed3998bcf9195c758b" "70403e220d6d7100bae7775b3334eddeb340ba9c37f4b39c189c2c29d458543b" "604648621aebec024d47c352b8e3411e63bdb384367c3dd2e8db39df81b475f5" "b571f92c9bfaf4a28cb64ae4b4cdbda95241cd62cf07d942be44dc8f46c491f4" "f78de13274781fbb6b01afd43327a4535438ebaeec91d93ebdbba1e3fba34d3c" "6c0a087a4f49c04d4002393ffd149672f70e4ab38d69bbe8b39059b61682b61c" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "6952b5d43bbd4f1c6727ff61bc9bf5677d385e101433b78ada9c3f0e3787af06" "a0dc0c1805398db495ecda1994c744ad1a91a9455f2a17b59b716f72d3585dde" "4cbec5d41c8ca9742e7c31cc13d8d4d5a18bd3a0961c18eb56d69972bbcf3071" "0ad9ed23b1f323e4ba36a7f0cbef6aff66128b94faa473aacd79317fbd24abda" "98cc377af705c0f2133bb6d340bf0becd08944a588804ee655809da5d8140de6" default)))
 '(package-selected-packages
   (quote
    (benchmark-init json-mode markdown-mode go-errcheck go-guru golint company-go go-eldoc go-mode py-autopep8 elpy powershell csharp-mode yasnippet rainbow-delimiters project-explorer ibuffer-projectile projectile indent-guide flycheck company-quickhelp gitignore-mode gitconfig-mode magit undo-tree cursor-chg which-key swap-buffers switch-window smooth-scroll smex org-plus-contrib ido-ubiquitous esup company zenburn-theme color-theme-solarized obsidian-theme mustang-theme monokai-theme molokai-theme moe-theme material-theme jbeans-theme gruvbox-theme darktooth-theme darkokai-theme badwolf-theme color-theme use-package))))


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


;; '(ansi-color-faces-vector
;;   [default bold shadow italic underline bold bold-italic bold])
;; '(ansi-color-names-vector
;;   ["#3c3836" "#fb4934" "#b8bb26" "#fabd2f" "#83a598" "#d3869b" "#8ec07c" "#ebdbb2"])
;; '(compilation-message-face (quote default))
;; '(fci-rule-color "#383838")
;; '(highlight-changes-colors (quote ("#ff8eff" "#ab7eff")))
;; '(highlight-tail-colors
;;   (quote
;;    (("#424748" . 0)
;;     ("#63de5d" . 20)
;;     ("#4BBEAE" . 30)
;;     ("#1DB4D0" . 50)
;;     ("#9A8F21" . 60)
;;     ("#A75B00" . 70)
;;     ("#F309DF" . 85)
;;     ("#424748" . 100))))
;; '(hl-sexp-background-color "#1c1f26")
;; '(linum-format " %3i ")
;; '(magit-diff-use-overlays nil)
;; '(notmuch-search-line-faces
;;   (quote
;;    (("unread" :foreground "#aeee00")
;;     ("flagged" :foreground "#0a9dff")
;;     ("deleted" :foreground "#ff2c4b" :bold t))))
;; '(nrepl-message-colors
;;   (quote
;;    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
;; '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
;; '(pos-tip-background-color "#36473A")
;; '(pos-tip-foreground-color "#FFFFC8")
;; '(vc-annotate-background "#2B2B2B")
;; '(vc-annotate-color-map
;;   (quote
;;    ((20 . "#BC8383")
;;     (40 . "#CC9393")
;;     (60 . "#DFAF8F")
;;     (80 . "#D0BF8F")
;;     (100 . "#E0CF9F")
;;     (120 . "#F0DFAF")
;;     (140 . "#5F7F5F")
;;     (160 . "#7F9F7F")
;;     (180 . "#8FB28F")
;;     (200 . "#9FC59F")
;;     (220 . "#AFD8AF")
;;     (240 . "#BFEBBF")
;;     (260 . "#93E0E3")
;;     (280 . "#6CA0A3")
;;     (300 . "#7CB8BB")
;;     (320 . "#8CD0D3")
;;     (340 . "#94BFF3")
;;     (360 . "#DC8CC3"))))
;; '(vc-annotate-very-old-color "#DC8CC3")
;; '(weechat-color-list
;;   (unspecified "#242728" "#424748" "#F70057" "#ff0066" "#86C30D" "#63de5d" "#BEB244" "#E6DB74" "#40CAE4" "#06d8ff" "#FF61FF" "#ff8eff" "#00b2ac" "#53f2dc" "#f8fbfc" "#ffffff"))
;; '(package-selected-packages
;;   (quote
;;    (org-plus-contrib org py-autopep8 elpy powershell csharp-mode rainbow-delimiters projectile indent-guide flycheck gitignore-mode gitconfig-mode magit smex ido-ubiquitous esup color-theme-solarized mustang-theme monokai-theme molokai-theme material-theme jbeans-theme gruvbox-theme darktooth-theme darkokai-theme badwolf-theme aurora-theme color-theme)))
 
;; ***************************************************************************
;; Local Variables:
;; mode: lisp
;; indent-tabs-mode: nil
;; tab-width: 4
;; End:
;; ***************************************************************************
