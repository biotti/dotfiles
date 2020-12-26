;;; init.el --- File di configurazione di GNU Emacs -*- mode: emacs-lisp; lexical-binding: t;-*-

;; Copyright (C) 2020 Geraldo Biotti

;; Author: Geraldo Biotti <gbiotti@gmail.com>
;; Created: 20200731
;; Keywords: init, early-init, .emacs.d, startup
;; Compatiblity: emacs-version >= 27

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Questo file contiene le impostazioni di GNU Emacs che vengono eseguite
;; durante la fase di Init.
;; La fase di Init viene eseguita successivamente a quella di Early Init
;;
;; Per maggiori informazioni fare riferimento al manuale di GNU Emacs:
;; 49.4 The Emacs Initialization File

;;; To do:

;;; Change log:

;;; Code:

;; ----------------------------------------------------------------------------------------
;; https://etienne.depar.is/emacs.d/init.html
;; https://github.com/MatthewZMD/.emacs.d

;; https://www.reddit.com/r/emacs/comments/2edbau/what_are_some_great_emacsd_examples/
;; https://www.reddit.com/r/emacs/comments/2edbau/what_are_some_great_emacsd_examples/

;; https://github.com/AndreaCrotti

;; https://github.com/grettke/

;; Pastebin off topic:
;; https://www.privacytools.io/
;; https://alternativeto.net/list/18434/xenmaster-s-privacy-tools

;; https://send-anywhere.com/
;; https://framasoft.org/en/
;; https://gofile.io/welcome
;; ----------------------------------------------------------------------------------------

;; N.B.: Ho rimosso l'impostazione del lexical-binding:
;;  -*- lexical-binding: t; -*-

;; Se la versione e' inferiore alla 26.1 emetto un warning
(when (version< emacs-version "26.1")
  (warn "E' necessario che GNU Emacs sia in versione 26.1 o successiva!"))

(defun gb/emacs/package-setup ()
  "Function che imposta 'package'"
  ;; Carico il modulo di gestione dei packages
  (require 'package)
  ;; Carica sempre il file piu' recente tra '.el' e '.elc'
  (setq load-prefer-newer t)
  ;; Aggiungo all'elenco dei repositories da cui scaricare i packages
  ;; la versione "unstable" di Melpa
  (add-to-list 'package-archives
		       '("melpa" . "https://melpa.org/packages/"))
  ;; Genera dei warnings con i package-install 
  (unless (bound-and-true-p package--initialized)
    (package-initialize))
  )

(defun gb/emacs/init-old-emacs-version ()
  "Function eseguita per il setup di init.el quando si sta usando Emacs
  in versione precedente alla 27"
  ;; Early-init e' gestito automaticamente dalla versione 27 in poi
  ;; Se esiste early-init.el lo carico
  (let ((gb/emacs/early-init-file (expand-file-name "early-init.el" user-emacs-directory)))
    (when (file-exists-p gb/emacs/early-init-file)
	  (require 'early-init gb/emacs/early-init-file)))
  (gb/emacs/package-setup)
  )

(defun gb/emacs/init-new-emacs-version ()
  "Function eseguita per il setup di init.el quando si sta usando Emacs
  in versione 27+"
  ;; Avvio package
  (gb/emacs/package-setup)
  )

;; Eseguo le impostazioni in base alla versione di GNU Emacs
(if (version< emacs-version "27")
    (gb/emacs/init-old-emacs-version)
  (gb/emacs/init-new-emacs-version))

;; Delight e' un package che viene usato da use-package
;; mi accerto che sia installato, se non lo e' lo installo
;; N.B.: Se non si vuole averlo come dipendenza e' bene
;; installarlo prima di use-package
(unless (package-installed-p 'delight)
  (unless package-archive-contents
    (package-refresh-contents))
  (package-install 'delight))

;; Diminish e' un package che viene usato da use-package
;; mi accerto che sia installato, se non lo e' lo installo
;; N.B.: Se non si vuole averlo come dipendenza e' bene
;; installarlo prima di use-package
(unless (package-installed-p 'diminish)
  (unless package-archive-contents
    (package-refresh-contents))
  (package-install 'diminish))

;; Mi accerto che use-package sia installato
;; se non lo e' lo installo
(unless (package-installed-p 'use-package)
  (unless package-archive-contents
    (package-refresh-contents))
  (package-install 'use-package))

;; Carico use-package
(eval-when-compile
  (require 'use-package))

;; Configuro use-package prima di caricarlo
(eval-and-compile
  (if init-file-debug
      (setq use-package-verbose t
            use-package-expand-minimally nil
            use-package-compute-statistics t
            debug-on-error t) ; True
    (setq use-package-verbose nil
          use-package-expand-minimally t) ; False
      )
  (setq use-package-enable-imenu-support t
        ;; Quanto segue e' MOLTO IMPORTANTE:
        ;; Usare sempre gli hook con il loro nome completo
        ;; al posto del nome abbreviato:
        ;; after-init --> after-init-hook
        ;; Questo migliora la gestione della documentazione
        ;; a riconoscere il contesto (vedi, ad esempio 'describe-symbol)
        use-package-hook-name-suffix nil)
  )

;; Configuro vc (package gestione "version cotrol"
(use-package vc
  :config
  ;; Questo perche' i miei "dotfiles" usano i link simbolici
  (setq vc-follow-symlinks t)
  )

;; Carico org
(use-package org)

;; Qui avviene la magia.
;; Carico la configurazione dal file "org"
;; Cerco pero' di ottimizzare un mimino la cosa:
;; se il file "el" generato da org-babel e' piu' recente
;; del file "org" allora carico "el" altrimenti passo
;; all'uso di org-babel
(progn (defvar gb/emacs/gb-init "gb-init")
       (defvar gb/emacs/conf-filename (expand-file-name gb/emacs/gb-init user-emacs-directory))
       (defvar gb/emacs/el-conf-filename (concat gb/emacs/conf-filename ".el"))
       (defvar gb/emacs/org-conf-filename (concat gb/emacs/conf-filename ".org"))
       (if (file-exists-p gb/emacs/el-conf-filename)
	       (if (file-newer-than-file-p gb/emacs/org-conf-filename gb/emacs/el-conf-filename)
               (progn (message "%s e' piu' recente di %s, ricreo e carico il .el"
                               gb/emacs/org-conf-filename
                               gb/emacs/el-conf-filename)
		              (org-babel-load-file gb/emacs/org-conf-filename))
	         (progn (message "%s e' meno recente di %s, carico il .el senza ricrearlo"
                             gb/emacs/org-conf-filename
                             gb/emacs/el-conf-filename)
		            (load-file gb/emacs/el-conf-filename)))
	     (progn (message "Creo e carico %s"  gb/emacs/el-conf-filename)
		        (org-babel-load-file gb/emacs/org-conf-filename))
	     )
       )

;; NON RIMUOVERE CUSTOM DA QUI
;; ---------------------------
;; Si potrebbe cedere alla tentazione di avere un init.el piu' "pulito"
;; spostando custom-set-variables e custom-set-faces in un file separato,
;; ma questo porta spesso a comportamenti altalenanti: se si installa un
;; package con use-package e la sua opzione :ensure, capita che il package
;; venga installato, ma la variabile package-selected-packages non venga
;; aggiornata correttamente portanto il package installato ad uno stato
;; di "dependency" in list-packages con invito alla rimozione qualora questo
;; non fosse effettivamente utilizzato anche come dipendenza da qualche altro
;; package
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(delete-selection-mode nil)
 '(package-selected-packages
   '(docker docker-compose-mode dockerfile-mode projectile eglot company-go go-errcheck go-mode company-auctex auctex sql-indent markdown-mode csharp-mode powershell counsel-projectile ibuffer-projectile yasnippet-snippets yasnippet rainbow-delimiters smartparens flycheck-pos-tip flycheck highlight-indent-guides aggressive-indent pcre2el emms pdf-tools csv-mode pretty-mode elfeed-protocol elfeed-org elfeed-goodies elfeed company-restclient restclient treemacs-all-the-icons treemacs-projectile treemacs-magit treemacs git-timemachine gitattributes-mode gitignore-mode gitconfig-mode magit undo-tree origami company-quickhelp company ace-window avy symon beacon htmlize org-edna org-bullets amx ivy-hydra all-the-icons-ivy-rich ivy-rich counsel swiper ivy hydra which-key dashboard minions doom-modeline base16-theme seti-theme moe-theme solarized-theme color-theme-sanityinc-tomorrow dracula-theme atom-one-dark-theme zerodark-theme modus-vivendi-theme modus-operandi-theme gruvbox-theme monokai-theme zenburn-theme material-theme spacemacs-theme doom-themes all-the-icons-ibuffer all-the-icons-dired all-the-icons async use-package diminish delight)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; ===========================================================================
;; Local Variables:
;; coding: utf-8-unix
;; indent-tabs-mode: nil
;; tab-width: 4
;; End:
;; ===========================================================================

;;; init.el ends here
