;;; early-init.el --- File di configurazione "early-init" di GNU Emacs -*- mode: emacs-lisp; lexical-binding: t;-*-

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
;; durante la fase di Early Init.
;;
;; La fase di Early Init e' stata introdotta con GNU Emacs versione 27
;;
;; Per maggiori informazioni fare riferimento al manuale di GNU Emacs
;; versione 27 o successiva: 49.4.6 - The Early Init File

;;; To do:

;;; Change log:

;;; Code:

;; N.B.: Ho rimosso l'impostazione del lexical-binding:
;;  -*- lexical-binding: t; -*-


;; Imposto l'ora di avvio di Emacs
;; Servira' alla fine per determinare quanto tempo e' trascorso
(defconst gb/emacs/emacs-startup-time (current-time))

;; Imposto le varibili di appoggio usate per ripristinare
;; le impostazioni di default procedura di inizializzazione
(defvar gb/emacs/gc-cons-threshold-original gc-cons-threshold
  "Valore originale di 'gc-cons-threshold' prima della modifica.
Salvato per ripristinarlo alla fine della procedura di inizializzazione")

(defvar gb/emacs/gc-cons-percentage-original gc-cons-percentage
  "Valore originale di 'gc-cons-percentage' prima della modifica.
Salvato per ripristinarlo alla fine della procedura di inizializzazione")

(defvar gb/emacs/file-name-handler-alist-original file-name-handler-alist
  "Valore originale di 'file-name-handler-alist' prima della modifica.
Salvato per ripristinarlo alla fine della procedura di inizializzazione")

;; Imposta la soglia del garbage collector
;; Da reimpostare poi ai valori corretti con apposito
;; codice richiamato in after-init-hook
(setq gc-cons-threshold (* 1024 (* 1024 1024))  ; 1 GByte
      gc-cons-percentage 0.6)

;; Imposta file-name-handler-alist
;; Da reimpostare poi ai valori corretti con apposito
;; codice richiamato in after-init-hook
(setq file-name-handler-alist nil)

;; Aggiungo ad after-init-hook il codice necessario
;; per reimpostare i valori di default nelle variabili
;; usate qui sopra e fare una garbage-collect finale.
;; Si usa una depth 90 (vedi docstring di di "add-hook")
(add-hook 'after-init-hook
          '(lambda ()
             ;; Non imposto piu' 'gc-cons-threshold' al suo valore originale ma, come
             ;; riportato in molti siti ad un valore molto piu' alto.
             ;; Si veda, ad esempio qui: https://emacs-lsp.github.io/lsp-mode/page/performance/
             ;; (consultato 31/08/2020)
             ;; (setq gc-cons-threshold       gb/emacs/gc-cons-threshold-original)
             ;; 100 Mb = (* 1024 (* 1024 100)))
             (setq gc-cons-threshold       (* 1024 (* 1024 100)))
             ;; Sempre https://emacs-lsp.github.io/lsp-mode/page/performance/
             ;; raccomanda di impostare 'read-process-output-max' ad un valore di 1Mb
             ;; (numero massimo di bytes letti in un singolo chunk dai subprocess)
             (setq read-process-output-max (* 1024 1024))
             (setq gc-cons-percentage      gb/emacs/gc-cons-percentage-original)
             (setq file-name-handler-alist gb/emacs/file-name-handler-alist-original)
             (garbage-collect)
             (defvar gb/emacs/elapsed (float-time
				                       (time-subtract (current-time) gb/emacs/emacs-startup-time))
	           )
	         (message (emacs-init-time))
	         (message "Loading done in %.3fs seconds and %d garbage collections [after-init]"
                      gb/emacs/elapsed
                      gcs-done)
             )
          90
          )

;; Non rende disponibili i package all'avvio di Emacs
;; da usare qui e non in init.el
(setq package-enable-at-startup nil)

;; Per GNU Emacs versione 27 e successive
(when (not (version< emacs-version "27"))
  (progn
    ;; Consente il caricamento dalla cache dei package
    (setq package-quickstart t)
    )
  )

;; Non ridimnensiona il frame in questo momento
(setq frame-inhibit-implied-resize t)

;; Su Windows, assumendo di aver installato Scoop, ne metto il path
;; in testa, altrimenti vengono prima trovati gli eseguibili nelle
;; directory di sistema.  Questo crea confusione, ad esempio concat
;; "find" che esiste sia in ambiente Linux che in Windows, ovviamente
;; con sintassi completamente diverse.  Generalmente mi apsetto che
;; le funzionalita' siano quelle del mondo Linux e non quelle del
;; mondo Windows per cui faccio in modo che vengano lette per prima.
;; Da notare che Scoop aggiunge le sue directory al Path, ma queste
;; sono di tipo utente e vengono aggiunte al path dopo quelle di
;; sistema.  Si avra' un "doppione" nel path, ma va bene.
(when (eq system-type 'windows-nt)
  (defvar gb/emacs/scoop-shim-path
    (concat (expand-file-name "~/scoop/shims")
	        path-separator)
    "Percorso per 'scoop/shims' da aggiungere in testa al PATH."
    )
  ;;(add-to-list 'exec-path "c:/Users/Geraldo/scoop/shims")
  (add-to-list 'exec-path gb/emacs/scoop-shim-path)
  ;; (setenv "PATH" (concat gb/emacs/scoop-shim-path
  ;;                        (getenv "PATH")))
  )

(provide 'early-init)

;;(progn
;;  (setq gb/frame-font
;;        (cond ((find-font (font-spec :name "DejaVu Sans mono")) '(font . "DejaVu Sans Mono-10"))
;;	          ((find-font (font-spec :name "Consolas"))         '(font . "Consolas-10"))
;;	          ((find-font (font-spec :name "Inconsolata"))      '(font . "Inconsolata-10"))
;;	          ((find-font (font-spec :name "Courier New"))      '(font . "Courier New-10"))
;;	          ))
;;  (print gb/frame-font)
;;  (add-to-list 'default-frame-alist gb/frame-font)
;;  (add-to-list 'initial-frame-alist gb/frame-font))

;; ===========================================================================
;; Local Variables:
;; coding: utf-8-unix
;; indent-tabs-mode: nil
;; tab-width: 4
;; End:
;; ===========================================================================

;;; early-init.el ends here
