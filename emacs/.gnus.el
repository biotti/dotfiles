;; https://www.emacswiki.org/emacs/GnusTutorial
;; Impostazione 
(setq user-email-address "private@domain.com"
	  user-full-name     "private")
;; Impostazione del news server principale
(setq gnus-select-method '(nntp "nntp.aioe.org"))
;; E' possibile aggiungere ulteriori news servers
(add-to-list 'gnus-secondary-select-methods '(nntp "news.gnus.org"))
;; (add-to-list 'gnus-secondary-select-methods '(nntp "localhost"))

