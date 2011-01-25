;; incantation to put in .emacs for reasonable handling of SFDL:
;; - based on C mode
;; - with some alignm
;; *********** SFDL ************
(c-add-style "sfdl"
	     '("gnu"
	       (c-offsets-alist
		(statement-cont . 0))))

;; switch to SFDL style for sfdl files
(setq auto-mode-alist
      (append
       '(("\\.sfdl$"	. (lambda ()
			    (c-mode)
			    (c-set-style "sfdl")))
	 ) auto-mode-alist))
