(require 'cl)

(defstruct room 
  type name short from-home to-home extra)
  

(defstruct edible 
  type price HPinc SPinc command)


(setq green (make-room :name 'green  :type 'home :short "The Village Green"))

(setq firebreather (make-edible :type 'alcohol :price 150 :HPinc 50 :SPinc 50
      :command '("buy firebreather" "drink firebreather")))
 
(setq pub (make-room :name 'pub    :type 'pub  :short "The Rose and Dragon"
	   :from-home '("e" "e" "n" "e") 
	   :to-home '("w" "s" "e" "n" "sell bottles" "s" "w" "w" "w")
	   :extra (list firebreather)))


(defun emud-xml-handler-hpinfo (proc face node)
  (let ((hpstr (xml-get-children node)))
    (when (string-match 
	 (concat "HP:\\S-+\\([:digit:]+\\)/\\([:digit:]+\\)\\S-+"
		 "SP:\\S-+\\([:digit:]+\\)/\\([:digit:]+\\)")
	 hpstr)
      (setq emud-curr-HP (string-to-number (match-string 0 hpstr))
	    emud-max-HP  (string-to-number (match-string 1 hpstr))
	    emud-curr-SP (string-to-number (match-string 2 hpstr))
	    emud-max-SP  (string-to-number (match-string 3 hpst))))
    (emud-xml-default-handler proc face node)))

(defun emud-xml-handler-hpinfo (proc face node)
  (set emud-curr-short ((xml-get-children node)))
  (emud-xml-default-handler proc face node))
