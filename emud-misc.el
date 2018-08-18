(require 'cl)

(defstruct emud-edible 
  name type price (HPinc 0) (SPinc 0) 
  (nt-inc 0) (dr-inc 0) (sa-inc 0) command)

(defstruct emud-player
  name alignment xp qp gold np hp sp max-hp max-sp con dex str int satiated 
  not-thirsty drunk)

(defvar emud-player-current nil)
(defvar emud-player-haven nil)
(defvar emud-player-moved t)
(defvar emud-player-current-edible nil)
(defvar emud-player-run-hp 100)
(defvar emud-player-healing nil)
(defvar emud-player-attack "sp")
(defvar emud-player-kill-room nil)

(defun emud-player-kill ()
  (interactive)
  (when emud-player-kill-room
    (emud-map-path (emud-room-number emud-player-kill-room)))) 

(emud-get-make-xml-handler 'hpinfo)
(defadvice emud-xml-handler-hpinfo (before emud-player-hpinfo (proc face node))
  (let ((hpstr (car (xml-node-children node)))
	(last-hp (emud-player-hp emud-player-current))
	(last-sp (emud-player-sp emud-player-current))
	curr-hp curr-sp max-sp max-hp eat edible-list edible command)
    (when (string-match 
	   (concat "HP:\\s-+\\([0-9]+\\)/\\([0-9]+\\)\\s-+"
		   "SP:\\s-+\\([0-9]+\\)/\\([0-9]+\\)")
	   hpstr)
      (setq curr-hp
	    (string-to-number (match-string 1 hpstr)))
      (setf (emud-player-hp emud-player-current)
	    curr-hp)
      
      (setq max-hp (string-to-number (match-string 2 hpstr)))
      (setf (emud-player-max-hp emud-player-current)
	    max-hp)
      
      (setq curr-sp (string-to-number (match-string 3 hpstr)))
      (setf (emud-player-sp emud-player-current)
	    curr-sp)
      
      (setq max-sp (string-to-number (match-string 4 hpstr)))
      (setf (emud-player-max-sp emud-player-current)
	    max-sp)
      
      (when emud-map-curr-room
	(when (setq edible (emud-player-find-edible))
	  (setq emud-player-current-edible edible)
	  (when (and (> curr-hp last-hp) (< curr-hp max-hp))
	    (setf (emud-edible-HPinc edible) (- curr-hp last-hp)))
	  
	  (when (and (> curr-sp last-sp) (< curr-sp max-sp))
	    (setf (emud-edible-SPinc edible) (- curr-sp last-sp))))
	  
	(when (and emud-player-moved emud-player-haven 
		   (< curr-hp last-hp) (< curr-hp emud-player-run-hp))
	  (setq emud-player-moved nil)
	  (emud-map-path emud-player-haven))))))


(emud-get-make-xml-handler 'shape_info)
(defadvice emud-xml-handler-shape_info 
  (before emud-player-shape_info (proc face node))
  (setq emud-player-kill-room emud-map-curr-room)
  (emud-simple-send proc emud-player-attack))

(emud-get-make-xml-handler 'prompt)
(defadvice emud-xml-handler-prompt (before emud-player-prompt (proc face node))
  (when (stringp (car (xml-node-children node)))
    (let ((prompt-str (car (xml-node-children node)))
	  (last-dr  (emud-player-drunk emud-player-current))
	  (last-sat (emud-player-satiated emud-player-current))
	  (last-nt  (emud-player-not-thirsty emud-player-current))
	  curr-dr curr-sat curr-nt edible)
          
      (when (string-match "Dr: *\\([0-9]+\\)%" prompt-str)
	(setf (emud-player-drunk emud-player-current) 
	      (setq curr-dr (string-to-number (match-string 1 prompt-str)))))
      
      (when (string-match "Sa: *\\([0-9]+\\)%" prompt-str)
	(setf (emud-player-satiated emud-player-current) 
	      (setq curr-sat (string-to-number (match-string 1 prompt-str)))))
    
      (when (string-match "Nt: *\\([0-9]+\\)%" prompt-str)
	(setf (emud-player-not-thirsty emud-player-current) 
	      (setq curr-nt (string-to-number (match-string 1 prompt-str)))))
      
      (when (string-match "^\\([a-zA-Z]+\\)(\\([a-z]+\\))" prompt-str)
	(setf (emud-player-name emud-player-current)
	      (match-string-no-properties 1 prompt-str))
	(setf (emud-player-alignment emud-player-current)
	      (match-string-no-properties 2 prompt-str)))
      
      (when (setq edible emud-player-current-edible)
	(setq emud-player-current-edible nil)
	(when (and last-dr curr-dr (> curr-dr last-dr))
	  (setf (emud-edible-type edible) 'alcohol)
	  (setf (emud-edible-dr-inc edible) (- curr-dr last-dr)))
	(when (and last-sat curr-sat (> curr-sat last-sat))
	  (setf (emud-edible-type edible) 'food)
	  (setf (emud-edible-sa-inc edible) (- curr-sat last-sat)))
	(when (and last-nt curr-nt (> curr-nt last-nt))
	  (setf (emud-edible-type edible) 'drink)
	  (setf (emud-edible-nt-inc edible) (- curr-nt last-nt))))
      (when emud-player-healing
	(emud-player-do-heal proc)))))

(defun emud-player-do-heal (proc)
  (cond
   ((and emud-map-curr-room (eq (emud-room-type emud-map-curr-room) 'pub)
	 emud-player-current)
    (let ((edible-list (cdr (emud-room-extra emud-map-curr-room)))
	  (hp-need     (- (emud-player-max-hp emud-player-current) 
			  (emud-player-hp emud-player-current)))

	  (sp-need     (- (emud-player-max-sp emud-player-current) 
			  (emud-player-sp emud-player-current)))
	  edible choice-edible (best-heal 0) healing)
      (setq healing 
	    (cond 
	     ((> hp-need 10)
	      'hp)
	     ((> sp-need 10)
	      'sp)
	     (t
	      (setq emud-player-healing nil))))
      
      (when healing
	(while edible-list
	  (setq edible (pop edible-list))
	  (when 
	      (and 
	       (or
		(and (eq healing 'hp)
		     (>  (emud-edible-HPinc edible) best-heal)
		     (<= (emud-edible-HPinc edible) (+ hp-need 10)))
		(and (eq healing 'sp)
		     (>  (emud-edible-SPinc edible) best-heal)
		     (<= (emud-edible-SPinc edible) (+ sp-need 10))))
	       
	       (or (=  (emud-edible-dr-inc edible) 0)
		   (<  (emud-player-drunk emud-player-current) 90)
		   (<= (+ (emud-edible-dr-inc edible)
			   (emud-player-drunk emud-player-current))
			100))
	       (or (=  (emud-edible-sa-inc edible) 0)
		   (<  (emud-player-satiated emud-player-current) 90)
		   (<= (+ (emud-edible-sa-inc edible)
			   (emud-player-satiated emud-player-current))
			100))
	       (or (=  (emud-edible-nt-inc edible) 0)
		   (<  (emud-player-not-thirsty emud-player-current) 90)
		   (<= (+ (emud-edible-nt-inc edible)
			   (emud-player-not-thirsty emud-player-current))
			100)))
	    (setq choice-edible edible)
	    (setq best-heal
		  (cond
		   ((eq 'hp healing)
		    (emud-edible-HPinc edible))
		   ((eq 'sp healing)
		    (emud-edible-SPinc edible))))))
	(if choice-edible
	    (let ((cmd-list (emud-edible-command choice-edible)))
	      (while cmd-list
		(emud-simple-send proc (pop cmd-list))))
	  (setq emud-player-healing nil)))))
   (t
    (setq emud-player-healing nil))))

	       
	  
      

(defun emud-player-find-edible ()
  (when emud-map-curr-room
    (let (eat edible edible-list command)
      (cond
       ((string-match "^buy \\(.*\\)$" emud-last-command)
	(setq eat (match-string-no-properties 1 emud-last-command))
	(setq command (list (concat "buy " eat))))
       ((string-match "^eat \\(\\.*\\)$" emud-last-command)
	(setq eat (match-string-no-properties 1  emud-last-command))
	(setq command (list (concat "buy " eat) (concat "eat" eat))))
       ((string-match "^drink \\(\\.*\\)$" emud-last-command)
	(setq eat (match-string-no-properties 1  emud-last-command))
	(setq command (list (concat "buy " eat) (concat "drink" eat)))))
      
      (when eat
	(setf (emud-room-type emud-map-curr-room) 'pub)
	(setq edible-list (emud-room-extra emud-map-curr-room))
	(unless edible-list
	  (setf (emud-room-extra emud-map-curr-room) '(t)))
	(setq edible-list (cdr edible-list))
	(while (and (not edible) edible-list)
	  (setq edible (pop edible-list))
	  (unless (string= eat (emud-edible-name edible))
	    (setq edible nil)))
	(unless edible
	  (setq edible (make-emud-edible :name eat :command command))
	  (setf (emud-room-extra emud-map-curr-room)
		(append (emud-room-extra emud-map-curr-room) (list edible))))
	edible))))
    

    
  
(emud-get-make-xml-handler 'short)
(defadvice emud-xml-handler-short (before emud-player-short (proc face node))
  (setq emud-player-moved t))

(defun emud-player-start ()
  (interactive)
  
  (unless emud-player-current
    (setq emud-player-current (make-emud-player)))
  (setq emud-player-healing nil)
  
  (emud-add-trigger "Your barkskin fades away" "barkskin")
  (emud-add-trigger "^HP: [0-9]+"  'emud-player-read-score-trigger)

  (ad-enable-advice 'emud-xml-handler-hpinfo 'before 'emud-player-hpinfo)
  (ad-activate 'emud-xml-handler-hpinfo)

  (ad-enable-advice 'emud-xml-handler-prompt 'before 'emud-player-prompt)
  (ad-activate 'emud-xml-handler-prompt)
  
  (ad-enable-advice 'emud-xml-handler-short 'before 'emud-player-short)
  (ad-activate 'emud-xml-handler-short)

  (ad-enable-advice 'emud-xml-handler-shape_info 'before 
		    'emud-player-shape_info)
  (ad-activate 'emud-xml-handler-shape_info))

(defun emud-player-stop ()
  (interactive)
  
  (emud-del-trigger "Your barkskin fades away")
  (emud-del-trigger "^HP: [0-9]+" )

  (ad-disable-advice 'emud-xml-handler-hpinfo 'before 'emud-player-hpinfo)
  (ad-activate 'emud-xml-handler-hpinfo)

  (ad-disable-advice 'emud-xml-handler-prompt 'before 'emud-player-prompt)
  (ad-activate 'emud-xml-handler-prompt)

  (ad-disable-advice 'emud-xml-handler-short 'before 'emud-player-short)
  (ad-activate 'emud-xml-handler-short)

  (ad-disable-advice 'emud-xml-handler-shape_info 'before 
		     'emud-player-shape_info)
  (ad-activate 'emud-xml-handler-shape_info))

(defun emud-player-set-haven ()
  (interactive)
  (setq emud-player-haven (emud-room-number emud-map-curr-room)))

(defun emud-player-set-run-hp (hp)
  (interactive "nLeave on HP? ")
  (setq emud-player-run-hp hp))


(defun emud-player-read-score-trigger (proc string)
  (when (string-match "HP: \\([0-9]+\\)" string)
    (setf (emud-player-hp emud-player-current)
	  (string-to-number (match-string 1 string))))

  (when (string-match "SP: \\([0-9]+\\)" string)
    (setf (emud-player-sp emud-player-current)
	  (string-to-number (match-string 1 string))))
  
  (when (string-match "GC: \\([0-9]+\\)" string)
    (setf (emud-player-gold emud-player-current)
	  (string-to-number (match-string 1 string))))

  (when (string-match "QP: \\([0-9]+\\)" string)
    (setf (emud-player-qp emud-player-current)
	  (string-to-number (match-string 1 string))))

   (when (string-match "NP: \\([0-9]+\\)" string)
     (setf (emud-player-np emud-player-current)
	  (string-to-number (match-string 1 string)))))

(defun emud-player-heal ()
  (interactive)
  (if (and emud-map-curr-room (eq 'pub (emud-room-type emud-map-curr-room)))
      (setq emud-player-healing t)
    (message "Can't heal. We aren't in a pub.")))