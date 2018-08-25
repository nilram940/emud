; emud-map.el
;; $Id$
;; functions and data structures to add mapping capabilities to emud.

(require 'cl)
(defvar emud-map-draw-flag nil)
(defvar emud-map-path-cache nil)

(defvar emud-map-bad-commands
  '(look sp sm barkskin))

(defvar emud-mapping-flag nil
  "t when mapping, nil otherwise")

(defvar emud-map-curr-room nil
  "Contains data for the current room when mapping" )

(defvar emud-map-coord [0 0 0]
  )

(defvar emud-map-room-info nil
  "Alist containing data for the current room")

(defvar emud-map-last-room nil
  "" )


(defvar emud-map-last-cmd nil
  "Contains the last command recieved")

(defvar emud-map-ship-exit nil)

(defvar emud-curr-map nil
  "The current map of the game")


(defconst emud-map-directions
  '((n  . [0 1 0]) (s  . [0 -1 0]) (e  . [1 0 0])  (w  . [-1 0 0])
    (ne . [1 1 0]) (se . [1 -1 0]) (nw . [-1 1 0]) (sw . [-1 -1 0])
    (u  . [0 0 1]) (d  . [0 0 -1])))


(defconst emud-map-direction-abbrev
  '((n . north) (e . east) (u . up) (sw . southwest) (nw . northwest)
    (s . south) (w . west) (d . down) (ne . northeast) (se . southeast)))

(defconst emud-map-reverse
  '((n . s) (e . w) (u . d) (sw . ne) (nw . se) 
    (s . n) (w . e) (d . u) (ne . sw) (se . nw)))


(defstruct emud-map 
  (size 100) 
  (last 0)
  (hole nil)
  (sibling-hash (make-hash-table :test 'equal))
  alias-list
  (arr (make-vector size nil)))

(defstruct emud-room
  number alias short type obv-exits long coord exits entrances 
  siblings extra)

(defmacro emud-map-get-room (map number)
  `(aref (emud-map-arr ,map) ,number))

(defmacro emud-map-get-siblings (map short)
  `(gethash ,short (emud-map-sibling-hash ,map)))

(define-key emud-mode-map "\C-c\C-m" 'emud-toggle-mapping)
(defun emud-toggle-mapping ()
  (interactive)
  (if emud-mapping-flag
      (emud-map-stop)
    (emud-map-start))
  (message (concat 
	    "Mapping turned " (if emud-mapping-flag
				  "on" "off"))))
(defun emud-map-start ()
  (interactive)
  (unless emud-mapping-flag
    (unless emud-curr-map 
      (setq emud-curr-map (make-emud-map)))
    (setq emud-map-curr-room nil
	  emud-mapping-flag t)

    (emud-add-trigger "A dark room" 'emud-map-dark-room)
    (emud-add-trigger "Shell doing:" 'emud-map-shell-doing)
    (emud-add-trigger "We have arrived" 'emud-map-ship-trigger)
    
    (add-hook 'emud-new-command-hook 'emud-map-new-command)

    ;;short
    (ad-enable-advice 'emud-xml-handler-short 'before 'emud-map-short)
    (ad-activate 'emud-xml-handler-short)

    ;;wimpy
    (ad-enable-advice 'emud-xml-handler-wimpy 'before 'emud-map-wimpy)
    (ad-activate 'emud-xml-handler-wimpy)
    
    ;;leave
    ;(ad-enable-advice 'emud-xml-handler-leave 'before 'emud-map-leave)
    ;(ad-activate 'emud-xml-handler-leave)

    ;;prompt
    (ad-enable-advice 'emud-xml-handler-prompt 'before 'emud-map-prompt)
    (ad-activate 'emud-xml-handler-prompt)

    ;;long
    ;(ad-enable-advice 'emud-xml-handler-long 'before 'emud-map-long)
    ;(ad-activate 'emud-xml-handler-long)

    ;;exit
    (ad-enable-advice 'emud-xml-handler-exit 'before 'emud-map-exit)
    (ad-activate 'emud-xml-handler-exit)))

(defun emud-map-stop ()
  (interactive)
 (when emud-mapping-flag
   (setq emud-mapping-flag nil)
   (emud-del-trigger "A dark room.")
   (emud-del-trigger "Shell doing:")
   (emud-del-trigger "We have arrived")

   (remove-hook 'emud-new-command-hook 'emud-map-new-command)
   
   ;;short
   (ad-disable-advice 'emud-xml-handler-short 'before 'emud-map-short)
   (ad-activate 'emud-xml-handler-short)

   ;;wimpy
   (ad-disable-advice 'emud-xml-handler-wimpy 'before 'emud-map-wimpy)
   (ad-activate 'emud-xml-handler-wimpy)
  
   ;;leave
   (ad-disable-advice 'emud-xml-handler-leave 'before 'emud-map-leave)
   (ad-activate 'emud-xml-handler-leave)
   
   ;;prompt
   (ad-disable-advice 'emud-xml-handler-prompt 'before 'emud-map-prompt)
   (ad-activate 'emud-xml-handler-prompt)
    
   ;;long
   ;(ad-disable-advice 'emud-xml-handler-long 'before 'emud-map-long)
   ;(ad-activate 'emud-xml-handler-long)
   
   ;;exit
   (ad-disable-advice 'emud-xml-handler-exit 'before 'emud-map-exit)
   (ad-activate 'emud-xml-handler-exit)))


(defun emud-map-ship-trigger (proc string)
  (when (and
	 emud-map-curr-room
	 (string-match "arrived at \\(.*\\) and are disembarking." string))
    (setf (emud-room-type emud-map-curr-room) 'ship)
    (setq emud-map-ship-exit (match-string 1 string))))

		      
(defun emud-map-shell-doing (proc string)
  (if (string-match "Shell doing: \\(.*\\)$" string)
      (setq emud-map-last-cmd (match-string 1 string))))

(defun emud-map-dark-room (proc string)
  (let ((room emud-map-curr-room)
	(last-cmd  (and emud-map-last-cmd
			(intern emud-map-last-cmd)))
	(map emud-curr-map)
	new-room)
    (setq emud-map-last-cmd nil
	  emud-map-curr-room 
	  (when (and room 
		     (emud-room-exits room)
		     (setq new-room 
			   (assq last-cmd
				 (emud-room-exits room))))
	    (aref (emud-map-arr map) (cdr new-room))))))


(defun emud-map-new-command ()
  (setq emud-map-last-cmd emud-last-command)
  (when (string-match "^\\s-+" emud-map-last-cmd)
    (replace-match "" t t emud-map-last-cmd)))
  ;(emud-warn (format "Setting emud-map-last-cmd to %s" emud-map-last-cmd)))

(emud-get-make-xml-handler 'prompt)
(defadvice emud-xml-handler-prompt (before emud-map-prompt  (proc face node))
  (when emud-map-room-info 
    (emud-map-new-room)))

(emud-get-make-xml-handler 'wimpy)
(defadvice emud-xml-handler-wimpy (before emud-map-wimpy  (proc face node))
  (let ((string (car (xml-node-children node)))
	cmd cmd-abbrev)
    (when (string-match "legs run \\(\\S-+\\) with" string)
      (setq cmd (intern (match-string 1 string)))
      (when (setq cmd-abbrev (car (rassq cmd emud-map-direction-abbrev)))
	(setq cmd cmd-abbrev))
      (emud-map-set-room-info 'cmd cmd)
      (emud-map-new-room))))
      

(emud-get-make-xml-handler 'leave)
(defadvice emud-xml-handler-leave (before emud-map-leave (proc face node))
  (let ((string (car (xml-node-children node))))
    (when (string-match "leaves \\([^.]+\\)." string)
      (setq emud-map-last-cmd (match-string 1 string)))))

(defun emud-map-set-room-info (tag data)
  (let (assoc delta)
    (cond
     ((setq assoc (assq tag emud-map-room-info))
      (emud-warn (format "Rewriting %s" tag))
      (setcdr assoc data))
     (t
      (when (and (eq tag 'cmd)
		 (setq delta (cdr (assq data emud-map-directions))))
	(setq emud-map-coord (emud-add-vec emud-map-coord delta)))
	;(emud-warn (format "Setting cmd to %s" data)))
      (setq emud-map-room-info 
	    (cons (cons tag data) emud-map-room-info))))))

(emud-get-make-xml-handler 'short)
(defadvice emud-xml-handler-short (before emud-map-short (proc face node))
  (let ((short (emud-xml-find-string (xml-node-children node)))
	(map emud-curr-map)
	(map-last-cmd (and emud-map-last-cmd
			   (< (length emud-map-last-cmd) 100)
			   (intern emud-map-last-cmd)))
	cmd-abbrev elist)
    (when emud-map-room-info
      (emud-map-new-room))

    (setq emud-map-last-cmd nil)
    (when (and emud-map-curr-room
	       emud-map-ship-exit
	       (eq (emud-room-type emud-map-curr-room) 'ship)
	       (eq map-last-cmd 'out))
      (setq map-last-cmd (intern emud-map-ship-exit))
      (setq emud-map-ship-exit nil))
	       

    (when (setq cmd-abbrev (rassq map-last-cmd emud-map-direction-abbrev))
      (setq map-last-cmd (car cmd-abbrev)))
    ;(emud-warn (format "Short: setting cmd to %s" map-last-cmd))
    (emud-map-set-room-info 'cmd map-last-cmd)
    (emud-map-set-room-info 'short short)))
    



		  
(defun emud-map-new-room-old ()
  (let ((map emud-curr-map)
	(room emud-map-curr-room)
	(last-cmd (cdr (assq 'cmd emud-map-room-info)))
	(short    (cdr (assq 'short emud-map-room-info)))
	(obv-exits (cdr (assq 'obv-exits emud-map-room-info)))
	new-room number coord)
     ;(message (format "%s->%s%s: cmd-Q:%s" last-cmd short
	; 	       obv-exits emud-xml-command-queue))
    (setq emud-map-room-info nil)
    (cond
     ((not (stringp short))
      (setq emud-map-curr-room nil)) ;no short means no room.
     ((or (not emud-map-curr-room) 
					;no current room
	  (not (string= short (emud-room-short emud-map-curr-room)))
					;short is NOT that of the current room
	  (assq last-cmd emud-map-reverse))
                                        ;we have a direction command (n s ...)  
      (if (and last-cmd
 	       (not (memq last-cmd emud-map-bad-commands)))
	  ; we have a good command
	  (progn
	    (setq emud-map-curr-room
		  (cond
		   ((and room 
			 (emud-room-exits room)
			 (setq new-room 
			       (emud-map-follow-exit last-cmd
						     (emud-room-exits room)
						     map short)))
		    (aref (emud-map-arr map) new-room))
		   ; we have this direction in our exits. 
		   ; Follow it to the next room.
		   ((and room 
			 (setq number 
			       (emud-map-check-entrances map 
							 room last-cmd short)))
		    (aref (emud-map-arr map) number))
		   ; Theres an entrance leading to this room that
		   ; matches the direction follow it back. 
		   (t  (emud-map-add-room map (make-emud-room 
					       :short short
			       		       :obv-exits obv-exits)))))
	               ;make a new room

	    (unless (string= (emud-room-short emud-map-curr-room) short)
	      (emud-warn (format "Short mismatch: %s %s" 
			       (emud-room-short emud-map-curr-room) short))
	      (setq emud-map-curr-room nil))
	    ;check for short mismatch if so we are lost.
	    (when (and emud-map-curr-room 
			 (not (string= (emud-room-obv-exits emud-map-curr-room)
			     obv-exits)))
	      (emud-warn (format "Obvious exit mismatch %s %s"
			       (emud-room-obv-exits emud-map-curr-room) 
			       obv-exits))
	      (setq emud-map-curr-room nil))
	    ;check for exits mismatch if so carp.
	    (when (and room emud-map-curr-room)
	      (when (emud-room-coord emud-map-curr-room)
		(setq emud-map-coord (emud-room-coord emud-map-curr-room)))
	      (emud-map-add-exit room
				 last-cmd emud-map-curr-room)
	      ;;Set the sibling adjacent flag on rooms with same short
	      (when (and (not (car (emud-room-siblings room))) 
			 (eq (emud-room-siblings room)
			     (emud-room-siblings emud-map-curr-room)))
		(setcar (emud-room-siblings room) t))
	      ;;Check for rooms tagged with coordinates and add coordinates
	      ;; to adjacent rooms
	      (when (and
		     (eq (or (car (emud-room-siblings  room))
			     (car (emud-room-siblings  emud-map-curr-room)))
			 'coord)
		     (emud-room-coord room)
		     (setq coord (cdr (assq last-cmd emud-map-directions))))
		(setf (emud-room-coord emud-map-curr-room)
		      (emud-add-vec coord (emud-room-coord room))))
	      ;;For non-adjacent rooms check entrances for
	      ;; possible room merges
	      (unless (or (car (emud-room-siblings room))
			  (car (emud-room-siblings emud-map-curr-room)))
		(emud-map-check-sibling-entrances 
		 map room last-cmd emud-map-curr-room))))
	;else -- the last command was invalid. We're lost.
 	(setq emud-map-curr-room nil))))))


(defun emud-map-new-room ()
  (let ((map emud-curr-map)
	(source-room emud-map-curr-room) 
					; room we are moving from
	(last-cmd (cdr (assq 'cmd emud-map-room-info)))
					;command that is causing the move
					;gathered by handlers
	(dest-short    (cdr (assq 'short emud-map-room-info)))
					;short description of the destination
					;room as gathered by handlers
	(dest-exits (cdr (assq 'obv-exits emud-map-room-info)))
					;short obvious exits of the destination
					;room as gathered by handlers
	dest-room
					;room we are moving to
	dest-number)
					;room number of destination room

	
     (emud-warn (format "(%d) %s->%s%s: cmd-Q:%s" (if source-room (emud-room-number source-room) -1) last-cmd dest-short
			dest-exits emud-xml-command-queue))
    (setq emud-map-room-info nil)
    ;; We've consumed all the information gathered by handlers and so
    ;; we destroy the data to prevent it from polluting the next room.
    
    (cond
     ((not (stringp dest-short))
      (setq emud-map-curr-room nil))
					; We have no short desctription and
					; cannot proceed.
					; We are lost.

       ;; The following two lines check that we actually have a
       ;; movement command and it is not listed in the command blacklist
       ;; this is likely rendered redundant by the "or" checks below.
     ((and
       last-cmd
       (not (memq last-cmd emud-map-bad-commands))
      					; we have a good command
      
      ;; The following or checks to ensure that we have actually moved to a
      ;; new room. It does this in three phases noted below.
       (or (not source-room) 
					;If there is no current room, there
					;is no need for further checks
	   (not (string= dest-short (emud-room-short source-room)))
					;If dest short and source short do
					;not match, movement likely occured
	   (assq last-cmd emud-map-reverse)))
                                        ;Both source and destination have the
					;same short. If the last command was
					;a direction command, we conclude that
					;movement has occured and proceed.

      (setq dest-room ;dest-room setq
	    (cond  
	     ;; This cond attempts to find the destination room.
	     (;; First we check the exit list of the source room for one that
	      ;; matches the current command. If a matching exit is found
	      ;; we conclude that the destination room is the exit
	      (and source-room 
		   (emud-room-exits source-room)
		   (setq dest-number 
			  (emud-map-follow-exit last-cmd
						(emud-room-exits source-room)
						     map dest-short)))
	      ;; Once we've found the destination in the exit list
	      ;; we return the new room as the destination room 
	      (emud-map-get-room map dest-number))
	     
	     (;; Next we check the entrances of the source room for entrances
	       ;; leading from the destination room with a reversed command.
	      (and source-room 
		   (setq dest-number 
			 (emud-map-check-entrances map 
						   source-room last-cmd dest-short)))
	       ;; Once we've found a matching entrance, we return the
	       ;; new room as the destination room 
	       (emud-map-get-room map dest-number))

	     (;; Next check for coordinate labeled rooms and attempt to find
	      ;; a room at the calculated coordinate
	      
	      (and source-room
		   (setq dest-number
			 (emud-map-check-coord-siblings map
							source-room
							last-cmd dest-short)))
	      ;; Once we've found a room with a matching coordinate, return the
	      ;; new room as the destination room  
	      (emud-map-get-room map dest-number))
	     
	     (t
	       ;; We are unable to find the destination-room so we create a
	       ;; new room and add it to the map. 
	       (emud-map-add-room map (make-emud-room 
					  :short dest-short
					  :obv-exits dest-exits)))))
					; end dest-room setq

       ;; The next phase is to check the sanity of the destination room
       ;; First verify the that the short descriptions match.
       (unless (string= (emud-room-short dest-room) dest-short)
					;Do the short descriptions match?

	 (emud-warn (format "Short mismatch: %s %s" 
			    (emud-room-short dest-room) dest-short))
	 (setq dest-room nil))
					; No. We are lost. Carp and delete
					; the destination room
					; This really should not happen.
       
       ;; Next verify that the exits match. Obvious exits can change
       ;; but this is worth a warning.
       (when (and dest-room 
		  (not (string= (emud-room-obv-exits dest-room)
				dest-exits)))
					; Do the exits match?
	 (emud-warn (format "Obvious exit mismatch %s %s"
			    (emud-room-obv-exits dest-room) 
			    dest-exits)))
					;No. Carp and move on.

       ;; The next phase is to clean up the global map
       ;; this includes adding exits, entrances and coordinates as appropriate
       ;; As well as looking for merge opportunitys.

       (setq emud-map-curr-room dest-room)
					; The merging process can delete
					; dest-room but will preserve
					; curr room so we set curr-room early.

       ;; First verify that we have both a source and destination room
       (when (and source-room dest-room)
	 ;; Add a last command exit leading from source-room to dest-room
	 ;; via last-cmd
	 (emud-map-add-exit source-room
			    last-cmd dest-room)

	 ;; If the flag of either the source or destination room is  coord
	 ;; and the source room has a coordinate compute the coordinate and
	 ;; add it to the destination room
	 (when (and (emud-room-coord source-room)
		    (or (eq (car (emud-room-siblings dest-room)) 'coord)
			(eq (car (emud-room-siblings source-room)) 'coord)))
	   
	   (setf (emud-room-coord dest-room)
		 (emud-map-walk-coord
		  last-cmd
		  (emud-room-coord source-room))))
	 
	 ;; Check if the source and destination rooms are siblings
	 ;; (have the same short). Set the sibling adjacent flag if it is
	 ;; not set already.




	 ;; Check if rooms share the same sibling list -- this indicates
	 ;; that they are siblings and have the same short

	 (if
	     (and
	      (eq (emud-room-siblings source-room)
		  (emud-room-siblings dest-room))
	      (not (car (emud-room-siblings dest-room))))
	     
	     (setcar (emud-room-siblings source-room) t)
	     
	   ;; If room are not  sibling adjacent check entrances for
	   ;; possible room merges
	   (emud-map-check-sibling-entrances 
	    map source-room last-cmd dest-room))))
       
				

     ;; If we determing that no move was made. Do nothing
     ))
  ;; Check if the the current room has a coordinate.
  ;; if so update the global coordinate. Keep in mind
  ;; the global coordinate is NOT authorative.
  (when (and emud-map-curr-room (emud-room-coord emud-map-curr-room))
    (setq emud-map-coord (emud-room-coord emud-map-curr-room)))
  emud-map-curr-room)

(defun emud-map-set-origin ()
  (interactive)
  (setq emud-map-coord [0 0 0])
  (setf (emud-room-coord emud-map-curr-room) [0 0 0])
  (emud-map-add-alias "origin"))

(defun emud-map-set-coord ()
  (interactive)
  (setcar (emud-room-siblings emud-map-curr-room) 'coord)
  (unless (emud-room-coord emud-map-curr-room)
    (setf (emud-room-coord emud-map-curr-room) emud-map-coord))
  (emud-map-coord-adj emud-curr-map emud-map-curr-room)
  (emud-map-merge-coord-siblings emud-curr-map emud-map-curr-room))
  

(defun emud-map-make-uniq ()
  (interactive)
  (let ((siblings (emud-room-siblings emud-map-curr-room))
	(number   (emud-room-number emud-map-curr-room)))
    (if (car siblings)
	(message "Refusing to uniq difficult room.")
      (emud-map-merge-all-siblings emud-curr-map emud-map-curr-room)
      (setf (emud-room-siblings emud-map-curr-room) number)
      (puthash (emud-room-short emud-map-curr-room) 
	       number (emud-map-sibling-hash emud-curr-map)))))

      
(defun emud-map-follow-exit (cmd exits map short)
  "Finds room associated with last exit command -- if it exists"
  (let ((exit (cdr (assq cmd exits)))
	sibling)
    (when exit
      (cond
       ((listp exit)
	(setq sibling (gethash short (emud-map-sibling-hash map)))
	(car (intersection exit sibling)))
       (t
	exit)))))
		    
(defun emud-map-check-sibling-entrances (map room last-cmd new-room)
  "Compiles list of entances to new-room and checks siblings to create
   possible merge list"
  (let ((new-siblings (emud-room-siblings new-room))
	(siblings (emud-room-siblings room))
	main sibling entrances merge-list merge main
	number)
    (unless (or (car siblings)
		(car new-siblings)
		(equal (list (emud-room-number room)) (cdr siblings)))
      ;;Only check rooms that are not sibling adjacent and
      ;; skip if room has no siblings.
      
      (setq siblings (cdr siblings)
	    new-siblings (cdr new-siblings)) ; remove flag from siblings lists
      (while new-siblings
	;; Go through each sibling of new-room and build
	;; combined entrance list
	(setq sibling (aref (emud-map-arr map) (pop new-siblings)))
	(setq entrances 
	      (append entrances 
		      (cdr (assq last-cmd (emud-room-entrances sibling))))))
      (when (setq merge-list (intersection siblings entrances))
	;; compare entrance list to room siblings and create merge-list
	(setq number (apply 'min merge-list))
	(setq merge-list (delete number merge-list))
	(setq main (aref (emud-map-arr map) number))
	(while merge-list
	  (setq merge (pop merge-list))
	  ;; attempt to merge each room in merge-list
	  (emud-map-merge-map-rooms map main 
				    (aref (emud-map-arr map) merge)))))))

(defun emud-map-check-entrances (map room last-cmd short)
  "Checks room entrances for possible destinations of the last exit command"
  (let ((siblings (gethash short (emud-map-sibling-hash map)))
	(rev-cmd (cdr (assq last-cmd emud-map-reverse)))
	sibling)
    (and siblings rev-cmd
	 (setq sibling (cdr (assq rev-cmd (emud-room-entrances room))))
	 (car (intersection sibling siblings)))))

(defun emud-map-add-room (map room)
  "Function adds room to the map:
   -- Finds an available spot and expands the map if necessary
   -- Finds or creates the correct sibling list"
  (unless (emud-room-number room)
    (let (number
	  (hole (emud-map-hole map))
	  ;(hole nil)
	  (siblings
	   (gethash (emud-room-short room) (emud-map-sibling-hash map))))
      (if (and siblings (not (listp siblings)))
	  (aref (emud-map-arr map) siblings)
	(if hole
	    (progn
	      (setq number (pop hole))
	      (setf (emud-map-hole map) hole))
	  (setq number (emud-map-last map))
	  (incf (emud-map-last map))
	  (when (>= (emud-map-last map) (emud-map-size map))
	    (emud-map-grow map)))
	
	(aset (emud-map-arr map) number room)
	(setf (emud-room-number room) number)
	
	(if siblings
	    (nconc siblings (list number))
	  (setq siblings (list nil number))
	  (puthash (emud-room-short room) 
		   siblings (emud-map-sibling-hash map)))
	
	(setf (emud-room-siblings room) siblings)
	room))))

	       
(defun emud-map-add-exit (room cmd new-room)
  "Adds an exit from room to new-room"
  (let ((exit (emud-room-number new-room))
	(obv-exits (emud-room-obv-exits room))
	number exits )
    (unless (and obv-exits (string-match-p (regexp-quote (format "%s" cmd)) obv-exits))
      (emud-warn 
       (format "Non-obvious exit (%s) found in room: %d" 
	       cmd (emud-room-number room))))
    
    (cond
    ((not (setq exits (emud-room-exits room)))
     (emud-map-add-entrance new-room cmd room)
     (setf (emud-room-exits room) (list (cons cmd exit))))
    
    ((not (setq number (cdr (assq cmd exits))))
     (emud-map-add-entrance new-room cmd room)
     (nconc exits (list (cons cmd exit))))

    ((listp number)
     (emud-map-add-entrance new-room cmd room)
     (nconc number (list exit))
     exits)
    
    ((= number exit)
	      exits)
    (t
     (emud-warn (format "Redefining exit %s of %d from %d to %d" cmd 
		      (emud-room-number room) number exit))))))

	       
(defun emud-map-add-entrance (room cmd new-room)
  (let ((entrance (emud-room-number new-room))
	number entrances)
    (cond
    ((not (setq entrances (emud-room-entrances room)))
	  (setf (emud-room-entrances room) (list (list cmd entrance))))
    
    ((not (setq number (cdr (assq cmd entrances))))
     (nconc entrances (list (list cmd entrance))))
    (t
     (emud-warn (format "Adding multiple '%s entrances to room %d"
			cmd (emud-room-number room)))
     (nconc number (list entrance))))))


(emud-get-make-xml-handler 'exit)
(defadvice emud-xml-handler-exit (before emud-map-exit (proc face node))
  (let ((exit (car (xml-node-children node))))
    (when (and 	(stringp exit) (string-match "^\\s-+<" exit))
      (emud-map-set-room-info 'obv-exits (replace-match "<" t t exit)))))


(emud-get-make-xml-handler 'long)
;; currently not used
(defadvice emud-xml-handler-long (before emud-map-long (proc face node))
  (let ((long (car (xml-node-children node))))
    (when (string-match "^It is [^.]+\\.$" long)
      (setq long (replace-match "" t t long)))
    (if (emud-room-long emud-map-curr-room)
	(unless (string= long
			 (emud-room-long emud-map-curr-room))
	    (message "Long description mismatch"))
	(setf (emud-room-long 
	       emud-map-curr-room) long))))

(defun emud-map-merge-map-rooms (map room1 room2)
  (let ((number (emud-room-number room2))
	alias merge-list merge)
    (setq merge (cons (emud-room-number room1)
		      (emud-room-number room2)))
    (setq merge-list (list merge))
    (while merge-list
      (setq merge (pop merge-list))
      (setq room1  (aref (emud-map-arr map) (car merge))
	    room2  (aref (emud-map-arr map) (cdr merge)))
      (setq number (cdr merge))
      (when (and room1 room2)
	(if (eq room1 room2)
	    (emud-warn "Attempt to merge room with itself")
	  (emud-warn (format "Merging %s: %d %d" (emud-room-short room1)
			   (emud-room-number room1) number))
	  (when (eq room2 emud-map-curr-room)
	    (setq emud-map-curr-room room1))
	  (if (setq alias (rassq number (emud-map-alias-list map)))
	      (setcdr alias (emud-room-number room1)))
	  (setq merge-list 
		(append (emud-map-merge-rooms map room1 room2) merge-list))
	  (when (emud-room-coord room2)
	    (setf (emud-room-coord room1) (emud-room-coord room2)))
	  (emud-map-fix-exits map room1 room2)
	  (emud-map-fix-entrances map room1 room2)
	  (aset (emud-map-arr map) number nil)
	  (unless (memq number (emud-room-siblings room1))
	    (setf (emud-map-hole map) 
		  (append (emud-map-hole map) (list number)))))))))
	

				    
			
  

(defun emud-map-fix-exits (map room1 room2)
  "This function is called when merging room1 and room2 in map.
This function systematically finds all the rooms that that have exits 
leading to room1 or room2 and replaces them with corresponding exits that 
lead to room1."
  (let ((new-exit (emud-room-number room1)) 
					;room1's number the exit to change to
	(old-exit (emud-room-number room2))
					;room2's number the exit to change from
	entrance-alist ;list of entrances for room1 and room2
		       ;gives the rooms that lead to room1 or room2
	entrances exit entrance
	entrance-list entrance-num)
    (setq entrance-alist (union (emud-room-entrances room2) 
				(emud-room-entrances room1)))
    (while entrance-alist
      (setq entrance-list (pop entrance-alist))
      (setq entrances (cdr entrance-list))
      (while entrances
	(setq entrance-num (pop entrances))
	(setq entrance (aref (emud-map-arr map) entrance-num))
	(if entrance
	    (when (setq exit (rassq old-exit (emud-room-exits entrance)))
	      (setcdr exit new-exit))
	  (emud-warn (format "Bad entrance %d -> %d, deleting" 
			   entrance-num new-exit))
	  (delete entrance-num entrance-list))))))

(defun emud-map-fix-entrances (map room1 room2)
  (let ((new-entrance (emud-room-number room1))
	(old-entrance (emud-room-number room2))
	exit-alist direction exit-cons exit entrance entrances)
    (setq exit-alist (union (emud-room-exits room1) (emud-room-exits room2)))
    (while  exit-alist
      (setq exit-cons (pop exit-alist))
      (setq direction (car exit-cons)) 
      (setq exit (aref (emud-map-arr map) (cdr exit-cons)))
      (when (and exit 
	     (setq entrances (assq direction (emud-room-entrances exit)))
		 (member old-entrance entrances))
	(delete old-entrance entrances)
	(unless (member new-entrance entrances)
	  (nconc entrances (list new-entrance))
	   (when (> 2 (length entrances))
	     (emud-warn "Suspicious entrance list")))))))
    
	      
(defun emud-map-merge-rooms (map room1 room2)
  (let ( exits1 exits2 exit1 exit2 
	 entrances1 entrances2 entrance1 entrance2
	 entrances merge)
    (unless (string= (emud-room-short room1) (emud-room-short room2))
      (emud-warn "Merging rooms with different shorts"))
    (when (and (emud-room-obv-exits room2)
	       (not (string= (emud-room-obv-exits room1) 
			     (emud-room-obv-exits room2))))
      (emud-warn "Merging rooms with different obv-exits"))
    (unless (string= (emud-room-long room1) (emud-room-long room2))
      (emud-warn "Merging rooms with different long"))
    (setq exits1 (emud-room-exits room1))
    (setq exits2 (emud-room-exits room2))
    (setf (emud-room-exits room1) 
	  (union exits1 exits2 :test 'equal)) 
    (while exits1
      (setq exit1 (pop exits1))
      (setq exit2 (assq (car exit1) exits2))
      (when (and exit2 (not (equal exit1 exit2)))
	(cond 
	 ((> (cdr exit2) (cdr exit1))
	  (setf (emud-room-exits room1) 
		(delete exit2 (emud-room-exits room1)))
	  (setq merge (append merge (list (cons (cdr exit1) (cdr exit2))))))
	 (t
	  (setf (emud-room-exits room1) 
		(delete exit1 (emud-room-exits room1)))
	  (setq merge (append merge (list (cons (cdr exit2) (cdr exit1)))))))))
    (setq entrances1 (emud-room-entrances room1)) 
    (setq entrances2 (emud-room-entrances room2))
    (while entrances1 
      (setq entrance1 (pop entrances1))
      (setq entrance2 (assq (car entrance1) entrances2))
					;need to check for entrances that 
					;need to be merged
      (cond
       (entrance2
	(setq entrances2 (delete entrance2 entrances2))
	(let* ((ent-list (union (cdr entrance1) (cdr entrance2)))
	       (ent-list2 ent-list)
	       ent1 ent2 room siblings)
	  (while ent-list
	    (setq ent1 (pop ent-list))
	    (setq room (aref (emud-map-arr map) ent1))
	    (cond
	     (room
	      (setq siblings (intersection (emud-room-siblings room) ent-list))
	      (when siblings
		(setq ent2 (car siblings))
		(setq ent-list (delete ent2 ent-list))
		(cond 
		 ((> ent2 ent1)
		  (setq merge (union merge (list (cons ent1 ent2))))
		  (setq ent-list2 (delete ent2 ent-list2)))
		 (t
		  (setq merge (union merge (list (cons ent2 ent1))))
		  (setq ent-list2 (delete ent1 ent-list2))))))
	     (t
	      (emud-warn (format "Bad entrance %d -> %d, deleting (merge-rooms)" 
			       ent1 (emud-room-number room1)))
	      (setq ent-list2 (delete ent1 ent-list2)))))
	  (setq entrances (append entrances 
				  (list 
				   (cons (car entrance1) ent-list2))))))
       (t
	(setq entrances (append entrances 
				(list entrance1))))))
    
    (setq entrances (append entrances entrances2))
    (setf (emud-room-entrances room1) entrances)
    (setf (emud-room-siblings room1)
	  (delete (emud-room-number room2) (emud-room-siblings room1)))
    (setq exits1 (emud-room-exits room1))
    (while exits1
      (setq exit1 (pop exits1))
      (setq entrance1 (assq (cdr 
                              (assq (car exit1) emud-map-reverse)) entrances))
      (when entrance1
        (let (main room siblings)
          (setq room (aref (emud-map-arr map) (cdr exit1)))
          (setq siblings (intersection (emud-room-siblings room) entrance1))
          (when siblings
            (setq siblings (union (list (cdr exit1)) siblings))
            (setq main (apply 'min siblings))
            (setq siblings (delete main siblings))
            (while siblings
              (setq merge (append merge 
                                  (list (cons main (pop siblings)))))))))) 
   merge))

(defun emud-map-merge-int ()
  (interactive)
  (emud-map-merge-all-siblings emud-curr-map emud-map-curr-room))

(defun emud-map-check-coord-siblings (map source-room cmd short)
  "When moving from source-room into room with short description short,
check it's sibling list for room with appropriate coordinates"
  (let ((siblings (emud-map-get-siblings map short))
	(source-coord (emud-room-coord source-room))
	dest-coord
	check-function
	check-room)
    (when (and
	   siblings
	   (eq (car siblings) 'coord)
	   source-coord
	   (setq dest-coord (emud-map-walk-coord cmd source-coord)))
      
      (setq check-function
	    (lambda (number)
	      (equal (emud-room-coord (emud-map-get-room map number)) dest-coord)))
      (setq siblings (remove-if-not check-function (cdr siblings)))
      (unless (cdr siblings)
	(car siblings)))))
    
      
      
(defun emud-map-coord-adj (map room)
  "Add coordinates to rooms adjacent to room"
  (let* ((exits (emud-room-exits room))
	 (map-array (emud-map-arr map))
	 (coord (emud-room-coord room))
	 (flag (car (emud-room-siblings room)))
	 exit
	 delta
	 adj-room)
    (when (and (eq flag 'coord) coord)
      (while exits
	(setq exit (pop exits))
	(when (setq coord (emud-map-walk-coord (car exit)
					       (emud-room-coord room)))
	  (setq adj-room (aref map-array (cdr exit)))
	  (if (emud-room-coord adj-room)
	      (when (eq (emud-room-coord adj-room) coord)
		(emud-map-coord-adj map adj-room))
	    (setf (emud-room-coord adj-room) coord)
	    (emud-map-coord-adj map adj-room)))))))
	

  (defun emud-map-walk-coord (dir coord)
    (let (delta)
      (when (setq delta (assq dir  emud-map-directions))
	(emud-add-vec (cdr delta) coord))))
      
    
(defun emud-map-merge-coord-siblings (map room)
  (let* ((siblings (copy-seq (emud-room-siblings room)))
	 (map-array (emud-map-arr map))
	 check-sibs main test main-room test-room)
    (if (not (eq (car siblings) 'coord))
	(emud-warn "Attempting merge of non-coord room")
      (setq siblings (sort (cdr siblings) '<))
      (while siblings
	(setq main (pop siblings))
	(setq main-room (aref map-array main))
	(when (and main-room
		   (emud-room-coord main-room))
	  (setq check-sibs siblings)
	  (while check-sibs
	    (setq test (pop check-sibs))
	    (setq test-room (aref map-array test))
		  (when (and test-room 
			     (emud-room-coord test-room)
			     (equal (emud-room-coord test-room)
				    (emud-room-coord main-room)))
		    (setq siblings (delq test siblings))
		    (emud-map-merge-map-rooms map main-room test-room))))))))
		      
				 
(defun emud-map-merge-all-siblings (map room)
  (let* ((siblings (emud-room-siblings room))
	 (main (apply 'min (cdr siblings)))
	 (map-array (emud-map-arr map)))

    (if (car siblings)
	(message "Refusing to merge siblings of difficult room.")
      (setq siblings (remove main (cdr siblings)))
      (setq main (aref map-array main))
      (while siblings
	(emud-map-merge-map-rooms map main 
				  (aref map-array (pop siblings)))))))
				  
	 
	 
	 
			   
(defun emud-map-add-alias (alias)
  (interactive "sRoom alias: ")
  (setq alias (intern alias))
    (let ((number (emud-room-number emud-map-curr-room))
	  (alias-list (emud-map-alias-list emud-curr-map))) 
      ;(setf (emud-room-alias emud-map-curr-room) alias)
      (setf (emud-map-alias-list emud-curr-map)
	    (cons (cons alias number) alias-list))
      (message (format "Alias \"%s\" added for room \"%s\"" alias
		       (emud-room-short emud-map-curr-room)))))

(defun emud-map-back ()
  (interactive)
  (emud-map-path emud-map-last-room))

(defun emud-map-goto (to)
    (interactive "sTo room: ")
    (setq to (intern to))
    (let (number)
      (if (setq number (emud-map-lookup-alias emud-curr-map to))
	  (emud-map-path number)
	(error "Room alias \"%s\" not found." to))))

(defun emud-map-lookup-alias (map alias)
  (cdr (assq alias (emud-map-alias-list map))))
     
(defun emud-map-path (to)
  (interactive "nTo room: ")
  (let ((path (emud-map-walk emud-curr-map nil to))
	(proc (get-buffer-process (current-buffer)))
	rev-path step)
    (cond
     (path
      (setq emud-map-last-room (emud-room-number emud-map-curr-room))
      (emud-send-list proc (mapcar (lambda (sym) (format "%s" sym)) path)))
    ((y-or-n-p "Path not found.  Try reverse? ")
     (setq path (reverse 
		 (emud-map-walk emud-curr-map to 
				(emud-room-number emud-map-curr-room))))
     (while path
       (if (setq step (cdr (assq (pop path) emud-map-reverse)))
	   (setq rev-path (append rev-path (list step)))
	 (setq path nil
	       rev-path nil)))
     (if rev-path
	 (progn
	   (setq emud-map-last-room (emud-room-number emud-map-curr-room))
	   (while rev-path
	     (setq step (pop rev-path))
	     (emud-simple-send proc (format "%s" step))))
       (message "Path not found."))))))

;(defun emud-map-path-reverse (path)
;  (let (rev-path step)
;    (setq path (reverse path))
    
    
  
(defun emud-map-walk (map from to &optional len old-path)
  ;(print old-path)
  (let (room vec map-arr 
	     exit exits room-stack been-there path too-far)
    (setq room  (or from (emud-room-number emud-map-curr-room))
	  map-arr    (emud-map-arr map)
	  been-there nil
	  exits      (emud-room-exits (aref map-arr room)))
    (while (and (not (= room to)) exits)
      (setq exit (pop exits))
      (unless (listp (cdr exit))
	(when (or (not len)
		  (< (length path) (1- len)))
	  (push room been-there))
	(when exits
	  (push (vector room path exits been-there) room-stack))
	(setq path (append path (list (car exit)))
	      room (cdr exit)
	      exits (emud-room-exits (aref map-arr room)))
	(when (or (and len		;or have we gone too far?
		       (< len (length path)))
		  (member room been-there) ;are we going in circles?
		  (not exits))		;or out of exits?
	  
	  (if room-stack		       ;are there any exits we skiped?
	      (progn
					;(print path)
		(setq vec        (pop room-stack)
		      room       (aref vec 0)
		      path       (aref vec 1)
		      exits      (aref vec 2)
		      been-there (aref vec 3)))

	    (setq exits nil		;no more rooms to check
		  path nil)))))		;bail out
    (if path
	(emud-map-walk map from to (1- (length path)) path)
      old-path)))
		 
(defun emud-map-grow (map)
  (setf (emud-map-arr map) 
	(vconcat (emud-map-arr map) 
		 (make-vector (emud-map-size map) nil)))
  (setf (emud-map-size map) (length (emud-map-arr map))))
	       
(defun emud-add-vec (v1 v2)
  (let* ((len1 (length v1))  (len2 (length v2))
	 (len (max len1 len2))
	 (vec (make-vector len nil))
	 (count 0))
    (while (< count len)
      (aset vec count (+ (if (< count len1) (aref v1 count) 0)
			 (if (< count len2) (aref v2 count) 0)))
      (setq count (1+ count)))
    vec))


(defun emud-map-save (map file)
  (let ((array (emud-map-arr map))
	(count 0)
	(hole (emud-map-hole map))
	(stop (emud-map-last map))
	sib-str sib-var room  siblings
	let-point main-point)
 
    (with-current-buffer (get-buffer-create file)
      (erase-buffer)
      (insert ";; Emud save file\n") 
      (insert (concat ";; Created on " (current-time-string) "\n")) 
      (insert "\n(defun emud-map-new ()\n")
      (insert "  (let (")
      (setq let-point (point))
      (insert (format "(array (make-vector %d nil))\n" (emud-map-size map)))
      (insert 
       (format 
	"         (sibling-hash (make-hash-table :size %d :test 'equal))\n" 
	(emud-map-size map)))
      (insert "         room)\n\n")
      
      (while (< count stop)
	(setq room (aref array count))
	(cond
	 (room
	  (setq siblings (emud-room-siblings room))
	  (setq sib-var (format "sib%03d" (cadr siblings)))
	  (when (= (cadr siblings) count)
	    (setq sib-str (format "(%s '%s)\n	" sib-var siblings))
	    (setq main-point (+ (length sib-str) (point)))
	    (goto-char let-point)
	    (insert sib-str)
	    (goto-char main-point)
	    (insert (format "    (puthash %S %s sibling-hash)\n" 
			    (emud-room-short room) sib-var)))
	  (insert 
	   (concat (format "    (setq room (make-emud-room :number %d\n"
			   count)
		   (format "                               :short %S\n"
			   (emud-room-short room))
		   (format "                               :type '%S\n"
			   (emud-room-type room))
		   ;(format "                               :alias '%S\n"
		   ;	   (emud-room-alias room))
		   (format "                               :obv-exits %S\n"
			   (emud-room-obv-exits room))
		   (format "                               :long %S\n"
			   (emud-room-long room))
			   
		   (format "                               :exits '%S\n"
			   (emud-room-exits room))
		   (format "                               :entrances '%S\n"
			   (emud-room-entrances room))		 
		   (format "                               :coord%s\n"
			   (emud-room-coord room))		 
		   (format "                               :extra '%S\n"
			   (emud-room-extra room))		 
		   (format "                               :siblings %s))\n"
			   sib-var)))
	  (insert (format "    (aset array %d room)\n" count)))
	 ((not (memq count hole))
	  (message "Null room found: %d" count)
	  (setq hole (cons count hole))))
	(setq hole (sort hole '<))
	(setf (emud-map-hole map) hole)
	(setq count (1+ count)))
      (insert (concat 
	       (format "    (make-emud-map :size %d\n" (emud-map-size map))
	       (format "                   :last %d\n" (emud-map-last map))
	       (format "                   :alias-list '%S\n" 
		       (emud-map-alias-list map))
	       (format "                   :hole '%s\n" hole)
	       "                   :sibling-hash sibling-hash\n"
	       "                   :arr array)))\n")))))

 
(defun emud-map-load (file)
  (interactive "fLoad Map File: ")
  (load file)
  (setq emud-curr-map (emud-map-new)))

(defun emud-map-check (map)
  (let ((count 0)
	(array (emud-map-arr map))
	(stop (emud-map-last map))
	room)
    
    (while (< count stop)
      (when (and (setq room (aref array count))
		 (not (emud-room-entrances room)))
	(emud-map-clean-null-entrance map room))
      (setq count (1+ count)))))

(defun emud-map-clean-null-entrance (map room)
  (when (and room
	       (not (emud-room-entrances room)))
    (let ((exits (emud-room-exits room))
	  (number (emud-room-number room))
	  (array (emud-map-arr map))
	  exit)
      (message "Room %d is unreachable. Deleting." number)
      (emud-map-delete-room map room)
      (while (setq exit (pop exits))
	(emud-map-clean-null-entrance map (aref array (cdr exit)))))))
						
    
    

(defun emud-map-delete-room (map room)
  (let ((number (emud-room-number room))
	(hole (emud-map-hole map))
	(array  (emud-map-arr map))
	(exits  (emud-room-exits room))
	entrances 
	other-room
	other-number
	command
	exit entrance)

    (delq number (emud-room-siblings room))
    (aset array number nil)
    
    (setf (emud-map-hole map) (append hole (list number)))
    
    (while (setq exit (pop exits))
      (when (setq other-room (aref array (cdr exit)))
	(setq entrances (emud-room-entrances other-room))
	(when (setq entrance (assq (car exit) entrances))
	  (delq  number entrance)
	  (unless (cdr entrance)
	    (setf (emud-room-entrances other-room) (delq entrance entrances))))))
    
    (setq entrances (emud-room-entrances room))
    (while (setq entrance (pop entrances))
      (setq command (pop entrance))
      (while (setq other-number (pop entrance))
	(when (setq other-room (aref array other-number))
	  (setq exits (emud-room-exits other-room))
	  (when (and (setq exit (assq command exits))
		     (= (cdr exit) number))
	    (setf (emud-room-exits other-room) (delq exit exits))))))))

(defun emud-map-defrag (map)
  (let ((count 0)
	(array (emud-map-arr map))
	(stop (emud-map-last map))
	(size (emud-map-size map)) 
	array-list
	number
	command
	siblings
	hole room exits exit alias
	other-room entrances entrance other-number)
    
    (while (< count stop)
      (setq room (aref array count))
      (if room
   	  (when hole
   	    (setq number (pop hole)
		  exits (emud-room-exits room))
 	    (while (setq exit (pop exits))
	      (setq other-room (aref array (cdr exit)))
	      (setq entrances (emud-room-entrances other-room))
	      (when (setq entrance (assq (car exit) entrances))
		(delq count entrance)
		(nconc entrance (list number))))
	    (setq entrances (emud-room-entrances room))
 	    (while (setq entrance (pop entrances))
	      (setq command (pop entrance))
	      (while (setq other-number (pop entrance))
		(setq other-room (aref array other-number))
		(setq exits (emud-room-exits other-room))
		(when (and (setq exit (assq command exits))
			   (= (cdr exit) count)) 
		  (setcdr exit number))))
	    (setq siblings (emud-room-siblings room))
	    (delq count siblings)
	    (nconc siblings (list number))
	    (setf (emud-room-number room) number)
	    (aset array count nil)
	    (aset array number room)
	    (when (setq alias (rassq count (emud-map-alias-list map)))
	      (setcdr alias number))
	    (setq hole (append hole (list count))))
	(setq hole (append hole (list count))))
      (setq count (1+ count)))
    (setq hole (sort hole '>))
    (while (and  hole 
		 (= (1- stop) (car hole)))
		 (setq stop (1- stop)
		       hole (cdr hole)))
      (setf (emud-map-hole map) (reverse hole))
      (setf (emud-map-last map) stop)
      (while (< stop (/ size 2))
	(setq size (/ size 2)))
      (when (< size (emud-map-size map))
	(setq array-list (append array nil))
	(setcdr (nthcdr (1- size) array-list) nil)
	(setq array (vconcat array-list))
	(setf (emud-map-size map) size)
	(setf (emud-map-arr map) array))))
	

(defun emud-map-check2 (map)
  (let ((count 0)
	(array (emud-map-arr map))
	(stop (emud-map-last map))
	(hole (emud-map-hole map))
	entrances exits entrance exit
	short siblings sibling
	adjacent check-sib exit-adj
	command other-room other-number
	room)
    (set-buffer (get-buffer-create "*Output*"))
    (erase-buffer)
    (while (< count stop)
      (setq check-sib nil)
      (setq room (aref array count))
      (cond
       (room
	(setq siblings (emud-room-siblings room))
	(unless (memq count siblings)
	  (insert (format "Room %d is not in its sibling list.\n" count)))
	(when (and (cadr siblings) (= (cadr siblings) count))
	  (setq adjacent (pop siblings)
		check-sib nil)
	  (setq siblings (cdr siblings)
		short (emud-room-short room))
	  (while (setq sibling (pop siblings))
	    (if (setq other-room (aref array sibling))
		(unless (string= short (emud-room-short other-room))
		  (insert 
		   (format 
		    (concat "Room %d, \"%s\", found in"
			    " sibling list for \"%s\".\n")
		    sibling (emud-room-short other-room) short)))
	      (insert
	       (format "Nil room %d in sibling list for %d.\n"
		       sibling count)))))

	(when (memq count hole)
	  (insert (format "Non-nil room %d in hole.\n" count)))
	(setq entrances (emud-room-entrances room))
	(if entrances
	    (while (setq entrance (pop entrances))
	      (setq command (pop entrance))
	      (when (memq count entrance)
		(insert (format "Room %d has %s entrance from self.\n"
				count command)))
	      (unless (= (length entrance) 1)
		(insert (format (concat "Room %d has suspicious number" 
					" of %s entrances: %d.\n")
				count command (length entrance))))
	      (while (setq other-number (pop entrance))
		(if (setq other-room (aref array other-number))
		    (progn
		      (setq exits (emud-room-exits other-room))
		      (unless (and (setq exit (assq command exits))
				   (= (cdr exit) count))
			(insert (format
				 (concat "Room %d has entrance going %s from " 
					 "%d but %d has no corresponing exit.\n")
				 count command other-number other-number))))
		  (insert (format (concat "Room %d has %s entrance from" 
					  " non-existant room %d.\n")
				  count command other-number)))))
	  (insert (format "Room %d has no entrances.\n" count)))
	(setq exits (emud-room-exits room))
	(if exits
	    (progn
	      (setq siblings (emud-room-siblings room))
	      (setq exit-adj nil)
	      (while (setq exit (pop exits))
		(setq command (car exit) 
		      other-number (cdr exit))
		(when check-sib
		  (setq exit-adj (or exit-adj (memq other-number siblings))))
		(when (= other-number count)
		  (insert (format "Room %d has %s exit to self.\n"
				  count command)))
		(if (setq other-room (aref array other-number))
		    (progn 
		      (setq entrances (emud-room-entrances other-room))
		      (unless (and (setq entrance (assq command entrances))
				   (memq count entrance))
			(insert 
			 (format
			  (concat
			   "Room %d has %s exit to %d but %d"
			   " has no corresponding entrance.\n")
			  count command other-number other-number))))
		  (insert
		   (format
		    "Room %d has %s exit to non-existant room %d.\n"
		    count command other-number))))
	      (when (and check-sib
			 (not (eq exit-adj adjacent)))
		(insert
		 (format
		  (if adjacent
		      (concat "Siblings of %d marked as adjacent"
			      " but no adjacent rooms found.\n")
		    (concat "Siblings of %d marked as non-adjacent"
			    " but adjacent rooms found.\n"))
		    count))))
	  (insert 
	   (format "Room %d has no exits\n" count)))
	(unless (= count (emud-room-number room))
	  (insert (format "Room %d has wrong number: %d.\n"
			  count (emud-room-number room)))))
       
       ((not (memq count hole))
	(insert (format "Nil room %d not in hole.\n" count))))
      (setq count (1+ count)))))

(defun emud-map-merge (map1 map2)
  (let ((offset (emud-map-size map1))
	(array (emud-map-arr map2))
	(stop (emud-map-last map2))
	(sib-hash1 (emud-map-sibling-hash map1))
	room
	exits exit entrances entrance
	count siblings1 siblings2)
    (defun add-offset (number)
      (if (numberp number)
	  (+ offset number)
	number))
    (setq count 0)
    (while (< count stop)
      (when (setq room (aref array count))
	(setf (emud-room-number room) (+ count offset))
	(setq siblings1 (gethash (emud-room-short room) sib-hash1)
	      siblings2 (emud-room-siblings room))
	(when (= count (cadr siblings2))
	  (if siblings1
	      (progn
		(setcar siblings1 (or (car siblings1) (car siblings2)))
		(nconc siblings1 (mapcar 'add-offset (cdr siblings2))))
	    (setq siblings1 (mapcar 'add-offset siblings2))
	    (puthash (emud-room-short room) 
		     siblings1 sib-hash1)))
	(setf (emud-room-siblings room) siblings1)
	
	(setq exits (emud-room-exits room))
	(while (setq exit (pop exits))
	  (setcdr exit (+ (cdr exit) offset)))
	(setq entrances (emud-room-entrances room))
	(while (setq entrance (pop entrances))
	  (setcdr entrance (mapcar 'add-offset (cdr entrance)))))
      (setq count (1+ count)))
    (setq array
	  (vconcat (emud-map-arr map1) array))
    (setf (emud-map-arr map1) array)
    (setf (emud-map-last map1) (+ offset (emud-map-last map2)))
    (setf (emud-map-size map1) (length (emud-map-arr map1)))
    
    (setq count 0
	  stop (emud-map-last map2))
    (while (< count stop)
      (when (and (setq room (aref array count))
		 (not (car (emud-room-siblings room))))
	(setq exits (emud-room-exits room))
	(while (setq exit (pop exits))
	  (emud-map-check-sibling-entrances map1 room (car exit)
					    (aref array (cdr exit)))))
      (setq count (1+ count)))
    (emud-map-defrag map1)
    map1))
    
(defun emud-warn (string)
  (let ((xml-point (with-current-buffer emud-xml-buffer
		     (point))))
    (with-current-buffer (get-buffer-create "*Emud-warn*")
      (goto-char (point-max))
      (insert (format "%d : %s\n" 
		      xml-point
		      string)))))
