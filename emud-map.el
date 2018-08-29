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
					; size of the map array
  (last 0)
					; The next available sequential
					; slot in the array
  (hole nil)
					; a list of open non-sequential slots
					; freed buy deleting rooms
  (sibling-hash (make-hash-table :test 'equal))
					; hash table to map shorts to silbling
					; lists
  alias-list
					; list of room aliases
  (arr (make-vector size nil)))
					; the main array of the map where all
					; rooms are stored
(defstruct emud-room
  number
  alias
  short
  type
  obv-exits
  long
  coord
  exits
  entrances 
  siblings
  extra)

(defmacro emud-room-cmd-entrance (room cmd)
  `(cdr (assq ,cmd (emud-room-entrances ,room))))

(defmacro emud-room-cmd-exit (room cmd)
  `(cdr (assq ,cmd (emud-room-exits ,room))))

(defmacro emud-map-get-room (map number)
  `(aref (emud-map-arr ,map) ,number))

(defmacro emud-map-get-siblings (map short)
  `(gethash ,short (emud-map-sibling-hash ,map)))

(defun emud-map-make-merge-con ( a b)
  (if (< a b)
       (cons a b)
     (cons b a)))
    
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
	  emud-map-room-info nil
	  emud-map-last-cmd nil
	  emud-map-ship-exit nil
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
    (emud-warn (format "ADD-INFO: Setting %s to %s" tag data))
    (cond
     ((setq assoc (assq tag emud-map-room-info))
      (emud-warn (format "Rewriting %s" tag))
      (setcdr assoc data))
     (t
      (when (and (eq tag 'cmd)
		 (setq delta (cdr (assq data emud-map-directions))))
	(setq emud-map-coord (emud-add-vec emud-map-coord delta)))
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

    ;;(when (and (not last-cmd) emud-xml-command-queue)
    ;;  (setq last-cmd (intern (pop emud-xml-command-queue))))
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
		    


(defun emud-map-check-sibling-entrances (map source-room last-cmd dest-room)
  "Compiles list of entances to new-room and checks siblings to create
   possible merge list"
  (let ((dest-siblings (emud-room-siblings dest-room))
					; siblings of the destination room
	(source-siblings (emud-room-siblings source-room))
					; siblings of the source room
	main-room
					; The principle room in the merge
					; process this room will be kept and
					; not merged
	main-number
					; The number for the main-room
	sibling-room
					; working variable containing rooms
					; from the destination siblings being
					; considered from merge
	sibling-entrances
					; list of entrances leading to siblings
					; via last-cmd
	merge-list
					; List of room numbers to attempt to
					; merge
	merge-number
					; Room number of room we are attempting
					; to merge with main 
	)
    (unless (or (car dest-siblings)
		(car source-siblings)
		(equal (list (emud-room-number source-room))
		       (cdr source-siblings)))
      ;;Only check rooms that are not sibling adjacent and
      ;; skip if source room has no siblings.
      
      (setq source-siblings (cdr source-siblings)
	    dest-siblings (cdr dest-siblings)) ; remove flag from siblings lists
      (while dest-siblings
	;; Go through each sibling of destination-room and collect all
	;; entrances using last-cmd
	(setq sibling-room (emud-map-get-room map (pop dest-siblings)))
	(setq sibling-entrances 
	      (append sibling-entrances 
		      (emud-room-cmd-entrance sibling-room last-cmd))))
      (when (setq merge-list (intersection source-siblings sibling-entrances))
	;; Check the list of entrances against all siblings of the source room
	;; and attempt to merge those that match.
	;; This means that given source -cmd-> dest, there is a sibling such
	;; that sibling -cmd-> dest and so source and sibling are possibly
	;; the same room  
	(setq main-number (apply 'min merge-list))
	;; Find the smallest number in the merge list and make this the
	;; number for the primary room

	(setq merge-list (delete main-number merge-list))
	;; delete the main room from the merge list to avoid attemping to
	;; merge main with itself
	(setq main-room (emud-map-get-room map main-number))
	(while (setq merge-number (pop merge-list))
	  ;; Go through the merge list and attempt to merge it with 
	  ;; attempt to merge each room with main
	  (emud-map-merge-map-rooms map main-room 
				    (emud-map-get-room map merge-number))))))
    dest-room)

(defun emud-map-check-entrances (map room last-cmd short)
  "Checks room entrances for possible destinations of the last exit command"
  (let ((siblings (gethash short (emud-map-sibling-hash map)))
	(rev-cmd (cdr (assq last-cmd emud-map-reverse)))
	(source-siblings (emud-room-siblings room))
	sibling
	dest-numbers)
    (and siblings rev-cmd (not (eq (car source-siblings) 'maze))
	 (setq sibling (cdr (assq rev-cmd (emud-room-entrances room))))
	 (setq dest-numbers (intersection sibling siblings))
	 (unless (cdr dest-numbers)
	   (car dest-numbers)))))

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
	  (emud-map-get-room map siblings)
	(if hole
	    (progn
	      (setq number (pop hole))
	      (setf (emud-map-hole map) hole))
	  (setq number (emud-map-last map))
	  (incf (emud-map-last map))
	  (when (>= (emud-map-last map) (emud-map-size map))
	    (emud-map-grow map)))
	
	(setf (emud-map-get-room map number) room)
	(setf (emud-room-number room) number)
	
	(if siblings
	    (nconc siblings (list number))
	  (setq siblings (if (string-match "maze" (emud-room-short room))
			     (list 'maze number)
			   (list nil number)))
			   
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



(defun emud-map-merge-map-rooms (map main-room target-room)
  (let ((target-number (emud-room-number target-room))
					; Room number for target room
	alias
	
	merge-list
					; List of pairs to be merged of the form
					; (main-room . target-room)
	merge-cons
					; A single cons from the merge list
	)
    (setq merge-cons (cons (emud-room-number main-room)
		      target-number))
    (setq merge-list (list merge-cons))
    ;; create intial merg list consisting of a single cons
    ;; (main-room . target-room)
    
    (while (setq merge-cons (pop merge-list))
      ;; Go though each cons in the merge list and attempt to merge the pair
      (setq main-room    (emud-map-get-room map (car merge-cons))
	    target-room  (emud-map-get-room map (cdr merge-cons)))
      ;; Extract the main-room and target-room from the cons pair
      (setq target-number (cdr merge-cons))
      (when (and main-room target-room)
	(if (eq main-room target-room)
	    (emud-warn "Attempt to merge room with itself")
	  (emud-warn (format "Merging %s: %d %d" (emud-room-short main-room)
			   (emud-room-number main-room) target-number))
	  (when (eq target-room emud-map-curr-room)
	    (setq emud-map-curr-room main-room))
	  ;; When the global current room is the target room move the
	  ;; current room to the main room

	  (if (setq alias (rassq target-number (emud-map-alias-list map)))
	      (setcdr alias (emud-room-number main-room)))
	  ;; If we have an alias that points to the target room, move it to the
	  ;; main room

	  (setq merge-list 
		(append (emud-map-merge-rooms map main-room target-room) merge-list))
	  (emud-warn (format "merge-list: <%S>" merge-list))
	 ;; perform the merge and build a list of necessary merges to perform

	  (when (emud-room-coord target-room)
	    (setf (emud-room-coord main-room) (emud-room-coord target-room)))
	  ;; Move the coordinate of the target room to the main room
	  ;; This likely needs more sanity checking.
	  
	  (emud-map-fix-exits map main-room target-room)
	  ;; fix the exits of rooms leading to target-room

	  (emud-map-fix-entrances map main-room target-room)
	  ;; fix the entrances of rooms entered from target-room

	  (setf (emud-map-get-room map target-number) nil)
	  ;; remove the target room  from the map.

	  (unless (memq target-number (emud-room-siblings main-room))
	    ;; Reclaim target rooms spot in the map. Unless it is still a
	    ;; sibling of the main room -- this last part should not happen.
	    ;; This test may be obsolete.

	    (setf (emud-map-hole map) 
		  (append (emud-map-hole map) (list target-number)))))))))
	

				    
			
  

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
    
	      
(defun emud-map-merge-rooms  (map
				 main-room
					; All combined data will be put in
					; this room
				 target-room
					; The data from this room will be
					; copied into main-room and this room
					; will be deleted.
				 )
  (let ( main-exits
					; exit list from the main room
	 target-exits
					; exit list from the target room
	 main-exit
					; a single exit from main room exit
					; list
	 target-exit
					; a single exit from target room exit
					; list
	 main-entrances
					; entrance list from the main room
	 target-entrances
					; entrance list from the target room
	 main-entrance
	 				; a single entrance from main room
					; entrance list
	 target-entrance
	 				; a single entrance from main room
					; entrance list
	 entrances
	 merge-list
					; List of rooms to be merged computed
					; from this merge event

	 )
    ;; Begin by checking room properties of the rooms to be merged and warn
    ;; about suspicious differences. Rooms with different shorts should NOT be
    ;; merged.
    (unless (string= (emud-room-short main-room) (emud-room-short target-room))
      (emud-warn "Merging rooms with different shorts"))
    (when (and (emud-room-obv-exits target-room)
	       (not (string= (emud-room-obv-exits main-room) 
			     (emud-room-obv-exits target-room))))
      (emud-warn "Merging rooms with different obv-exits"))
    (unless (string= (emud-room-long main-room) (emud-room-long target-room))
      (emud-warn "Merging rooms with different long"))

    (setq merge-list
	  (union (emud-map-merge-exits main-room target-room)
		  (emud-map-merge-entrances map main-room target-room)))
		  
    (setf (emud-room-siblings main-room)
	  (delete (emud-room-number target-room)
		  (emud-room-siblings main-room)))
    ;; Delete the target-room number from the main-room's sibling list


    (setq merge-list
	  (union merge-list
		  (emud-map-merge-by-direction map main-room)))
    merge-list))


(defun emud-map-merge-by-direction (map main-room)
  ;; Check the exits of main-room for corresponding entrances in the sibling
  ;; list. If such siblings exist add then to the merge list. For example
  ;; main has an exit (e . 5) and a cooresponding entrance list (w 5 10)
  ;; we check room 5 and see the sibling list (t 2 3 5 7 10)
  ;; we would merge rooms 5 and 10.
  
  (let ((main-entrances (emud-room-entrances main-room))
					; entrances to the main room
	main-exits 
					; exits from the main room
	main-exit
					; a single exit from the main room
					; worker variable for loops
	main-entrance
					; a single entrance list to the main
					; room used in loops
	dest-room
					; destination room corresponding to
					; main-exit
	merge-candidates
					; list of siblings that should be
					; merged.
	dest-flag
					; flag from destination siblings
	merge-list
					; list of rooms to be merged
	main-number
					; main room in the returned merge list
	)
    (unless (eq (car (emud-room-siblings main-room)) 'maze)
      ;; Don't run this test on mazes
      (setq main-exits (emud-room-exits main-room))
      (while (setq main-exit (pop main-exits))
	(setq main-entrance (assq (cdr
				   (assq (car main-exit) emud-map-reverse))
				  main-entrances))
	;; For each exit find a corresponding entrance from the opposite
	;; direction e.g. for (s . 5) find (n 5)
	
	(when main-entrance
	  (setq dest-room (emud-map-get-room map (cdr main-exit)))
	  ;; find the destination room corresponding to the main-exit
	  (setq dest-flag (car (emud-room-siblings dest-room)))
	  (setq merge-candidates
		(intersection (emud-room-siblings dest-room) main-entrance))
	  ;; compute the intersection of the entrance and siblings e.g if the
	  ;; entrance is (n 5 10) and the siblings are (t 3 5 7 10).
	  ;; The intersection is (5 10) and these are merge candidates.
	  (when (and merge-candidates (not (eq dest-flag 'maze)))
	    ;; when the intersection is non-empty and dest-room is not a maze
	    ;; proceed with the merge
	    
	    (setq merge-candidates (union (list (cdr main-exit))
					  merge-candidates))
	    ;; Add the main exit to the list of merge candidates.
	    (setq main-number (apply 'min merge-candidates))
	    ;; pull the smallest number from the merge candidates and us it as
	    ;; the main room 
	    (setq merge-candidates (delete main-number merge-candidates))
	    (setq merge-list (append merge-list
				     (mapcar (lambda (x) (cons main-number x))
					     merge-candidates))))))
      ;; build a merge list merging main-number with each remaining
      ;; merge-candidate
      merge-list)))

(defun emud-map-merge-exits (main-room target-room)
  (let ( main-exits
					; exit list from the main room
	 target-exits
					; exit list from the target room
	 main-exit
					; a single exit from main room exit
					; list
	 target-exit
					; a single exit from target room exit
					; list
	 merge-list)
					; List of rooms to be merged computed
					; from this merge event
	 
    (setq main-exits (emud-room-exits main-room))
    (setq target-exits (emud-room-exits target-room))
    ;; Collect the exits for the main and target rooms.
    (setf (emud-room-exits main-room) 
	  (union main-exits target-exits :test 'equal))
    ;; Combine the exits and set the the combined exits as the new exit
    ;; list for main room
    
    (while (setq main-exit (pop main-exits))
      ;; Foreach exit from the original main rooms exit list, compare it to
      ;; any corresponding but non-equal in the target room 

      (setq target-exit (assq (car main-exit) target-exits))
      (when (and target-exit (not (equal main-exit target-exit)))
	;; When a cooresponding exit is found choose the smaller numbered exit
	;; to keep and add the destination rooms of the exit to the merge list

	(cond 
	 ((> (cdr target-exit) (cdr main-exit))
	  (setf (emud-room-exits main-room) 
		(delete target-exit (emud-room-exits main-room)))
	  (setq merge-list
		(append merge-list
			(list (cons (cdr main-exit) (cdr target-exit))))))
	 ;; If the main exit has a smaller number remove the target exit from
	 ;; the main exit list and add the destination rooms to the merge list
	 ;; in accending order

	 (t
	  (setf (emud-room-exits main-room) 
		(delete main-exit (emud-room-exits main-room)))
	  (setq merge-list
		(append merge-list
			(list (cons (cdr target-exit) (cdr main-exit)))))))
	;; If the target exit has a smaller number remove the main exit from
	;; the main exit list and add the destination rooms to the merge list
	;; in accending order
	))
    merge-list))

(defun emud-map-merge-entrances (map main-room target-room)
  (let (main-entrances
					; entrance list from the main room
	target-entrances
					; entrance list from the target room
	main-entrance
	 				; a single entrance from main room
					; entrance list
	target-entrance
	 				; a single entrance from main room
					; entrance list
	source-list
					; list of source room numbers
					; corresponding to entrances for 
					; a given command
	source-number
					; Room number for a source room (taken
					; from the source-list)
	source-room
					; map room corresponding to
					; source-number. A source room for
					; the main or target rooms
	source-siblings
					; List of siblings of source room that
					; correspond to appropriate entrances
					; for the main or target rooms
	sibling-number
					; number for a single sibling of the
					; source room
	main-list
					; Computed list of entrances numbers
					; for given command.
					; (command . main-entnum-list)
					; will be added as an entrance for
					; main-room
	merged-entrances
					; computed entrance list for the
					; merged room. The ultimate entrance
					; list for main room
	entrance-list
					; Combined list of entrance numbers
					; for a given command
	merge-list
					; List of rooms to be merged computed
					; from this merge event
	)
    (setq main-entrances (emud-room-entrances main-room)) 
    (setq target-entrances (emud-room-entrances target-room))
    ;; Collect the entrances from main and target rooms.
    (while (setq main-entrance (pop main-entrances))
      (setq target-entrance (assq (car main-entrance) target-entrances))
      ;; For each entrance in the entrance list of the main room
      ;; Find the corresponding entrance in the target room entrance list
      
      (cond
       (target-entrance
	(setq target-entrances (delete target-entrance target-entrances))
	;; When we have a target entrance delete it from the list of target
	;; entrances. So that we can recombine the target-entrances with the
	;; main entrances later and not worry about overlap
	
	(setq source-list (union (cdr main-entrance)
				 (cdr target-entrance)))
	;; Build a combined source entrance list from main entrance and
	;; corresponding target entrance
	
	(setq main-list source-list)
	;; copy the combined source list to the main list as the beginning
	;; of the computed entrance for main 
	
	(while (setq source-number (pop source-list))
	  ;; For each number in the source list, check the source room's
	  ;; siblings for corresponding entries and possible merges for
	  ;; the source room
	  ;; For example if target has an entrance (e 5) and main has an
	  ;; entrance (e 7). We check room 5 and find a sibling list
	  ;; (nil 5 7) this indicates that rooms 5 and 7 should merge. 
	  
	  (setq source-room
		(emud-map-get-room map source-number))
	  ;; find the source room corresponding to the source-number
	  
	  (cond
	   (source-room
	    ;; When we have a source room check its siblings for merges
	    
	    (setq source-siblings
		  (intersection (emud-room-siblings source-room)
				source-list))
	    ;; Check the source siblings for siblings corresponding to the
	    ;; list of source rooms and add corresponding rooms to the merge
	    ;; list
	    ;; NOTE: source-number will not be in this list since it was
	    ;; popped out before the intersection.
	    
	    (when source-siblings
	      ;; When we have a corresponding sibling add it to the merge
	      ;; list (Can we find multiple -- should this be while?)
	      ;; Likely this should build a list of merge rooms and add them
	      ;; to the list in pairs
	      (setq sibling-number (car source-siblings))
	      (setq source-list
		    (delete sibling-number source-list))
	      ;; Remove the sibling number from the source list so we do not
	      ;; check it again

	      (cond 
	       ((> sibling-number source-number)
		(setq merge-list
		      (union merge-list
			     (list (cons source-number sibling-number))))
		(setq main-list
		      (delete sibling-number main-list)))
	       (t
		(setq merge-list
		      (union merge-list
			     (list (cons sibling-number source-number))))
		(setq main-list
		      (delete source-number main-list))))))
	   ;; Add the source-number and sibling number to the merge list in
	   ;; ascending order. Delete the larger element from the main list

	   (t ; not source-room
	    (emud-warn (format "Bad entrance %d -> %d, deleting (merge-rooms)" 
			       source-number (emud-room-number main-room)))
	    (setq main-list (delete source-number main-list)))))
	;; We found an entrance that does not correspond to a room. Delete
	;; the entrance from the main list and complain.
	;; This should not happen.
	  
	(setq merged-entrances (append merged-entrances 
					 (list 
					  (cons (car main-entrance)
					       merged-entrances)))))
       ;; build an entrance containg (cmd . main-list) and add it to the 
       ;; merged entrance list
       
       (t ; no entrance in the target room match the command
	(setq merged-entrances (append merged-entrances 
				       (list main-entrance))))))
    ;; There was no target entrance that corresponded to this entrance in the
    ;; main list so add the main-entrace to the merged entrances
    
    (setq merged-entrances (append merged-entrances
				   target-entrances))
    ;; Add any remaiing target entrances to the merged entrance list
    
    (setf (emud-room-entrances main-room) merged-entrances)
    
    ;; Add the merged entrance list as the entrance list of the main-room 
    merge-list
    ;; return the list of rooms to be merged
    ))




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
