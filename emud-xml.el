;; emud-xml.el
;; $Revision: 1.4 $
;; XML-components of emud

;; Built around the xml.el that ships with emacs 21.4 May work with others
;; includes work arounds for several bugs in xml.el

(require 'xml)

(defconst emud-xml-string-alist 
  (list '("lt" ."<") '("gt" . ">") '("amp" . "&") '("nbsp" . " ")
	'("RET" . "\n") '("TAB" . "\t")))

(copy-face 'default 'emud-face-attack)
(set-face-attribute 'emud-face-attack nil
                    :weight 'bold
                    :foreground "green")
(copy-face 'default 'emud-face-attacked)
(set-face-attribute 'emud-face-attacked nil
                    :weight 'bold
                    :foreground "green")
(copy-face 'default 'emud-face-exit)
(set-face-attribute 'emud-face-exit nil
                    :weight 'bold
                    :foreground "red")
(copy-face 'default 'emud-face-hpinfo)
(set-face-attribute 'emud-face-hpinfo nil
                    :weight 'bold
                    :foreground "magenta")
(copy-face 'default 'emud-face-long)
(set-face-attribute 'emud-face-long nil
                    :background "cyan"
                    :weight 'bold
                    :foreground "blue")
(copy-face 'default 'emud-face-misc_in_long)
(set-face-attribute 'emud-face-misc_in_long nil
                    :foreground "red")
(copy-face 'default 'emud-face-player)
(set-face-attribute 'emud-face-player nil
                    :weight 'bold
                    :foreground "magenta")
(copy-face 'default 'emud-face-replied)
(set-face-attribute 'emud-face-replied nil
                    :foreground "green")
(copy-face 'default 'emud-face-reply)
(set-face-attribute 'emud-face-reply nil
                    :foreground "green")
(copy-face 'default 'emud-face-said)
(set-face-attribute 'emud-face-said nil
                    :foreground "green")
(copy-face 'default 'emud-face-say)
(set-face-attribute 'emud-face-say nil
                    :foreground "green")
(copy-face 'default 'emud-face-shape_info)
(set-face-attribute 'emud-face-shape_info nil
                    :weight 'bold
                    :foreground "magenta")
(copy-face 'default 'emud-face-short)
(set-face-attribute 'emud-face-short nil
                    :background "cyan"
                    :weight 'bold
                    :foreground "blue")
(copy-face 'default 'emud-face-tell)
(set-face-attribute 'emud-face-tell nil
                    :foreground "green")
(copy-face 'default 'emud-face-told)
(set-face-attribute 'emud-face-told nil
                    :foreground "green")
(copy-face 'default 'emud-face-whisper)
(set-face-attribute 'emud-face-whisper nil
                    :foreground "green")
(copy-face 'default 'emud-face-whispered)
(set-face-attribute 'emud-face-whispered nil
                    :foreground "green")

(copy-face 'default 'emud-face-prompt)
(set-face-attribute 'emud-face-prompt nil
		    :weight 'bold
		    :foreground "red")

(copy-face 'default 'emud-face-CMD)
(set-face-attribute 'emud-face-CMD nil
		    :foreground "blue"
		    :underline "red")
(defvar emud-psuedo-xml-list
  '(("^\\+--+|" "^[^|+]" 'score)))

(defvar emud-xml-tag-alist
  '(
    ;;tag         buffer   face                     handler
    (attack       nil      emud-face-attack         nil)
    (attacked     nil      emud-face-attacked       nil)
    (exit         nil      emud-face-exit           nil)
    (hpinfo       nil      emud-face-hpinfo         nil)
    (long         nil      emud-face-long           nil)
    (misc_in_long nil      emud-face-misc_in_long   nil)
    (player       nil      emud-face-player         nil)
    (replied      nil      emud-face-replied        nil)
    (reply        nil      emud-face-reply          nil)
    (said         nil      emud-face-said           nil)
    (say          nil      emud-face-say            nil)
    (shape_info   nil      emud-face-shape_info     nil)
    (short        nil      emud-face-short          nil)
    (tell         nil      emud-face-tell           nil)
    (told         nil      emud-face-told           nil)
    (whisper      nil      emud-face-whisper        nil)
    (whispered    nil      emud-face-whispered      nil)
    (BR           nil      nil                      emud-xml-handler-BR)
    (PROMPT       nil      emud-face-prompt         emud-xml-handler-prompt)
    (SPACE        nil      nil                      emud-xml-handler-SPACE)
    (prompt       nil      emud-face-prompt         emud-xml-handler-prompt)
    ;(BREAK        nil      nil                      emud-xml-handler-BREAK)
    (CMD          nil      emud-face-CMD            emud-xml-handler-CMD)))
      
(defvar emud-triggers nil)
(defvar emud-xml-get-command-flag nil)
(defvar emud-xml-command-queue nil)
(defvar emud-last-command nil)
(defvar emud-new-command-hook nil)

	

(defun emud-do-xml (proc xml)
  (let* ((node (car xml))
	 (tail (cdr xml))
	 node-list buffer face handler)
    (setq node-list (assq (car node) emud-xml-tag-alist))
    (when node-list 
	(setq buffer  (nth 1 node-list)
	      face    (nth 2 node-list)
	      handler (nth 3 node-list)))

    (unless handler
      (setq handler 'emud-xml-default-handler))
    (if buffer
	(with-current-buffer buffer
	  (goto-char (point-max))
	  (funcall handler proc face node))
      (funcall handler proc face node)) 

      
    (when tail
	(emud-do-xml proc tail))))

(defun emud-string-decode (str)
  (let ((start 0) 
	(retstr nil)
	(elt nil))

    (while (string-match "&\\([^;]+\\);" str start)
      (setq elt (assoc (match-string 1 str) emud-xml-string-alist))
      (if elt
	  (setq elt (cdr elt))
	(setq elt (match-string 0 str)))
      (setq retstr (concat retstr 
			   (substring str start (match-beginning 0))
			   elt))
      (setq start (match-end 0)))
    (concat retstr (substring str start))))

(defun emud-xml-handler-BR (proc face node)
  ;(message "inserting Break")
  (insert-before-markers "\n"))

(defun emud-xml-handler-BREAK (proc face node)
  nil)

(defun emud-xml-handler-SPACE (proc face node)
  ;(message "inserting SPACE")
  (let ((space (xml-get-attribute node 'space)))
    (when space
      (insert-before-markers space))))

(defun emud-xml-default-handler (proc face node)
  (let ((children (xml-node-children node))
	 child)
    
    (while children
      (setq child (pop children))
      (cond
       ((stringp child)
	(insert-before-markers (if face 
				   (propertize child 'face face)
				 child))
	
	(when  emud-triggers
	  (emud-run-triggers proc child)))
       ((listp child)
	(emud-do-xml proc (list child)))))))
       
	

(defun emud-xml-handler-prompt (proc face node)
  (setq emud-xml-get-command-flag t)
  (emud-xml-default-handler proc face node))

(defun emud-xml-handler-CMD (proc face node)
;  (edebug)
  (let ((xml-list (xml-node-children node))
	(cmd (xml-get-attribute node 'cmd)))
    (unless (and cmd (not (string= "" cmd)))
      (while (and (setq cmd (pop xml-list))
		  (not (stringp cmd))))
      
      (setq cmd (if cmd 
		    (xml-substitute-special cmd)
		  "")))
    (setq emud-xml-command-queue (append emud-xml-command-queue (list cmd))))
 ; (message (format "emud-xml-command-queue: %S" emud-xml-command-queue))
  (emud-xml-default-handler proc face node))

(defun emud-xml-find-string (xml-list)
  (let (string)
    (while (and (setq string (pop xml-list))
		(not (stringp string))))
    string))

    
 
(defun emud-unxml-tag (end)
  (when (and (< (point) end) (eq (char-after) ?<))
    (delete-char 1 nil)
    (insert "&lt;")
    (setq end (+ 3 end))
    (skip-chars-forward "^<>")
    (when (and (< (point) end) 
	       (eq (char-after) ?>))
      (delete-char 1 nil)
      (insert "&gt;")
      (setq end (+ 3 end))))
  end)
      

(defun emud-unxml-string (string)
  (let ((pos -1))
  (while (setq pos (string-match "&" string (1+ pos)))
    (set 'string (replace-match "&amp;"  t nil string)))
  (while (string-match "<" string)
    (set 'string (replace-match "&lt;"   t nil string)))
  (while (string-match ">" string)
    (set 'string (replace-match "&gt;"   t nil string)))
  (while (string-match "'" string)
    (set 'string (replace-match "&apos;" t nil string)))
  (while (string-match "\"" string)
    (set 'string (replace-match "&quot;" t nil string)))
  string))


(defun emud-assoc-looking-at (regex char)
  (looking-at regex))

(defun emud-make-regex ()
  (let ((psuedo-list emud-psuedo-xml-list)
	tag start regex)
    (while psuedo-list
      (setq tag (pop psuedo-list)
	    start (car tag)
	    regex (concat regex "\\(?:" start "\\)\\|")))
    (concat regex "\\(?:<")))

;; (defun emud-xml-psuedo ()
;;   (let ((tag-list (assoc-default 0 emud-psuedo-xml-list 
;; 				 'emud-assoc-looking-at)
;; 	region-start region-end str-tag))
;; 	(setq region-start (point))
;; 	(cond 
;; 	 (tag-list
;; 	  (when (re-search-forward (car tag-list) (point-max) t)
;; 	    (setq str-tag (format "%s" (cadr tag-list))) 
;; 	    (setq region-end (+ (point) 2 (length str-tag)))
;; 	    (goto-char region-start)
;; 	    (insert (concat "<" str-tag ">"))
;; 	    (goto-char region-end)
;; 	    (insert (concat "</" str-tag ">"))
;; 	    (list region-start region-end)))
;; 	 (t
;; 	  (insert "<NOXML>")
;; 	  (if (re-search-forward 
;; 	       (emud-make-regex)
;; 	       (point-max) t)
;; 	      (progn
;; 		(goto-char (match-beginning 0))
;; 		(when (looking-at "^")
;; 		  (forward-char -1)
;; 		  (setq region-end (point))
;; 		  (insert "<BR/>")
;; 		  (goto-char region-end)))
;; 	    (goto-char (point-max)))
;; 	  (insert "</NOXML>")
;; 	  (list region-start (point))))))
  
(defun emud-get-make-xml-handler (tag)
  (or (emud-get-xml-handler tag)
      (let ((handler (intern (format "emud-xml-handler-%s" tag))))
	(fset handler 'emud-xml-default-handler)
	(emud-set-xml-handler tag handler)
			      handler)))
			  
(defun emud-get-xml-handler (tag)
  (nth 3 (assq tag emud-xml-tag-alist)))

(defun emud-set-xml-handler (tag handler)
  (let ((node-list (assq tag emud-xml-tag-alist)))
    (if node-list
	(setcdr (cddr node-list) (list handler))
      (nconc emud-xml-tag-alist (list (list tag nil nil handler))))))

(defun emud-run-triggers (proc string)
  (let ((triggers emud-triggers)
	trigger action)
    (when (setq action 
		(assoc-default string emud-triggers 'string-match))
      (cond
       ((stringp action)
	(emud-simple-send proc action))
       ((symbolp action)
	(funcall action proc string))
       (t
	(error "Bad trigger"))))))
      

(defun emud-add-trigger (trigger action)
   (interactive "sTrigger: \nsAction: ")
   (setq emud-triggers (cons (cons trigger action) emud-triggers)))

(defun emud-del-trigger (trigger)
     (interactive "sDelete Trigger: ")
     (let (list-elt)
       (while (setq list-elt (assoc trigger emud-triggers))
	 (setq emud-triggers (delq list-elt emud-triggers)))))




(defun emud-xml-parse-region2 (buffer start)
  (let (region-start
	region-end 
	node-name
	arg-string
	arg-start
	arg-end
	attr
	xml)
    (with-current-buffer buffer
      (setq case-fold-search nil)
      (goto-char start)
      (skip-chars-forward " \t") ;skip whitespace
      (cond
       ((= (point) (point-max))
	nil)
       ((looking-at "[\n\r]")
	(setq region-start (point))
	(skip-chars-forward " \n\t\r")
	(setq xml (list (list 'BR nil)))
        (list xml region-start (point)))
       ((looking-at "<\\([^/> \t\n]*\\)")  ;eureka we have xml
	(setq region-start (point))
	(setq node-name (match-string 1))
	(goto-char (match-end 1))
	(search-forward ">" (point-max) t)
	(forward-char -2)
	(cond 
	 ((looking-at "/>") ;is tag empty?
	  (forward-char 2)
	  (when (and (eq (car xml) 'CMD)
		   (assq 'cmd attr))
	      (let ((cmd (xml-substitute-special (cdr (assq 'cmd attr)))))
		(when (looking-at (regexp-quote cmd))
		  (goto-char (match-end 0)))))
	  (skip-chars-forward " \t\n\r")
	  (setq region-end (point))
	  (goto-char region-start)
	  (setq xml (emud-xml-parse-children region-end))
	 (list xml region-start (point)))
					; find the end tag
	 ((re-search-forward (concat "</" node-name "\\s-*>") (point-max) t)
	  (setq region-end (point))
	  (goto-char region-start)
	  (setq xml (emud-xml-parse-children region-end))
	  (list xml region-start region-end))
	 (
	  (or (re-search-forward "<prompt>" (point-max) t)
	      (and (re-search-forward emud-prompt-pattern (point-max) t)
	 	   (> (- (point-max) region-start) 1000)))
	       
	  ;;prompts usually do not appear inside other tags
	  (goto-char (match-beginning 0))
	  (setq arg-string (buffer-substring-no-properties region-start (point)))
	  (setq xml (list (list 'NOXML nil arg-string)))
	  (list xml region-start (point)))

	 (t ;; can't find an end tag :(
	  nil)))

       ((looking-at emud-prompt-pattern) ;are we looking at a prompt?
	(setq region-start (point))
	(goto-char (match-end 0))
	(setq arg-string (buffer-substring-no-properties region-start (point)))
	(setq xml (list (list 'PROMPT nil arg-string)))
	(list xml region-start (point)))
       ;(looking-at "^.*died.")
       (t
	(setq region-start (point))   ;no xml -- start psuedo-xml
	(if (re-search-forward 
	     (concat "\\(?:" emud-prompt-pattern "\\)\\|\\(?:<\\)" ) 
	     (point-max) t)
	    (progn
	      (goto-char (match-beginning 0))
	      (when (looking-at emud-prompt-pattern)
		(forward-char -1)
		(setq region-end (point))
		(goto-char region-end)))
	  (goto-char (point-max)))
	(setq arg-string (buffer-substring-no-properties region-start (point)))
	(setq xml (list (list 'NOXML nil arg-string)))
	(list xml region-start (point)))))))


(defun emud-xml-parse-children (stop &optional top-level)
  (let ((start (point))
	children
	last-child
	child
	)
    (while (< (point) stop)
      (setq start (point))
      (cond
       ((and (looking-at "<")
	     (setq child (emud-xml-parse-children-xml stop)))
	(setq children (append children
			       (when last-child
				 (if top-level
				     (list (list 'NOXML nil last-child))
				   (list last-child)))
			       child)
	      last-child nil))
	(t
	 (setq last-child (concat last-child
					
					(emud-xml-parse-children-str stop))))))
    (append children (when last-child
		       (if top-level
			   (list (list 'NOXML nil last-child))
		       (list last-child))))))
      
      
(defun emud-xml-parse-children-xml (stop)
  (let ((start (point))
	node-name
	arg-start
	arg-end
	attr
	attr-start
	region-end
	xml)
    (when (looking-at "<\\([^/> \t\n]*\\)")
      (setq node-name (match-string 1))
      (setq xml (list (intern node-name)))
      (goto-char (match-end 1))
      (setq attr-start (point))
      (search-forward ">" stop t)
      (setq arg-start (point))
      (goto-char attr-start)
      (while (re-search-forward "\\([a-z]+\\) *= *\"\\([^\"]+\\)\"" arg-start t)
	(setq attr (append attr (list (cons (intern (match-string-no-properties 1)) (match-string-no-properties 2))))))
      (goto-char arg-start)
      (forward-char -2)
      (setq xml (append xml (list attr)))

      (cond
       ((looking-at "/>") ;is tag empty?
	(forward-char 2)
	(if (and (eq (car xml) 'CMD)
		 (assq 'cmd attr))
	    (let ((cmd (xml-substitute-special (cdr (assq 'cmd attr))))
		  arg-string)
	      (when (looking-at (regexp-quote cmd))
		(setq arg-string (buffer-substring-no-properties (point) (match-end 0)))
		(goto-char (match-end 0))
		(setq xml (append xml (list arg-string))))
	      (skip-chars-forward " \t\n\r" stop)
	      (list xml (list 'BR nil)))
	  (skip-chars-forward " \t\n\r" stop)
	  (list xml)))
       ((re-search-forward (concat "</" node-name "\\s-*>") stop t)
	(setq region-end (point))
	(setq arg-end (match-beginning 0))
	(goto-char arg-start)
	(setq xml (append xml (emud-xml-parse-children arg-end)))
	(goto-char region-end)
	(list xml))
       (t
	(goto-char start)
	nil)))))

(defun emud-xml-parse-children-str (stop)
  (let ((arg-start (point))
	(arg-end))
    (forward-char 1)
    (setq arg-end
	  (if (search-forward "<" stop t)
	      (progn
		(forward-char -1)
		(point))
	    stop))
    (goto-char arg-end)
    (buffer-substring-no-properties arg-start arg-end)))
     
(defun emud-xml-parse-buffer (proc xml-curr-char xml-buffer) 
  (let (region xml-list xml region-start region-stop)
    
    (setq xml-list 
	  (emud-xml-parse-region2 xml-buffer xml-curr-char))
    (while xml-list
      (setq xml (pop xml-list)
	    region-start (pop xml-list)
	    region-stop (pop xml-list))
      (setq emud-xml-list (append emud-xml-list xml))
      (setq xml-curr-char region-stop)
      (emud-do-xml proc xml)
      (when (and emud-xml-get-command-flag  emud-xml-command-queue)
	(setq emud-last-command (pop emud-xml-command-queue))
	(setq emud-xml-get-command-flag nil)
	(run-hooks 'emud-new-command-hook))
      (setq xml-list 
	    (emud-xml-parse-region2 xml-buffer xml-curr-char)))
    xml-curr-char))



      
