;emud-picture.el
;$Revision: 1.1 $
;Functions and variable to draw picture maps

(require 'picture)

(defvar emud-picture-offset [50 10])
(defvar emud-picture-dim [5 -3])
(defvar emud-picture-buffer nil)

(copy-face 'default 'emud-curr-room-face)
(set-face-attribute 'emud-curr-room-face nil
                    :foreground "blue")

(copy-face 'default 'emud-room-face)
(set-face-attribute 'emud-room-face nil
                    :foreground "white")


(defun emud-picture-get-buffer nil
  (if (bufferp emud-picture-buffer)
      emud-picture-buffer
    (setq emud-picture-buffer (get-buffer-create "*pic-buf*"))
    (save-excursion
      (set-buffer emud-picture-buffer)
      (setq truncate-lines t)
      (insert "\n"))
    emud-picture-buffer))

(defun emud-draw-map (map &optional from)
  (let (room vec map-arr 
	     exit exits room-stack been-there path)
    (setq room  (or from 0)
	  prev-room  nil
	  prev-coord nil
	  map-arr    (emud-map-arr map)
	  been-there (make-hash-table)
	  exits      (emud-room-exits (aref map-arr room))
	  coord      [0 0 0])
    (while exits
      (when prev-room
	(emud-draw-room (aref map-arr prev-room) prev-coord))
      (emud-draw-room (aref map-arr room) coord t)
      (setq exit (pop exits)
	    prev-room room
	    prev-coord coord)
      (puthash room 't been-there)
      (when exits
	(push (vector room path exits coord) room-stack))
      (setq path (append path (list (car exit)))
	    room (cdr exit)
	    exits (emud-room-exits (aref map-arr room))
	    coord (if (setq dir-vec 
			    (cdr (assoc (car exit) emud-map-directions)))
		      (emud-add-vec dir-vec coord)
		    ;(save-excursion
		      ;(set-buffer emud-picture-buffer)
		      ;(erase-buffer)
		      ;(goto-char (point-min))
		      ;(insert "\n"))
		    coord))
      (when (or (gethash room been-there);are we going in circles?
		(not exits)) ;or out of exits?
	(if room-stack ;are there any exits we skiped?
	    (setq vec   (pop room-stack)
		  room  (aref vec 0)
		  path  (aref vec 1)
		  exits (aref vec 2)
		  coord (aref vec 3))
	  (setq exits nil ;no more rooms to check
		path nil)))) ;bail out
    path))

(defun emud-draw-room (room coord &optional curr-room)
  (let ((exits (emud-room-exits room)) 
	(number (if (emud-room-number room) (format "%03d" (emud-room-number room)) "???"))
	exit x y strings face)

      ;(picture-mode)
      (setq coord (emud-add-vec emud-picture-offset 
				(emud-mul-vec coord emud-picture-dim))
	    x (aref coord 0)
	    y (aref coord 1))
      (setq strings (make-vector 3 nil))
      (aset strings 0 (make-string 5 ?\ ))
      (aset strings 1 (concat " " number " "))
      (aset strings 2 (make-string 5 ?\ ))

      (while exits
	(setq exit (car (pop exits)))
	(cond 
	 ((eq 'e exit)
	  (aset (aref strings 1) 4 ?-))
	 ((eq 'w exit)
	  (aset (aref strings 1) 0 ?-))
	 ((eq exit 'n)
	  (aset (aref strings 0) 2 ?|))
	 ((eq exit 's)
	  (aset (aref strings 2) 2 ?|))
	 ((eq exit 'nw)
	  (aset (aref strings 0) 1 ?\\))
	 ((eq exit 'se)
	  (aset (aref strings 2) 4 ?\\))
	 ((eq exit 'ne)
	  (aset (aref strings 0) 3 ?/))
	 ((eq exit 'sw)
	  (aset (aref strings 2) 0 ?/))))
      (setq face (if curr-room
		     'emud-curr-room-face
		   'default-face))
      (aset strings 0 (propertize (aref strings 0) 
				  'face face))
      (aset strings 1 (propertize (aref strings 1)
				  'face face))
      (aset strings 2 (propertize (aref strings 2)
				  'face face))
      (save-excursion
	(set-buffer (emud-picture-get-buffer))
	(goto-char (point-min))
	(end-of-line)
	(delete-region (point-min) (point))
	(insert (concat number " " (emud-room-short room)))
        (emud-picture-goto-coord (- x 2) (1- y))
	(emud-picture-insert-string (aref strings 0))
	(picture-backward-column 5)
	(picture-move-down 1)
	(emud-picture-insert-string (aref strings 1))
	(picture-backward-column 5)
	(picture-move-down 1)
	(emud-picture-insert-string (aref strings 2)))))


(defun emud-picture-insert-string (string)
  (let ((here (point)) (len (length string)))
    (move-to-column (+ (current-column) len) t )
    (delete-region here (point))
    (insert string)))
    
(defun emud-picture-goto-coord (x y)
  (goto-char 0)
  (picture-forward-column x) 
  (picture-move-down y))

(defun emud-mul-vec (v1 v2)
  (let* ((len1 (length v1))  (len2 (length v2))
	 (len (max len1 len2))
	 (vec (make-vector len nil))
	 (count 0))
    (while (< count len)
      (aset vec count (* (if (< count len1) (aref v1 count) 0)
			 (if (< count len2) (aref v2 count) 0)))
      (setq count (1+ count)))
    vec))

