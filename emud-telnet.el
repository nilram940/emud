;;; emud-telnet.el --- run a emud session from within an Emacs buffer
;;; $Revision: 1.3.1.4 $

;;; networking code for emud. Largely taken from telnet.el

(require 'comint)

(defvar emud-partial-command "")
(defvar emud-host-alist nil)
(defvar emud-new-line "\r")
(defvar emud-mode-map nil)
(defvar emud-prompt-pattern "^Help: [^>]*> *\\|^Mail\.*\\[.*[0-9]) *")
(defvar emud-replace-c-g nil)
(make-variable-buffer-local
 (defvar emud-remote-echoes t
   "True if the emud process will echo input."))
(make-variable-buffer-local
 (defvar emud-interrupt-string "\C-c" "String sent by C-c."))

(defvar emud-count 0
  "Number of output strings from emud process while looking for password.")
(make-variable-buffer-local 'emud-count)

(defvar emud-program "telnet"
  "Program to run to open a mud connection.")

(defvar emud-initial-count -50
  "Initial value of `emud-count'.  Should be set to the negative of the
number of terminal writes emud will make setting up the host connection.")

(defvar emud-maximum-count 4
  "Maximum value `emud-count' can have.
After this many passes, we stop looking for initial setup data.
Should be set to the number of terminal writes emud will make
rejecting one login and prompting again for a username and password.")

(defvar emud-xml-buffer nil)
(defvar emud-xml-list  nil)
(defvar emud-xml-curr-char 0)
;(defvar emud-clear-to-send-flag t)
(defvar emud-command-queue nil)
;(defvar emud-last-cmd nil)
(defvar emud-login nil)
(defvar emud-password nil)

(defun emud-interrupt-subjob ()
  (interactive)
  "Interrupt the program running through emud on the remote host."
  (send-string nil emud-interrupt-string))

(defun emud-c-z ()
  (interactive)
  (send-string nil "\C-z"))

(defun send-process-next-char ()
  (interactive)
  (send-string nil
	       (char-to-string
		(let ((inhibit-quit t))
		  (prog1 (read-char)
		    (setq quit-flag nil))))))

; initialization on first load.
(if emud-mode-map
    nil
  (setq emud-mode-map (nconc (make-sparse-keymap) comint-mode-map))
  (define-key emud-mode-map "\C-m" 'emud-send-input)
  (define-key emud-mode-map "\C-j" 'emud-super-send)
  (define-key emud-mode-map "\C-c\C-q" 'send-process-next-char)
  (define-key emud-mode-map "\C-c\C-c" 'emud-interrupt-subjob) 
  (define-key emud-mode-map "\C-c\C-z" 'emud-c-z))

;;maybe should have a flag for when have found type
(defun emud-check-software-type-initialize (string)
  "Tries to put correct initializations in.  Needs work."
  (let ((case-fold-search t))
    (cond ((string-match "unix" string)
	 (setq emud-prompt-pattern comint-prompt-regexp)
	 (setq emud-new-line "\n"))
	((string-match "tops-20" string) ;;maybe add emud-replace-c-g
	 (setq emud-prompt-pattern  "[@>]*"))
	((string-match "its" string)
	 (setq emud-prompt-pattern  "^[^*>\n]*[*>] *"))
	((string-match "explorer" string)  ;;explorer emud needs work
	 (setq emud-replace-c-g ?\n))))
  (setq comint-prompt-regexp emud-prompt-pattern))

(defun emud-initial-filter (proc string)
  ;For reading up to and including password; also will get machine type.
  (save-current-buffer
    (set-buffer (process-buffer proc))
    (let ((case-fold-search t))
      (cond ((string-match "No such host" string)
	     (kill-buffer (process-buffer proc))
	     (error "No such host"))
	    ((and emud-login (string-match "name:" string))
	     (emud-filter proc string)
	     (send-string proc (concat emud-login emud-new-line))
	     (clear-this-command-keys))
	    ((string-match "passw" string)
	     (emud-filter proc string)
	     (setq emud-count 0)
	     (send-string proc (concat (or emud-password
					 (comint-read-noecho "Password: " t))
				       emud-new-line))
	     (clear-this-command-keys))
	    (t ;(emud-check-software-type-initialize string)
	       (emud-filter proc string)
	       (cond ((> emud-count emud-maximum-count)
		      (set-process-filter proc 'emud-filter))
		     (t (setq emud-count (1+ emud-count)))))))))

;; Identical to comint-simple-send, except that it sends emud-new-line
;; instead of "\n".
(defun emud-simple-send (proc string)
  (let ((len (length string)))
    (when (and proc string
	       (or (< len 100)
		   (y-or-n-p (format "Long command (%d chars). Continue? " len))))
    (with-current-buffer emud-xml-buffer
      (goto-char (point-max))
      (insert  (if emud-remote-echoes
		   (concat "<CMD cmd=\""
			   (if (string= string "")
			       " "
			     (emud-unxml-string string))
			   "\"/>")
		 (concat "<CMD>" 
			 (emud-unxml-string string)
			 "</CMD><BR/>\n"))))
    (comint-send-string 
     proc 
     (concat string
	     emud-new-line)))))
    
(defun emud-send-list (proc list)
  (when proc
    (let (string send-string)
      (while (setq string (pop list))
	(save-excursion
	  (set-buffer emud-xml-buffer)
	  (goto-char (point-max))
	  (insert  (if emud-remote-echoes
		       (concat "<CMD cmd=\""
			       (if (string= string "")
				   " "
				 (emud-unxml-string string))
			       "\"/>")
		     (concat "<CMD>" 
			     (emud-unxml-string string)
			     "</CMD><BR/>\n"))))
	(setq send-string (concat send-string string "\n")))
      (comint-send-string proc send-string))))


;;   (let (cmd)
    
;;     (when (and emud-clear-to-send-flag emud-command-queue)
;;       (setq cmd (pop emud-command-queue))
;;       (setq emud-last-cmd cmd)
;;       (setq emud-clear-to-send-flag nil))))
     

(defun emud-super-send ()
  (interactive)
    (let (proc (get-buffer-process (current-buffer)))
      (setq emud-command-queue nil)
      (emud-simple-send proc nil)))

(defun emud-filter (proc string)
  (with-current-buffer (process-buffer proc)
    (let* ((last-insertion (marker-position (process-mark proc)))
	   (delta (- (point) last-insertion))
	   (ie (and comint-last-input-end
		    (marker-position comint-last-input-end)))
	   (w (get-buffer-window (current-buffer)))
	   (ws (and w (window-start w))))
      (with-current-buffer emud-xml-buffer
	(goto-char (point-max))
	(insert (delete ?\r string))
	(insert "<BREAK/>"))
      (goto-char last-insertion)
      (setq emud-xml-curr-char
	    (emud-xml-parse-buffer proc emud-xml-curr-char emud-xml-buffer))

      (set-marker comint-last-output-start last-insertion)
      (set-marker (process-mark proc) (point))
      (if ws (set-window-start w ws t))
      (if ie (set-marker comint-last-input-end ie))
      (while (progn (skip-chars-backward "^\C-m" last-insertion)
		    (> (point) last-insertion))
	(delete-region (1- (point)) (point)))
      (goto-char (process-mark proc))
      (and emud-replace-c-g
	   (subst-char-in-region last-insertion (point) ?\C-g
				 emud-replace-c-g t))
      ;; If point is after the insertion place, move it
      ;; along with the text.
      (if (> delta 0)
	  (goto-char (+ (process-mark proc) delta))))))

(defun emud-check-command (string)
  (let (xml-string command pre-string count)
    (setq string (concat emud-partial-command string))
    (setq emud-partial-command "")
    (while (and (setq command (car emud-command-queue))
		(>= (length string) (1+ (length command)))
		(string= (substring string 0 
				    (1+ (length command))) 
			 (concat command "\n")))
      (setq xml-string (concat xml-string "<CMD>" 
			       (emud-unxml-string (pop emud-command-queue))
			       "</CMD><BR/>")
	    string (substring string (1+ (length command))
			      (length string))))
    (setq command (car emud-command-queue))
    (when (and command  
	       (<= (length string) (length command))
	       (string= string (substring command 0 (length string))))
      (setq emud-partial-command string)
      (setq string nil))

      (if (and
	   (not xml-string)
	   (setq command  (car emud-command-queue))
	   (setq command
		 (concat "\n" (substring (car emud-command-queue) 0 
					 (min 1 (length command)))))
	   (setq count (string-match (regexp-quote command) string)))
	     (progn
	       (setq pre-string (substring string 0 (1+ count))
		     string (substring string (1+ count) (length string)))
	       (concat pre-string (emud-check-command string)))
	(concat xml-string string))))
      

(defun emud-send-input ()
  (interactive)
;  (comint-send-input emud-new-line emud-remote-echoes)
  (comint-send-input)
  (delete-region comint-last-input-start
		 comint-last-input-end))


;;;###autoload (add-hook 'same-window-regexps "\\*emud-.*\\*\\(\\|<[0-9]+>\\)")

;;;###autoload
(defun emud (name)
  "Open a network login connection to host named HOST (a string).
Communication with HOST is recorded in a buffer `*PROGRAM-HOST*'
where PROGRAM is the emud program being used.  This program
is controlled by the contents of the global variable `emud-host-properties',
falling back on the value of the global variable `emud-program'.
Normally input is edited in Emacs and sent a line at a time."
  (interactive "sOpen connection to host: ")
  (let* ((host-list (assoc name emud-host-alist))
	 (host (if host-list (concat (nth 1 host-list) " " (nth 2 host-list)) name))
	 (comint-delimiter-argument-list '(?\  ?\t))
	 (buffer (get-buffer (concat "*" name "*")))
	 process)
    (if (and buffer (get-buffer-process buffer))
	(switch-to-buffer (concat "*" name "*"))
      (switch-to-buffer 
       (apply 'make-comint name emud-program nil))
      (setq process (get-buffer-process (current-buffer)))
      (set-process-filter process 'emud-initial-filter)
      (setq emud-xml-buffer (get-buffer-create (concat "*" name "-xml*")))
      (save-excursion
	(set-buffer emud-xml-buffer)
	(erase-buffer))
      (setq emud-xml-list nil)
      (setq emud-xml-curr-char 0
	    ;emud-clear-to-send-flag t
	    emud-command-queue nil)
      (when host-list
	(setq emud-login    (nth 3 host-list)
	      emud-password (nth 4 host-list)))

      ;; Don't send the `open' cmd till telnet is ready for it.
      (accept-process-output process)
      (erase-buffer)
      (send-string process (concat "open " host "\n"))
      (emud-mode)
      (setq comint-input-sender 'emud-simple-send)
      (setq emud-count emud-initial-count))))

(put 'emud-mode 'mode-class 'special)

(defun emud-mode ()
  "This mode is for using emud (or rsh) from a buffer to another host.
It has most of the same commands as comint-mode.
There is a variable ``emud-interrupt-string'' which is the character
sent to try to stop execution of a job on the remote host.
Data is sent to the remote host when RET is typed.

\\{emud-mode-map}
"
  (interactive)
  (comint-mode)
  (setq major-mode 'emud-mode
	mode-name "Emud"
	comint-prompt-regexp emud-prompt-pattern)
  (use-local-map emud-mode-map)
  (run-hooks 'emud-mode-hook))



;;; emud-telnet.el ends here
