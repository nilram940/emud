;; emud.el
;; $Revision: 1.10 $
;; Loads the individual components that make up emud

(let
    ((emud-path "~/emud/"))
  (load (concat emud-path "emud-xml.elc"))
  (load (concat emud-path "emud-telnet.elc")))

(defun emud-no-process (in-buffer)
  (let ((proc nil)
	(out-buffer (get-buffer-create "*Emud-out*")))
    (setq emud-xml-buffer in-buffer)
    ;(emud-map-start)
    (save-excursion
      (set-buffer out-buffer)
      (erase-buffer)
      ;;(emud-parse-xml-buffer proc 0 in-buffer))))
      (emud-xml-parse-buffer proc 0 in-buffer))))

