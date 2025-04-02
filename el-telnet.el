(require 'telnet)

(defvar el-telnet-process nil
  "Stores the active Telnet process.")



(defun el-telnet-start (name buffer host service filter)
  "Replacement for external telnet command using make-network-process.
NAME is the process name, BUFFER is the buffer for communication,
HOST is the remote host, and SERVICE is the port."
  (setq el-telnet-process
        (make-network-process
         :name name
         :buffer buffer
         :host host
         :service service
         :filter filter
         :sentinel 'el-telnet-sentinel
         :coding 'utf-8))
  el-telnet-process)

(defun el-telnet-filter (process output)
  "Process incoming Telnet data."
  (when (process-live-p process)
    (let ((buffer (process-buffer process)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (goto-char (point-max))
          (insert output)
          (goto-char (point-max)))))))

(defun el-telnet-sentinel (process event)
  "Handle process events."
  (when (buffer-live-p (process-buffer process))
    (with-current-buffer (process-buffer process)
      (goto-char (point-max))
      (insert (format "\n[Connection %s]\n" (string-trim event)))
      (when (string-match "closed" event)
        (setq el-telnet-process nil)))))



