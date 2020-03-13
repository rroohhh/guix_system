;;; Record all keystrokes, with timestamps at millisecond resolution.
;;; (Actually it's Emacs commands instead of keystrokes, but that's close enough.)

;;; Restricted to particular modes, but I really advise NOT RUNNING
;;; THIS AT ALL if you do much of anything sensitive inside Emacs.

(defvar keylogger-path "/home/robin/.emacs.d/keys"
  "Where to store the log.")

(defcustom keylogger-autosave-frequency (* 60 15)
  "How often to auto-save the collected data on key pair stroke frequencies.")

(defvar keylogger-autosave-timer nil)



;; (defvar keylogger-major-modes ('(text-mode) '(org-mode) '(c++-mode)
;;   "What modes to record data for.
;; Only include a mode if you're sure you won't type in any secrets!")

;; (defun keylogger-loggable-p ()
;;   (or (memq major-mode keylogger-major-modes)
;;       (equal (buffer-name) "speed-type")))

(defun keylogger-go ()
  (interactive)
  "Start logging Emacs commands and keystrokes."
  (add-hook 'pre-command-hook 'keylogger-log)
  (add-hook 'kill-emacs-hook 'keylogger-on-exit)
  (setq keylogger-autosave-timer (run-at-time "15 min" 900 'keylogger-save)))

(defun keylogger-on-exit ()
  (with-demoted-errors
	  (keylogger-save)))

(defun keylogger-stop ()
  (interactive)
  "Stop logging Emacs commands and keystrokes."
  (remove-hook 'pre-command-hook 'keylogger-log))

(defvar keylogger-events '()
  "The data not yet saved in the logfile, newer first.")

(defun keylogger-clear-unsaved ()
  (interactive)
  "Erase that part of the log not yet written to the logfile."
  (setq keylogger-events '()))

(defun keylogger-log ()
  ;; (Would be `with-demoted-errors' instead if not for paranoia from
  ;; an actual user.)
  (when (this-command-keys)
	;;	(message "%s" this-command)
	(push (vector (current-time) major-mode this-command (this-command-keys))
		  keylogger-events)))

(defun keylogger-save ()
  (interactive)
  "Save the keylogger buffer to keylogger-path, and clear it."
  ;; N.B. not atomic
  (when keylogger-events
    (with-temp-buffer
      (dolist (k (nreverse keylogger-events))
        (let ((time (aref k 0))
              (mode (aref k 1))
			  (cmd  (aref k 2))
              (keys (aref k 3)))
          (insert (format "%s %S %S %s\n"
                          (format-time-string "%s.%3N" time)
                          mode
						  cmd
                          (key-description keys)))))
      (append-to-file nil nil keylogger-path)
      (setq keylogger-events '()))))
