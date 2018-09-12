;;; bott.el --- simple rcirc bot -*- lexical-binding: t -*-

;; Author:   Basil L. Contovounesios <contovob@tcd.ie>
;; Homepage: https://gitlab.com/basil-conto/bott

;;; Commentary:

;; Simple IRC bot built atop `rcirc'.

;;; Code:

(require 'rcirc)
(eval-when-compile (require 'subr-x))

(defvar bott-functions (list #'bott-dave)
  "Hook functions run when a user message is received.
The input string is passed to each function in turn until one
returns a non-nil reply, indicating the input was handled.")

(defun bott-echo (&rest args)
  "Intended for debugging `rcirc-receive-message-functions'."
  (let* ((names '("proc" "cmd" "sender" "args" "line"))
         (fmt   (format ">>> %%-%ds %%S"
                        (apply #'max (mapcar #'string-width names)))))
    (dolist (arg args)
      (message fmt (or (pop names) "???") arg))))

(defun bott-dave (str)
  "Return a generic HAL 9000 reply to STR.
If STR does not begin with \"!\", return nil instead."
  (and (= (string-to-char str) ?!)
       (concat "I'm sorry Dave, I'm afraid I can't " (substring str 1))))

(defun bott-fn (proc cmd sender args _line)
  "Intended for `rcirc-receive-message-functions'."
  (when-let* (((string= cmd "PRIVMSG"))
              ((not (string= sender (rcirc-nick proc))))
              (output (run-hook-with-args-until-success
                       'bott-functions (cadr args)))
              (target (car args))
              (target (if (rcirc-channel-p target) target sender)))
    (rcirc-send-privmsg proc target output)))

(defun bott-init ()
  "Shake your bott."
  (rcirc-connect "irc.netsoc.tcd.ie" nil "bott" "blc" "bott.el")
  (add-hook 'rcirc-receive-message-functions #'bott-fn))

(provide 'bott)

;;; bott.el ends here
