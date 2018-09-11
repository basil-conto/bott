;;; bott.el --- simple rcirc bot -*- lexical-binding: t -*-

;; Author:   Basil L. Contovounesios <contovob@tcd.ie>
;; Homepage: https://gitlab.com/basil-conto/bott

;;; Commentary:

;; Simple IRC bot built atop `rcirc'.

;;; Code:

(require 'rcirc)
(eval-when-compile (require 'subr-x))

(defun bott-echo (&rest args)
  "Intended for debugging `rcirc-receive-message-functions'."
  (let* ((names '("proc" "cmd" "sender" "args" "line"))
         (fmt   (format ">>> %%-%ds %%S"
                        (apply #'max (mapcar #'string-width names)))))
    (dolist (arg args)
      (message fmt (or (pop names) "???") arg))))

(defun bott-fn (proc cmd _sender args _line)
  "Intended for `rcirc-receive-message-functions'."
  (when-let* (((equal cmd "PRIVMSG"))
              (target (car  args))
              (input  (cadr args))
              ((rcirc-channel-p target))
              ((> (length input) 1))
              ((= (aref input 0) ?!)))
    (rcirc-send-privmsg proc target
                        (concat "I'm sorry Dave, I'm afraid I can't "
                                (substring input 1)))))

(defun bott-init ()
  "Shake your bott."
  (rcirc-connect "irc.netsoc.tcd.ie" nil "bott" "blc" "bott.el")
  (add-hook 'rcirc-receive-message-functions #'bott-fn))

(provide 'bott)

;;; bott.el ends here
