;;; bott.el --- simple rcirc bot -*- lexical-binding: t -*-

;; Author:   Basil L. Contovounesios <contovob@tcd.ie>
;; Homepage: https://gitlab.com/basil-conto/bott

;; Written in 2018 by Basil L. Contovounesios <contovob@tcd.ie>.

;; This file is NOT part of GNU Emacs.

;; To the extent possible under law, the author has dedicated all
;; copyright and related and neighbouring rights to this software to
;; the public domain worldwide.  This software is distributed without
;; any warranty.

;; You should have received a copy of the CC0 Public Domain Dedication
;; along with this software.  If not, see
;; <https://creativecommons.org/publicdomain/zero/1.0/>.

;;; Commentary:

;; Simple IRC bot built atop `rcirc'.

;;; Code:

(require 'rcirc)
(eval-when-compile (require 'subr-x))

(defvar bott-functions (list #'bott-dave #'bott-ydl)
  "Hook functions run when a user message is received.
The input string is passed to each function in turn until one
returns a non-nil reply, indicating the input was handled.")

(defvar bott-ydl-program "youtube-dl"
  "The name by which to invoke \"youtube-dl\".")

(defvar bott-ydl-switches '("--dump-single-json"
                            "--flat-playlist"
                            "--no-warnings")
  "List of options to pass to `bott-ydl-program'.")

(defun bott-echo (&rest args)
  "Intended for debugging `rcirc-receive-message-functions'."
  (dolist (name '("proc" "cmd" "sender" "args" "line"))
    (message ">>> %-6s %S" name (pop args))))

(defun bott-dave (str)
  "Return a generic HAL 9000 reply to STR.
If STR does not begin with \"!\", return nil instead."
  (and (= (string-to-char str) ?!)
       (concat "I'm sorry Dave, I'm afraid I can't " (substring str 1))))

(defun bott-ydl (url)
  "Return title of URL via \"youtube-dl\", or nil on error."
  (with-temp-buffer
    (when (eq 0 (apply #'call-process bott-ydl-program nil t nil
                       (append bott-ydl-switches (list url))))
      (goto-char (point-min))
      (gethash "title" (json-parse-buffer)))))

(defun bott-fn (proc cmd sender args _line)
  "Intended for `rcirc-receive-message-functions'."
  (when-let* (((string= cmd "PRIVMSG"))
              ((not (string= sender (rcirc-nick proc))))
              (output (condition-case err
                          (run-hook-with-args-until-success
                           'bott-functions (cadr args))
                        (error err)))
              (target (car args))
              (target (if (rcirc-channel-p target) target sender)))
    (if (stringp output)
        (rcirc-send-privmsg proc target output)
      (rcirc-cmd-me (format "%s: %s" (car output) (error-message-string output))
                    proc target))))

(defun bott-init ()
  "Shake your bott."
  (rcirc-connect "irc.netsoc.tcd.ie" nil "bott" "blc" "bott.el")
  (add-hook 'rcirc-receive-message-functions #'bott-fn))

(provide 'bott)

;;; bott.el ends here
