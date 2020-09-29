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

(require 'dom)
(require 'rcirc)
(require 'seq)
(eval-when-compile (require 'subr-x))

(defvar bott-functions (list #'bott-dave #'bott-url)
  "Hook functions run when a user message is received.
The input string is passed to each function in turn until one
returns a non-nil reply, indicating the input was handled.")

(defvar bott-url-functions
  (list #'bott-url-ydl #'bott-url-html #'bott-url-curl)
  "Hook functions for determining the title of a given URL.
Each function should return a process object.  They are called in
turn until the first process exits with a non-nil :bott-value
property, indicating the URL was handled.  A string value stands
for the title of the URL and may contain mIRC colour codes.")

(defvar bott-ydl-program "youtube-dl"
  "The name by which to invoke \"youtube-dl\".
See also `bott-ydl-switches'.")

(defvar bott-ydl-switches
  '("--dump-single-json" "--flat-playlist" "--no-warnings")
  "List of options to pass to `bott-ydl-program'.")

(defvar bott-curl-program "curl"
  "The name by which to invoke \"curl\".
See also `bott-curl-switches'.")

(defvar bott-curl-switches '("--fail" "--location" "--silent")
  "List of options to pass to `bott-curl-program'.")

(defvar bott-timeout 20
  "Number of seconds to wait for asynchronous process output.")

(defun bott-dave (str)
  "Return a generic HAL 9000 reply to STR.
If STR does not begin with a bang command, return nil instead."
  (and (string-match-p (rx bos ?! graph) str)
       (pcase (substring-no-properties str 1)
         ((rx bos "shrug"  eow) "¯\\_(ツ)_/¯")
         ((rx bos "dunno"  eow) "¯\\(°_o)/¯")
         ((rx bos "shades" eow) '("(•_•)" "(•_•)>⌐■-■" "(⌐■_■)" "YEEEEAAAHHH"))
         ((rx bos "fuck" (? eow (+ nonl) bow) (| "off" "you" "u") eow) "no u")
         ((rx bos "fuck" (? eow (+ nonl) bow) "me" eow) "yes u")
         (str (concat "I'm sorry Dave, I'm afraid I can't " str)))))

(defun bott--url-sentinel (proc _msg)
  "Process sentinel for `bott--url-proc', which see."
  (when (and (not (process-live-p proc))
             (buffer-live-p (process-buffer proc)))
    (when (zerop (process-exit-status proc))
      (with-current-buffer (process-buffer proc)
        (funcall (process-get proc :sentinel) proc)))
    (kill-buffer (process-buffer proc))))

(defun bott--url-proc (name cmd sentinel &rest props)
  "Run CMD in a subprocess with NAME and return the process.
Add PROPS to its plist and call SENTINEL on successful exit."
  (let ((proc (make-process
               :name name
               :buffer (generate-new-buffer-name (format " *%s*" name))
               :command cmd
               :connection-type 'pipe
               :sentinel #'bott--url-sentinel)))
    (while props (process-put proc (pop props) (pop props)))
    (process-put proc :sentinel sentinel)
    proc))

(defalias 'bott--parse-json
  (if (fboundp 'json-parse-buffer)
      (lambda ()
        (json-parse-buffer :null-object nil))
    (require 'json)
    (defvar json-object-type)
    (declare-function json-read "json" ())
    (lambda ()
      (let ((json-object-type 'hash-table))
        (json-read))))
  "Read JSON object from current buffer starting at point.
Objects are decoded as hash-tables and null as nil.")

(defun bott--secs (secs)
  "Format SECS as a human-readable string."
  (format-seconds "%h:%z%.2m:%.2s" secs))

(defun bott--url-ydl-sentinel (proc)
  "Process sentinel for `bott-url-ydl', which see."
  (goto-char (point-min))
  (let* ((base  (url-file-nondirectory (process-get proc :url)))
         (base  (url-unhex-string (file-name-sans-extension base)))
         (json  (bott--parse-json))
         (title (gethash "title"      json))
         (time  (gethash "duration"   json))
         (start (gethash "start_time" json)))
    (unless (or (not title)
                (string-blank-p title)
                (string= title base))
      (setq title (list "\C-b" title))
      (and time  (setq title `(,(bott--secs  time) " " ,@title))
           start (setq title `(,(bott--secs start) "/" ,@title)))
      (process-put proc :bott-value (apply #'concat title)))))

(defun bott-url-ydl (url)
  "Return process using `bott-ydl-program' to find title of URL.
Intended for `bott-url-functions', which see."
  (bott--url-proc "bott-url-ydl"
                  `(,bott-ydl-program ,@bott-ydl-switches ,url)
                  #'bott--url-ydl-sentinel :url url))

(defun bott--url-html-sentinel (proc)
  "Process sentinel for `bott-url-html', which see."
  (unless (string-prefix-p "text/html" (mail-fetch-field "content-type"))
    (process-put proc :bott-value t)))

(defun bott-url-html (url)
  "Return process determining whether URL's contents are HTML.
Intended for `bott-url-functions', which see."
  (bott--url-proc "bott-url-html"
                  `(,bott-curl-program "--head" ,@bott-curl-switches ,url)
                  #'bott--url-html-sentinel :url url))

(defun bott--url-curl-sentinel (proc)
  "Process sentinel for `bott-url-curl', which see."
  (let* ((dom   (libxml-parse-html-region (point-min) (point-max)
                                          (process-get proc :url)))
         (title (string-trim (dom-text (dom-by-tag dom 'title)))))
    (unless (string-empty-p title)
      (setq title (replace-regexp-in-string "[\n\r]+" "" title t t))
      (process-put proc :bott-value (concat "\C-b" title)))))

(defun bott-url-curl (url)
  "Return process using `bott-curl-program' to find title of URL.
Intended for `bott-url-functions', which see."
  (bott--url-proc "bott-url-curl"
                  `(,bott-curl-program ,@bott-curl-switches ,url)
                  #'bott--url-curl-sentinel :url url))

(defun bott--url-nsfw (str)
  "Determine whether STR mentions \"NSFL\" or \"NSFW\".
Return mIRC-formatted string with trailing space that includes
the corresponding initialism if found; otherwise return nil."
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    (while (re-search-forward rcirc-url-regexp nil t)
      (replace-match "" t t))
    (goto-char (point-min))
    (and (or (re-search-forward (rx bow "nsfl" eow) nil t)
             (re-search-forward (rx bow "nsfw" eow) nil t))
         (format "\C-b\C-c5[%s]\C-o "
                 (upcase (match-string-no-properties 0))))))

(defun bott--url-value (proc)
  "Read entire output of PROC and return its :bott-value."
  (while (accept-process-output proc bott-timeout))
  (process-get proc :bott-value))

(defun bott-url (str)
  "Return title of first URL found in STR, or nil on failure.
Uses `bott-url-functions', which see."
  (when (string-match rcirc-url-regexp str)
    (let* ((url   (match-string-no-properties 0 str))
           (procs (mapcar (lambda (fn) (funcall fn url))
                          bott-url-functions))
           (val   (seq-some #'bott--url-value procs)))
      (mapc #'delete-process procs)
      (and (stringp val)
           (concat (bott--url-nsfw str) val)))))

(defun bott-receive-message (proc cmd sender args _line)
  "Gateway between `rcirc' and `bott'.
Intended for `rcirc-receive-message-functions'."
  (when-let* (((string= cmd "PRIVMSG"))
              ((not (string= sender (rcirc-nick proc))))
              (output (run-hook-with-args-until-success
                       'bott-functions (cadr args)))
              (target (car args))
              (target (if (rcirc-channel-p target) target sender)))
    (dolist (line (if (consp output) output (list output)))
      (rcirc-send-privmsg proc target line))))

(defun bott-truncate-log (&rest _)
  "Truncate `rcirc-debug-buffer'.
See `rcirc-buffer-maximum-lines' for controlling truncation."
  (and rcirc-buffer-maximum-lines
       (> rcirc-buffer-maximum-lines 0)
       (buffer-live-p (get-buffer rcirc-debug-buffer))
       (with-current-buffer rcirc-debug-buffer
         (save-excursion
           (goto-char (point-max))
           (forward-line (- rcirc-buffer-maximum-lines))
           (let ((inhibit-read-only t))
             (delete-region (point-min) (point)))))))

(defun bott-init ()
  "Shake your bott."
  (setq rcirc-debug-flag t)
  (setq rcirc-buffer-maximum-lines messages-buffer-max-lines)
  (rcirc-connect "irc.netsoc.tcd.ie" nil "bott" "blc" "bott.el")
  (add-hook 'rcirc-receive-message-functions #'bott-truncate-log)
  (add-hook 'rcirc-receive-message-functions #'bott-receive-message))

(provide 'bott)

;;; bott.el ends here
