;;; bott-tests.el --- tests for bott.el -*- lexical-binding: t -*-

;; Author:   Basil L. Contovounesios <contovob@tcd.ie>
;; Homepage: https://gitlab.com/basil-conto/bott

;; Written in 2019 by Basil L. Contovounesios <contovob@tcd.ie>.

;; This file is NOT part of GNU Emacs.

;; To the extent possible under law, the author has dedicated all
;; copyright and related and neighbouring rights to this software to
;; the public domain worldwide.  This software is distributed without
;; any warranty.

;; You should have received a copy of the CC0 Public Domain Dedication
;; along with this software.  If not, see
;; <https://creativecommons.org/publicdomain/zero/1.0/>.

;;; Code:

(require 'bott)
(require 'ert)
(require 'rcirc)

(defun bott-tests--report (max)
  "Return a closure over a numerical progress reporter.
The reporter counts from 0 through MAX.  Calling the closure
increments the reporter by 1."
  (let* ((i 0)
         (report (make-progress-reporter "Testing URLs" i max)))
    (lambda ()
      (if (< i (aref (cdr report) 2))
          (progress-reporter-update report (setq i (1+ i))))
      (if (>= i (aref (cdr report) 2))
          (progress-reporter-done report)))))

(ert-deftest bott-url-ydl-youtube ()
  "Test `bott-url-ydl' with YouTube URLs."
  (let* ((yt     "https://youtube.com/")
         (vid    "https://youtu.be/dQw4w9WgXcQ")
         (vtitle " \C-bRick Astley - Never Gonna Give You Up (Video)")
         (vids   '((""          .           "03:32")
                   ("?t=0"      .     "00:00/03:32")
                   ("?t=43"     .     "00:43/03:32")
                   ("?t=85"     .     "01:25/03:32")
                   ("?t=85s"    .     "01:25/03:32")
                   ("?t=1m25s"  .     "01:25/03:32")
                   ("?t=256"    .     "04:16/03:32")
                   ("?t=4096"   .   "1:08:16/03:32")
                   ("?t=524288" . "145:38:08/03:32")))
         (list   "?list=OLAK5uy_m_WIrXd1iNF0mbmSsYauKUo1x4i9Vj4d4")
         (ltitle "\C-bThe Best Of Rick Astley")
         (lists  (list (concat yt "playlist" list)
                       (concat vid list)))
         (report (bott-tests--report (+ 1 (length vids) (length lists)))))
    (pcase-dolist (`(,query . ,time) vids)
      (should (equal (bott-url (concat vid query))
                     (concat time vtitle)))
      (funcall report))
    (dolist (url lists)
      (should (equal (bott-url url) ltitle))
      (funcall report))
    (should (equal (bott-url (concat yt "channel/UCuAXFkgsw1L7xaCfnd5JJOw"))
                   "\C-bUploads from Official Rick Astley"))
    (funcall report)))

(ert-deftest bott-url-ydl-dailymotion ()
  "Test `bott-url-ydl' with Dailymotion URLs."
  (let* ((dm     "https://dailymotion.com/")
         (vid    "https://dai.ly/x206fok")
         (vids   '("" "?start=41"))
         (vtitle "04:22 \C-bQueen - I Want To Break Free (Official Video)")
         (others `(("queen_official"  . "\C-bQueen Official")
                   ("playlist/x65lx2" . "\C-bQueen")
                   ("video/x206fok?playlist=x65lx2" . ,vtitle)))
         (report (bott-tests--report (+ (length vids) (length others)))))
    (dolist (query vids)
      (should (equal (bott-url (concat vid query)) vtitle))
      (funcall report))
    (pcase-dolist (`(,file . ,title) others)
      (should (equal (bott-url (concat dm file)) title))
      (funcall report))))

(ert-deftest bott-truncate-log ()
  "Test `bott-truncate-log' behaviour."
  (with-temp-buffer
    (let ((rcirc-debug-buffer (current-buffer))
          (lines 10))
      (dotimes (_ lines)
        (insert "line\n"))
      (dolist (max '(nil 0 8 1))
        (let ((rcirc-buffer-maximum-lines max))
          (bott-truncate-log))
        (or max (setq max 0))
        (should (= (line-number-at-pos (point-max))
                   (1+ (if (> max 0) max lines))))))))

(provide 'bott-tests)

;;; bott-tests.el ends here
