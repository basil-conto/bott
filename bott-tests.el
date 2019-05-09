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
