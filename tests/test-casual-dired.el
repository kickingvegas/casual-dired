;;; test-casual-dired.el --- Casual Dired Tests      -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Charles Choi

;; Author: Charles Choi <kickingvegas@gmail.com>
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'ert)
(require 'casual-dired-test-utils)
(require 'casual-dired)

(ert-deftest test-casual-dired-tmenu-bindings ()
  (casualt-setup)
  (let ((test-vectors (list)))
    (push (casualt-suffix-test-vector "o" #'dired-find-file-other-window) test-vectors)
    (push (casualt-suffix-test-vector "C" #'dired-do-copy) test-vectors)
    (push (casualt-suffix-test-vector "R" #'dired-do-rename) test-vectors)
    (push (casualt-suffix-test-vector "D" #'dired-do-delete) test-vectors)
    (push (casualt-suffix-test-vector "S" #'dired-do-symlink) test-vectors)
    (push (casualt-suffix-test-vector "c" #'casual-dired-change-tmenu) test-vectors)
    (push (casualt-suffix-test-vector "w" #'dired-copy-filename-as-kill) test-vectors)
    ;;(push (casualt-suffix-test-vector "!" #'dired-do-shell-command) test-vectors)
    ;;("&" dired-do-async-shell-command)
    (push (casualt-suffix-test-vector "W" #'browse-url-of-dired-file) test-vectors)

    (push (casualt-suffix-test-vector "sq" #'casual-dired-sort-by-tmenu) test-vectors)
    (push (casualt-suffix-test-vector "hq" #'dired-hide-details-mode) test-vectors)
    (push (casualt-suffix-test-vector "Oq" #'dired-omit-mode) test-vectors)
    ;; (push (casualt-suffix-test-vector "iq" #'dired-maybe-insert-subdir 2) test-vectors)
    (push (casualt-suffix-test-vector "$q" #'dired-hide-subdir) test-vectors)
    (push (casualt-suffix-test-vector "kq" #'dired-do-kill-lines) test-vectors)
    (push (casualt-suffix-test-vector "gq" #'revert-buffer) test-vectors)
    (push (casualt-suffix-test-vector "f" #'casual-dired-find-dired-regexp) test-vectors)
    (push (casualt-suffix-test-vector "E" #'wdired-change-to-wdired-mode) test-vectors)
    ;;(push (casualt-suffix-test-vector "T" image-dired) test-vectors)
    (push (casualt-suffix-test-vector "Iq" #'casual-dired-image-info) test-vectors)

    (push (casualt-suffix-test-vector "mq" #'dired-mark) test-vectors)
    (push (casualt-suffix-test-vector "uq" #'dired-unmark) test-vectors)
    (push (casualt-suffix-test-vector "Uq" #'dired-unmark-all-marks) test-vectors)
    (push (casualt-suffix-test-vector "tq" #'dired-toggle-marks) test-vectors)
    (push (casualt-suffix-test-vector "~q" #'dired-flag-backup-files) test-vectors)
    (push (casualt-suffix-test-vector "xq" #'dired-do-flagged-delete) test-vectors)
    (push (casualt-suffix-test-vector "r" #'casual-dired-regexp-tmenu) test-vectors)

    (push (casualt-suffix-test-vector "^q" #'dired-up-directory) test-vectors)
    (push (casualt-suffix-test-vector "pq" #'dired-previous-line) test-vectors)
    (push (casualt-suffix-test-vector "nq" #'dired-next-line) test-vectors)
    (push (casualt-suffix-test-vector "ðq" #'dired-prev-dirline) test-vectors)
    (push (casualt-suffix-test-vector "îq" #'dired-next-dirline) test-vectors)
    ;;(push (casualt-suffix-test-vector "[" #'dired-prev-subdir) test-vectors)
    ;;(push (casualt-suffix-test-vector "]" #'dired-next-subdir) test-vectors)

    (push (casualt-suffix-test-vector "j" #'bookmark-jump) test-vectors)
    (push (casualt-suffix-test-vector "B" #'bookmark-set-no-overwrite) test-vectors)
    (push (casualt-suffix-test-vector "b" #'ibuffer) test-vectors)

    ;;(push (casualt-suffix-test-vector "" dired-isearch-filenames) test-vectors)
    (push (casualt-suffix-test-vector "ó" #'dired-isearch-filenames-regexp) test-vectors)
    ;;(push (casualt-suffix-test-vector "+q" #'dired-create-directory) test-vectors)
    (push (casualt-suffix-test-vector "Fq" #'dired-create-empty-file) test-vectors)

    (casualt-suffix-testbench-runner test-vectors
                                     #'casual-dired-tmenu
                                     '(lambda () (random 5000))))
  (casualt-breakdown t))


(ert-deftest test-casual-dired-regexp-tmenu-bindings ()
  (casualt-setup)
  (let ((test-vectors (list)))
    (push (casualt-suffix-test-vector "m" #'dired-mark-files-regexp) test-vectors)
    (push (casualt-suffix-test-vector "c" #'dired-mark-files-containing-regexp) test-vectors)
    (push (casualt-suffix-test-vector "d" #'dired-flag-files-regexp) test-vectors)
    (push (casualt-suffix-test-vector "C" #'dired-do-copy-regexp) test-vectors)
    (push (casualt-suffix-test-vector "r" #'dired-do-rename-regexp) test-vectors)

    (casualt-suffix-testbench-runner test-vectors
                                     #'casual-dired-regexp-tmenu
                                     '(lambda () (random 5000))))
  (casualt-breakdown t))


(ert-deftest test-casual-dired-change-tmenu-bindings ()
  (casualt-setup)
  (let ((test-vectors (list)))
    (push (casualt-suffix-test-vector "Mq" #'dired-do-chmod) test-vectors)
    (push (casualt-suffix-test-vector "Gq" #'dired-do-chgrp) test-vectors)
    (push (casualt-suffix-test-vector "Oq" #'dired-do-chown) test-vectors)
    (push (casualt-suffix-test-vector "Tq" #'dired-do-touch) test-vectors)

    (casualt-suffix-testbench-runner test-vectors
                                     #'casual-dired-change-tmenu
                                     '(lambda () (random 5000))))
  (casualt-breakdown t))

(provide 'test-casual-dired)
;;; test-casual-dired.el ends here
