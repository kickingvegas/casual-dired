;;; test-casual-dired-sort-by.el --- Casual Dired Sort By Tests  -*- lexical-binding: t; -*-

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
(require 'casual-dired-sort-by)

(ert-deftest test-casual-dired-sort-by-tmenu-bindings ()
  (casualt-setup)
  (let ((test-vectors (list)))

    (push (casualt-suffix-test-vector "n" #'casual-dired--sort-by-name) test-vectors)
    (push (casualt-suffix-test-vector "k" #'casual-dired--sort-by-kind) test-vectors)
    (push (casualt-suffix-test-vector "l" #'casual-dired--sort-by-date-last-opened) test-vectors)
    (push (casualt-suffix-test-vector "a" #'casual-dired--sort-by-date-added) test-vectors)
    (push (casualt-suffix-test-vector "m" #'casual-dired--sort-by-date-modified) test-vectors)
    (push (casualt-suffix-test-vector "M" #'casual-dired--sort-by-date-metadata-changed) test-vectors)
    (push (casualt-suffix-test-vector "v" #'casual-dired--sort-by-version) test-vectors)
    (push (casualt-suffix-test-vector "s" #'casual-dired--sort-by-size) test-vectors)

    (casualt-suffix-testbench-runner test-vectors
                                     #'casual-dired-sort-by-tmenu
                                     '(lambda () (random 5000))))
  (casualt-breakdown t))



(provide 'test-casual-dired-sort-by)
;;; test-casual-dired-sort-by.el ends here
