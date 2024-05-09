;;; casual-dired-utils.el --- Casual Dired Utils Menu  -*- lexical-binding: t; -*-

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

(require 'transient)
(require 'dired)
(require 'dired-x)
(require 'checkdoc)
(require 'elint)

(transient-define-prefix casual-dired-utils-tmenu ()
  ["Utils - Marked Files or File under Point"
   ["Files"
    ("F" "Open" dired-do-find-marked-files :transient nil)
    ("Z" "Compress" dired-do-compress :transient nil)]

   ["Rename"
    ("u" "Upcase…" dired-upcase :transient nil)
    ("d" "Downcase…" dired-downcase :transient nil)]

   ["Categories"
    ("s" "Search & Replace›" casual-dired-search-replace-tmenu :transient nil)
    ("e" "Emacs Lisp›" casual-dired-elisp-tmenu :transient nil)
    ("l" "Link›" casual-dired-link-tmenu :transient nil)]]

  [:class transient-row
          ("q" "Dismiss" ignore :transient transient--do-exit)])

(transient-define-prefix casual-dired-search-replace-tmenu ()
  ["Search & Replace"
   ["Search in Files"
     :pad-keys t
     ("f" "Find regexp…" dired-do-find-regexp :transient nil)
     ("C-s" "I-search…" dired-do-isearch :transient nil)
     ("M-s" "I-search regexp…" dired-do-isearch-regexp :transient nil)
     ("s" "Search first regexp match…" dired-do-search :transient nil)]

    ["Replace in Files"
     ("F" "Find regexp and replace…" dired-do-find-regexp-and-replace :transient nil)
     ("r" "Query regexp and replace…" dired-do-query-replace-regexp :transient nil)]]

  [:class transient-row
          ("q" "Dismiss" ignore :transient transient--do-exit)])

(transient-define-prefix casual-dired-elisp-tmenu ()
  ["Emacs Lisp"
   ["Byte-Compile"
    ("B" "Byte-compile…" dired-do-byte-compile :transient nil)
    ("D" "Byte-compile directory…" byte-recompile-directory :transient nil)]

   ["Load"
    ("L" "Load" dired-do-load :transient nil)]

   ["Verification"
    ("e" "Lint Directory…" elint-directory :transient nil)
    ("c" "Check documentation strings" checkdoc-dired :transient nil)]]

  [:class transient-row
          ("q" "Dismiss" ignore :transient transient--do-exit)])

(transient-define-prefix casual-dired-link-tmenu ()
  ["Link"
    ("h" "Hard link…" dired-do-hardlink :transient nil)
    ("H" "Hard link names with regexp…" dired-do-hardlink-regexp :transient nil)
    ("S" "Symbolic link names with regexp…" dired-do-symlink-regexp :transient nil)]

  [:class transient-row
          ("q" "Dismiss" ignore :transient transient--do-exit)])


(provide 'casual-dired-utils)
;;; casual-dired-utils.el ends here
