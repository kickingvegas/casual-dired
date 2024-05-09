;;; casual-dired.el --- Casual Dired                 -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Charles Choi

;; Author: Charles Choi <kickingvegas@gmail.com>
;; URL: https://github.com/kickingvegas/casual-dired
;; Keywords: tools
;; Version: 1.1.0
;; Package-Requires: ((emacs "29.1"))

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

;; Casual Dired is an opinionated Transient-based porcelain for Emacs Dired.

;; INSTALLATION
;; (require 'casual-dired)
;; (define-key dired-mode-map (kbd "C-o") #'casual-dired-tmenu)

;;; Code:
(require 'transient)
(require 'dired)
(require 'dired-x)
(require 'wdired)
(require 'image-dired)
(require 'casual-dired-sort-by)
(require 'casual-dired-version)
(require 'casual-dired-variables)
(require 'casual-dired-settings)
(require 'casual-dired-utils)

;;; Menus
;;;###autoload (autoload 'casual-dired-tmenu "casual-dired" nil t)
(transient-define-prefix casual-dired-tmenu ()
  "Transient menu for Dired."
  [["File"
    ("o" "Open Other" dired-find-file-other-window :transient nil)
    ("C" "Copy to…" dired-do-copy :transient nil)
    ("R" "Rename…" dired-do-rename :transient nil)
    ("D" "Delete…" dired-do-delete :transient nil)
    ("S" "Symlink…" dired-do-symlink :transient nil)
    ("c" "Change›" casual-dired-change-tmenu :transient nil)
    ("w" "Copy Name" dired-copy-filename-as-kill :transient nil)
    ("!" "Shell…" dired-do-shell-command :transient nil)
    ("&" "Shell &… " dired-do-async-shell-command :transient nil)
    ("W" "Browse" browse-url-of-dired-file :transient nil)]

   ["Directory"
    ("s" "Sort By›" casual-dired-sort-by-tmenu
     :if casual-dired-show-sort-by-tmenu-p
     :transient t)
    ("h" "Hide Details" dired-hide-details-mode
     :description
     (lambda ()
       (casual-dired--checkbox-label dired-hide-details-mode "Hide Details"))
     :if-not casual-dired-lisp-dired-buffer-p
     :transient t)
    ("O" "Omit Mode" dired-omit-mode
     :description
     (lambda () (casual-dired--checkbox-label dired-omit-mode "Omit Mode"))
     :transient t)
    ("i" "Insert Subdir" dired-maybe-insert-subdir
     :if-not casual-dired-lisp-dired-buffer-p
     :transient t)
    ("$" "Hide/Unhide Subdir" dired-hide-subdir
     :if-not casual-dired-lisp-dired-buffer-p
     :transient t)
    ("k" "Kill (Hide) Line(s)" dired-do-kill-lines :transient t)
    ("g" "Revert" revert-buffer :transient t)
    ("f" "Filter…" casual-dired-find-dired-regexp :transient nil)
    ("E" "Edit (wdired)" wdired-change-to-wdired-mode :transient nil)
    ("T" "Thumbnails…" image-dired :if display-graphic-p :transient n)
    ("I" "Image Info" casual-dired-image-info :transient t)]

   ["Mark"
    ("m" "Mark" dired-mark :transient t)
    ("u" "Unmark" dired-unmark :transient t)
    ("U" "Unmark All" dired-unmark-all-marks :transient t)
    ("t" "Toggle Marks" dired-toggle-marks :transient t)
    ("~" "Flag Backups" dired-flag-backup-files :transient t)
    ("x" "Delete Flagged" dired-do-flagged-delete :transient t)
    ("r" "Regexp›" casual-dired-regexp-tmenu :transient nil)
    ("#" "Utils›" casual-dired-utils-tmenu :transient nil)]

   ["Navigation"
    :pad-keys t
    ("^" ".." dired-up-directory
     :description (lambda () (format ".. %s" (casual-dired-directory-label
                                              casual-dired-use-unicode-symbols)))
     :transient t)
    ("p" " ↑ 📄" dired-previous-line
     :description (lambda ()
                    (format "%s %s"
                            (casual-dired-format-arrow
                             (casual-dired-up-arrow-label
                              casual-dired-use-unicode-symbols)
                             casual-dired-use-unicode-symbols)
                            (casual-dired-file-label
                             casual-dired-use-unicode-symbols)))
     :transient t)
    ("n" " ↓ 📄" dired-next-line
     :description (lambda ()
                    (format "%s %s"
                            (casual-dired-format-arrow
                             (casual-dired-down-arrow-label
                              casual-dired-use-unicode-symbols)
                             casual-dired-use-unicode-symbols)
                            (casual-dired-file-label
                             casual-dired-use-unicode-symbols)))
     :transient t)
    ("M-p" " ↑ 📁" dired-prev-dirline
     :if-not casual-dired-lisp-dired-buffer-p
     :description (lambda ()
                    (format "%s %s"
                            (casual-dired-format-arrow
                             (casual-dired-up-arrow-label
                              casual-dired-use-unicode-symbols)
                             casual-dired-use-unicode-symbols)
                            (casual-dired-directory-label
                             casual-dired-use-unicode-symbols)))
     :transient t)
    ("M-n" " ↓ 📁" dired-next-dirline
     :if-not casual-dired-lisp-dired-buffer-p
     :description (lambda ()
                    (format "%s %s"
                            (casual-dired-format-arrow
                             (casual-dired-down-arrow-label
                              casual-dired-use-unicode-symbols)
                             casual-dired-use-unicode-symbols)
                            (casual-dired-directory-label
                             casual-dired-use-unicode-symbols)))
     :transient t)
    ("[" " ↑ 🗂️" dired-prev-subdir
     :if-not casual-dired-lisp-dired-buffer-p
     :description (lambda ()
                    (format "%s %s"
                            (casual-dired-format-arrow
                             (casual-dired-up-arrow-label
                              casual-dired-use-unicode-symbols)
                             casual-dired-use-unicode-symbols)
                            (casual-dired-subdir-label
                             casual-dired-use-unicode-symbols)))
     :transient t)
    ("]" " ↓ 🗂️" dired-next-subdir
     :if-not casual-dired-lisp-dired-buffer-p
     :description (lambda ()
                    (format "%s %s"
                            (casual-dired-format-arrow
                             (casual-dired-down-arrow-label
                              casual-dired-use-unicode-symbols)
                             casual-dired-use-unicode-symbols)
                            (casual-dired-subdir-label
                             casual-dired-use-unicode-symbols)))
     :transient t)]]

  [["Quick"
    ("j" "Jump to Bookmark…" bookmark-jump :transient nil)
    ("B" "Add Bookmark…" bookmark-set-no-overwrite :transient nil)
    ("b" "List Buffers" ibuffer :transient nil)]

   ["Search"
    ("C-s" "I-Search…" dired-isearch-filenames :transient nil)
    ("M-s" "I-Search Regexp…" dired-isearch-filenames-regexp :transient nil)]

   ["New"
    ("+" "Directory" dired-create-directory :transient t)
    ("F" "File" dired-create-empty-file :transient t)]]

  [:class transient-row
          ("<return>" "Open" dired-find-file :transient nil)
          ("," "Settings" casual-dired-settings-tmenu :transient nil)
          ("q" "Dismiss" ignore :transient transient--do-exit)])

(transient-define-prefix casual-dired-regexp-tmenu ()
  "Transient menu for Dired mark regexp functions."
  ["Regexp Mark"
   ("m" "Files…" dired-mark-files-regexp :transient nil)
   ("c" "Files Containing…" dired-mark-files-containing-regexp :transient nil)
   ("d" "Files For Deletion…" dired-flag-files-regexp :transient nil)
   ("C" "Files To Copy…" dired-do-copy-regexp :transient nil)
   ("r" "Files To Rename…" dired-do-rename-regexp :transient nil)]
  [("q" "Dismiss" ignore :transient transient--do-exit)])

(transient-define-prefix casual-dired-change-tmenu ()
  ["Change"
   [("M" "Mode…" dired-do-chmod :transient t)
    ("G" "Group…" dired-do-chgrp :transient t)
    ("O" "Owner…" dired-do-chown :transient t)]
   [("T" "Touch" dired-do-touch :transient t)]]
  [("q" "Dismiss" ignore :transient transient--do-exit)])

;;; Functions
(defun casual-dired--identify-image (filename)
  "Get image information of FILENAME via Imagemagick identify utility."
  (car
   (process-lines
    "identify"
    "-format"
    "%m %wx%h %b"
    (expand-file-name filename))))

(defun casual-dired-image-file-p ()
  "Predicate if current file in Dired is an image file."
  (string-match-p (image-dired--file-name-regexp) (dired-get-filename)))

(defun casual-dired-image-info ()
  "Message image info in the minibuffer and push into `kill-ring'."
  (interactive)
  (if (casual-dired-image-file-p)
    (let* ((filename (dired-get-filename))
           (image-info (casual-dired--identify-image filename))
           (output (concat image-info
                           " "
                           (file-name-base filename)
                           "."
                           (file-name-extension filename))))
      (message output)
      (kill-new output))
    (message "Not an image file.")))

(defun casual-dired-lisp-dired-buffer-p ()
  "Predicate if buffer name is “*Find Lisp Dired*”.

This buffer is created by the command `find-lisp-find-dired'."
  (and (derived-mode-p 'dired-mode)
       (string-equal (buffer-name) "*Find Lisp Dired*")))

(defun casual-dired-show-sort-by-tmenu-p ()
  "Predicate to show `casual-dired-sort-by-tmenu'."
  (and (equal dired-use-ls-dired t)
       (not (casual-dired-lisp-dired-buffer-p))))

(defun casual-dired-find-dired-regexp (REGEXP)
  "Find files in current directory whose names match REGEXP."
  (interactive "sFind filenames with regex: ")
  (find-lisp-find-dired default-directory REGEXP))

;;; Labels
(defun casual-dired--variable-to-checkbox (v)
  "Checkbox string representation of variable V.
V is either nil or non-nil."
  (if casual-dired-use-unicode-symbols
      (if v "☑︎" "◻︎")
    (if v "[x]" "[ ]")))

(defun casual-dired--prefix-label (label prefix)
  "Label constructed with PREFIX and LABEL separated by a space."
  (format "%s %s" prefix label))

(defun casual-dired--checkbox-label (v label)
  "Checkbox label using variable V and LABEL."
  (casual-dired--prefix-label label (casual-dired--variable-to-checkbox v)))

(defun casual-dired-file-label (&optional unicode)
  "If UNICODE is non-nil, use Unicode symbol for file."
  (if unicode
      "📄"
    "File"))

(defun casual-dired-subdir-label (&optional unicode)
  "If UNICODE is non-nil, use Unicode symbol for subdir."
  (if unicode
      "🗂️"
    "Subdir"))

(defun casual-dired-directory-label (&optional unicode)
  "If UNICODE is non-nil, use Unicode symbol for directory."
  (if unicode
      "📁"
    "Dir"))

(defun casual-dired-up-arrow-label (&optional unicode)
  "If UNICODE is non-nil, use Unicode symbol for up arrow."
  (if unicode
      "↑"
    "Up"))

(defun casual-dired-down-arrow-label (&optional unicode)
  "If UNICODE is non-nil, use Unicode symbol for down arrow."
  (if unicode
      "↓"
    "Down"))

(defun casual-dired-format-arrow (buf typeset)
  "If TYPESET is non-nil, then format BUF string to have space."
  (if typeset
      (format " %s" buf)
    buf))

(provide 'casual-dired)
;;; casual-dired.el ends here
