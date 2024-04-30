;;; casual-dired-settings.el --- Casual Dired Settings  -*- lexical-binding: t; -*-

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
(require 'casual-dired-variables)
(require 'casual-dired-version)

;;; Menus
(transient-define-prefix casual-dired-settings-tmenu ()
  ["Dired Settings"
   ["Customize"
    ("l" "Uses ls with “--dired”"
     casual-dired--customize-dired-use-ls-dired
     :transient nil)
    ("r" "Revert Behavior"
     casual-dired--customize-dired-auto-revert-buffer
     :transient nil)
    ("t" "Target Directory"
     casual-dired--customize-dired-dwim-target
     :transient nil)
    ("s" "Dired Listing Switches"
     casual-dired--customize-dired-listing-switches
     :transient nil)
    ("c" "Casual Listing Switches"
     casual-dired--customize-casual-dired-listing-switches
     :transient nil)
    ("d" "Dired Group"
     casual-dired--customize-dired-group
     :transient nil)]]
  [""
   :class transient-row
   ("a" "About" casual-dired-about :transient nil)
   ("v" "Version" casual-dired-version :transient nil)
   ("q" "Dismiss" ignore :transient transient--do-exit)])

;;; Functions

(defun casual-dired--customize-dired-use-ls-dired ()
  "Customize if “--dired” switch is passed to ‘ls’.

Customize the variable `dired-use-ls-dired'. If
`dired-use-ls-dired' is non-nil, then pass the “--dired” option
to ‘ls’.

NOTE: If `dired-use-ls-dired' is t, Casual Dired presumes that
GNU ‘ls’ is installed."
  (interactive)
  (customize-variable 'dired-use-ls-dired))

(defun casual-dired--customize-dired-auto-revert-buffer ()
  "Customize Dired revert buffer policy.

Customize the variable `dired-auto-revert-buffer' which
determines if Dired buffers are automatically reverted on
revisiting their directory."
  (interactive)
  (customize-variable 'dired-auto-revert-buffer))

(defun casual-dired--customize-dired-dwim-target ()
  "Customize `dired-dwim-target'.

If non-nil, Dired tries to guess a default target directory."
  (interactive)
  (customize-variable 'dired-dwim-target))

(defun casual-dired--customize-dired-listing-switches ()
  "Customize `dired-listing-switches'.

Switches passed to ‘ls’ for Dired.  MUST contain the ‘l’ option."
  (interactive)
  (customize-variable 'dired-listing-switches))

(defun casual-dired--customize-casual-dired-listing-switches ()
  "Customize `casual-dired-listing-switches'.

Customizes switches that are enabled by the Casual “Dired Sort” By menu."
  (interactive)
  (customize-variable 'casual-dired-listing-switches))

(defun casual-dired--customize-dired-group ()
  "Call the Dired customization group."
  (interactive)
  (customize-group "dired"))

(defun casual-dired-about-dired ()
  "Casual is an opinionated porcelain for Emacs Dired.

Learn more about using Casual Dired at our discussion group on GitHub.
Any questions or comments about Casual should be made there.
URL `https://github.com/kickingvegas/casual-dired/discussions'

If you find a bug or have an enhancement request, please file an issue.
Our best effort will be made to answer it.
URL `https://github.com/kickingvegas/casual-dired/issues'

If you enjoy using Casual Dired, consider making a modest financial
contribution to help support its development and maintenance.
URL `https://www.buymeacoffee.com/kickingvegas'

Casual Dired was conceived and crafted by Charles Choi in
San Francisco, California.

Thank you for using Casual Dired and always choose love."
  (ignore))

(defun casual-dired-about ()
  "About information for Casual Dired."
  (interactive)
  (describe-function #'casual-dired-about-dired))

(provide 'casual-dired-settings)
;;; casual-dired-settings.el ends here
