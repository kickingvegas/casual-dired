;;; casual-dired-version.el --- Casual Version             -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Charles Choi

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

;;; Code:

(defconst casual-dired-version "1.2.1"
  "Casual Dired Version.")

(defun casual-dired-version ()
  "Show current version of Casual Dired."
  (interactive)
  (message casual-dired-version))

(provide 'casual-dired-version)
;;; casual-dired-version.el ends here
