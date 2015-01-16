;;; e2ansi-magic.el --- Recognize major modes based on file content.

;; Copyright (C) 2015 Anders Lindgren

;; Author: Anders Lindgren
;; Keywords: faces, languages
;; Created: 2015-01-15
;; URL: https://github.com/Lindydancer/e2ansi

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file set up `magic-mode-alist' so that Emacs recognizes a
;; number of file types based on the content.
;;
;; This is especially useful when using `less' in pipes, in which case
;; Emacs can't pick a suitable major mode based in a file name.

;;; Code:

(defun e2ansi-magic-diff-p ()
  "Return non-nil if the content of the current buffer looks like a diff."
  ;; This is the format used by "svn diff".
  (re-search-forward "^@@ " magic-mode-regexp-match-limit t))

(add-to-list 'magic-mode-alist
             '(e2ansi-magic-diff-p . diff-mode))

;;; e2ansi-magic.el ends here.
