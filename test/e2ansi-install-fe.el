;;; e2ansi-install-fe.el --- Check support package installation  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Anders Lindgren

;; Author: Anders Lindgren

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

;; Check that if `face-explorer' is installed using the package
;; system, `e2ansi-cat' can find it.
;;
;; This file assumes that the `face-explorer' repository is
;; side-by-side with `e2ansi'.
;;
;; This is typically used with a fake HOME directory.

;;; Code:

(defvar e2ansi-install-fe-directory
  (if load-file-name
      (file-name-directory load-file-name)
    default-directory))

(package-install-file (concat e2ansi-install-fe-directory
                              "../../face-explorer/face-explorer.el"))

(provide 'e2ansi-install-fe)
;;; e2ansi-install-fe.el ends here
