;;; e2ansi-load-init.el --- Load the `e2ansi' init file  -*- lexical-binding: t; -*-

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

;; This is used by the `e2ansi' command line tools to load an
;; e2ansi specific init file.

;;; Code:

(defvar e2ansi-suppress-init-file nil
  "When non-nil, don't load the e2ansi init file.")

;; Ensure that `e2ansi-silent' and `e2ansi-magic' are in the path.
(let ((dir (file-name-directory load-file-name)))
  (add-to-list 'load-path dir))

(unless e2ansi-suppress-init-file
  ;; Read e2ansi-specific user init file.
  (dolist (init-dir (list user-emacs-directory
                          (concat (or (getenv "XDG_CONFIG_HOME")
                                      (concat (getenv "HOME") "/.config"))
                                  "/emacs/")
                          (concat (getenv "HOME") "/")))
    (dolist (init-file '("e2ansi-init.el" ".e2ansi"))
      (let ((path (concat init-dir init-file)))
        (when (file-readable-p path)
          (load path nil t))))))

(provide 'e2ansi-load-init)
;;; e2ansi-load-init.el ends here
