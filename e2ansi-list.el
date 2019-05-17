;;; e2ansi-list.el --- Print various ANSI-related information.

;; Copyright (C) 2014,2015 Anders Lindgren

;; Author: Anders Lindgren
;; Keywords: faces, languages
;; Created: 2015-01-29
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

;; This is a support module to *e2ansi*, a package that renders
;; buffers with face information using ANSI sequences. The primary use
;; case for this is to provide syntax highligthing support for command
;; like tools like `more' and `less'.
;;
;; This package generates all sorts of ANSI-related information. It is
;; the core of the `e2ansi-info' command line tools.

;;; Code:

(require 'e2ansi)

;; ----------------------------------------
;; Print the basic 16 ANSI colors.
;;

(defun e2ansi-list-print-ansi-colors16 ()
  "Print the 16 ANSI colors."
  (interactive)
  (let ((zero-to-eight '(0 1 2 3 4 5 6 7)))
    (princ "           ")
    (dolist (no zero-to-eight)
      (princ (format "   4%dm" no)))
    (terpri)
    (dolist (row `(nil ,@zero-to-eight))
      (dolist (bold '(nil t))
        (dolist (col `(desc nil ,@zero-to-eight))
          (princ " ")
          (cond ((eq col 'desc)
                 (cond ((and bold
                             row)
                        (princ (format "1;3%dm" row)))
                       (bold
                        (princ "   1m"))
                       (row
                        (princ (format "  3%dm" row)))
                       (t
                        (princ "     "))))
                (t
                 (e2ansi-with-ansi-sequence standard-output
                  (when row
                    (e2ansi-emit-ansi-code (format "3%d" row)))
                  (when col
                    (e2ansi-emit-ansi-code (format "4%d" col)))
                  (when bold
                    (e2ansi-emit-ansi-code "1")))
                 (princ " gYw \x1b[0m"))))
        (terpri)))))


;; ----------------------------------------
;; Print the 256 ANSI colors.
;;

(defun e2ansi-list-print-ansi-colors256 (&optional dest)
  "Print tables of the ANSI colors using the 256 color mode."
  (interactive)
  ;; ----------
  ;; Basic colors.
  (terpri)
  (princ "Basic 16 colors:\n")
  (dolist (ground-mode '(:foreground :background))
    (e2ansi-list-print-ansi-range 0 16 8 ground-mode dest))
  ;; ----------
  ;; Color cube.
  (terpri)
  (princ "Color cube (6 * 6 * 6):\n")
  (dotimes (red 6)
    (dotimes (green 6)
      (e2ansi-list-print-ansi-range (+ 16 (* red 6 6) (* green 6))
                                    6 nil :foreground)
      (princ "   ")
      (e2ansi-list-print-ansi-range (+ 16 (* red 6 6) (* green 6))
                                    6 nil :background)
      (terpri))
    (terpri))

  ;; ----------
  ;; Greyscale.
  (terpri)
  (princ "Grayscale ramp:\n")
  (dolist (ground-mode '(:foreground :background))
    (e2ansi-list-print-ansi-range (+ 16 (* 6 6 6)) 24 12 ground-mode)))


(defun e2ansi-list-print-ansi-range (start count break-on ground-mode
                                           &optional dest)
  "Print a range of ANSI colors using the 256 color mode."
  (dotimes (i count)
    (let ((col (+ i start)))
      (e2ansi-with-ansi-sequence dest
        (e2ansi-emit-ansi-code (format "%d8;5;%d"
                                       (if (eq ground-mode :foreground)
                                           3
                                         4)
                                       col)))
      (princ (format " %3d " col start)))
    (e2ansi-with-ansi-sequence dest
      (e2ansi-emit-ansi-code "0"))
    (when (and break-on
               (eq (mod (+ i 1) break-on) 0))
      (terpri))))


;; ----------------------------------------
;; Print ANSI representation of common faces.
;;

;; TODO: Rename

(require 'diff-mode)

(defun e2ansi-list-list-faces ()
  "Display common faces.
Return the buffer displaying the faces."
  (with-output-to-temp-buffer "*Faces selection*"
    (with-current-buffer standard-output
      (setq font-lock-mode nil)
      (dolist (face '(default
                       error
                       trailing-whitespace
                       font-lock-builtin-face
                       font-lock-comment-face
                       font-lock-function-name-face
                       font-lock-negation-char-face
                       font-lock-keyword-face
                       font-lock-regexp-grouping-backslash
                       font-lock-regexp-grouping-construct
                       font-lock-string-face
                       font-lock-type-face
                       font-lock-variable-name-face
                       font-lock-warning-face
                       font-lock-doc-face
                       diff-removed
                       diff-refine-removed
                       diff-added
                       diff-refine-added))
        (let ((p (point)))
          (insert (symbol-name face))
          (font-lock-append-text-property p (point) 'face face))
        (insert "\n")))
    standard-output))


(defun e2ansi-list-print-ansi-faces (&optional dest)
  "Print an ANSI redation of standard faces."
  (let ((buffer (e2ansi-list-list-faces)))
    (e2ansi-print-buffer buffer dest)))


;; ----------------------------------------
;; Print Multi-line text with background.
;;

(defun e2ansi-list-print-multiline-text-with-background ()
  (princ "This demonstrates the behaviour when printing text spanning\n")
  (princ "multiple lines with a background color. Terminal windows\n")
  (princ "don't appear to handle this as Emacs does. In addition, they\n")
  (princ "behave differently if the text is emitted on the middle of\n")
  (princ "a page compared if the output causes a scroll of the window.\n")
  (let ((s "<-- This text\nwith background color\nspans multiple lines. -->"))
    (e2ansi-with-ansi-sequence standard-output
      (e2ansi-emit-ansi-code "45"))
    (princ s)
    (e2ansi-with-ansi-sequence standard-output
      (e2ansi-emit-ansi-code "0"))
    (terpri)))



;; -------------------------------------------------------------------
;; The end.
;;

(provide 'e2ansi-list)


;;; e2ansi-list.el ends here
