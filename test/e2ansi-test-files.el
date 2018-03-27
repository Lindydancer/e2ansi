;;; e2ansi-test-files.el --- Regression test e2ansi.

;; Copyright (C) 2014,2015 Anders Lindgren

;; Author: Anders Lindgren
;; Keywords: faces languages

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Regression test of `e2ansi', a package that renders syntax
;; highlighted buffers to text with ANSI escape sequences.
;;
;; The actual check is performed using `ert' (part of Emacs), with
;; test function provided by `faceup'. (Note: The "faceup" markup
;; language is not used in this test, however the multi line "equal"
;; and the "faceup-test-explain" infrastructure is.)

;;; Code:

(require 'faceup)

(defvar e2ansi-test-files-dir (faceup-this-file-directory)
  "The directory this file is located in.")
(prefer-coding-system 'utf-8)

(defun e2ansi-test-files-reference-file (file number-of-colors background)
  (concat file
          ".ansi"
          "-" (if (eq number-of-colors t)
                  "rgb8"
                (number-to-string number-of-colors))
          "-" (symbol-name background)))


(defun e2ansi-test-file (file number-of-colors background)
  "Test if e2ansi renders FILE as reference file.

The reference file is assumes to be named
FILE.ansi-NUMBER-OF-COLORS-BACKGROUND."
  (let ((reference-file (e2ansi-test-files-reference-file
                         file number-of-colors background)))
    (if (not (file-exists-p reference-file))
        (if faceup-test-explain
            (list 'file-not-found reference-file)
          nil)
      (faceup-test-equal
       (with-temp-buffer
         (let ((buffer (current-buffer)))
           (with-current-buffer (find-file-noselect file)
             (let ((noninteractive nil))
               (font-lock-mode 1))
             (let ((face-explorer-number-of-colors number-of-colors)
                   (face-explorer-background-mode background))
               (e2ansi-print-buffer (current-buffer) buffer)))
           (buffer-string)))
       (with-temp-buffer
         (insert-file-contents reference-file)
         (buffer-string))))))


(faceup-defexplainer e2ansi-test-file)

(defvar e2ansi-test-files '("files/hello/hello.c"
                            "files/utf8/utf8.txt"))


(defun e2ansi-test-generate-reference-files ()
  "Generate reference files for file in current buffer."
  (interactive)
  (dolist (number-of-colors '(8 16 256 t))
    (dolist (background '(dark light))
      (let ((reference-file
             (e2ansi-test-files-reference-file
              (buffer-file-name) number-of-colors background)))
        (let ((face-explorer-number-of-colors number-of-colors)
              (face-explorer-background-mode background))
          (e2ansi-write-file reference-file))))))


(defun e2ansi-test-files-generate ()
  "Create new reference files."
  (interactive "P")
  (dolist (file e2ansi-test-files)
    (setq file (concat e2ansi-test-files-dir file))
    (with-current-buffer (find-file-noselect file)
      (e2ansi-test-generate-reference-files))))


(ert-deftest e2ansi-file-test ()
  "Test e2ansi using known reference files."
  (dolist (number-of-colors '(8 16 256 t))
    (dolist (background '(dark light))
      (dolist (file e2ansi-test-files)
        (should (e2ansi-test-file file number-of-colors background))))))

(provide 'e2ansi-test-files)

;; e2ansi-test-files.el ends here.
