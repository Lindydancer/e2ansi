;;; e2ansi-test-files.el --- Regression test for Objc Font Lock.

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


(defun e2ansi-test-files-reference-file (file number-of-colors background)
  (concat file
          ".ansi"
          "-" (if (eq number-of-colors :rgb8)
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
             (let ((e2ansi-number-of-colors number-of-colors)
                   (e2ansi-background-mode background))
               (e2ansi-markup-to-buffer buffer)))
           (buffer-string)))
       (with-temp-buffer
         (insert-file-contents-literally reference-file)
         (buffer-string))))))


(faceup-defexplainer e2ansi-test-file)

(defvar e2ansi-test-files '("files/hello/hello.c"))


;; TODO: Implement a "generate reference files for this file", this
;; sledge hammer is too big. Maybe we need a function to generate a
;; single reference file.
;;
;; Beware: There is a bug in Emacs (I think, but haven't had time to
;; investigate) that writes the content of the wrong buffer if the
;; destination file already existed.
(defun e2ansi-test-files-generate (&optional all)
  "Create new reference files.
If ALL is non-nil, overwrite existing reference files."
  (interactive "P")
  (dolist (number-of-colors '(8 16 256 :rgb8))
    (dolist (background '(dark light))
      (dolist (file e2ansi-test-files)
        (setq file (concat e2ansi-test-files-dir file))
        (let ((reference-file
               (e2ansi-test-files-reference-file
                file number-of-colors background)))
          (if (or (not (file-exists-p reference-file))
                  all)
              (save-window-excursion
                (save-excursion
                  (find-file file)
                  (let ((e2ansi-number-of-colors number-of-colors)
                        (e2ansi-background-mode background))
                    (e2ansi-write-file reference-file))))))))))


(ert-deftest e2ansi-file-test ()
  "Test e2ansi using known reference files."
  (dolist (number-of-colors '(8 16 256 :rgb8))
    (dolist (background '(dark light))
      (dolist (file e2ansi-test-files)
        (should (e2ansi-test-file file number-of-colors background))))))

(provide 'e2ansi-test-files)

;; e2ansi-test-files.el ends here.
