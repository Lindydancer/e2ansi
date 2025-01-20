;;; e2ansi-test.el --- Tests for `e2ansi'.  -*- lexical-binding: t; -*-

;; Copyright (C) 2014 Anders Lindgren

;; Author: Anders Lindgren
;; Keywords: faces, languages
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

;; Test and support code for e2ansi.

;;; Code:

(require 'e2ansi)
(require 'ert)

;; ----------------------------------------------------------------------
;; Test themes
;;

;; --------------------
;; Check what happens when themes are applied on top of each other.
;;

;; Facts:
;;
;; Faces are specified using the `face' text property, the value is a
;; face, a list containing faces and property/value pairs. Faces
;; earlier in the list takes precedense.
;;
;; For example:
;;
;;    * font-lock-variable-name-face
;;    * (font-lock-string-face font-lock-variable-name-face)
;;    * (font-lock-string-face :underline t)
;;

(defface e2ansi-test-neutral-face '() "")

(defface e2ansi-test-red-face '((t :foreground "red")) "")
(defface e2ansi-test-blue-face '((t :foreground "blue")) "")

(defface e2ansi-test-face '((t :weight bold)) "")
(defface e2ansi-test-underline-face '((t :underline t)) "")

;; It looks like the second one takes precedense.
(defface e2ansi-test-illegalmulti-face '((t :foreground "red"
                                            :foreground "blue"))
  "Face with two :foreground attributes.")

;;(let ((s "test1234"))
;;  (set-text-properties 0 6 '(face (e2ansi-test-red-face
;;                                   e2ansi-test-blue-face)) s)
;;  (insert s))
;;
;; => Red
;;
;; Hence, the first face in the list takes precedence.

;; --------------------

(defface e2ansi-test-inherit-underline-face
  '((t :inherit e2ansi-test-underline-face)) "")
(defface e2ansi-test-inherit-red-face
  '((t :inherit e2ansi-test-red-face)) "")


;;(let ((s "test1234"))
;;  (set-text-properties 0 6 '(face (e2ansi-test-inherit-underline-face
;;                                   e2ansi-test-inherit-red-face)) s)
;;  (insert s))
;;
;; => Red and underlined.
;;
;; Hence, the :inherit property is handled for each face separately
;; (good).


(defface e2ansi-test-double-inherit
  '((t :inherit e2ansi-test-underline-face :inherit e2ansi-test-red-face))
  "")


;;(let ((s "test1234"))
;;  (set-text-properties 0 6 '(face (e2ansi-test-double-inherit)) s)
;;  (insert s))
;;
;; => Red
;;
;; Hence, a face can effectively only inherit from one face and only
;; the last inherit takes effect.

(defface e2ansi-test-green-green-face
  '((t :foreground "green" :background "green")) "")

(defface e2ansi-test-inherit-in-the-middle
  '((t :foreground "red"
       :inherit e2ansi-test-green-green-face
       :background "red")) "")


;;(let ((s "test1234"))
;;  (set-text-properties 0 6 '(face (e2ansi-test-inherit-in-the-middle)) s)
;;  (insert s))
;;
;; => Red red
;;
;; Hence, inherited values are overwritten, regardless of position in
;; the propoerty list.

(defface e2ansi-test-theme-face1 '((t)) "")
(defface e2ansi-test-theme-face2 '((t)) "")

(deftheme e2ansi-test-theme1 "")

(custom-theme-set-faces 'e2ansi-test-theme1
                        '(e2ansi-test-theme-face1 ((t :foreground "red")))
                        '(e2ansi-test-theme-face2 ((t :underline t))))

(provide-theme 'e2ansi-test-theme1)
(enable-theme 'e2ansi-test-theme1)


(deftheme e2ansi-test-theme2 "")

(custom-theme-set-faces 'e2ansi-test-theme2
                        '(e2ansi-test-theme-face1 ((t :background "blue")))
                        '(e2ansi-test-theme-face2 ((t :underline nil))))

(provide-theme 'e2ansi-test-theme2)
(enable-theme 'e2ansi-test-theme2)

(deftheme e2ansi-test-theme3 "")

(custom-theme-set-faces 'e2ansi-test-theme3
                        '(e2ansi-test-theme-face1 ((t :background "magenta"
                                                      :underline t))))

(provide-theme 'e2ansi-test-theme3)
(enable-theme 'e2ansi-test-theme3)


;; TODO: The :inherit property.


;; ----------------------------------------------------------------------
;; Ert test cases
;;

;; ----------------------------------------
;; Face properties
;;

(defun e2ansi-test-set-face (str &rest face-properties)
  (set-text-properties 0 (length str) (cons 'face face-properties) str)
  str)


(ert-deftest e2ansi-test-strings ()
  (let ((face-explorer-number-of-colors 8)
        (plain "plain")
        (def (e2ansi-test-set-face "default" 'default))
        (red (e2ansi-test-set-face "red" 'e2ansi-test-red-face)))
    (should (equal (e2ansi-string-to-ansi plain) "plain"))
    ;; In some font-lock packages, the face `default' is used to
    ;; ensure that later rules don't overwrite parts of a string. One
    ;; such example is `cmake-font-lock'.
    (should (equal (e2ansi-string-to-ansi def) "default"))
    (should (equal (e2ansi-string-to-ansi red) "\x1b[31mred\x1b[0m"))))


(ert-deftest e2ansi-test-face-spec ()
  (let ((face-explorer-number-of-colors 8))
    (should (equal (e2ansi-ansi-state '(e2ansi-test-red-face
                                        e2ansi-test-blue-face))
                   '(1 normal normal normal normal)))
    (should (equal (e2ansi-ansi-state '(e2ansi-test-theme-face2 underline))
                   '(normal normal normal normal normal)))
    ))


;; ----------------------------------------
;; Color deduction
;;

(ert-deftest e2ansi-test-closest-color ()
  (let ((face-explorer-number-of-colors 8)
        (e2ansi-closest-color-number-cache '()))
    (should (equal (e2ansi-find-closest-color-number "red" :foreground)
                   1)))

  (let ((face-explorer-number-of-colors 16)
        (e2ansi-closest-color-number-cache '()))
    (should (equal (e2ansi-find-closest-color-number "red" :foreground)
                   9)))

  (let ((face-explorer-number-of-colors 256)
        (e2ansi-closest-color-number-cache '()))
    (should (equal (e2ansi-find-closest-color-number "red" :foreground)
                   196)))

  (let ((face-explorer-number-of-colors (* 256 256 256))
        (e2ansi-closest-color-number-cache '()))
    (should (equal (e2ansi-find-closest-color-number "red" :foreground)
                   196)))

  (let ((face-explorer-number-of-colors t)
        (e2ansi-closest-color-number-cache '()))
    (should (equal (e2ansi-find-closest-color-number "red" :foreground)
                   196))))


(provide 'e2ansi-test-basic)

;;; e2ansi-test.el ends here
