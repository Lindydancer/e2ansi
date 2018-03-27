;;; e2ansi-test.el --- Tests for e2ansi.

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

(defface e2ansi-test-bold
  '((t :weight bold))
  "Bold face to test e2ansi.")

(defface e2ansi-test-light
  '((t :weight light))
  "Light face to test e2ansi.")

(defface e2ansi-test-underline
  '((t :underline t))
  "Underlined face to test e2ansi.")

;; :slant can be "italic" "normal" or "oblique", "reverse",
;; "reverse-italic", "reverse-oblique"

(defface e2ansi-test-italic
  '((t :slant italic))
  "Italic face to test e2ansi.")

(defface e2ansi-test-oblique
  '((t :slant oblique))
  "Oblique face to test e2ansi.")

(defface e2ansi-test-reverse-italic
  '((t :slant reverse-italic))
  "Reverse italic face to test e2ansi.")

(defface e2ansi-test-reverse-oblique
  '((t :slant reverse-oblique))
  "Reverse oblique face to test e2ansi.")

(defun e2ansi-test-list-test-faces ()
  (interactive)
  (with-output-to-temp-buffer "*E2ansi test*"
    (with-current-buffer standard-output
      (setq font-lock-mode nil)
      (dolist (weight '(nil light bold))
        (dolist (slant '(nil italic oblique reverse-italic reverse-oblique))
          (dolist (underline '(nil t))
            (let ((p (point)))
              (insert "a_1 ")
              (cond ((eq weight 'light)
                     (font-lock-append-text-property
                      p (point) 'face 'e2ansi-test-light))
                    ((eq weight 'bold)
                     (font-lock-append-text-property
                      p (point) 'face 'e2ansi-test-bold)))
              (cond ((eq slant 'italic)
                     (font-lock-append-text-property
                      p (point) 'face 'e2ansi-test-italic))
                    ((eq slant 'oblique)
                     (font-lock-append-text-property
                      p (point) 'face 'e2ansi-test-oblique))
                    ((eq slant 'reverse-italic)
                     (font-lock-append-text-property
                      p (point) 'face 'e2ansi-test-reverse-italic))
                    ((eq slant 'reverse-oblique)
                     (font-lock-append-text-property
                      p (point) 'face 'e2ansi-test-reverse-oblique)))
              (when underline
                (font-lock-append-text-property
                 p (point) 'face 'e2ansi-test-underline))))))
      (insert "\n")
      (message "%S" (buffer-substring (point-min) (point-max))))
    standard-output))


(defun e2ansi-test-batch ()
  (let ((buffer (e2ansi-test-list-test-faces)))
    (with-current-buffer buffer
      (e2ansi-write-file "e2ansi-test.ansi"))))


(defun e2ansi-test-make-string-with-color (s color)
  (setq s (concat s))
  (set-text-properties 0 (length s)
                       (list 'face
                             (list :foreground color))
                       s)
  s)

;; Drop "ground mode" or pass it to the above.
(defun e2ansi-test-make-string-with-color-number (s ground-mode color-number)
  (let ((rgb (e2ansi-ansi-color-values color-number)))
    (e2ansi-test-make-string-with-color s
                                        (format "#%02x%02x%02x"
                                                (/ (nth 0 rgb) 256)
                                                (/ (nth 1 rgb) 256)
                                                (/ (nth 2 rgb) 256)))))

(defun e2ansi-test-list-ansi-colors ()
  ""
  (interactive)
  (let ((i 0))
    (with-output-to-temp-buffer "*AnsiColors*"
      (set-buffer standard-output)
      (insert "\
This buffer contains e2ansi:s view of colors. It may differ from
Emacs:s, when `e2ansi-use-window-system-color-values' is nil and
from the actual color used in the terminal.\n\n")
      (while (< i 256)
        (let ((s (format "%3d: %s" i
                         (if (< i 16)
                             (car (nth i e2ansi-colors))
                           (format "color-%d" i))))
              (rgb (e2ansi-ansi-color-values i)))
          (set-text-properties 0 (length s)
                               (list 'face
                                     (list :foreground
                                           (format "#%02x%02x%02x"
                                                   (/ (nth 0 rgb) 256)
                                                   (/ (nth 1 rgb) 256)
                                                   (/ (nth 2 rgb) 256))))
                               s)
          (insert s)
          (insert "\n")
          (setq i (+ i 1))))
      (display-buffer (current-buffer)))))



(defun e2ansi-test-list-closest-color (colors)
  "List COLORS and the ansi color they are mapped to."
  (with-output-to-temp-buffer "*ColorsToAnsi*"
    (set-buffer standard-output)
    (insert "\
This buffer contains a mapping from colors to the closest ANSI
color. Note that colors with the same name as the basic ANSI
colors will be mapped to the corresponding color number. (In
e2ansi:s view of the color might differ when compared by the
window system `e2ansi-use-window-system-color-values' is
nil.)\n\n")
    (dolist (color colors)
      (let ((ansi-color-number
             (e2ansi-find-closest-color-number color :foreground)))
        (let ((orig (e2ansi-test-make-string-with-color
                     (format "%-20s" color) color))
              (ansi (e2ansi-test-make-string-with-color-number
                     (format "(%d) %s"
                             ansi-color-number
                             (if (< ansi-color-number 16)
                                 (car (nth ansi-color-number e2ansi-colors))
                               (format "color-%d" ansi-color-number)))
                     :foreground
                     ansi-color-number)))
          (insert orig " " ansi "\n"))))
    (display-buffer (current-buffer))))


;; TODO: Rewrite so that this accepts the present number of colors
;; (e.g. 256) rather than the exact.
(defun e2ansi-test-collect-all-face-colors (number-of-colors
                                            ground-mode
                                            &optional background)
  "List of colors appearing in all defined faces, with restrictions.

NUMBER-OF-COLORS is the exact number of colors in the face spec,
GROUND-MODE is either :foreground and :background, and BACKGROUND
\(if present) is `dark' or `light'."
  (let ((res '())
        (key (list 'min-colors number-of-colors)))
    (dolist (face (face-list))
      (let ((spec (face-default-spec face)))
        (dolist (entry spec)
          (let ((background-pair (and (not (memq (nth 0 entry) '(t default)))
                                      (assq 'background (nth 0 entry)))))
            (when (and (member key (nth 0 entry))
                       (or
                        (not background)
                        (not background-pair)
                        (eq (nth 1 background-pair) background)))
              (let ((plist (cdr entry)))
                (when (consp (car-safe plist))
                  (setq plist (car plist)))
                (let ((color (plist-get plist ground-mode)))
                  (when color
                    (add-to-list 'res color)))))))))
    res))


(defun e2ansi-test-list-collected-faces (number-of-colors
                                         ground-mode
                                         background)
  (interactive
   (list (read-number "Number of colors: " 88)
         (if (y-or-n-p "Foreground")
             :foreground
           :background)
         (let ((s (read-string "Background: ")))
           (cond ((string= s "dark") 'dark)
                 ((string= s "light") 'light)
                 ((string= s "") nil)
                 (t (error "Expected 'dark', 'light', or empty"))))))
  (with-help-window "*CollectedColors*"
    (with-current-buffer standard-output
      (erase-buffer)
      (insert "The following ")
      (insert (if (eq ground-mode :foreground)
                  "foreground"
                "background"))
      (insert " colors are found, when assuming that there are ")
      (insert (number-to-string number-of-colors) "\n")
      (insert "number of colors")
      (when background
        (insert "with a %s background" background))
      (insert ".\n\n")
      (list-colors-print (e2ansi-test-collect-all-face-colors
                          number-of-colors ground-mode background))
      (set-buffer-modified-p nil)
      (setq truncate-lines t))))


;; ----------------------------------------
;; Example ANSI output.
;;

(defun e2ansi-test-show-colors-between (ground-mode from
                                                    &optional to &rest extra)
  "Insert examples of ANSI colors from color FROM to color TO.

If EXTRA is non-nil, emit it as an ANSI code in the ANSI sequence."
  (while
      (progn
        (e2ansi-with-ansi-sequence
         (e2ansi-emit-color-ansi-sequence from ground-mode)
         (dolist (arg extra)
           (e2ansi-emit-ansi-code arg)))
        (princ (if (< from 16)
                   (car (nth from e2ansi-colors))
                 (format "color-%d" from)))
        (e2ansi-with-ansi-sequence
         (e2ansi-emit-ansi-code "0"))
        (terpri)
        (setq from (+ from 1))
        (and to
             (< from to)))))


;;;###autoload
(defun e2ansi-test-show-example-text ()
  "Create a buffer with text with face information, to test the terminal."
  (interactive)
  (with-output-to-temp-buffer "*e2ansi example"
    (set-buffer standard-output)
    (princ "Examples on fontified text. This is the first line.")
    (terpri)
    (princ "(Should any output be printed above the first line, it is most")
    (terpri)
    (princ "likely generated by the Emacs init code.)")
    (terpri)
    (terpri)
    (princ "8 basic colors:")
    (terpri)
    (e2ansi-test-show-colors-between :foreground 0 7)
    (terpri)
    (princ "8 basic colors, bold:")
    (terpri)
    (e2ansi-test-show-colors-between :foreground 0 7 "1")
    (terpri)
    (princ "8 extra colors (not supported by all):")
    (terpri)
    (e2ansi-test-show-colors-between :foreground 8 15)
    (terpri)
    (princ "8 extra colors, bold (not supported by all):")
    (terpri)
    (e2ansi-test-show-colors-between :foreground 8 15 "1")
    (terpri)
    (princ "From the 256 color palette (not supported by all):")
    (terpri)
    (e2ansi-test-show-colors-between :foreground 30)
    (e2ansi-test-show-colors-between :foreground 50)
    (e2ansi-test-show-colors-between :foreground 100)
    (e2ansi-test-show-colors-between :foreground 180)
    (display-buffer (current-buffer))))



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


(deftheme e2ansi-test-theme2 "")

(custom-theme-set-faces 'e2ansi-test-theme2
                        '(e2ansi-test-theme-face1 ((t :background "blue")))
                        '(e2ansi-test-theme-face2 ((t :underline nil))))

(provide-theme 'e2ansi-test-theme2)

(deftheme e2ansi-test-theme3 "")

(custom-theme-set-faces 'e2ansi-test-theme3
                        '(e2ansi-test-theme-face1 ((t :background "magenta"
                                                      :underline t))))

(provide-theme 'e2ansi-test-theme3)

;; TODO: The :inherit property.


;; ----------------------------------------------------------------------
;; Ert test cases.
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


(provide 'e2ansi-test)

;;; e2ansi-test.el ends here
