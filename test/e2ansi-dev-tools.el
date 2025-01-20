;;; e2ansi-dev-tools.el --- Devlopment tools for `e2ansi'  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Anders Lindgren

;; Author: Anders Lindgren
;; Keywords: faces, terminals

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

;; Small tools for developing `e2ansi' and ANSI eacape codes.
;;
;; * `e2ansi-test-list-test-faces'
;;
;; * `e2ansi-test-list-ansi-colors'
;;
;; * `e2ansi-test-list-collected-faces'
;;
;; * `e2ansi-test-show-example-text'

;;; Code:

(require 'e2ansi)


;; --------------------------------------------------
;; Tests for mixing attributes
;;

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
  "Display a buffer with text that mix bold, italics, and underline.

This buffer can be used as test input to e2ansi."
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
                 p (point) 'face 'e2ansi-test-underline)))
            (insert (format "    Weight: %-10s Slant: %-20s Underline: %s\n"
                            weight
                            slant
                            (if underline "Yes" "No"))))))
      (insert "\n")
      (message "%S" (buffer-substring (point-min) (point-max))))
    standard-output))


(defun e2ansi-test-batch ()
  (let ((buffer (e2ansi-test-list-test-faces)))
    (with-current-buffer buffer
      (e2ansi-write-file "e2ansi-test.ansi"))))


;; --------------------------------------------------
;; List ansi colors
;;

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
  "Display buffer with the ANSI 256 colors."
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


;; --------------------------------------------------
;; List closest color
;;

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
            (when (and (or (memq (nth 0 entry) '(t default))
                           (member key (nth 0 entry)))
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
   (list (read-number "Number of colors: " 256)
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
        (insert (format "with a %s background" background)))
      (insert ".\n\n")
      (e2ansi-test-list-closest-color
       (e2ansi-test-collect-all-face-colors
        number-of-colors ground-mode background))
      (set-buffer-modified-p nil)
      (setq truncate-lines t))))


;; ----------------------------------------
;; Example ANSI output
;;

(defun e2ansi-test-show-colors-between (ground-mode from
                                                    &optional to &rest extra)
  "Insert examples of ANSI colors from color FROM to color TO.

If EXTRA is non-nil, emit it as an ANSI code in the ANSI sequence."
  (while
      (progn
        (e2ansi-with-ansi-sequence
            standard-output
         (e2ansi-emit-color-ansi-sequence from ground-mode)
         (dolist (arg extra)
           (e2ansi-emit-ansi-code arg)))
        (princ (if (< from 16)
                   (car (nth from e2ansi-colors))
                 (format "color-%d" from)))
        (e2ansi-with-ansi-sequence
            standard-output
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


(provide 'e2ansi-dev-tools)
;;; e2ansi-dev-tools.el ends here
