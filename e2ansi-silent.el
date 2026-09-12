;;; e2ansi-silent.el --- Load in batch mode to silence some messages.  -*- lexical-binding: t; -*-

;; Copyright (C) 2014 Anders Lindgren

;; Author: Anders Lindgren
;; Keywords: faces, languages
;; Created: 2014-12-29
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

;; In batch mode, load this file to silence messages from `message'
;; and `load'.
;;
;; For example:
;;
;;     emacs --batch -l e2ansi-silent.el -l another-file.el
;;
;;
;; Normally, the site init file is loaded before any file specified on
;; the command line. To silence messages in the system init file, you
;; can suppress loading the system init file, load this file, and then
;; load the system init file.
;;
;;     emacs -Q --batch -l e2ansi-silent.el -l ../site-lisp/site-start.el ...

;; Variables:
;;
;; * `e2ansi-silent-message'. When non-nil, messages are suppressed.
;;   To force messages to be emitted, bind this to nil.

;; Note:
;;
;; Even though this file is distributed with the `e2ansi' package, it
;; is generic. In the future it might be better if it could be
;; distributed together with batch-related tools or (even better)
;; built into Emacs itself.

;;; Code:

(defvar e2ansi-silent-message t
  "When non-nil, messages from `message' and `load' are suppressed.")


;; ----------------------------------------
;; Silence `message'
;;

(defun e2ansi-silent--maybe-silence-message (orig-fun &rest args)
  "Call ORIG-FUN (`message') with ARGS, if `e2ansi-silent-message' is nil.

When `e2ansi-silent-message' is non-nil, no message is emitted.

Either way, return the formatted string, like `message' normally do.

This is used as an advice on the `message' function."
  (apply (if e2ansi-silent-message
             #'format-message
           orig-fun)
         args))

(advice-add 'message :around #'e2ansi-silent--maybe-silence-message)


;; ----------------------------------------
;; Silence `load'
;;

(defun e2ansi-silent--maybe-silence-load (orig-fun file
                                                   &optional noerror nomessage
                                                   &rest args)
  "Call ORIG-FUN (`load') with FILE NOERROR NOMESSAGE and ARGS.

When `e2ansi-silent-message' is non-nil, NOMESSAGE is set to t,
silencing the load.

This is used as an advice on the `load' function."
  (when e2ansi-silent-message
    (setq nomessage t))
  (apply orig-fun file noerror nomessage args))

(advice-add 'load :around #'e2ansi-silent--maybe-silence-load)


;; Local Variables:
;; no-byte-compile: t
;; End:

(provide 'e2ansi-silent)

;;; e2ansi-silent.el ends here.
