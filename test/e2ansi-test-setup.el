;;; e2ansi-test-setup.el --- Setup and execute all tests.  -*- lexical-binding: t; -*-

;;; Commentary:

;; This package sets up a suitable enviroment for testing e2ansi, and
;; executes the tests.
;;
;; Usage:
;;
;;   emacs -Q -l e2ansi-test-setup.el
;;
;; Note that this package assumes that some packages are located in
;; specific locations.

;;; Code:

(setq inhibit-startup-screen t)

(defvar e2ansi-test-setup-directory
  (if load-file-name
      (file-name-directory load-file-name)
    default-directory))

(dolist (dir '("." ".." "../../faceup" "../../face-explorer"))
  (add-to-list 'load-path (concat e2ansi-test-setup-directory dir)))

(require 'e2ansi)
(require 'e2ansi-test-basic)
(require 'e2ansi-test-files)

(if noninteractive
    (ert-run-tests-batch-and-exit)
  (ert t))

;;; e2ansi-test-setup.el ends here
