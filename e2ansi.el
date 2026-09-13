;;; e2ansi.el --- Syntax highlighting for `less', powered by Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2014,2015,2017,2025,2026 Anders Lindgren

;; Author: Anders Lindgren
;; Keywords: faces, languages
;; Created: 2014-12-07
;; Version: 0.2.2
;; Package-Requires: ((emacs "25.1") (face-explorer "0.0.7"))
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

;; *e2ansi* (Emacs to ANSI) converts a text highlighted by Emacs to a
;; text with ANSI escape codes, which can be displayed in a terminal
;; window, with the highlighting visible.
;;
;; The `e2ansi-cat' command line tool can be used to highlight text
;; directly in the terminal.  The actual syntax highlighting is
;; performed by Emacs running in batch mode.
;;
;; Pager applications like `more' and `less' can be configured to
;; automatically invoke `e2ansi-cat', so that all viewed files will be
;; syntax highlighted.  A nice side effect is that other conversions
;; that Emacs normally performs, like uncompressing files, are also
;; automatically applied.
;;
;; This package can highlight everything that Emacs supports,
;; inclduing many programming and markup languages.  Since Emacs is
;; extensible you can use third-party packages or implement support
;; for your own language formats.
;;
;; Example:
;;
;; | Before                     | After                         |
;; | ------                     | -----                         |
;; | ![](doc/no_color_dark.png) | ![](doc/default_dark_256.png) |

;; Quick install:
;;
;; Install the `e2ansi' package using the Emacs package manager.  This
;; installs the main package, all dependencies, and a number of
;; command-line tools like `e2ansi-cat'.
;;
;; Optionally create an e2ansi-specific init file,
;; e.g. `~/.e2ansi'. (See below.)

;; Integration with `less':
;;
;; The pager applications `more' and `less' can be configured to use
;; `e2ansi-cat' to highlight viewed files.  This is done by defining
;; the `LESSOPEN' environment variable.
;;
;; Make sure that the `emacs' command is in the path.
;;
;; The Emacs package manager includes the version number in the
;; installation location of `e2ansi'.  This, unfortunately, means that
;; the shell configuration must be updated every time `e2ansi' is
;; updated.
;;
;; Semi-automatic setup:
;;
;; Run the Emacs command `e2ansi-display-shell-setup' and copy
;; relevant lines to a suitable shell init file, like
;; `~/.bashrc'.  (The output assumes that a bash-compatible shell is
;; used.  The syntax might need to be adjusted for other shells.)
;;
;; Manual setup:
;;
;; Define the `LESSOPEN', `LESS', and `MORE' environment variables in
;; your shell init file.  For example (replacing `PATH-TO-E2ANSI' with
;; the location of the `e2ansi' module):
;;
;;     export "LESSOPEN=||-PATH-TO-E2ANSI/bin/e2ansi-cat %s"
;;     export "LESS=-R"
;;     export "MORE=-R"
;;
;; In the `LESSOPEN' environment variable `%s' is the placeholder for
;; the file to highlight and the `||-' prefix says that the script can
;; work in a pipe.
;;
;; The `-R' option in the `LESS' and `MORE' environment variables
;; tells the tools to send ANSI sequences to the terminal.

;; The *e2ansi* init file:
;;
;; When using Emacs in batch mode, Emacs does not read the user init
;; file.  However, for *e2ansi*, it is often desirable to load the
;; user init files, for example, to configure font-lock settings and
;; add additional packages.
;;
;; When the command line tools `e2ansi-cat' and `e2ansi-info' are
;; launched, they try to load the init files `.e2ansi' and
;; `e2ansi-init.el' from the following locations:
;;
;; * The user home directory.
;;
;; * The `emacs' XDG config directory (typically `~/.config/emacs').
;;
;; * The user Emacs directory (typically `~/.emacs.d').
;;
;; This file can include configuration specific to *e2ansi*, or it can
;; load the normal user init files.  This is a good place to specify
;; display properties such as the background mode.
;;
;; For example:
;;
;;     ;;; .e2ansi --- Init file for e2ansi.  -*- emacs-lisp -*-
;;     (require 'e2ansi-silent)
;;     (require 'e2ansi-magic)
;;     (setq face-explorer-background-mode 'dark)
;;     (load "~/.emacs" nil t)
;;     ;;; .e2ansi ends here
;;
;; Adapting init files to batch mode:
;;
;; As Emacs most often is used in interactive mode there is a risk
;; that parts of the system or your init file doesn't work in batch
;; mode.
;;
;; To exclude something when in batch mode, you can use:
;;
;;     (unless noninteractive
;;       .. code not suitable for batch mode code goes here ... )

;; The *e2ansi* modules:
;;
;; * `bin/e2ansi-cat' -- Command line tool to add highlighting a file
;;   using ANSI escape codes.
;;
;; * `bin/e2ansi-info' -- Command line tool to display various
;;   ANSI-related information.
;;
;; * `e2ansi.el' -- The rendering engine for ANSI escape codes.
;;
;; * `e2ansi-magic.el' -- Set up `magic-mode-alist' to recognize file
;;   formats based on the content of files.  This is useful when using
;;   `less' in pipes where Emacs can't use the file name extension to
;;   select a suitable major mode.
;;
;; * `e2ansi-silent.el' -- Load this in batch mode to silence some
;;   messages from init files.
;;
;; * `e2ansi-load-init.el' -- Support module for the command line
;;   tools to load the *e2ansi* init file.

;; The `e2ansi-cat' command line tool:
;;
;; Syntax:
;;
;;     e2ansi-cat [OPTION ...] file ...
;;
;; If the name of the file is `-', the input is read from standard
;; input, so that the tool can be used in pipes:
;;
;;     diff alpha.txt beta.txt | e2ansi-cat -
;;
;; Options:
;;
;; * `--theme' -- Specify the Emacs color theme to use.
;;
;; * `--usage' or `--help' -- Show help text.  (Note: Unless `--help'
;;   is preceded by `--', Emacs will display its own help text.)
;;
;; Options for display properties:
;;
;; * `--background-mode' -- Specify `light' or `dark' background mode.
;;
;; * `--colors' -- Number of colors, or `rgb24' for full 24 bit
;;   colors.  This is both used when parsing the `min-colors'
;;   requirement in face definitions (c.f.  `defface') and when
;;   deciding the kind of ANSI escape codes that is used.
;;
;; * `--color-class' -- Specify one of the `color', `grayscale' or
;;   `mono' face specification requirement (c.f.  `defface').

;; The `e2ansi-info' command line tool:
;;
;; Syntax:
;;
;;     e2ansi-info WHAT [OPTIONS]
;;
;; OPTIONS are the same as accepted by `e2ansi-cat'.
;;
;; Where WHAT can be:
;;
;; * `settings' -- Print the terminal setting of e2ansi.
;;
;; * `ansi16' -- Print a color table with the 16 basic colors.
;;
;; * `ansi256' -- Print a color table with the 256 color palette.
;;
;; * `faces' -- Print a selection of standard faces.
;;
;; * `mbg' -- Print text with background spanning multiple lines.

;; Launching Emacs script:
;;
;; In some cases it's not possible to launch Emacs command line tools
;; `e2ansi-cat' directly, for example when using MS-Windows.
;;
;; Instead, `emacs' can be used in match mode, for example:
;;
;;     emacs --batch -l PATH-TO-E2ANSI/bin/e2ansi-cat
;;
;; Additional Emacs options, like `-Q' (suppress the site init file)
;; can be specified.

;; The *e2ansi* Emacs module:
;;
;; Emacs commands:
;;
;; * `e2ansi-write-file' -- Generate a file with ANSI escape codes.
;;
;; * `e2ansi-view-buffer' -- Display the content of the buffer, with
;;   ANSI escape codes.  (Typically, this doesn't look good, but it is
;;   useful to see which ANSI escape codes are generated.)
;;
;; Emacs functions and macros:
;;
;; * `e2ansi-string-to-ansi' -- Convert a string with Emacs faces to a
;;   string with ANSI escape codes.
;;
;; * `e2ansi-with-fictitious-display-as-terminal' -- Call block with
;;   the face-explorer fictitious display macting the terminal.

;; The `face-explorer' library:
;;
;; In batch mode, Emacs natively doesn't provide face attributes.
;; Instead, *e2ansi* uses the `face-explorer' library to deduce the
;; properties of faces, based on the underlying face definitions.
;;
;; The following variables controls the display environment that
;; `face-explorer' uses.  The variables can, for example, be set using
;; *e2ansi* command line options or in the *e2ansi* or the Emacs init
;; file.
;;
;; Each variable corresponds to a display property in face
;; specifications (see `defface').
;;
;; * `face-explorer-background-mode' -- `light' or `dark'.  This
;;   corresponds to the `background' display property.
;;
;; * `face-explorer-number-of-colors' -- Number of colors, e.g. 8, 16,
;;   256, or t. Corresponds to the `min-color' display property.  This
;;   also is used to decide the kind of ANSI escape codes to use.
;;
;; * `face-explorer-color-class' -- `color', `grayscale', or
;;   `mono'.  This corresponds to the `class' display property.
;;
;; * `face-explorer-window-system-type' -- The window system
;;   used.  This can be a symbol, a list of symbols, or t to match any
;;   type.  Corresponds to the `type' display property.

;; Background:
;;
;; What is Emacs?:
;;
;; Emacs is a the mother of all text editors.  It originates from the
;; 1970:s, but is still in active development.  It runs under all major
;; operating systems, including MS-Windows, macOS, and various
;; UNIX-like systems like Linux.  You can use normal windows, run it in
;; a terminal window (great when working remotely), or use it to run
;; scripts in batch mode, which is how it is used by the command line
;; tools provided by this package.
;;
;; Emacs provides state-of-the-art syntax highlighting.
;;
;; Why use Emacs to power syntax highlighting in the terminal?:
;;
;; There are many advantages:
;;
;; * Emacs has support for a vast range of programming languages and
;;   other structured text formats.  Many are provided by the basic
;;   Emacs distribution, others can be installed as separate packages.
;;
;; * Emacs is fast and accurate -- it is designed for interactive use,
;;   and provides advanced support for parsing programming languages
;;   and other structured text.
;;
;; * Emacs supports color themes.  If you don't like the ones provided,
;;   and can't find one on internet, you can easily write your own.
;;
;; * Emacs is *extensible*.  You can add an Emacs *major mode* for any
;;   structured format, or you can add a *minor mode* that can be used
;;   together with existing major modes.  Syntax highlighting in Emacs
;;   is typically provided by *Font Lock rules*, which can range from
;;   using simple pattern matching to very complex code.

;; ANSI escape codes:
;;
;; ANSI escape codes, formally known as ISO/IEC 6429, is a system used
;; by various physical terminals and console programs to, for example,
;; to add colors and attributes such as bold and italics to text.
;;
;; See [Wikipedia](http://en.wikipedia.org/wiki/ANSI_escape_code) for
;; more information.
;;
;; Colors:
;;
;; Both foreground and background colors can be rendered.  Note that
;; faces with the same background as the default face is not rendered
;; with an explicit background color.
;;
;; Four modes are supported:
;;
;; * 8 -- The eight basic ANSI colors.
;;
;; * 16 -- The eight basic colors, plus 8 "bright" colors.  These are
;;   represented as "bold" versions of the above.
;;
;; * 256 -- Some modern terminal programs support a larger palette.
;;   This consist of the 16 basic colors, a 6*6*6 color cube plus a
;;   grayscale.
;;
;; * 24 bit -- A palette with 256*256*256 colors.
;;
;; Attributes:
;;
;; * Bold
;;
;; * Italics
;;
;; * Underline

;; Co-existing with `lesspipe':
;;
;; There are other applications that use the `LESSOPEN' environment
;; variable, most notably [lesspipe](https://lesspipe.org).
;;
;; As of version 2.25 of *lesspipe*, it can be configured to used
;; *e2ansi* to highlight source files, while other features of
;; lesspipe are retained.  Simply set the environment variable
;; `LESSCOLORIZER' to `e2ansi-cat' to accomplish this.

;; More about `less':
;;
;; The pager application `less' is preinstalled on most systems.  If
;; it is missing or outdated on your system it's easy to download and
;; build a new version from http://www.greenwoodsoftware.com/less
;;
;; The document [LessWindows](doc/LessWindows.md) describes how to
;; build `less' on Windows using `cmake', a modern build system.

;; Gallery:
;;
;; All images are screen captures of `less' running in a terminal
;; window.  White or black backgrounds were used, even though some
;; themes have other backgrounds, when used inside Emacs.
;;
;; Default 8 colors:
;;
;; | Light                        | Dark                        |
;; | ------                       | -----                       |
;; | ![](doc/default_light_8.png) | ![](doc/default_dark_8.png) |
;;
;; Default 256 colors:
;;
;; | Light                          | Dark                          |
;; | ------                         | -----                         |
;; | ![](doc/default_light_256.png) | ![](doc/default_dark_256.png) |
;;
;; Grayscale 256 colors:
;;
;; | Light                            | Dark                            |
;; | ------                           | -----                           |
;; | ![](doc/grayscale_light_256.png) | ![](doc/grayscale_dark_256.png) |
;;
;; Selected themes:
;;
;; The following themes are included in the Emacs distribution.
;;
;; | Tango                | Tsdh light              |
;; | ------               | -----                   |
;; | ![](doc/tango.png)   | ![](doc/tsdh-light.png) |
;;
;; | Adwaita              | Misterioso              |
;; | ------               | -----                   |
;; | ![](doc/adwaita.png) | ![](doc/misterioso.png) |
;;
;;

;;; Code:

(require 'face-explorer)


(defvar e2ansi-use-window-system-color-values nil
  "When non-nil, use the current window system opinion of color values.

When nil, or when in batch mode, color values are based on
information in `color-name-rgb-alist'.

The advantage of using the window system color values is that the
end result will be slightly more alike the colors used in an
interactive Emacs.  The disadvantage is that the result might be
different than when generated in batch mode." )


(defvar e2ansi-line-by-line t
  "When non-nil, extra ANSI codes are emitted at the start of each line.

The advantage with this is that the output is highlighted correctly
even if only parts of it is printed to a terminal.  In addition,
pager applications like `more' and `less' don't lose highlighting
when scrolling.")


(defvar e2ansi-colors '(("black"         .  0)
                        ("red"           .  1)
                        ("green"         .  2)
                        ("yellow"        .  3)
                        ("blue"          .  4)
                        ("magenta"       .  5)
                        ("cyan"          .  6)
                        ("white"         .  7)
                        ("brightblack"   .  8)
                        ("brightred"     .  9)
                        ("brightgreen"   . 10)
                        ("brightyellow"  . 11)
                        ("brightblue"    . 12)
                        ("brightmagenta" . 13)
                        ("brightcyan"    . 14)
                        ("brightwhite"   . 15))
  "Alist of basic ANSI colors and the color number.")


;; In 8 color mode, all built-in Emacs types use the basic ANSI colors,
;; or a suitable one is found using the standard algorithm. In 256
;; color mode, there are plenty of colors to choose from. The problem
;; is the 16 color mode, where the faces are specified in a variety of
;; colors even though only a handful is available.
;;
;; Note: The main intention of this is to ensure that the default
;; Emacs setting in 16 color mode look OK:ish. If you really would
;; like to tweak how faces are rendered, I would suggest writing a
;; custom theme.
(defvar e2ansi-color-override-alist
  '(((16 light) . (("rosybrown" . "red"))))
  "Alist of colors that should be rendered in ANSI using another color.

The key in the list is a list on the form (NUMBER-OF-COLORS BACKGROUND-MODE)
and the value is an alist with a mapping from color to color.")


(defvar e2ansi-null-ansi-state '(normal normal normal normal normal)
  "Ansi state representing normal text.

See `e2ansi-ansi-state' for details on ansi states.")


;; -------------------------------------------------------------------
;; Utilities
;;

(defun e2ansi-user-error (format &rest args)
  "Like `user-error' but don't emit a backtrace in batch mode.

FORMAT is a format string and ARGS arguments to format."
  (let ((backtrace-on-error-noninteractive nil))
    (apply #'user-error (concat "e2ansi: " format) args)))


(defun e2ansi-format-message (string &rest objects)
  "Call `format-message' with STRING and OBJECTS.

This is used by `e2ansi-with-silent-messages' to override
`message' when messages are silenced."
  (apply #'format-message string objects))


;; This is used to silence major modes like `sh-mode' that emits
;; various messages when started.
(defmacro e2ansi-with-silent-messages (&rest body)
  "Execute BODY just like `progn' and silence calls to `message'."
  `(if (advice-member-p #'e2ansi-format-message 'message)
       ;; Protect against nested `e2ansi-with-silent-messages'.
       (progn ,@body)
     (advice-add 'message :override #'e2ansi-format-message)
     (unwind-protect
         (progn ,@body)
       (advice-remove 'message #'e2ansi-format-message))))


;; ----------------------------------------------------------------------
;; Commands
;;

;;;###autoload
(defun e2ansi-view-buffer ()
  "Display the e2ansi representation of the selected buffer."
  (interactive)
  (let ((buffer (get-buffer-create "*e2ansi*")))
    (with-current-buffer buffer
      (delete-region (point-min) (point-max)))
    (e2ansi-print-buffer (current-buffer) buffer)
    (display-buffer buffer)))


;;;###autoload
(defun e2ansi-write-file (&optional file-name confirm)
  "Save the e2ansi representation of the current buffer to the file FILE-NAME.

Unless a name is given, the file will be named xxx.ansi, where
xxx is the file name associated with the buffer.

If CONFIRM is non-nil, ask for confirmation before overwriting an
existing file.  Interactively, confirmation is required unless you
supply a prefix argument."
  (interactive
   (let ((suggested-name (and (buffer-file-name)
                              (concat (buffer-file-name)
                                      ".ansi"))))
     (list (read-file-name "Write ANSI file: "
                           default-directory
                           suggested-name
                           nil
                           (file-name-nondirectory suggested-name))
           (not current-prefix-arg))))
  (unless file-name
    (setq file-name (concat (buffer-file-name) ".ansi")))
  (let ((buffer (current-buffer)))
    (with-temp-buffer
      (e2ansi-print-buffer buffer (current-buffer))
      ;; Note: Must set `require-final-newline' inside
      ;; `with-temp-buffer', otherwise the value will be overridden by
      ;; the buffers local value.
      ;;
      ;; Clear `window-size-change-functions' as a workaround for
      ;; Emacs bug#19576 (`write-file' saves the wrong buffer if a
      ;; function in the list change current buffer).
      (let ((require-final-newline nil)
            (window-size-change-functions '()))
        (write-file file-name confirm)))))


;; ----------------------------------------
;; Shell installation support.
;;

(defvar e2ansi-directory
  (if load-file-name
      (file-name-directory load-file-name)
    default-directory)
  "The directory of the e2ansi package.")


(defun e2ansi-simplify-filename (filename)
  "Return FILENAME, or a simplified version of it.

Replace the path to the home directory with `~'."
  (let ((home (getenv "HOME")))
    (if home
        (let ((home-re (concat "^" (regexp-quote home) "/")))
          (if (string-match home-re filename)
              (setq filename (replace-match "~/" nil nil filename 0))))))
  filename)


(defun e2ansi-emit-comment (lines)
  "Insert LINES as a comment preceeded by a # character."
  (dolist (line (split-string lines "\n"))
    (insert "# " line "\n"))
  (insert "#\n"))

;;;###autoload
(defun e2ansi-display-shell-setup ()
  "Display a typical bash environment variable setup for `less'."
  (interactive)
  (let ((buf (get-buffer-create "*e2ansi-bash*")))
    (with-current-buffer buf
      (sh-mode)
      ;; Silence "Warning: the function 'sh-set-shell' might not be
      ;; defined at runtime."
      (with-no-warnings
        (sh-set-shell "bash" t nil))
      (erase-buffer)
      (let ((emacs-cmd (or (car-safe command-line-args)
                           "emacs")))
        (e2ansi-emit-comment "\
---------------------------------------------------
Use the Emacs package `e2ansi' to highlight `more' and `less'.")
        (e2ansi-emit-comment "\
The `more' and `less' command line tools read the environment
variable `LESSOPEN'. This should contain an application that (in
one way or another) modifies the viewed filed. This is used by
`e2ansi' to add ANSI sequences to highlight the file.")
        (e2ansi-emit-comment "\
The code below silences messages before loading the user and
site init code.")
        (e2ansi-emit-comment "\
Note: This reflects the state of the system when this snippet was
generated. If that should change (e.g. if `e2ansi' is updated),
please regenerate it with `M-x e2ansi-display-shell-setup'.")
        (insert "\n")
        (e2ansi-emit-comment "\
--------------------
Alternative 1:")
        (insert "export \"LESSOPEN=||-")
        (insert (e2ansi-simplify-filename e2ansi-directory))
        (insert "bin/e2ansi-cat %s\"\n")
        (insert "\n")
        (insert "\n")
        (e2ansi-emit-comment "\
--------------------
Alternative 2: Use this when UNIX-style scripts can't be
executed directly, for example in MS-Windows.")
        (insert "\n")
        (insert "export \"LESSOPEN=||-")
        (insert emacs-cmd)
        (insert " --batch")
        (insert " -Q")
        (insert " -l ")
        (insert (e2ansi-simplify-filename e2ansi-directory))
        (insert "bin/e2ansi-cat %s\"\n")
        (insert "\n")
        (insert "\n")
        (insert "# --------------------\n")
        (insert "# Command line options to `more' and `less'.\n")
        (insert "#\n")
        (insert "# -R -- Emit raw bytes (needed to display ANSI sequences).\n")
        (insert "\n")
        (insert "export \"LESS=-R\"\n")
        (insert "export \"MORE=-R\"\n"))
      (view-mode)
      (goto-char (point-min)))
    (display-buffer buf)))



;; ----------------------------------------------------------------------
;; Dedcue from terminal
;;


(defun e2ansi-terminal-fg-and-bg-color-numbers ()
  "The color numbers used for the foreground and background.

Return (FG . BG) where FG and BG are the ANSI color numbers of the
foreground and background, respectively.

The values are retrieved from the COLORFGBG environment variable.  When
this variable is undefined the foreground is assumed to be black (0) and
the background white (7)."
  (let ((color-fgbg (getenv "COLORFGBG"))
        ;; Defaults to black text on white background.
        (res '(0 . 7)))
    (when color-fgbg
      (if (string-match "\\`\\([0-9]+\\);\\([0-9]+\\)\\'" color-fgbg)
          (let ((fg (string-to-number (match-string 1 color-fgbg)))
                (bg (string-to-number (match-string 2 color-fgbg))))
            ;; Typically, this is a number in the range of 0 to 15.
            ;; However, the limit is selected to match the range of
            ;; `e2ansi-ansi-color-values'.
            (when (and (<= fg 255)
                       (<= bg 255))
              (setq res (cons fg bg))))))
    res))


(defun e2ansi-terminal-background-mode ()
  "Determine the background mode of the terminal.

See `e2ansi-terminal-fg-and-bg-color-numbers'."
  (let ((pair (e2ansi-terminal-fg-and-bg-color-numbers)))
    (if (< (e2ansi-ansi-color-brightness (car pair))  ; Foreground
           (e2ansi-ansi-color-brightness (cdr pair))) ; Background
        'light
      'dark)))


(defun e2ansi-terminal-number-of-colors ()
  "Estimate the number of colors a terminal supports.

Return the number of colors, or t if the terminal supports full
24-bit colors."
  (let ((term (getenv "TERM")))
    (if term
        (cond ((string-match "-256color\\'" term) 256)
              ;; Full RGB support.
              ((string-match "-direct\\'" term) t)
              (t 8))
      8)))


(defun e2ansi-set-fictitious-display-as-terminal ()
  "Set the face-explorer fictitious display to match the terminal."
  (setq face-explorer-number-of-colors
        (e2ansi-terminal-number-of-colors))
  (setq face-explorer-window-system-type 'tty)
  (setq face-explorer-color-class 'color)
  (setq face-explorer-background-mode
        (e2ansi-terminal-background-mode))
  (setq face-explorer-match-supports-function
        #'face-explorer-default-match-supports-function))


(defmacro e2ansi-with-fictitious-display-as-terminal (&rest body)
  "Run BODY with a face-explorer fictitious display to match the terminal."
  `(face-explorer-with-fictitious-display
    (e2ansi-set-fictitious-display-as-terminal)
    ,@body))



;; ----------------------------------------------------------------------
;; Batch support
;;

;; Note: Emacs itself handles all command line arguments it
;; recognizes, including the "--help" option. This makes life
;; unnecessary hard for stand-alone scripts written in Emacs lisp.
;;
;; A better solution would be to leave all options after --script
;; for the script to parse.


(defvar e2ansi-batch-major-mode-name nil
  "The argument passed to the --mode command line parameter.")

(defvar e2ansi-batch-help-text
  "The e2ansi package renders syntax highlighted Emacs buffers using
ANSI escape sequences."
  "Help text presented by batch commands.")

(defvar e2ansi-batch-options
  '(("--colors" :arg
     (lambda (arg)
       (setq face-explorer-number-of-colors
             (cond ((string= arg "rgb24") t)
                   ((string-match "^[0-9]+$" arg) (string-to-number arg))
                   (t (e2ansi-user-error "\
Incorrect argument to --colors: \"%s\"" arg)))))
     "Number of colors (e.g. 8, 16, or 256), or \"rgb24\" for 24 bit colors")
    ("--background-mode" :arg
     (lambda (arg)
       (let ((background-mode (intern arg)))
         (unless (member background-mode '(light dark))
           (e2ansi-user-error "Incorrect background mode: \"%s\"" arg))
         (setq face-explorer-background-mode background-mode)))
     "Background mode, \"light\" (default) or \"dark\"")
    ("--color-class" :arg
     (lambda (arg)
       (let ((color-class (intern arg)))
         (unless (member color-class '(color grayscale mono))
           (e2ansi-user-error "Incorrect argument to --color-class: \"%s\""
                              arg))
         (setq face-explorer-color-class color-class)))
     "Color class, \"color\" (default), \"grayscale\", or \"mono\"")
    ("--mode" :arg
     (lambda (arg)
       (setq e2ansi-batch-major-mode-name arg))
     "Use major mode")
    ("--theme" :arg
     (lambda (arg)
       (condition-case nil
           (load-theme (intern arg) t)
         (error
          (e2ansi-user-error "Failed to load theme \"%s\"" arg))))
     "Load custom color theme")
    ("--usage" :none
     (lambda ()
       (e2ansi-batch-usage #'princ))
     "Display this help text")
    ;; Emacs quirk:
    ;;
    ;; "--help" => Emacs display its help text.
    ;; "-- --help" => Emacs gives us both "--" and "--help".
    ("--" :none
     (lambda ()))
    ;; Only works when preceeded by `--'.
    ("--help" :none
     (lambda ()
       (e2ansi-batch-usage #'princ))
     "Display this help text"))
  "Command line options used in batch mode.")


(defun e2ansi-batch-usage (&optional function)
  "Print usage information using FUNCTION.

When FUNCTION is nil, `user-error' is used.

In batch mode `princ' prints on stdout, `message' and
`user-error' on stdout.  In addition `user-error' terminates the
process with an error code."
  ;; Silence the compiler in case `e2ansi-silent' hasn't been loaded.
  (defvar e2ansi-silent-message)
  ;; Ensure output is seen even when `e2ansi-silent.el' was loaded
  ;; by the user.
  (let ((e2ansi-silent-message nil)
        (backtrace-on-error-noninteractive nil))
    (funcall (or function #'user-error)
     (let ((widest 0))
       (dolist (opt e2ansi-batch-options)
         (setq widest (max widest (length (nth 0 opt)))))
       (let ((s (concat e2ansi-batch-help-text
                        "\n\n"
                        "Available options:\n\n")))
         (dolist (opt e2ansi-batch-options)
           (if (nth 3 opt)
               (setq s (concat s
                               (nth 0 opt)
                               (make-string (+ (- widest (length (nth 0 opt)))
                                               1)
                                            32) ; 32 = ASCII code of space
                               (nth 3 opt)
                               "\n"))))
         s)))))


(defun e2ansi-batch-parse-options ()
  "Parse batch command line options.

Issue an error if an illegal option is specified.

See `e2ansi-batch-options' for options."
  (while (and command-line-args-left
              ;; The "." ensures that a single "-" isn't matched.
              (string-match "^-." (car command-line-args-left)))
    (let* ((option (pop command-line-args-left))
           (desc (assoc option e2ansi-batch-options)))
      (if desc
          (let ((args '()))
            (if (eq (nth 1 desc) :arg)
                (if command-line-args-left
                    (push (pop command-line-args-left) args)
                  (e2ansi-user-error
                   "Missing argument for option: '%s'" option)))
            ;; Right now, only one argument is parsed, the reverse
            ;; is needed in case we parse more in the future.
            (apply (nth 2 desc) (reverse args)))
        (e2ansi-user-error "Unknown command line option: \"%s\"" option)))))


;;;###autoload
(defun e2ansi-batch-convert ()
  "Convert the remaining files on the command line to ANSI format."
  (e2ansi-with-fictitious-display-as-terminal
   (e2ansi-batch-parse-options)
   (while command-line-args-left
     (let ((source (pop command-line-args-left)))
       (if (string= source "-")
           (let ((buf (generate-new-buffer "*stdin*")))
             (set-buffer buf)
             (while (condition-case nil
                        (let ((s (read-string "")))
                          (insert s)
                          (insert "\n")
                          t)
                      (error nil)))
             ;; Help `normal-mode' to pick the right major mode.
             (let ((env (getenv "E2ANSI_FILE_NAME")))
               (when env
                 (setq buffer-file-name env)))
             (e2ansi-with-silent-messages
              (normal-mode)))
         (unless (file-exists-p source)
           (e2ansi-user-error "File not found: %s" source))
         (let ((large-file-warning-threshold nil))
           (e2ansi-with-silent-messages
            (find-file source)))))
     ;; Override major mode, if --mode was specified.
     (when e2ansi-batch-major-mode-name
       (let ((mode nil))
         (dolist (s1 (list e2ansi-batch-major-mode-name
                           (concat e2ansi-batch-major-mode-name "-mode")))
           (dolist (s2 (list s1 (downcase s1)))
             (let ((candidate (intern s2)))
               (when (fboundp candidate)
                 (setq mode candidate)))))
         (if mode
             (e2ansi-with-silent-messages
              (funcall mode))
           (e2ansi-user-error
            "Unknown major mode: \"%s\"" e2ansi-batch-major-mode-name))))
     (save-excursion
       (goto-char (point-min))
       ;; Don't highlight buffers containing existing ansi
       ;; sequences.
       (if (search-forward "\x1b[" (point-max) t)
           (princ (buffer-string))
         (let ((noninteractive nil))
           (font-lock-mode 1))
         (e2ansi-print-buffer (current-buffer)))))))


(defun e2ansi-batch-print-setting ()
  "Print the current e2ansi settings to standard output."
  (dolist (pair `(("Number of colors"   . ,face-explorer-number-of-colors)
                  ("Color class"        . ,face-explorer-color-class)
                  ("Backgrounod mode"   . ,face-explorer-background-mode)
                  ("Window system type" . ,face-explorer-window-system-type)))
    (princ (format "%20s: %s\n" (car pair) (cdr pair)))))


;; ----------------------------------------------------------------------
;; ANSI Colors
;;

;; The following ANSI variants are supported:
;;
;; * 8 basic colors.
;;
;; * 8 basic colors + 8 bright colors.
;;
;;   The bright colors are typically accessed by setting the bold
;;   attribute, and are only available for the foreground color. In
;;   some cases, not all colors appears to be available as bright
;;   (but we ignore this.)
;;
;; * 256 colors, consisting of the 8 + 8 basic colors, 6*6*6 = 216
;;   colors in a color cube, and 24 shades of gray. When a face has
;;   specified to use a named color, and that color is one of the
;;   basic ones, that color is used. Otherwise the one with the
;;   closest color value in the color cube or in the grayscale is
;;   picked. The ANSI escape sequences "ESC [ 38 ; 5 ; N m" and "48"
;;   are used.
;;
;; * 24 bit RGB (256 * 256 * 256). The exact 3*8 bit RGB color value
;;   is emitted using the "ESC [ 38 ; 2 ; R ; G ; B m" and "48"
;;   sequences.

(defun e2ansi-color-values (name)
  "Like `color-values' but work in batch mode as well.

NAME is the name of a color.

In batch mode, or when `e2ansi-use-window-system-color-values' is
nil, the color values are based on `color-name-rgb-alist'."
  (if (and (not noninteractive)
           e2ansi-use-window-system-color-values)
      (color-values name)
    (let ((entry (assoc name color-name-rgb-alist)))
      (setq name (tty-color-canonicalize name))
      (if entry
          (cdr entry)
        ;; Parse things like #ABCDEF
        (tty-color-standard-values name)))))


(defvar e2ansi-basic-color-values (list (e2ansi-color-values "#000000")
                                        (e2ansi-color-values "#cd0000")
                                        (e2ansi-color-values "#00cd00")
                                        (e2ansi-color-values "#cdcd00")
                                        (e2ansi-color-values "#0000ee")
                                        (e2ansi-color-values "#cd00cd")
                                        (e2ansi-color-values "#00cdcd")
                                        (e2ansi-color-values "#e5e5e5")
                                        (e2ansi-color-values "#7f7f7f")
                                        (e2ansi-color-values "#ff0000")
                                        (e2ansi-color-values "#00ff00")
                                        (e2ansi-color-values "#ffff00")
                                        (e2ansi-color-values "#5c5cff")
                                        (e2ansi-color-values "#ff00ff")
                                        (e2ansi-color-values "#00ffff")
                                        (e2ansi-color-values "#ffffff"))
  "Color values of basic ANSI colors.

Different terminal programs seem to use slightly different color
values, and they are often user configurable.  The color values
here correspond to the values used in xterm.")


(defvar e2ansi-color-cube-steps '(#x0000 #x5F00 #x8700 #xAF00 #xD700 #xFF00))
(defvar e2ansi-greyscale-colors (list (e2ansi-color-values "#080808")
                                      (e2ansi-color-values "#121212")
                                      (e2ansi-color-values "#1c1c1c")
                                      (e2ansi-color-values "#262626")
                                      (e2ansi-color-values "#303030")
                                      (e2ansi-color-values "#3a3a3a")
                                      (e2ansi-color-values "#444444")
                                      (e2ansi-color-values "#4e4e4e")
                                      (e2ansi-color-values "#585858")
                                      (e2ansi-color-values "#626262")
                                      (e2ansi-color-values "#6c6c6c")
                                      (e2ansi-color-values "#767676")
                                      (e2ansi-color-values "#808080")
                                      (e2ansi-color-values "#8a8a8a")
                                      (e2ansi-color-values "#949494")
                                      (e2ansi-color-values "#9e9e9e")
                                      (e2ansi-color-values "#a8a8a8")
                                      (e2ansi-color-values "#b2b2b2")
                                      (e2ansi-color-values "#bcbcbc")
                                      (e2ansi-color-values "#c6c6c6")
                                      (e2ansi-color-values "#d0d0d0")
                                      (e2ansi-color-values "#dadada")
                                      (e2ansi-color-values "#e4e4e4")
                                      (e2ansi-color-values "#eeeeee")))


(defun e2ansi-ansi-color-values (number)
  "The color values for ANSI color NUMBER."
  (cond ((< number 16)
         (nth number e2ansi-basic-color-values))
        ((< number #xE8)
         ;; Color.
         (let* ((color-index (- number 16))
                (r (% (/ color-index 36) 6))
                (g (% (/ color-index 6)  6))
                (b (% color-index        6)))
           (list (nth r e2ansi-color-cube-steps)
                 (nth g e2ansi-color-cube-steps)
                 (nth b e2ansi-color-cube-steps))))
        (t
         ;; Grayscale
         (nth (- number #xE8) e2ansi-greyscale-colors))))


(defun e2ansi-ansi-color-brightness (number)
  "The brightness of ANSI color NUMBER.

The brightness is the sum of all RGB components and is intended
to be used when comparing the brightness of two colors."
  (apply '+ (e2ansi-ansi-color-values number)))



(defun e2ansi-score-rgb-values (candidate-rgb wanted-rgb)
  "Return a value scoring how good CANDIDATE-RGB represents WANTED-RGB.
The lower the value, the better."
  ;; This calculates the squared distance between the two colors, in
  ;; the three-dimensional color cube. By returning the squared
  ;; distance, rather than the distance itself, a square root
  ;; operation is reduced.
  (let ((sum 0))
    (while candidate-rgb
      (let ((diff (- (pop candidate-rgb) (pop wanted-rgb))))
        (setq sum (+ sum (* (/ diff 256) (/ diff 256))))))
    sum))


(defun e2ansi-try-color-number (number ground-mode)
  "True, if color NUMBER is included when searching for the closest color.

NUMBER is less than `face-explorer-number-of-colors' (unless the
latter is t).  GROUND-MODE is either :foreground or :background."
  (cond ((or (eq face-explorer-number-of-colors t)
             (>= face-explorer-number-of-colors 256))
         ;; The color number of the basic 16 colors vary between
         ;; terminals, so they are not included when trying to find
         ;; the best matched color.
         (>= number 16))
        ((> face-explorer-number-of-colors 8)
         ;; In ANSI, 16 bit color mode provides 16 foreground colors
         ;; but only 8 backaground colors.
         (or (eq ground-mode :foreground)
             (< number 8)))
        (t
         t)))


(defvar e2ansi-closest-color-number-cache '())

(defun e2ansi-find-closest-color-number (name ground-mode)
  "Find the nearest ANSI color to color NAME, in the ANSI 256 palette.

If `face-explorer-number-of-colors' is at least 256, exclude the
basic 16 ANSI colors as their color values are not well defined.
GROUND-MODE is either :foreground or :background."
  (let* ((number-of-colors (if (or (eq face-explorer-number-of-colors t)
                                   (> face-explorer-number-of-colors 256))
                               256
                             face-explorer-number-of-colors))
         (key (list name ground-mode number-of-colors))
         (pair (assoc key e2ansi-closest-color-number-cache))
         (debug nil))
    (if pair
        (cdr pair)
      (let ((rgb-values (e2ansi-color-values name))
            (best-index nil)
            (i 0)
            best-score)
        (when debug
          (message "Target RGB: %s" rgb-values))
        (while (< i number-of-colors)
          (when (e2ansi-try-color-number i ground-mode)
            (let ((candidate-rgb-values (e2ansi-ansi-color-values i)))
              (let ((score (e2ansi-score-rgb-values candidate-rgb-values
                                                    rgb-values)))
                (when debug
                  (message "%d: Candidate RGB: %s score: %s"
                           i candidate-rgb-values score))
                (when (or (null best-index)
                          (< score best-score))
                  (when debug
                    (message "***"))
                  (setq best-index i)
                  (setq best-score score)))))
          (setq i (+ i 1)))
        (push (cons key best-index) e2ansi-closest-color-number-cache)
        best-index))))


(defun e2ansi-default-color (ground-mode &optional frame)
  "The name of the default color.

GROUND-MODE is either :foreground or :background.  Optional FRAME
is the frame to use."
  (let* ((is-background (eq ground-mode :background))
         (color (frame-parameter frame (if is-background
                                           'background-color
                                         'foreground-color))))
    (if (and color
             (not (string= color
                           (if is-background
                               "unspecified-bg"
                             "unspecified-fg"))))
        color
      ;; Batch mode:
      (if (eq (eq face-explorer-background-mode 'light)
              is-background)
          "white"
        "black"))))


(defun e2ansi-color-number (name ground-mode)
  "The ANSI color number, or the color values, that corresponds to NAME.

GROUND-MODE is either :foreground or :background."
  (and name
       (if (or (eq face-explorer-number-of-colors t)
               (> face-explorer-number-of-colors 256))
           (e2ansi-color-values name)
         (let ((pair (assoc name e2ansi-colors)))
           (cond (pair
                  (cdr pair))
                 ((string-match "^color-\\([0-9]+\\)$" name)
                  (string-to-number (match-string 1 name)))
                 (t
                  (e2ansi-find-closest-color-number name ground-mode)))))))


(defun e2ansi-color-number-or-normal (name ground-mode)
  "The color number, the color values, or `normal' of NAME.

Return nil when NAME is nil.

GROUND-MODE is either :foreground or :background."
  (and name
       (progn
         (setq name (tty-color-canonicalize name))
         (let ((override-entry (assoc (list face-explorer-number-of-colors
                                            face-explorer-background-mode)
                                      e2ansi-color-override-alist)))
           (when override-entry
             (let ((pair (assoc name (cdr override-entry))))
               (when pair
                 (setq name (cdr pair))))))
         (if (string= name (e2ansi-default-color ground-mode))
             'normal
           (e2ansi-color-number name ground-mode)))))


;; ----------------------------------------------------------------------
;; Output ANSI
;;

(defun e2ansi-ansi-state (faces)
  "The ansi state as a list that corresponds to FACES, a list of faces.
The list may also contain \"naked\" faces, i.e. property and value elements.

Ansi state is a list of (FOREGROUND BACKGROUND WEIGHT SLANT
UNDERLINE), where the FOREGROUND and BACKGROUND is the ANSI color
number, a color value, or nil, WEIGHT is `bold' `light', or
`normal'. SLANT is `normal' or `italic', and UNDERLINE is a
non-nil value or `normal'."
  (let ((spec
         (face-explorer-face-prop-attributes-for-fictitious-display faces)))
    (let ((weight (plist-get spec :weight))
          (slant  (plist-get spec :slant)))
      ;; Normalize weight.
      (cond ((memq weight '(semi-bold extra-bold ultra-bold))
             (setq weight 'bold))
            ((memq weight '(semi-light extra-light ultra-light))
             (setq weight 'light)))
      ;; Normalize slant
      (unless (memq slant '(nil normal))
        (setq slant 'italic))
      (let ((foreground (e2ansi-color-number-or-normal
                         (plist-get spec :foreground)
                         :foreground)))
        (when (and (eq face-explorer-number-of-colors 16)
                   (numberp foreground)
                   (>= foreground 8))
          (setq foreground (- foreground 8))
          (setq weight 'bold))
        (list
         (or foreground 'normal)
         (or (e2ansi-color-number-or-normal
              (plist-get spec :background)
              :background)
             'normal)
         (or weight 'normal)
         (or slant 'normal)
         (or (plist-get spec :underline)
             'normal))))))


(defvar e2ansi-seen-ansi-sequence nil)
(defvar e2ansi-with-ansi-sequence-destination nil)

(defmacro e2ansi-with-ansi-sequence (dest &rest body)
  "Create block where any number of ANSI codes could be emitted.
Evaluates BODY.  Emit one ANSI sequence consisting of all ANSI
codes passed to `e2ansi-emit-ansi-code'.

DEST is a valid destination to print to, typically a buffer."
  (declare (indent 1))
  `(let ((e2ansi-with-ansi-sequence-destination ,dest)
         (e2ansi-seen-ansi-sequence nil))
     ,@body
     (when e2ansi-seen-ansi-sequence
       (princ "m" ,dest))))


(defun e2ansi-print (start end &optional dest)
  "Print the text between START and END to DEST."
  (let ((s (buffer-substring-no-properties start end)))
    (princ s dest)))


(defun e2ansi-emit-ansi-code (code)
  "Emit the ANSI escape code CODE to `standard-output'.

This is assumed to be called from within the body of
`e2ansi-with-ansi-sequence'.  This ANSI escape code can be
combined using the semicolon ANSI syntax with other escape codes
emitted from the same block."
  (if e2ansi-seen-ansi-sequence
      (princ ";" e2ansi-with-ansi-sequence-destination)
    (princ "\x1b[" e2ansi-with-ansi-sequence-destination))
  (princ code e2ansi-with-ansi-sequence-destination)
  (setq e2ansi-seen-ansi-sequence t))


(defun e2ansi-emit-color-ansi-sequence (color-number-or-values ground-mode)
  "Emit ANSI sequence to set color COLOR-NUMBER-OR-VALUES.
GROUND-MODE is :foreground or :background."
  (let* ((is-foreground (eq ground-mode :foreground))
         (ground-code (if is-foreground "3" "4")))
    (cond ((or (null color-number-or-values)
               (eq color-number-or-values 'normal))
           ;; Reset to default color.
           (e2ansi-emit-ansi-code (if is-foreground "39" "49")))
          ((listp color-number-or-values)
           (e2ansi-emit-ansi-code (if is-foreground "38" "48"))
           (e2ansi-emit-ansi-code "2")
           (e2ansi-emit-ansi-code (number-to-string
                                   (/ (nth 0 color-number-or-values) 256)))
           (e2ansi-emit-ansi-code (number-to-string
                                   (/ (nth 1 color-number-or-values) 256)))
           (e2ansi-emit-ansi-code (number-to-string
                                   (/ (nth 2 color-number-or-values) 256))))
          ((< color-number-or-values 8)
           (e2ansi-emit-ansi-code (format "%s%d" ground-code
                                          color-number-or-values)))
          ((< color-number-or-values 16)
           (e2ansi-emit-ansi-code (format "%s8" ground-code))
           (e2ansi-emit-ansi-code "5")
           (e2ansi-emit-ansi-code (number-to-string color-number-or-values)))
          (t
           (e2ansi-emit-ansi-code (if is-foreground "38" "48"))
           (e2ansi-emit-ansi-code "5")  ; 256 color mode.
           (e2ansi-emit-ansi-code
            (number-to-string color-number-or-values))))))


(defun e2ansi-emit-ansi-sequences (old-state new-state force-reset dest)
  "Print ANSI sequence to go from OLD-STATE to NEW-STATE to DEST.

If FORCE-RESET is non-nil, don't assume that the output terminal
necessarily has emitted previous text."
  (when (or force-reset
            (not (equal old-state new-state)))
    (e2ansi-with-ansi-sequence dest
      (if (equal new-state e2ansi-null-ansi-state)
          ;; Reset all.
          (e2ansi-emit-ansi-code "0")
        ;; Weight (must be first, as it might issue a reset).
        (let ((old-weight (nth 2 old-state))
              (new-weight (nth 2 new-state)))
          (unless (eq new-weight old-weight)
            (when (or (eq new-weight 'normal)
                      (and (memq old-weight '(light bold))
                           (memq new-weight '(light bold))))
              ;; Some terminals don't understand "ESC [ 22 m". Instead
              ;; a full reset is performed.
              (e2ansi-emit-ansi-code "0")
              (setq old-state e2ansi-null-ansi-state))
            (cond ((eq new-weight 'bold)  (e2ansi-emit-ansi-code "1"))
                  ((eq new-weight 'light) (e2ansi-emit-ansi-code "2")))))
        ;; Foreground
        (let ((color-number-or-values (nth 0 new-state)))
          (when (not (equal (nth 0 old-state) color-number-or-values))
            (e2ansi-emit-color-ansi-sequence color-number-or-values
                                             :foreground)))
        ;; Background
        (let ((color-number-or-values (nth 1 new-state)))
          (when (not (equal (nth 1 old-state) color-number-or-values))
            (e2ansi-emit-color-ansi-sequence color-number-or-values
                                             :background)))
        ;; Italics
        (when (not (eq (nth 3 old-state) (nth 3 new-state)))
          (e2ansi-emit-ansi-code (if (nth 3 new-state) "3" "23")))
        ;; Underline
        (when (not (eq (nth 4 old-state) (nth 4 new-state)))
          (e2ansi-emit-ansi-code (if (eq (nth 4 new-state) t) "4" "24")))))))


(defun e2ansi-min (&rest args)
  "Like `min' but nil arguments in ARGS are ignored.

Return nil when applied to no non-nil arguments."
  (let ((res nil))
    (dolist (value args)
      (if res
          (when value
            (setq res (min res value)))
        (setq res value)))
    res))


;;;###autoload
(defun e2ansi-print-buffer (&optional buffer dest)
  "Convert content of BUFFER to ANSI and print to DEST."
  (with-current-buffer (or buffer (current-buffer))
    (ignore-errors
      (face-explorer-fontify-buffer))
    (let ((last-pos (point-min))
          (pos nil)
          (ansi-state e2ansi-null-ansi-state))
      (while
          ;; Here, `last-pos' and `pos' are equal except in the
          ;; first iteration where `pos' is nil. This allows `n-f-p'
          ;; to return point-min.
          (setq pos (e2ansi-min
                     (face-explorer-next-face-property-change pos)
                     ;; Start of next line.
                     (and e2ansi-line-by-line
                          (not (eq last-pos (point-max)))
                          (save-excursion
                            (goto-char last-pos)
                            (forward-line)
                            (point)))
                     ;; End-of-line.
                     (save-excursion
                       (goto-char last-pos)
                       (and (not (eolp))
                            (or e2ansi-line-by-line
                                (and (not (eq (nth 1 ansi-state) 'normal))))
                            (line-end-position)))))
        (e2ansi-print last-pos pos dest)
        (setq last-pos pos)
        (save-excursion
          (goto-char pos)
          (let ((faces (face-explorer-face-text-props-at pos)))
            (let ((new-state (e2ansi-ansi-state faces))
                  (force-reset nil))
              (when (and e2ansi-line-by-line
                         (and (bolp)
                              (not (eobp))))
                (unless (equal ansi-state new-state)
                  (setq force-reset t))
                (setq ansi-state e2ansi-null-ansi-state))
              (when (and (eolp)
                         (or e2ansi-line-by-line
                             (not (equal new-state e2ansi-null-ansi-state))))
                (setq new-state e2ansi-null-ansi-state))
              ;; Clear background color before newline, terminals don't
              ;; seem to handle them well.
              (when (eolp)
                (setq new-state (cons (nth 0 new-state)
                                      (cons 'normal
                                            (nthcdr 2 new-state)))))
              (e2ansi-emit-ansi-sequences
               ansi-state new-state force-reset dest)
              (setq ansi-state new-state)))))
      ;; Insert whatever is left after the last face change.
      (e2ansi-print last-pos (point-max) dest))))


(defun e2ansi-string-to-ansi (str)
  "Return the ansi representation of STR."
  (with-temp-buffer                     ; Result buffer
    (let ((dest (current-buffer)))
      (with-temp-buffer                 ; Source buffer
        (insert str)
        (e2ansi-print-buffer (current-buffer) dest)))
    (buffer-substring-no-properties (point-min) (point-max))))


;; ------------------------------------------------------------
;; The end
;;

(provide 'e2ansi)

;;; e2ansi.el ends here
