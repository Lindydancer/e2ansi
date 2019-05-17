;;; e2ansi.el --- Syntax highlighting support for `less', powered by Emacs.

;; Copyright (C) 2014,2015 Anders Lindgren

;; Author: Anders Lindgren
;; Keywords: faces, languages
;; Created: 2014-12-07
;; Version: 0.1.2
;; Package-Requires: ((face-explorer "0.0.4"))
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

;; *e2ansi* (Emacs to ANSI) provides syntax highlighting support for
;; `less' and for other tools that run in terminal windows.
;;
;; The `e2ansi-cat' command line tool use ANSI escape sequences to
;; syntax highlight source files. The actual syntax highlighting is
;; performed by Emacs, the mother of all text editors, running in
;; batch mode.
;;
;; Pager applications like `more' and `less' can be configured to
;; automatically invoke `e2ansi-cat', so that all viewed files will be
;; syntax highlighted. A nice side effect is that other conversions
;; that Emacs normally performs, like uncompressing files, are also
;; automatically applied.
;;
;; Example:
;;
;; | Before                     | After                         |
;; | ------                     | -----                         |
;; | ![](doc/no_color_dark.png) | ![](doc/default_dark_256.png) |

;; Syntax:
;;
;; The `e2ansi-cat' command line tool is written in Emacs Lisp. To
;; start it, use:
;;
;;     emacs --batch [...Emacs options...] -l bin/e2ansi-cat [...options...]
;;
;; Alternatively, on UNIX-like operating systems the `e2ansi-cat' can
;; be executed directly, assuming that the `bin' directory is in the
;; load path. This assumes that Emacs is installed as `/usr/bin/emacs':
;;
;;     e2ansi-cat [...options...]
;;
;; Note, due to how Emacs parses options, some options passed to
;; `e2ansi-cat' is parsed by Emacs. Most notably, passing the option
;; `--help' to `e2ansi-cat' displays help for Emacs itself.
;;
;; Command line options:
;;
;; * `--background-mode' -- Specify `light' or `dark' background mode.
;;
;; * `--colors' -- Number of colors, or `rgb24' for full 24 bit
;;   colors. This is both used when mapping faces to actual colors and
;;   to decide the kind of ANSI sequences that is used.
;;
;; * `--color-class' -- Specify `color', `grayscale' or `mono'.
;;
;; * `--theme' -- Specify the color theme to use.
;;
;; * `--usage' -- Show help text.

;; Integration with `less':
;;
;; The standard command line tool `less' can be configured to
;; preprocess any output given to it. `e2ansi-cat' can be used to
;; generate a syntax highlighted version. This is enabled by setting
;; the following environment variables, for example, to (using bash
;; syntax):
;;
;;     export "LESSOPEN=|emacs --batch -l ~/.emacs -l bin/e2ansi-cat %s"
;;     export "LESS=-R"
;;     export "MORE=-R"
;;
;; The `LESSOPEN' environment variable is used by `less' to specify an
;; input preprocessor. When using `e2ansi', the first character should
;; be a `|', this signals to less that the input preprocessor prints
;; the result on standard output.
;;
;; The above example assumes that your init file is named `.emacs' and
;; located in your home directory. It also assumes that it adds the
;; location of the `e2ansi' package to the load path.
;;
;; `less' and pipes:
;;
;; Modern versions of `less' can also use the input preprocessor when
;; used in a pipe, for example:
;;
;;     svn diff | less
;;
;; For this, the `LESSOPEN' environment variable must start with `|-'.
;; In this case, the file name `-' is passed to the input
;; preprocessor, which is expected to read from standard input.
;;
;;     export "LESSOPEN=|-emacs --batch -l ~/.emacs -l bin/e2ansi-cat %s"
;;
;; Note: If your version of `less' is too old, using `|-' typically
;; yields error like "/bin/bash: -/: invalid option".
;;
;; Also note that this makes it hard for Emacs to select a suitable
;; major mode, as it can not base this on the file name extension.
;; Typically, this result in incorrect or no highlighting.
;; Fortunately, Emacs has the ability inspect the file content when
;; selecting major mode, see `magic-mode-alist' for details. The
;; support file `e2ansi-magic.el' adds some file types to the list,
;; e.g. the `diff' format used by `svn'. You must explicitly load it
;; to take effect, either using `-l e2ansi-magic.el' or by loading in
;; from another file that is being loaded, like your init file.

;; Emacs init files:
;;
;; When using Emacs in batch mode, Emacs reads the system init file
;; but not the user init file.
;;
;; If you want to load your personal init file, you can load it using
;; -l command line option, for example:
;;
;;     emacs -l ~/.emacs -l bin/e2ansi-cat file ...
;;
;; To avoid loading the system init file, you can specify the -Q
;; command line option:
;;
;;     emacs -Q -l ~/.emacs -l bin/e2ansi-cat file ...
;;
;; Adapting your init file to batch mode:
;;
;; As Emacs most often is used in interactive mode there is a risk
;; that parts of the system or your init file doesn't work in batch
;; mode.
;;
;; To exclude something when in batch mode, you can use:
;;
;;     (unless noninteractive
;;       .. original code goes here ... )

;; Silencing messages:
;;
;; When Emacs is used in batch mode, message are emitted on the
;; standard error stream. This includes messages emitted when files
;; are loaded.
;;
;; On UNIX-like operating systems, the standard error stream can be
;; redirected to /dev/null, for example:
;;
;;     emacs --batch -l ~/.emacs -l bin/e2ansi-cat 2> /dev/null
;;
;; Alternatively, the file `e2ansi-silent' can be loaded. This file
;; has the advantages that the Usage information of `e2ansi-cat' is
;; printed (when started without arguments) and that it works on all
;; operating systems. (If you load it before your init file, you might
;; need to specify the full path.)
;;
;;     emacs --batch -l e2ansi-silent -l ~/.emacs -l bin/e2ansi-cat ...
;;
;; Silencing messages in `site-start.el':
;;
;; Normally, the file `site-start.el' is loaded by Emacs before any
;; file specified on the command line. To silence messages emitted by
;; this file, you can suppress loading it using -Q, load
;; `e2ansi-silent' and explicitly load the site-start file:
;;
;;     emacs --batch -Q -l e2ansi-silent -l ../site-lisp/site-start -l ...
;;
;; Note: When -Q is used, the `site-lisp' directory is not included in
;; the load path, the `../site-lisp' part compensate for this.

;; Emacs modules:
;;
;; * `e2ansi.el' -- Render a syntax highlighted buffer using ANSI
;;   escape sequences. This can be used both in normal interactive
;;   mode and in batch mode.
;;
;; * `e2ansi-magic.el' -- Set up `magic-mode-alist' to recognize file
;;   formats based on the content of files. This is useful when using
;;   `less' in pipes where Emacs can't use the file name extension to
;;   select a suitable major mode.
;;
;; * `e2ansi-silent.el' -- Load this in batch mode to silence some
;;   messages from init files.
;;
;; * `bin/e2ansi-cat' -- The command line tool for converting files in
;;   batch mode.
;;
;; * `bin/e2ansi-info' -- Print various ANSI-related information to
;;   help you trim your ANSI environment.

;; Background:
;;
;; What is Emacs:
;;
;; Emacs is a the mother of all text editors. It originates from the
;; 1970:s, but is still in active development. It runs under all major
;; operating systems, including MS-Windows, Mac OS X, and various
;; UNIX-like systems like Linux. You can use normal windows, run it in
;; a terminal window (great when working remotely), or use it to run
;; scripts in batch mode, which is how it is used by the command line
;; tools provided by this package.
;;
;; Emacs provides state-of-the-art syntax highlighting.
;;
;; Why use Emacs to power syntax highlighting in the terminal:
;;
;; * Emacs has support for a vast range of programming languages and
;;   other structured text formats.
;;
;; * Emacs is fast and accurate -- it is designed for interactive use,
;;   and provides advanced support for ensuring that a source buffer
;;   is parsed correctly.
;;
;; * Emacs supports color themes. If you don't like the ones provided,
;;   and can't find one on internet, you can easily write your own.
;;
;; * To add syntax highlighting support for other formats can easily be
;;   done by providing a standard Emacs major mode, where the syntax
;;   highlighting is provided by Font Lock keywords. You can use
;;   `font-lock-studio' to debug those keywords, it allows you single
;;   step match by match and it visualizes matches using a palette of
;;   background colors.

;; ANSI sequences:
;;
;; ANSI sequences, formally known as ISO/IEC 6429, is a system used by
;; various physical terminals and console programs to, for example,
;; render colors and text attributes such as bold and italics.
;;
;; See [Wikipedia](http://en.wikipedia.org/wiki/ANSI_escape_code) for
;; more information.
;;
;; Colors:
;;
;; Both foreground and background colors can be rendered. Note that
;; faces with the same background as the default face is not rendered
;; with a background.
;;
;; Four modes are supported:
;;
;; * 8 -- The eight basic ANSI colors are supported.
;;
;; * 16 -- The eight basic colors, plus 8 "bright" colors. These are
;;   represented as "bold" versions of the above.
;;
;; * 256 -- Some modern terminal programs support a larger palette.
;;   This consist of the 16 basic colors, a 6*6*6 color cube plus a
;;   grayscale.
;;
;; * 24 bit -- Support for 256*256*256 colors.
;;
;; Attributes:
;;
;; * Bold
;;
;; * Italics
;;
;; * Underline

;; Operating system notes:
;;
;; Mac OS X:
;;
;; Mas OS X comes bundled with Emacs. Unfortunately, it's a relatively
;; old version, 22.1.1. It is fully functional, but it lacks some
;; features. Most notably, the default face definitions are broken
;; when 8 colors are used.
;;
;; You can download a modern version from
;; [EmacsForOSX](http://emacsforosx.com). Once installed, the path to
;; the Emacs binary is typically
;; `/Applications/Emacs.app/Contents/MacOS/Emacs'.
;;
;; The version of `less' that is preinstalled on 10.9 is 418 dating
;; from 2007. It supports the input preprocessors for named files (the
;; `|' syntax) but not for pipes (The `|-' syntax).
;;
;; Fortunately, it's easy to download and build a new version of
;; `less' from http://www.greenwoodsoftware.com/less
;;
;; Why Apple has not included a newer version, I do not understand.
;; Some claim that Apple is actively avoiding software licensed under
;; GPLv3+. However, `less' is available under two licenses so this
;; should not be a problem. I guess it's down to laziness and opting
;; to spend resources to invent yet another way of making menus
;; semi-opaque rather than providing their users with up to date
;; tools.
;;
;; Microsoft Windows:
;;
;; The command window in Microsoft Windows does not understand ANSI
;; sequences. Fortunately, modern versions of `less' is capable of
;; rendering ANSI sequences using colors.
;;
;; Unfortunately, I'm not aware of any up to date distribution of
;; prebuilt versions of `less'. To make things worse, the build
;; scripts provided with `less' is somewhat hard to use.
;;
;; In the document
;; [LessWindows](https://github.com/Lindydancer/e2ansi/doc/LessWindows.md)
;; I describe how to build `less' using `cmake', a modern build
;; system.

;; Gallery:
;;
;; All images are screen captures of `less' running in a terminal
;; window. White or black backgrounds were used, even though some
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

;; Future development:
;;
;; This sections consists of things that might or might not appear in
;; this package in the future.
;;
;; More command line tools:
;;
;; More command line tools could make life simpler. For example:
;;
;; * `e2ansi-exec' -- Run another command line tools and convert all
;;   arguments (that looks like files) to the corresponding ansi
;;   format. (To avoid converting files that should not be converted,
;;   this tool must include knowledge regarding common commands.) One
;;   problem with this tool is the bizarre way Emacs treats command
;;   line options -- hence, the entire tool, or parts of it, must be
;;   written in another script language like Ruby.q
;;
;; * `e2ansi-tmp' -- Render a syntax highlighted version of a file as
;;   a temporary file and print the file name. This way a user could
;;   use the shell backquote syntax to feed syntax highlighted
;;   versions of a file to a third tool, for example:
;;
;;         atool `e2ansi-tmp afile.c`
;;
;;   A technical problem with this tool is that it leaves the
;;   temporary file behind. Hence, some kind of garbage collection
;;   mechanism must be put in place.
;;
;; Generalization:
;;
;; Much of the code in this package is generic. It could most likely
;; be used to generate other output formats like Markdown. One idea is
;; to break out the generic parts to a separate package.
;;
;; Faster response time:
;;
;; * By using a resident Emacs process, response time could be greatly
;;   reduced. Today, a bare-bone Emacs start fast, however, if you use
;;   a heavy init file (like I do) the start-up time goes up
;;   noticeably.
;;
;; * Incremental syntax highlighting. Today, the entire buffer is
;;   syntax highlighted at once, before it is converted to ANSI. By
;;   only run font-lock on parts of the buffer before printing it, a
;;   receiving application like `less' could start faster.
;;
;; Miscellaneous:
;;
;; Various things.
;;
;; * Check how things look when using a dark background, maybe
;;   something would need to be added to the overrides list.
;;
;; * Don't use the "brightxxx" color names, they are not used by Emacs
;;   and it's only confusing.
;;
;; * Add unit tests for individual parts and full source file tests
;;   where the entire output is compared against a known ANSI
;;   representation, as done in my `faceup' package.
;;
;; * Describe Emacs font specification selection process.
;;
;; * Better error handling when parsing command line arguments.
;;
;; * Customization support.
;;
;; * Optimize ANSI sequences. Today, whenever there is a change, a
;;   reset is emitted, plus codes to set all properties. However,
;;   sometimes it would be shorter to simple, say, add underline, and
;;   without touching the other properties.
;;
;; * Security issues: Don't allow file local variables, or anything
;;   else, allow arbitrary elisp code to be executed when a file is
;;   viewed.
;;
;; * Use floating point numbers when scoring, rather than scaling
;;   down.
;;
;; * Promote some of the "list" commands from the "test" package.
;;
;; * Inhibit rendering ANSI sequences when running `less' on a file
;;   already containing ANSI sequences.
;;
;; * Add `--force' to force e2ansi to render a file using ANSI
;;   sequences, even when such sequences are found in the file.
;;
;; * Mention `||-' in the documentation.
;;
;; * Investigate why backgrounds spanning mutiple lines misbehaves in
;;   Terminal.app (and other terminals). Maybe always reset the
;;   background at the end of each line?

;; Acknowledgment:
;;
;; Mark, thanks for writing `less'!

;;; Code:

(require 'face-explorer)


(defvar e2ansi-use-window-system-color-values nil
  "When non-nil, use the current window system opinion of color values.

When nil, or when in batch mode, color values are based on
information in `color-name-rgb-alist'.

The advantage of using the window system color values is that the
end result will be slightly more alike the colors used in an
interactive Emacs. The disadvantage is that the result might be
different than when generated in batch mode." )


(defvar e2ansi-line-by-line t
  "When non-nil, extra ANSI codes are emitted at the start of each line.

The advantage with this is that the output is highlighted correctly
even if only parts of it is printed to a terminal. In addition,
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


(defalias 'e2ansi-user-error
  (if (fboundp 'user-error)
      'user-error
    'error))


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
existing file. Interactively, confirmation is required unless you
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
             (if (string= arg "rgb24")
                 t
               (string-to-number arg))))
     "Number of colors (8, 16, or 256), or 'rgb24' for 24 bit colors")
    ("--background-mode" :arg
     (lambda (arg)
       (setq face-explorer-background-mode (intern arg)))
     "Background mode, 'light' (default) or 'dark'")
    ("--color-class" :arg
     (lambda (arg)
       (setq face-explorer-color-class (intern arg)))
     "Color class, 'color' (default), 'grayscale', or 'mono'")
    ("--mode" :arg
     (lambda (arg)
       (setq e2ansi-batch-major-mode-name arg))
     "Use major mode")
    ("--theme" :arg
     (lambda (arg)
       (load-theme (intern arg)))
     "Load custom color theme")
    ("--usage" :none
     (lambda ()
       (e2ansi-batch-usage))
     "Display this help text"))
  "Command line options used in batch mode.")


(defun e2ansi-batch-usage ()
  "Print usage information."
  ;; Ensure output is seen even though `e2ansi-silent.el' was loaded
  ;; by the user.
  (let ((e2ansi-silent-message nil))
    (message
     (let ((widest 0))
       (dolist (opt e2ansi-batch-options)
         (setq widest (max widest (length (nth 0 opt)))))
       (let ((s (concat e2ansi-batch-help-text
                        "\n\n"
                        "Available options:\n\n")))
         (dolist (opt e2ansi-batch-options)
           (setq s (concat s
                           (nth 0 opt)
                           (make-string (+ (- widest (length (nth 0 opt)))
                                           1)
                                        32) ; 32 = ASCII code of space
                           (nth 3 opt)
                           "\n")))
         s)))))


(defun e2ansi-batch-parse-options ()
  "Parse batch command line options.

See `e2ansi-batch-options' for options."
  (let ((res t))
    (while (and res
                command-line-args-left
                ;; The "." ensures that a single "-" isn't matched.
                (string-match "^-." (car command-line-args-left)))
      (let* ((option (pop command-line-args-left))
             (desc (assoc option e2ansi-batch-options)))
        (if desc
            (let ((args '()))
              (if (eq (nth 1 desc) :arg)
                  (if command-line-args-left
                      (push (pop command-line-args-left) args)
                    (message "Missing argument for option: '%s'" option)
                    (setq res nil)))
              (when res
                ;; Right now, only one argument is parsed, the reverse
                ;; is needed in case we parse more in the future.
                (apply (nth 2 desc) (reverse args))))
          (message "Unknown command line option: '%s'"
                   option)
          (setq res nil))))
    res))


;; TODO: Rewrite in terms of the functions below.

;;;###autoload
(defun e2ansi-batch-convert ()
  "Convert the remaining files on the command line to ANSI format."
  (if (e2ansi-batch-parse-options)
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
                (normal-mode))
            (unless (file-exists-p source)
              (e2ansi-user-error "File not found: %s" source))
            (let ((large-file-warning-threshold nil))
              (find-file source))))
        ;; Override major mode, if --mode was specified.
        (when e2ansi-batch-major-mode-name
          (let ((mode nil))
            (dolist (s1 (list e2ansi-batch-major-mode-name
                              (concat e2ansi-batch-major-mode-name "-mode")))
              (dolist (s2 (list s1 (downcase s1)))
                (let ((candidate (intern s2)))
                  (when (fboundp candidate)
                    (setq mode candidate)))))
            (when mode
              (funcall mode))))
        (save-excursion
          (goto-char (point-min))
          ;; Don't highlight buffers containing existing ansi
          ;; sequences.
          ;;
          ;; TODO: Implement some kind of "--force" option to override
          ;; this.
          (if (search-forward "\x1b[" (point-max) t)
              (princ (buffer-string))
            (let ((noninteractive nil))
              (font-lock-mode 1))
            (e2ansi-print-buffer (current-buffer)))))
    (e2ansi-batch-usage)))


(defun e2ansi-batch-write-to-file (file &optional mode dest-file)
  (with-temp-buffer
    (e2ansi-batch-convert-file file mode (current-buffer))
    (write-region (point-min) (point-max) dest-file)))


(defun e2ansi-batch-convert-file (file &optional mode dest)
  (if (file-exists-p file)
      (let ((large-file-warning-threshold nil))
        (with-temp-buffer
          ;; Font-lock isn't activated on temporary buffers, i.e.
          ;; buffers whose name start with a space.
          (rename-buffer "*e2ansi*" 'unique)
          (insert-file-contents file 'visit)
          (normal-mode)
          (e2ansi-batch-convert-buffer (current-buffer) mode dest)))
    (e2ansi-user-error "File not found: %s" file)))


(defun e2ansi-batch-convert-buffer (buffer &optional mode dest)
  (with-current-buffer buffer
    ;; Override major mode, if --mode was specified.
    (when mode
      (let ((mode-symbol nil))
        (dolist (s1 (list mode
                          (concat mode "-mode")))
          (dolist (s2 (list s1 (downcase s1)))
            (let ((candidate (intern s2)))
              (when (fboundp candidate)
                (setq mode-symbol candidate)))))
        (when mode-symbol
          (funcall mode-symbol))))
    (save-excursion
      (goto-char (point-min))
      ;; Don't highlight buffers containing existing ansi
      ;; sequences.
      ;;
      ;; TODO: Implement some kind of "--force" option to override
      ;; this.
      (if (search-forward "\x1b[" (point-max) t)
          (princ (buffer-string) dest)
        (let ((noninteractive nil))
          (font-lock-mode 1))
        (e2ansi-print-buffer (current-buffer) dest)))))


(defun e2ansi-batch-print-setting ()
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
values, and they are often user configurable. The color values
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
  "True, if color NUMBER is included when searching for the closest color."
  (cond ((eq face-explorer-number-of-colors 8)
         (< number 8))
        ((eq face-explorer-number-of-colors 16)
         ;; In ANSI, 16 bit color mode provides 16 foreground colors
         ;; but only 8 backaground colors.
         (< number (if (eq ground-mode :foreground)
                       16
                     8)))
        ((eq face-explorer-number-of-colors 256)
         ;; The color number of the basic 16 colors vary between
         ;; terminals, so they are not included when trying to find
         ;; the best matched color.
         (>= number 16))
        (t
         t)))


(defvar e2ansi-closest-color-number-cache '())

(defun e2ansi-find-closest-color-number (name ground-mode)
  "Find the nearest ANSI color to color NAME.

This does not inspect the basic 16 ANSI colors as their color
values are not well defined."
  (let* ((key (list name ground-mode face-explorer-number-of-colors))
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
        (while (< i face-explorer-number-of-colors)
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

GROUND-MODE is either :foreground or :background. Optional FRAME
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
  "The ANSI color number, or the color values, that corresponds to NAME."
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
  "The color number, the color values, or `normal'."
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
The list may also contain 'naked' faces, i.e. property and value elements.

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
         ;; the function call never returns nil, drop the or (check this)
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
Evaluates BODY. Emit one ANSI sequence consisting of all ANSI
codes passed to `e2ansi-emit-ansi-code'."
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
  "Emit an ANSI escape code to `standard-output'.

This is assumed to be called from within the body of
`e2ansi-with-ansi-sequence'. This ANSI escape code can be
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

If FORCE-START is non-nil, don't assume that the output terminal
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
  "Like `min' but ignores nil arguments.
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
    (face-explorer-fontify-buffer)
    (let ((last-pos (point-min))
          (pos nil)
          (state '())                   ; List of faces.
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
