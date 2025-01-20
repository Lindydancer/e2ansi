# Tests for `e2ansi`

This directory contains various tests and development tools.

## Unit tests

The following files contains unit tests using the ERT framework.

* `e2ansi-test-basic.el` -- Test individual functions.

* `e2ansi-test-files.el` -- Test complete files. The files, and
  reference ANSI files, are in the `files` directory.

Use the following to run the tests:

```
emacs --batch -Q -l e2ansi-test-setup.el
```

## Command tests (cmdtest)

The file 'CMDTEST_e2ansi.rb' file tests `e2ansi-cat` using the
[CMDTEST] framework.

Run these tests using the command line tool `cmdtest`.

## Installation tests

The file `Rakefile` contains one task `test_install_face_explorer`
intended to run `e2ansi-cat` in a fresh home directory. It tests that
e2ansi can find it's dependencies. It also checks that it can be
executed via a symbolic link.

## Development tools

The file `e2ansi-dev-tools.el` contains a number of functions to
investigate Emacs faces and ANSI escape codes.

## See also

* https://bitbucket.org/holmberg556/cmdtest.git

<!-- Links -->
[CMDTEST]: https://bitbucket.org/holmberg556/cmdtest.git
