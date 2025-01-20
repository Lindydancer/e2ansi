# Build `less` under Windows

This describes how to build `less` under Windows. It uses CMake, a
modern build tool rather than the provided Makefile:s.

## Downloading `less`

Download a suitable version from  http://www.greenwoodsoftware.com/less

## Build

* Unpack the archive. This will create a subdirectory names
  `less-NNN`, where NNN corresponds to the selected version, for
  example `NNN-471`.

* Copy `defines.wn` to `defines.h`

* Create a file named `CMakeLists.txt` with the following content:

```cmake
set(SOURCES
  main.c screen.c brac.c ch.c charset.c cmdbuf.c
  command.c cvt.c decode.c edit.c filename.c forwback.c
  help.c ifile.c input.c jump.c line.c linenum.c
  lsystem.c mark.c optfunc.c option.c opttbl.c os.c
  output.c pattern.c position.c prompt.c search.c signal.c
  tags.c  ttyin.c version.c regexp.c)

include_directories(${CMAKE_CURRENT_SOURCE_DIR})

add_executable(less ${SOURCES})
```

* Create a new directory, and move there. For example `build` parallel
  with the source directory.

```
mkdir build
cd build
```

* Create build projects:

```
cmake ..\less-471
```

* Build

```
cmake --build . --config Release
```

* Copy the resulting binary somewhere in your path:

```
copy Release\less.exe SOMEWHERE
```
