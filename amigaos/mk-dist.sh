#! /bin/sh

# this is not fully automatic, because it's less error prone.

if [ $# -eq 0 ] ; then
file=/ext/inst/emacs-amiga.tar.gz
else
file=$1
fi

#if suffix is ".tar" then don't compress the archive
if [ $file = ${file%.tar} ] ; then
z=z
else
z=
fi

#verbose option used by some commands
v=
#v=-v

srcdir=/gg/gg-src/emacs-common

cat >/dev/null <<\EOF
;; filter local files
(progn
    (delete-matching-lines "\.flc$")
    (delete-matching-lines "\.orig$")
    (delete-matching-lines "\..*#")
    (delete-matching-lines "\old$")
    (delete-matching-lines "/TAGS$")
    (delete-matching-lines "/bat$")
    (delete-matching-lines "/a\.out$")
    (delete-matching-lines "/src/Changelog$") # we take the link ChangeLog.amiga
    (delete-matching-lines "\.~$"))

EOF

# the list was created by "find -type f"

xargs tar -C$srcdir $v -hc${z} -f $file <<\EOF
./amigaos/README
./amigaos/Doc
./amigaos/Doc-19.34
./amigaos/gcrt0.o.uue
./amigaos/mk-binary-dist-20.2
./amigaos/mk-dist.sh
./amigaos/Hints
./amigaos/Todo
./amigaos/examples/README
./amigaos/examples/_emacs-2.el
./amigaos/examples/_sh.uue
./amigaos/examples/emacs-debug.el
./amigaos/examples/latex
./amigaos/examples/site-start.el
./amigaos/examples/_emacs
./amigaos/findfunc.pl
./amigaos/modtest.sh
./amigaos/findsym.pl
./amigaos/mk-binary-dist
./src/amiga_window_color_data.c
./src/amiga_rexx.c
./src/amiga_window_xapi.c
./src/amiga_window.h
./src/amiga_smalloc.c
./src/amiga_smalloc.h
./src/amiga_malloc.c
./src/amiga-make-tags
./src/amiga.h
./src/amiga_dump.fmt
./src/amiga_fileio.c
./src/amiga_firstfile.c
./src/amiga_lastfile.c
./src/amiga_simplerexx.c
./src/amiga_simplerexx.h
./src/amiga_window_.h
./src/amiga_window_gc.c
./src/amiga_window_select.c
./src/amiga_defs.h
./src/amiga_dump_pl.c
./src/amiga_sysdep.c
./src/amiga_window_input.c
./src/amiga_window_color.c
./src/amiga_dump.c
./src/amiga_window.c
./src/amiga_window_defs.h
./src/s/amigaos.h
./src/m/amiga.h
./src/amiga_window_menu.c
./src/amiga_window_wd.c
./src/ChangeLog.amiga
./lisp/amigaos-patch.el
./lisp/term/amiga-intui.el
./lisp/term/amiga.el
EOF

#gzip -9f /ext/inst/emacs-amiga.tar
