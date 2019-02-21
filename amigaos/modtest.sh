#! /bin/sh

## Compile and run all module standalone tests for:
## ../src/amiga_smalloc.c
## ../src/amiga_dump.c


/* amiga_smalloc.c */
echo "amiga_smalloc.c - 1"
gcc -I. -I/ebuild -g -Wall -Wno-comment -DFULLDEBUG -DTEST=1 amiga_smalloc.c	
a.out 2000
echo "amiga_smalloc.c - 2"
gcc -I. -I/ebuild -g -Wall -Wno-comment -DFULLDEBUG -DTEST=2 amiga_smalloc.c	
a.out 100
echo "amiga_smalloc.c - 3"
gcc -I. -I/ebuild -g -Wall -Wno-comment -DFULLDEBUG -DTEST=3 amiga_smalloc.c	
a.out 100
echo "amiga_smalloc.c - 4"
gcc -I. -I/ebuild -g -Wall -Wno-comment -DFULLDEBUG -DTEST=4 amiga_smalloc.c	
a.out 100



/* amiga_dump.c */
echo "amiga_dump.c"
gcc -I. -I/ebuild -g -Wall -Wno-comment -DFULLDEBUG -DTEST=0  amiga_dump.c
a.out
a.out dummy


exit


#senseless!

echo "amiga_dump.c + amiga_smalloc.c"
/* test dump code using our own malloc from amiga_smalloc.c */
gcc -I. -I/ebuild -g -Wall -Wno-comment -DAsma_alloc=malloc -DAsma_free=free -DAsma_realloc=realloc -DFULLDEBUG -DTEST=0  amiga_dump.c amiga_smalloc.c

a.out
a.out dummy
