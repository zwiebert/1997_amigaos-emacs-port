
#ifdef DEBUG_AMIGA_FILEIO
#define eprintf(format, args...)				\
fprintf (stderr, "%s:%d::%s:" format , __FILE__, __LINE__,	\
 __FUNCTION__, ## args)
#else
#define eprintf(format, args...)
#endif


#ifndef emacsclient
#include <config.h>
#endif /* not emacsclient */

#include <assert.h>
#include "amiga.h"
#include "amiga_defs.h"

#ifdef emacsclient
#undef check_precond
#define check_precond assert
#undef check_postcond
#define check_postcond(expr)
#endif /* emacsclient */

char *
Asys_absolute_dos_to_unix (char *buf, int blen, const char *path, int plen)
{
  int colon_ct = 0;
  int i = 0;
  const char *src = path;
  char *dst = buf;

  check_precond (buf && blen > 0 && path && plen > 0);
  check_precond (blen >= plen+2);

  if (blen <= plen+1)
    return 0;
  *dst++ = '/';

  for (i=0; i < plen; ++i, ++dst, ++src)
    if (*src != ':')
      *dst = *src;
    else
      {
	++colon_ct;
	*dst = '/';
      }

  *dst='\0';

  check_postcond ((path + plen) >= src);
  check_postcond ((buf + blen) >= dst);

  if (colon_ct != 1)
    return 0;

  return buf;
}

#ifndef emacsclient

#include "amiga.h"
#include "amiga_defs.h"

#undef LONGBITS
#include <proto/dos.h>
#include <proto/exec.h>
#include <stdio.h>
#include <string.h>
#include <bstring.h>
#include <assert.h>
#include <sys/types.h>
#include <sys/stat.h>
 
/* Strip leading paths. */
/* The RESULT is the position of last absolute path component or 0. */
int
Asys_trim_path (const char *begin, const char *end)
{
  const char *tail=0;
  const char *i=0;
  const char *start=0;

  check_precond (begin && begin < end);

  tail = begin;

  /* find_last_of "//" or hold old */
  for (i = end-1; i > begin; --i)
    if (i[0] == '/' && i[-1] == '/')
      {
	tail = i;
	break;
      }

  start = tail;

  /* find_last_of ":" or hold old */
  for (i = end-1; i > tail; --i)
    if (*i == ':')
      {
	tail = i;
	break;
      }

  /* test for ftp path. */
  /* (ftp ==  "^/[^/]*:") */
  for (i=start+1; *i != '/' && *i != ':';)
    if (++i >= tail)
      return start - begin;

  /* find start of `colon' part */
  if (tail && tail[0] == ':' && begin != tail)
    while (tail > begin && tail[-1] != '/' && tail[-1] != ':')
      --tail;
  
  return tail - begin;
}


/* Expand NAME, which is only the name without the colon, if it's an
   assign and write the result as null terminated char string to BUF
   if it fits in. Returns BUF or 0 if an error occured. */
char *
Asys_vol_truename (char *buf, int blen, const char *name, int len)
{
  char *r=0;

  check_precond (buf && blen > 0 && name && len > 0);
  check_precond (!memchr (name, ':', len) && !memchr (name, '/', len));
  
  if (!(len + 2 > 256 || blen < len))
    {
      char buf2[256];
      struct stat sb;
      BPTR lock;
  
      bcopy (name, buf2, len);
      buf2[len] = ':';
      buf2[len+1] = '\0';

      if (stat (buf2, &sb) != -1) /* avoids requesters according to ixprefs */
	if ((lock = Lock (buf2, ACCESS_READ)))
	  {
	    if (NameFromLock (lock, buf+1, blen-1))
	      {
		char *colon = strchr (buf+1, ':');
		if (colon) 
		  {
		    *colon = buf[0] = '/';
		    r = buf;
		  }
		UnLock (lock);
	      }
	  }
    }
  eprintf ("truename: <%s>\n", r);
  return r;
}


/* Dereference the absolute (including ":") path NAME.  All links and
   assigns are expanded.  The result is BUF for succes or NULL for
   error. */
char *
Asys_file_truename (char *buf, int blen, const char *name, int len)
{
  char *r=0;

  check_precond (buf && blen > 0 && name && len > 0);
  check_precond (memchr (name, ':', len) || memchr (name, '/', len));
  
  if (!(len + 2 > 256 || blen < len))
    {
      char buf2[256];
      struct stat sb;
      BPTR lock;
      const char *path = name;

      if (name[0] == '/')
	if (len == 1)
	  {
	    strcpy (buf, name);
	    return buf;		/* UNIX root */
	  }
	else
	  {
	    char *colon;

	    path = buf2;
	    bcopy (name+1, buf2, len-1);
	    if ((colon = memchr (buf2, '/', len-1)))
	      {
		*colon= ':';
		buf2[len-1] = '\0';
	      }
	    else
	      {
		buf2[len-1] = ':';
		buf2[len] = '\0';
	      }
	  }

      if (stat (path, &sb) != -1) /* avoids requesters according to ixprefs */
	if ((lock = Lock (path, ACCESS_READ)))
	  {
	    if (NameFromLock (lock, buf+1, blen-1))
	      {
		char *colon = strchr (buf+1, ':');
		if (colon) 
		  {
		    *colon = buf[0] = '/';
		    r = buf;
		  }
		UnLock (lock);
	      }
	  }
    }
  eprintf ("truename: <%s>\n", r);
  return r;
}


/* Expands assigns in absolute AmigaDOS or UNIX path. */
/* Expand assigns in PATH and wrote the result to BUF.  Size of BUF
   must greater or equeal to BUFLEN.  If IX is not 0 the first part of
   the resulting path will maded in UNIX syntax.  The RESULT is 0 if
   it will not fit into BUF. If the part before the colon cannot be
   locked we return also 0.  BUF is returned for success. */
char *
Asys_expand_path (char *buf, int blen, const char *path)
{
  const char *head=0, *tail="";
  int headsz=0;
  char *r=0;
  int pathsz = strlen (path);
  char dir_flag=0;

  check_precond (buf && blen > 0 && path);

  eprintf ("path arg: <%s>\n", path);

  if (pathsz > blen)
    return 0;


  if (path[pathsz-1] == ':' || path[pathsz-1] == '/')
    dir_flag = 1;

  if (path[0] == '/' 
      && (strchr (path+1, ':')  /* Syntax error. */
	  || path[1] == '\0'))  /* UNIX root. */
    return 0;

  if (!(path[0] == '/' || strchr (path, ':')))
    return 0;			/* not absolute nor relative to vol root */

  if (path[0] == ':')		/* Volume root. (":xx/yy") */
    {
      tail = path+1;
      head = "";
      headsz = 0;
    }
  else if (path[0] == '/')	/* UNIX absolute. ("/sys/xx") */
    {
      char *sep = 0;
      
      sep = strchr (path+1, '/');
      tail = (sep) ? sep+1 : "";
      if (!sep)
	sep = strchr (path+1, '\0');
      head = path+1;
      headsz = sep - head;
    }
  else				/* Amiga absolute. ("sys:xxx") */
    {
      char *sep = 0;

      sep = strchr (path, ':');
      assert (sep);
      tail = sep+1;
      head = path;
      headsz = sep - head;
    }      
  
  r = Asys_vol_truename (buf, blen - strlen (tail), head, headsz);

  /* tack on path tail */
  if (r)
    {
      char *end = strchr (r, '\0');
      if (end != r && end[-1] != ':' && end[-1] != '/')
	*end++ = '/';
      strcpy (end, tail);
    }

  /* Append slash for directories */
  if (r && dir_flag)
    {
      char *end = strchr (r, '\0');
      if (end != r && end[-1] != ':' && end[-1] != '/')
	{
	  *end++ = '/';
	  *end = '\0';
	}
    }
  assert (!r || strlen (buf) <= blen);

  eprintf ("result: <%s>\n", r);
  return  r;
}

#endif /* not emacsclient */
