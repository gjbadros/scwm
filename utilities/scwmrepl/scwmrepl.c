#include "../libscwmexec/scwmexec.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


Display *display;
char *dispName;

int
init_display()
{
  if (!(dispName = getenv("DISPLAY")))
    return 0;
  if (!(display = XOpenDisplay(dispName)))
    return 0;
  return 1;
}


void die(char *str)
{
  fputs(str,stderr);
  exit(-1);
}

int appending_fgets(char **sofar)
{
  char buffer [512];
  
  do {
    fgets(buffer, 512, stdin);
    if (strlen(buffer)==0) {
      return 0;
    }
    *sofar=realloc(*sofar,strlen(*sofar)+strlen(buffer)+1);
    strcat(*sofar,buffer);
  } while (buffer[strlen(buffer)-1]!='\n');

  return 1;
}
  
int check_balance(char *expr) {
  /* If you think _this_ is hairy, try doing it for C statements. */
  int i;
  int end;
  int non_whitespace_p=0;
  int paren_count=0;
  int prev_separator=1;
  int quote_wait=0;

  end=strlen(expr);
  i=0;
  while (i<end) {
    switch(expr[i]) {
    case ';' : 
      /* skip till newline. */
      do {
	i++;
      } while (expr[i]!='\n' && i<end);
      break;
    case ' ':
    case '\n':
    case '\t':
    case '\r': 
      if (non_whitespace_p && paren_count==0 && !quote_wait) {
	return i;
      } else {
	prev_separator=1;
	i++;
      }
      break;
    case '\"' :
      if (non_whitespace_p && paren_count==0 && 
	  !quote_wait) {
	return i;
      } else {
	/* skip past ", ignoring \" */
	do {
 	  i++;
	  if (i < end && expr[i]=='\\') {
	    i++;
	  }
	} while (i < end && expr[i]!='\"');
	i++;
	if (paren_count==0) {
	  if (i < end) {
	    return i;
	  } else {
	    return 0;
	  }
	} else {
	  prev_separator=1;
	  non_whitespace_p=1;
	  quote_wait=0;
	}
      }	  
      break;
    case '#' :
      if (non_whitespace_p && paren_count==0 && 
	  !quote_wait) {
	return i;
      } else {
	if (prev_separator && i+1<end && expr[i+1]=='{') {
	  /* skip past }#, ignoring \} */
	  do {
	    i++;
	    if (i < end && expr[i]=='\\') {
	      i++;
	    }
	  } while (i < end && !(expr[i]=='}' && i+1<end
				&& expr[i+1]=='#'));
	  i+=2;
	  if (paren_count==0) {
	    if (i < end) {
	      return i;
	    } else {
	      return 0;
	    }
	  } else {
	    prev_separator=1;
	    non_whitespace_p=1;
	    quote_wait=0;
	  }
	} else {
	  prev_separator=0;
	  quote_wait=0;
	  non_whitespace_p=1;
	  i++;
	}
      }	  
      break;
    case '(' :
      if (non_whitespace_p && paren_count==0 &&!quote_wait) {
	return i; 
      } else {
	i++;
	paren_count++;
	non_whitespace_p=1;
	prev_separator=1;
	quote_wait=0;
      }
      break;
    case ')' :
      paren_count--;
      if (non_whitespace_p && paren_count==0) {
	return i+1; 
      } else {
	i++;
	non_whitespace_p=1;
	prev_separator=1;
	quote_wait=0;
      }
      break;
    case '\'' :
      if (prev_separator) {
	non_whitespace_p=1;
	quote_wait=1;
	prev_separator=1;
	i++;
      } else {
	non_whitespace_p=1;
	prev_separator=0;
	i++;
      }
      break;
    default :
      prev_separator=0;
      quote_wait=0;
      non_whitespace_p=1;
      i++;
      break;
    }
  }
  return 0;
}

char *split_at(char **to_split, int i) {
  char *out;
  char *ret;

  out=strdup((*to_split)+i);

  (*to_split)[i]=0;
  ret=strdup(*to_split);
  free(*to_split);
  *to_split=out;
  return ret;
}

void 
main(int argc, char **argv)
{
  Window w;
  int splitpoint;
  char *expr;
  int done = 0;
  char *gather=calloc(1,1);

  if (argc != 1)
    die("Usage: scwmrepl");
  if (!init_display())
    die("Could not connect to server. Check your DISPLAY environment variable.\n");

  w=scwmexec_init(display);

  if (w==None)
    die ("Unable to establish scwmexec connection.\n");

  /* FIXMS: we should really be using readline */
  printf("scwm> ");
  while (!done) {
    if ((splitpoint=check_balance(gather))) {
      expr=split_at(&gather,splitpoint);
      printf("; value: %s",scwmexec_exec(display,w,expr));
      free(expr);
      printf("\nscwm> ");
    } else {
      done = !appending_fgets (&gather);
    }
  }

  XCloseDisplay (display);
  
  exit (0);
}




