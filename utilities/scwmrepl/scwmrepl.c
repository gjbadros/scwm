#include <config.h>
#include "../libscwmexec/scwmexec.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifdef HAVE_READLINE
#include <readline/readline.h>
#ifdef HAVE_HISTORY
#include <readline/history.h>
#endif /* HAVE_HISTORY */
#endif /* HAVE_READLINE */

Display *display;
char *dispName;
Window w;

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

#ifdef HAVE_READLINE
char *scwm_complete(char *text, int state)
{
  static unsigned char *output=NULL;
  static char *last=NULL;
  static char *completions[1024];

  if (!state) {
    unsigned char *c,*e,*query,*error,*result,hold;
    unsigned n;

    if (!last || strcmp(last,text)!=0) {
      if (last) {
	XFree(last);
      }
      last=strdup(text);
      if (output) {
	XFree(output);
      }
      query=(unsigned char *)malloc(strlen(text)+14);
      strcat(strcat(strcpy(query,"(apropos \"^"),text),"\")");
      result=scwmexec_exec_full(display,w,query,&output,&error);
      if (error) {
	XFree(error);
      }
      if (result) {
	XFree(result);
      }
      free(query);
    }

    c=output;
    n=0;
    while (n<1023) { 
      c=strstr(c,": ");
      if (!c)
	break;
      c+=2;
      e=strpbrk(c,"\t\n");
      if (!e) {
	completions[n++]=strdup(c);
	break;
      }
      hold=*e;
      *e=0;
      completions[n++]=strdup(c);
      *e=hold;
      c=strchr(e,'\n');
      if (!c)
	break;
      c++;
    }
    completions[n]=NULL;
  }    
  return completions[state];
}

void init_readline()
{
  rl_completion_entry_function=(Function *)scwm_complete;
}
#endif

int appending_fgets(char **sofar)
{
#ifdef HAVE_READLINE
  char *buffer;
  unsigned pos,len;

  buffer=readline("scwm> ");
  if (buffer==NULL) {
    return 0;
  }
  len=strlen(buffer);
#if HAVE_HISTORY
  if (len>0) {
    add_history(buffer);
  }
#endif
  pos=strlen(*sofar);
  *sofar=realloc(*sofar,pos+len+2);
  strncpy(*sofar+pos,buffer,len);
  (*sofar)[pos+len]='\n';
  (*sofar)[pos+len+1]=0;
#else
  char buffer [512];

  printf("scwm> ");
  do {
    fgets(buffer, 512, stdin);
    if (strlen(buffer)==0) {
      return 0;
    }
    *sofar=realloc(*sofar,strlen(*sofar)+strlen(buffer)+1);
    strcat(*sofar,buffer);
  } while (buffer[strlen(buffer)-1]!='\n');
#endif

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

int 
main(int argc, char **argv)
{
  int splitpoint;
  char *expr;
  int done = 0;
  char *gather=calloc(1,1);

  if (argc != 1)
    die("Usage: scwmrepl\n");
  if (!init_display())
    die("Could not connect to server. Check your DISPLAY environment variable.\n");

  w=scwmexec_init(display);

#ifdef HAVE_READLINE
  init_readline();
#endif

  if (w==None)
    die ("Unable to establish scwmexec connection.\n");

  while (!done) {
    if ((splitpoint=check_balance(gather))) {
      unsigned char *result, *error, *output;
      expr=split_at(&gather,splitpoint);
      result=scwmexec_exec_full(display,w,expr,&output,&error);

      printf(output);
      if (strlen(error)!=0) {
	fprintf(stderr,error);
      } else {
	fprintf(stdout,result);
      }
      putchar('\n');

      if (result!=NULL) {
	XFree(result);
      }
      if (output!=NULL) {
	XFree(output);
      }
      if (error!=NULL) {
	XFree(error);
      }
      free(expr);
    } else {
      done = !appending_fgets (&gather);
    }
  }

  XCloseDisplay (display);
  
  return 0;
}
