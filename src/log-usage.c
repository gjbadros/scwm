/* $Id$
 * log-usage.c
 * Greg Badros <gjb@cs.washington.edu> 
 * 
 * This code is copied and derived from GWM, the Generic Window Manager
 *
 * GWM - Generic Window Manager - Copyright (C) 1989-94 GROUPE BULL
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files
 * (the "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to permit
 * persons to whom the Software is furnished to do so, subject to the
 * following conditions:
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL GROUPE BULL BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 *
 * Except as contained in this notice, the name of GROUPE BULL shall not be
 * used in advertising or otherwise to promote the sale, use or other
 * dealings in this Software without prior written authorization from
 * GROUPE BULL.
 */

#ifdef HAVE_CONFIG_H
#include "scwmconfig.h"
#endif

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>

#include "log-usage.h"

#include "scwm.h"

int
SendUsagePacket(char *host, /* 0 -> REPORT_USAGE_HOST */
                int port,   /* 0 -> REPORT_USAGE_PORT */
                char *progName,   /* defines the log file */
                char *origin,    /* 0 -> hostname */
                char *msg)
{
#define FUNC_NAME "SendUsagePacket"
#ifndef NO_REPORT_USAGE
  int rc;

  struct sockaddr_in sin;
  struct hostent *h;
  int sock = 0;
  int flags = 0;

  if (getenv("SCWM_DO_NOT_LOG_USAGE")) {
    return 0;
  }

  /* This is here for now just so people
     know what is hanging in case things hang for them */
  scwm_msg(INFO,FUNC_NAME,"Opening socket for usage log...");

  /* getting hostname */
  if (!(h = gethostbyname(host ? host : REPORT_USAGE_HOST))) {
    return 1;
  }

  /* creating a socket */
  if ((sock = socket (AF_INET, SOCK_DGRAM, 0)) < 0) {
    return 2;
  }

  memset(&sin, 0, sizeof (sin));
  sin.sin_family = AF_INET;

  /* providing host server identity */
  memcpy(&sin.sin_addr, h->h_addr, h->h_length);

  /* affecting the port number of the server to the sin structure */
  sin.sin_port = htons(port ? port : REPORT_USAGE_PORT);

  /* connect to the server */
#if defined(linux) || defined(SVR4)
  rc = connect (sock, (struct sockaddr *) &sin, sizeof (sin));
#else
  rc = connect (sock, &sin, sizeof (sin));
#endif
  if (rc < 0) {
    return 3;
  }


  /* Make all sockets blocking */
  fcntl(sock, F_GETFL, &flags);
  flags &= ~O_NDELAY;
  fcntl(sock, F_SETFL, flags);

  /*
  ** Sending the string
  */
  { /* scope */
    unsigned char buf[USAGE_PACKET_SIZE], h[256];
    int msgLen;    /* left = msgLen + 2 */

    if (! origin)
      gethostname(h, 256);
    else
      strncpy(h, origin, 256);
    h[255] = '\0';

    /* create message */
    sprintf(buf, "XXX%s %s : ", progName, h);
    msgLen = strlen(buf);
    strncpy(buf + msgLen, msg, USAGE_PACKET_SIZE - msgLen - 1);
    buf[USAGE_PACKET_SIZE - 1] ='\0';

    msgLen = strlen(buf) + 1;
    if (msgLen > USAGE_PACKET_SIZE - 3)
      msgLen = USAGE_PACKET_SIZE - 3;

    { /* scope */
      /* write magic number and length of message */
      int i = 0;
      buf[i++] = USAGE_MAGIC_NUMBER & 0xff;
      buf[i++] = msgLen / 256;
      buf[i++] = msgLen % 256;
    }

    { /* scope */
      /* then message itself */
      int written;
      int index = 0;
      while (index < USAGE_PACKET_SIZE) {
        written = write(sock, & buf[index], USAGE_PACKET_SIZE - index);
        if (written <= 0) {
          return(6);
        }
        index += written;
      }
    }

    close(sock);
  }

  scwm_msg(INFO,FUNC_NAME,"Sent usage note with your hostname and version number-- thank you!");

#endif /* !NO_REPORT_USAGE */

  return 0;

}
#undef FUNC_NAME


/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
/* vim:ts=8:sw=2:sta 
 */

