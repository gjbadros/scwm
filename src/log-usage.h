/* $Id$
 * log-usage.h
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


#ifndef GWM_SPY_H__
#define GWM_SPY_H__

#ifdef HAVE_CONFIG_H
#include "scwmconfig.h"
#endif

#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <fcntl.h>        /* F_SETFL and likes */
#include <netinet/in.h>   /* sockaddr_in */
#include <netdb.h>        /* getservbyname() */


#define REPORT_USAGE_PORT 13671
#define REPORT_USAGE_HOST "uni.cs.washington.edu"

#define	USAGE_PACKET_SIZE 1000
#define USAGE_MAGIC_NUMBER (char) 0xfc

int
SendUsagePacket(char *host, /* 0 -> REPORT_USAGE_HOST */
                int port,   /* 0 -> REPORT_USAGE_PORT */
                char *progName,   /* defines the log file */
                char *origin,    /* 0 -> hostname */
                char *msg);

#endif
