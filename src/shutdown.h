/* $Id$ */
#ifndef SHUTDOWN_H
#define SHUTDOWN_H

void Done(int restart, char *command);
void SaveDesktopState(void);

SCM restart(SCM command);
SCM scwm_quit(SCM args);

void init_shutdown();

#endif /* SHUTDOWN_H */
