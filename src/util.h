#ifndef UTIL_H
#define UTIL_H

void redraw_titlebars(ScwmDecor * fl, int extra_height);

void refresh_common(Window win_or_root);

SCM call_thunk_with_message_handler(SCM thunk);

#endif	/* UTIL_H */
