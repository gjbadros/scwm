#ifndef BINDING_H
#define BINDING_H

SCM bind_key(SCM contexts, SCM key, SCM proc);
SCM bind_mouse(SCM contexts, SCM button, SCM proc);

void init_binding();
void find_mouse_event_type();
void clear_mouse_event_type();

SCM mouse_event_type();

#endif /* BINDING_H */
