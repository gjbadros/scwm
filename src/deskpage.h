#ifndef DESKPAGE_H
#define DESKPAGE_H


SCM set_current_desk_x(SCM sx);
SCM set_viewport_position_x(SCM sx, SCM sy);
SCM set_edge_scroll_x(SCM sx, SCM sy);
SCM set_edge_wrap_x(SCM sx, SCM sy);
SCM set_edge_resistance_x(SCM sr, SCM mr);
SCM set_desk_size_x(SCM sx, SCM sy);
SCM display_size();
SCM desk_size();
SCM viewport_position();
SCM current_desk();

#endif DESKPAGE_H



