#include <libguile.h>
#include <wlr/types/wlr_xdg_shell.h>
#include "../helper.c"

SCM_DEFINE(scm_wlr_xdg_surface_toplevel ,"wlr-xdg-surface-toplevel",1,0,0,(SCM o),"")
{
  return WRAP_WLR_XDG_TOPLEVEL(((struct wlr_xdg_surface*)(UNWRAP_WLR_XDG_SURFACE(o)))->toplevel);
}

void
scm_init_wlr_xdg_shell(void)
{
#ifndef SCM_MAGIC_SNARFER
#include "xdg-shell.x"
#endif
}
