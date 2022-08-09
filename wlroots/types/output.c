#include <libguile.h>
#include <wlr/types/wlr_output.h>
#include "../helper.c"
#define WRAP(o) (scm_make_foreign_object_1(output_type,o))
#define UNWRAP(o) (struct wlr_output*)(scm_foreign_object_ref(o, 0))

static SCM output_type;
void
init_output_type (void)
{
  SCM name, slots;
  scm_t_struct_finalize finalizer;

  name = scm_from_utf8_symbol ("<wlr-output>");
  slots = scm_list_1 (scm_from_utf8_symbol ("data"));
  finalizer = NULL;

  output_type =
    scm_make_foreign_object_type (name, slots, finalizer);
}

SCM_DEFINE(scm_wlr_output_backend,"%wlr-output-backend",1,0,0,(SCM o),"")
{
  return FROM_P((UNWRAP(o))->backend);
}
SCM_DEFINE(scm_wrap_wlr_output,"wrap-wlr-output",1,0,0,(SCM o),"")
{
  return WRAP(TO_P(o));
}
SCM_DEFINE(scm_unwrap_wlr_output,"unwrap-wlr-output",1,0,0,(SCM o),"")
{
  return FROM_P(UNWRAP(o));
}

SCM_DEFINE(scm_wlr_output_modes,"%wlr-output-modes",1,0,0,(SCM o),"")
{
  return FROM_P(&((UNWRAP(o))->modes));
}

SCM_DEFINE(scm_wlr_output_width,"wlr-output-width",1,0,0,(SCM o),"")
{
  return scm_from_int((UNWRAP(o))->width);
}
SCM_DEFINE(scm_wlr_output_height,"wlr-output-height",1,0,0,(SCM o),"")
{
  return scm_from_int((UNWRAP(o))->height);
}

void
scm_init_wlr_output(void)
{
  init_output_type();
#ifndef SCM_MAGIC_SNARFER
#include "output.x"
#endif
}
