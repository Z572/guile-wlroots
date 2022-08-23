#include <libguile.h>
#include <wlr/types/wlr_output.h>
#include "../helper.c"
#define WRAP(o) WRAP_WLR_OUTPUT(o)
#define UNWRAP(o) (struct wlr_output*)(UNWRAP_WLR_OUTPUT(o))
#define CHECK_TYPE(o) do { \
    SCM_ASSERT(scm_from_bool(SCM_IS_A_P(o ,scm_c_private_ref("wlroots types output","<wlr-output>"))) ,o, SCM_ARG1, FUNC_NAME); \
  } while (0)

SCM_DEFINE(scm_wlr_output_name,"wlr-output-name",1,0,0,(SCM o),"")
#define FUNC_NAME s_scm_wlr_output_name
{

  CHECK_TYPE(o);
  return scm_from_utf8_string((UNWRAP(o))->name);
}
#undef FUNC_NAME

SCM_DEFINE(scm_wlr_output_description,"wlr-output-description",1,0,0,(SCM o),"")
#define FUNC_NAME s_scm_wlr_output_description
{

  CHECK_TYPE(o);
  char *c = (UNWRAP(o))->description;
  return ((c) ? scm_from_utf8_string(c): SCM_BOOL_F) ;
}
#undef FUNC_NAME

SCM_DEFINE(scm_wlr_output_backend,"%wlr-output-backend",1,0,0,(SCM o),"")
#define FUNC_NAME s_scm_wlr_output_backend
{
  CHECK_TYPE(o);
  return FROM_P((UNWRAP(o))->backend);
}
#undef FUNC_NAME

SCM_DEFINE(scm_wlr_output_modes,"%wlr-output-modes",1,0,0,(SCM o),"")
#define FUNC_NAME s_scm_wlr_output_modes
{
  CHECK_TYPE(o);
  return FROM_P(&((UNWRAP(o))->modes));
}
#undef FUNC_NAME
SCM_DEFINE(scm_wlr_output_width,"wlr-output-width",1,0,0,(SCM o),"")
#define FUNC_NAME s_scm_wlr_output_width
{
  CHECK_TYPE(o);
  return scm_from_int((UNWRAP(o))->width);
}
#undef FUNC_NAME
SCM_DEFINE(scm_wlr_output_height,"wlr-output-height",1,0,0,(SCM o),"")
#define FUNC_NAME s_scm_wlr_output_height
{
  CHECK_TYPE(o);
  return scm_from_int((UNWRAP(o))->height);
}
#undef FUNC_NAME

SCM_DEFINE(scm_wlr_output_phys_width,"wlr-output-physical-width",1,0,0,(SCM o),"")
#define FUNC_NAME s_scm_wlr_output_phys_width
{
  CHECK_TYPE(o);
  return scm_from_int((UNWRAP(o))->phys_width);
}
#undef FUNC_NAME

SCM_DEFINE(scm_wlr_output_phys_height,"wlr-output-physical-height",1,0,0,(SCM o),"")
#define FUNC_NAME s_scm_wlr_output_phys_height
{
  CHECK_TYPE(o);
  return scm_from_int((UNWRAP(o))->phys_height);
}
#undef FUNC_NAME

SCM_DEFINE(scm_wlr_output_refresh,"wlr-output-refresh",1,0,0,(SCM o),"")
#define FUNC_NAME s_scm_wlr_output_refresh
{
  CHECK_TYPE(o);
  return scm_from_int((UNWRAP(o))->refresh);
}
#undef FUNC_NAME

SCM_DEFINE(scm_wlr_output_enabled,"wlr-output-enabled",1,0,0,(SCM o),"")
#define FUNC_NAME s_scm_wlr_output_enabled
{
  CHECK_TYPE(o);
  return scm_from_bool((UNWRAP(o))->enabled);
}
#undef FUNC_NAME

SCM_DEFINE(scm_wlr_output_scale,"wlr-output-scale",1,0,0,(SCM o),"")
#define FUNC_NAME s_scm_wlr_output_scale
{
  CHECK_TYPE(o);
  return scm_from_double((UNWRAP(o))->scale);
}
#undef FUNC_NAME

void
scm_init_wlr_output(void)
{
#ifndef SCM_MAGIC_SNARFER
#include "output.x"
#endif
}
