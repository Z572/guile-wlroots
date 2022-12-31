#include <libguile.h>
#include <wlr/types/wlr_output.h>
#include "../helper.c"
#define WRAP(o) WRAP_WLR_OUTPUT(o)
#define UNWRAP(o) (struct wlr_output*)(UNWRAP_WLR_OUTPUT(o))
#define CHECK_TYPE(o) do { \
    SCM_ASSERT(scm_from_bool(SCM_IS_A_P(o ,scm_c_private_ref("wlroots types output","<wlr-output>"))) ,o, SCM_ARG1, FUNC_NAME); \
  } while (0)

SCM_DEFINE(scm_wlr_output_modes,"%wlr-output-modes",1,0,0,(SCM o),"")
#define FUNC_NAME s_scm_wlr_output_modes
{
  CHECK_TYPE(o);
  return FROM_P(&((UNWRAP(o))->modes));
}
#undef FUNC_NAME

void
scm_init_wlr_output(void)
{
#ifndef SCM_MAGIC_SNARFER
#include "output.x"
#endif
}
