GOBJECTS = $(SOURCES:%.scm=%.go) $(NODIST_SOURCES:%.scm=%.go)

nobase_mod_DATA = $(SOURCES) $(NOCOMP_SOURCES) $(NODIST_SOURCES)
nobase_go_DATA = $(GOBJECTS)

snarfcppopts =  $(INCLUDES) $(CPPFLAGS) $(libguile_wlroots_la_CFLAGS)
libguile_wlroots_la_CFLAGS=$(WAYLAND_SERVER_CFLAGS) $(WLROOTS_CFLAGS) $(GUILE_CFLAGS) -DWLR_USE_UNSTABLE
lib_LTLIBRARIES = libguile-wlroots.la
libguile_wlroots_la_SOURCES= wlroots/types/output.c
BUILT_SOURCES= $(libguile_wlroots_la_SOURCES:%.c=%.x)
$(SOURCES): $(lib_LTLIBRARIES)
.c.x:
	$(AM_V_GEN)$(top_builddir)/pre-inst-env guile-snarf -o $@ $< $(snarfcppopts)

guile_install_go_files = install-nobase_goDATA
$(guile_install_go_files): install-nobase_modDATA

NOCOMP_SOURCES= wlroots/helper.c
CLEANFILES = $(GOBJECTS)
EXTRA_DIST = $(SOURCES) $(NOCOMP_SOURCES)
GUILE_WARNINGS =                                          \
  -Wunbound-variable -Warity-mismatch -Wshadowed-toplevel \
  -Wmacro-use-before-definition                           \
  -Wunused-variable                                       \
  -Wduplicate-case-datum -Wbad-case-datum                 \
  -Wformat

SUFFIXES = .scm .go .x
.scm.go:
	$(AM_V_GEN)$(top_builddir)/pre-inst-env $(GUILE_TOOLS) compile $(GUILE_TARGET) $(GUILE_WARNINGS) -o "$@" "$<"


moddir=$(prefix)/share/guile/site/$(GUILE_EFFECTIVE_VERSION)
godir=$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache

include wlroots/Makefile
