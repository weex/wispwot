# the name and executables of the project
bin_SCRIPTS = scripts/wispwot
# the data directory. Users can set this via --prefix: @datarootdir@ = PREFIX/share. See ./configure --help
wispwotdir = @datarootdir@/wispwot
# the data files
nobase_wispwot_DATA =

# where to install guile modules to import
# nobase_ as prefix prevents stripping leading directories
# see https://www.gnu.org/software/automake/manual/html_node/Alternative.html#index-nobase_005f
siteschemedir = $(datarootdir)/guile/site/$(GUILE_EFFECTIVE_VERSION)
nobase_sitescheme_DATA = language/wisp.scm language/wisp/spec.scm
sitewispdir = $(datarootdir)/guile/site/$(GUILE_EFFECTIVE_VERSION)
nobase_sitewisp_DATA = $(wildcard *.w) $(wildcard */*.w)

GOSCMBJECTS = $(nobase_sitescheme_DATA:%.scm=%.go)
nobase_goscm_DATA = $(GOSCMBJECTS)
goscmdir=$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache

GOWBJECTS = $(nobase_sitewisp_DATA:%.w=%.go)
nobase_gow_DATA = $(GOWBJECTS)
gowdir=$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache

# Make sure source files are installed first, so that the mtime of
# installed compiled files is greater than that of installed source
# files.  See
# <http://lists.gnu.org/archive/html/guile-devel/2010-07/msg00125.html>
# for details. The missing underscore before DATA is intentional.
guile_install_gow_files = install-nobase_gowDATA
$(guile_install_gow_files): install-nobase_sitewispDATA
guile_install_goscm_files = install-nobase_goscmDATA
$(guile_install_goscm_files): install-nobase_siteschemeDATA


all: $(wispwot) $(nobase_bin_SCRIPTS) ChangeLog AUTHORS  $(nobase_sitewisp_DATA) $(nobase_sitescheme_DATA)

SOURCES =               \
  wispwot/wispwot.w    \
  run-wispwot.w

EXTRA_DIST =            \
  $(nobase_wispwot_DATA) \
  $(nobase_sitescheme_DATA) \
  AUTHORS.in            \
  README                \
  bootstrap.sh          \
  pre-inst-env.in

CLEANFILES = $(GOSCMBJECTS) $(GOWBJECTS)
DISTCLEANFILES = 
# don't spout out lots of stuff at each distcheck. Disable for debugging.
AM_DISTCHECK_CONFIGURE_FLAGS="--quiet"

# precompile all scheme files
.scm.go:
	$(GUILE_TOOLS) compile -L @abs_top_builddir@ -L @abs_top_srcdir@ $(GUILE_WARNINGS) -o "$@" "$<"
.w.go:
	$(GUILE) -L @abs_top_builddir@ -L @abs_top_srcdir@ -c '(import (language wisp spec))' && $(GUILE_TOOLS) compile -L @abs_top_builddir@ -L @abs_top_srcdir@ -x .w -f wisp $(GUILE_WARNINGS) -o "$@" "$<"
## run-wispwot.w needs cached files from dependencies that do not get created by guild, so we import from guile once
run-wispwot.go : run-wispwot.w
	$(GUILE) -L @abs_top_builddir@ -L @abs_top_srcdir@ -c '(import (language wisp spec))' && $(GUILE) -L @abs_top_builddir@ -L @abs_top_srcdir@ --language=wisp -x .w -c 'import : run-wispwot' && $(GUILE_TOOLS) compile -L @abs_top_builddir@ -L @abs_top_srcdir@ -x .w -f wisp $(GUILE_WARNINGS) -o "$@" "$<"
language/wisp/spec.go: language/wisp/spec.scm language/wisp.go 

ACLOCAL_AMFLAGS = -I m4


# pseudo-lines to show help output for install, because the guile build setup requires leaving install and uninstall unchanged
#install: ## install to locations defined by ./configure
#uninstall: ## remove the installed files
.PHONY: help
help: ## Show this help message.
	@echo 'Usage:'
	@echo ':make [target] ...' | sed "s/\(target\)/\\x1b[36m\1\\x1b[m/" | column -c2 -t -s :
	@echo
	@echo 'Targets:'
	@echo -e "$$(grep -hE '^\S+:.*##' $(MAKEFILE_LIST) | sed 's/^#//' | sed -e 's/:.*##\s*/:/' -e 's/^\(.\+\):\(.*\)/:\\x1b[36m\1\\x1b[m:\2/' | column -c2 -t -s :)"
	@echo
	@echo '(see ./configure --help for setup options)'

# create basic files for the tarball
.SECONDARY: ChangeLog AUTHORS
ChangeLog: ## create the ChangeLog from the history
	@echo Generating $@
	@echo "For user-visible changes, see the NEWS file" > @abs_top_builddir@/$@
	@echo >> @abs_top_builddir@/$@
	@if test -d @abs_top_srcdir@/.git; then cd @abs_top_srcdir@; git log --date-order --date=short | sed -e '/^commit.*$/d' | awk '/^Author/ {sub(/\\$/,""); getline t; print $0 t; next}; 1' | sed -e 's/^Author: //g' | sed -e 's/\(.*\)>Date:   \([0-9]*-[0-9]*-[0-9]*\)/\2  \1>/g' | sed -e 's/^\(.*\) \(\)\t\(.*\)/\3    \1    \2/g' >> @abs_top_builddir@/$@; cd -; fi
	@if test -d @abs_top_srcdir@/.hg; then hg -R @abs_top_srcdir@ log --style changelog >> $@; fi
AUTHORS: AUTHORS.in ## create the AUTHORS file from the history and AUTHORS.in
	@echo Generating $@
	@cp "$<" "$@"
	@if test -d @abs_top_srcdir@/.git; then cd @abs_top_srcdir@; git log --format='%aN' | sort -u > @abs_top_builddir@/$@; cd -; fi
	@if test -d @abs_top_srcdir@/.hg; then hg -R @abs_top_srcdir@ --config extensions.churn= churn -t "{author}" > $@; fi

