## autogen.sh - generate all the twisty little files.

# Clean up these files -- maybe we should not have them in CVS? GJB:FIXME::
rm -f ltconfig ltmain.sh

# Generate aclocal.m4 from configure.in
aclocal

# Copy ltconfig and ltmain.sh for this version of libtool
libtoolize --automake --copy

# Generate include/config.h.in from acconfig.h and configure.in
autoheader

# Generate Makefile.in's from Makefile.am's and configure.in
# (adding missing files that may be needed for proper operation)
automake --add-missing --gnu

# Generate configure from configure.in and aclocal.m4
autoconf

# Rerun ./config.status, if it exists,
# to create new Makefiles under modules/* directories (e.g., modules/pie-menus)
# (Note that this only is useful if builddir == scrdir, otherwise the
#  user may need to run config.status from their builddir(s) manually)
#test -x ./config.status && ./config.status || true

# Instead of rerunning config.status, re-run configure from scratch
if [ -r config.status ]; then
  CMD=`awk '/^#.*\/?configure .*/ { $1 = ""; print; exit }' < config.status`
  echo "Running: $CMD $@" 1>&2
else
  CMD=./configure
fi
$CMD "$@" && echo && echo "Now type 'make install' to install scwm-icons."
