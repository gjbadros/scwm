## autogen.sh - generate all the twisty little files.

# Generate aclocal.m4 from configure.in
aclocal

# Generate Makefile.in's from Makefile.am's and configure.in
# (adding missing files that may be needed for proper operation)
automake --add-missing

# Generate include/config.h.in from acconfig.h and configure.in
autoheader

# Generate configure from configure.in and aclocal.m4
autoconf
