%define nam      guile
%define ver      1.3.3
%define rel      2gjb
%define prefix   /usr

Summary: A GNU implementation of Scheme for application extensibility.
Name:        %{nam}
Version:     %{ver}
Release: 1gjb
Source:      ftp://ftp.gnu.org/pub/gnu/%{nam}-%{ver}.tar.gz
Copyright:   GPL
Group: Development/Languages
Buildroot:   /var/tmp/%{nam}-%{ver}-root
Prereq:      /sbin/install-info
Requires:    umb-scheme

%description
GUILE (GNU's Ubiquitous Intelligent Language for Extension) is a library
implementation of the Scheme programming language, written in C.  GUILE
provides a machine-independent execution platform that can be linked in
as a library during the building of extensible programs.

Install the guile package if you'd like to add extensibility to programs
that you are developing.  You'll also need to install the guile-devel
package.

%package     devel
Summary: The libraries and header files for the GUILE extensibility library.
Group: Development/Libraries
Requires:    guile

%description devel
The guile-devel package includes the libraries, header files, etc., that
you'll need to develop applications that are linked with the GUILE
extensibility library.

You need to install the guile-devel package if you want to develop
applications that will be linked to GUILE.  You'll also need to install
the guile package.

%prep
%setup -q

%build
libtoolize --force --copy
CFLAGS="$RPM_OPT_FLAGS" ./configure --prefix=%{prefix} --enable-dynamic-linking
cd guile-readline; make readline.x; cd ..
make

%install
rm -rf $RPM_BUILD_ROOT

make install prefix=$RPM_BUILD_ROOT%{prefix}

pushd $RPM_BUILD_ROOT
strip .%{prefix}/bin/guile
chmod +x .%{prefix}/lib/libguile.so.*.0.0
gzip -9nf .%{prefix}/info/data-rep*
mkdir -p .%{prefix}/share/guile/site
ln -s ../../lib/umb-scheme/slib .%{prefix}/share/guile/slib
popd

%clean
rm -rf $RPM_BUILD_ROOT

%post   -p /sbin/ldconfig
%postun -p /sbin/ldconfig

%post devel
/sbin/install-info %{prefix}/info/data-rep.info.gz %{prefix}/info/dir

%preun devel
/sbin/install-info --delete %{prefix}/info/data-rep.info.info.gz %{prefix}/info/dir

%files
%defattr(-,root,root)
%doc AUTHORS COPYING ChangeLog GUILE-VERSION HACKING NEWS README TODO
%doc SNAPSHOTS ANON-CVS THANKS
%{prefix}/bin/guile
%{prefix}/lib/libguile.so.*.*
%{prefix}/lib/libguilereadline.so.*.*
%dir %{prefix}/share/guile
%dir %{prefix}/share/guile/site
%dir %{prefix}/share/guile/%{PACKAGE_VERSION}
%{prefix}/share/guile/%{PACKAGE_VERSION}/ice-9
%{prefix}/share/aclocal/*
%{prefix}/share/guile/slib

%files devel
%defattr(-,root,root)
%{prefix}/bin/guile-config
%{prefix}/bin/guile-snarf
%{prefix}/lib/lib*so
%{prefix}/lib/lib*a
%{prefix}/include/guile
%{prefix}/include/guile-readline
%{prefix}/include/libguile
%{prefix}/include/libguile.h
%{prefix}/info/data-rep*

%changelog
* Wed Sep 1 1999 Greg J. Badros <gjb@cs.washington.edu>
- Updated for guile-1.3.3, fix readline support of install

* Thu Aug 26 1999 Greg J. Badros <gjb@redhat.com>
- Updated for guile-1.3.2

* Sun Mar 21 1999 Cristian Gafton <gafton@redhat.com> 
- auto rebuild in the new build environment (release 6)

* Wed Mar 17 1999 Michael Johnson <johnsonm@redhat.com>
- added .ansi patch to fix #endif

* Wed Feb 10 1999 Cristian Gafton <gafton@redhat.com>
- add patch for the scm stuff

* Sun Jan 17 1999 Jeff Johnson <jbj@redhat.com>
- integrate changes from rhcn version (#640)

* Tue Jan 12 1999 Cristian Gafton <gafton@redhat.com>
- call libtoolize first to get it to compile on the arm

* Sat Jan  9 1999 Todd Larason <jtl@molehill.org>
- Added "Requires: guile" at suggestion of Manu Rouat <emmanuel.rouat@wanadoo.fr>

* Fri Jan  1 1999 Todd Larason <jtl@molehill.org>
- guile-devel does depend on guile
- remove devel dependancy on m4
- move guile-snarf from guile to guile-devel
- Converted to rhcn

* Wed Oct 21 1998 Jeff Johnson <jbj@redhat.com>
- update to 1.3.
- don't strip libguile.so.*.0.0. (but set the execute bits).

* Thu Sep 10 1998 Cristian Gafton <gafton@redhat.com>
- spec file fixups

* Wed Sep  2 1998 Michael Fulbright <msf@redhat.com>
- Updated for RH 5.2

* Mon Jan 26 1998 Marc Ewing <marc@redhat.com>
- Started with spec from Tomasz Koczko <kloczek@idk.com.pl>
- added slib link

* Thu Sep 18 1997 Tomasz Koczko <kloczek@idk.com.pl>          (1.2-3)
- added %attr(-, root, root) for %doc, 
- in %post, %postun ldconfig runed as parameter "-p",
- removed /bin/sh from requires,
- added %description,
- changes in %files.

* Fri Jul 11 1997 Tomasz Koczko <kloczek@rudy.mif.pg.gda.pl>  (1.2-2)
- all rewrited for using Buildroot,
- added %postun,
- removed making buid logs,
- removed "--inclededir", added "--enable-dynamic-linking" to configure
  parameters,
- added striping shared libs and /usr/bin/guile,
- added "Requires: /bin/sh" (for guile-snarf) in guile package and
  "Requires: m4" for guile-devel,
- added macro %{PACKAGE_VERSION} in "Source:" and %files,
- added %attr macros in %files.
