%define nam      guile-gtk
%define ver      26Aug99
%define rel      2gjb
%define prefix   /usr

Name: %nam
Summary: Glue code that make gtk accesible from guile
Version: %ver
Release: %rel
Copyright: GPL
Group: X11/GTK
BuildRoot: /tmp/%{nam}-%{ver}-build
Source: ftp://scwm.mit.edu/pub/scwm/%{nam}-%{ver}.tar.gz
Packager: Greg J. Badros <gjb@cs.washington.edu>
URL: http://scwm.mit.edu
Docdir: %{prefix}/doc

%description


%changelog
* Thu Aug 26 1999 Greg J. Badros <gjb@cs.washington.edu>
  Built from CVS especially for the Scwm-0.99.2 on RH6.

* Fri Jul 30 1999 Ariel Rios <jarios@usa.net>
  This rpm was built from cvs especially for the Linux PPP 6.0 distribution

%prep
%setup

%build
  CFLAGS="$RPM_OPT_FLAGS" ./configure --prefix=%prefix
  make

%install
make prefix=$RPM_BUILD_ROOT%{prefix} \
     ROOT=$RPM_BUILD_ROOT \
     sitedir=$RPM_BUILD_ROOT%{prefix}/share/guile/site \
     schemedir=$RPM_BUILD_ROOT%{prefix}/share/guile \
     install 

%clean
rm -rf $RPM_BUILD_ROOT

%post -p /sbin/ldconfig

%postun -p /sbin/ldconfig

%files
%defattr(-, root, root)

%doc AUTHORS COPYING ChangeLog NEWS README

%{prefix}/bin/*
%{prefix}/include/*
%{prefix}/lib/*
%{prefix}/share/guile-gtk/*
%{prefix}/share/guile/gtk/*.scm
%{prefix}/share/guile/gtk-1.2/*.scm
