
noinst_DATA = scwm

DISTCLEANFILES = scwm

scwm: ${top_srcdir}/scheme
	if [ "$(LN_S)" = "ln -s" ]; then \
	   $(LN_S) ${top_srcdir}/scheme ./scwm ; \
	else \
	   mkdir ./scwm ; \
	   (cd scwm ; $(LN_S) ${top_srcdir}/scheme/*.scm .) ; \
	fi
