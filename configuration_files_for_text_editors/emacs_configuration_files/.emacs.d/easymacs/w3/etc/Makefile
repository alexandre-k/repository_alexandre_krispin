# Generated automatically from Makefile.in by configure.
srcdir  = .
prefix  = /home/pj/tmp-emacs/
datadir = ${prefix}/share
top_srcdir = ..

EMACS     = emacs
INSTALL   = /usr/bin/install -c
RM        = rm -f

TARGETS = default.css

all:

install:
	( if [ ! -d $(datadir) ]; then mkdir -p $(datadir); fi )
	for x in $(TARGETS); do					\
		if [ -f $$x ]; then				\
			$(INSTALL) $$x $(datadir);		\
		else						\
			$(INSTALL) $(srcdir)/$$x $(datadir);	\
		fi						\
	done

distclean: clean
	$(RM) Makefile

clean:
	$(RM) *.orig *.rej *~

version: $(TARGETS) descrip.mms clean 
	test -d $(DIRNAME)/etc || mkdir -p $(DIRNAME)/etc
	for x in $(TARGETS) Makefile.in; do $(INSTALL) -m 644 $$x $(DIRNAME)/etc; done

# Automatic makefile rebuilding, etc.
Makefile: $(srcdir)/Makefile.in ../config.status
	( cd .. ; CONFIG_FILES=etc/Makefile ./config.status )

$(top_srcdir)/configure: $(top_srcdir)/configure.in $(top_srcdir)/aclocal.m4
	cd $(top_srcdir) && autoreconf

../config.status: $(top_srcdir)/configure
	cd .. && ./config.status --recheck
