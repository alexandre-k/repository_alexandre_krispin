# Generated automatically from Makefile.in by configure.
# where the w3 lisp files should go
srcdir  = .
prefix  = /home/pj/tmp-emacs/
datadir = ${prefix}/share
lispdir = $(datadir)/emacs/site-lisp
top_srcdir = ..
confdir = $(datadir)/emacs/w3


############## no user servicable parts beyond this point ###################
# Have to preload a few things to get a nice clean compile

EMACS     = emacs
GNUSDIR   = /usr/local/share/emacs/22.0.50/lisp/gnus/
URLDIR    = /usr/local/share/emacs/22.0.50/lisp/url/
INSTALL   = /usr/bin/install -c
RM        = rm -f

DEPS = -l $(srcdir)/docomp.el -l $(URLDIR)/url-vars.el -l $(srcdir)/w3-vars.el

# compile with noninteractive and relatively clean environment
BATCHFLAGS = -batch -q -no-site-file

W3_SOURCES =								\
	css.el								\
	url-hotlist.el							\
	w3-cfg.el							\
	w3-cus.el							\
	w3-dired.el							\
	w3-display.el							\
	w3-emacs.el							\
	w3-emulate.el							\
	w3-forms.el							\
	w3-fast-parse.el						\
	w3-hot.el							\
	w3-hotindex.el							\
	w3-imap.el							\
	w3-java.el							\
	w3-keymap.el							\
	w3-menu.el							\
	w3-mouse.el							\
	w3-parse.el							\
	w3-print.el							\
	w3-props.el							\
	w3-speak-table.el						\
	w3-speak.el							\
	w3-style.el							\
	w3-toolbar.el							\
	w3-vars.el							\
	w3-widget.el							\
	w3-xemac.el							\
	w3.el

UTIL_SOURCES = 								\
	base64.el devices.el font.el images.el md5.el socks.el ssl.el

AUTO_SOURCES = auto-autoloads.el custom-load.el w3-auto.el w3-cfg.el

W3_OBJECTS = $(W3_SOURCES:.el=.elc)
UTIL_OBJECTS = $(UTIL_SOURCES:.el=.elc)
AUTO_OBJECTS = $(AUTO_SOURCES:.el=.elc)

# Some makes appear to choke if I try to do a substitution on a really
# large macro like SOURCES when it included all of these, so lets combine
# everything now that it has been appropriately munged in smaller chunks.
ALLSOURCES = $(W3_SOURCES) $(UTIL_SOURCES) $(AUTO_SOURCES)
ALLOBJECTS = $(W3_OBJECTS) $(UTIL_OBJECTS) $(AUTO_OBJECTS)

# Warning!  Currently, the following file can _NOT_ be bytecompiled.
EXTRAS = w3-sysdp.el docomp.el

.SUFFIXES: .elc .el

.el,v.el:
	co -q $<

.el.elc:
	GNUSDIR=$(GNUSDIR) URLDIR=$(URLDIR) W3SRCDIR=$(srcdir) $(EMACS) $(BATCHFLAGS) $(DEPS) -f batch-byte-compile $<

all:	$(AUTO_SOURCES) w3

w3-auto.el: auto-autoloads.el
	GNUSDIR=$(GNUSDIR) URLDIR=$(URLDIR) W3SRCDIR=$(srcdir) $(EMACS) $(BATCHFLAGS) -l $(srcdir)/docomp.el -f emacs-batch-build-autoloads $(srcdir) auto-autoloads.el

auto-autoloads.el: $(SOURCES)
	GNUSDIR=$(GNUSDIR) URLDIR=$(URLDIR) W3SRCDIR=$(srcdir) $(EMACS) $(BATCHFLAGS) -l $(srcdir)/docomp.el -f emacs-batch-build-autoloads $(srcdir) auto-autoloads.el

custom-load.el: $(SOURCES)
	GNUSDIR=$(GNUSDIR) URLDIR=$(URLDIR) W3SRCDIR=$(srcdir) $(EMACS) $(BATCHFLAGS) $(DEPS) -f emacs-batch-build-custom-load $(srcdir)
	if [ -f cus-load.el ]; then mv cus-load.el custom-load.el; fi

w3-cfg.el: $(srcdir)/w3-cfg.el.in ../config.status
	( cd ../ ; CONFIG_FILES=lisp/w3-cfg.el CONFIG_HEADERS= ./config.status)

w3:	$(ALLSOURCES) $(ALLOBJECTS)
	@echo Build of w3 complete...

xemacs-w3: $(ALLSOURCES) $(ALLOBJECTS)
	@echo Build of w3 complete...

fast:	$(ALLSOURCES) $(EXTRAS)
	GNUSDIR=$(GNUSDIR) URLDIR=$(URLDIR) W3SRCDIR=$(srcdir) $(EMACS) $(BATCHFLAGS) $(DEPS) -f batch-byte-compile $(ALLSOURCES)
	@echo Build of w3 complete...

autoloads:
	$(EMACS) $(BATCHFLAGS) -eval '(setq autoload-package-name "w3")' -l autoload -f batch-update-directory .

install: all
	@echo Installing in $(lispdir)
	( if [ ! -d $(lispdir) ]; then mkdir -p $(lispdir); fi )
	for x in $(ALLSOURCES) $(EXTRAS) $(ALLOBJECTS); do		\
		if [ -f $$x ]; then					\
			$(INSTALL) -m 644 $$x $(lispdir);		\
		else							\
			$(INSTALL) -m 644 $(srcdir)/$$x $(lispdir);	\
		fi							\
	done;

distclean: clean
	$(RM) config.* Makefile w3-cfg.el w3-auto.el $(AUTO_SOURCES)

clean:
	$(RM) *.elc *~

# Automatic makefile rebuilding
Makefile: $(srcdir)/Makefile.in ../config.status
	( cd .. ; CONFIG_FILES=lisp/Makefile ./config.status )

$(top_srcdir)/configure: $(top_srcdir)/configure.in $(top_srcdir)/aclocal.m4
	cd $(top_srcdir) && autoreconf

../config.status: $(top_srcdir)/configure
	cd .. && ./config.status --recheck

# Distribution building
version: $(ALLSOURCES) $(EXTRAS) descrip.mms
	test -d $(DIRNAME)/lisp || mkdir -p $(DIRNAME)/lisp
	for x in descrip.mms $(ALLSOURCES) $(EXTRAS) w3-cfg.el.in w3-cfg.nt Makefile.in; do $(INSTALL) -m 644 $$x $(DIRNAME)/lisp; done
	cd $(DIRNAME)/lisp && $(RM) $(AUTO_SOURCES)
