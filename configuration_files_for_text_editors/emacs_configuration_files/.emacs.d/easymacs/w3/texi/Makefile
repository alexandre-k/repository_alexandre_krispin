# Generated automatically from Makefile.in by configure.
srcdir  = .
prefix  = /home/pj/tmp-emacs/
infodir = ${prefix}/info
top_srcdir = ..

EMACS     = emacs
INSTALL   = /usr/bin/install -c
RM        = rm -f
MAKEINFO  = makeinfo
TEXI2HTML = echo Cannot create html version of
TEXI2HTML_FLAGS = 
INSTALLINFO = install-info
DVIPS     = dvips

.SUFFIXES: .txi .dvi .info .html

%.txi: %.txi,v
	co -q $@

%.html: %.txi
	$(TEXI2HTML) $(TEXI2HTML_FLAGS) $<

%.dvi: %.txi
	$(TEXI2DVI) $(TEXI2DVI_FLAGS) $<
	$(RM) 	$(*).cp  $(*).fn  $(*).ky  $(*).pg  $(*).tp  $(*).vr 	\
		$(*).cps $(*).fns $(*).kys $(*).pgs $(*).tps $(*).vrs	\
		$(*).log $(*).toc $(*).aux

%.ps: %.dvi
	$(DVIPS) -o $@ $<

%.info: %.txi
#	$(EMACS) -batch -q -no-site-file -eval '(find-file "$<")' -l texinfmt -f texinfo-every-node-update -f save-buffer
	$(MAKEINFO) $<

MANUALS=w3.txi w3-faq.txi

all: info dvi

install:
	( if [ ! -d $(infodir) ]; then mkdir -p $(infodir); fi )
	for x in *.info*; do $(INSTALL) $$x $(infodir); done
	-for x in *.info; do $(INSTALLINFO) $$x $(infodir)/dir; done

distclean: clean
	$(RM) Makefile

clean:
	$(RM) *.dvi *.info* *.html *.ps

dvi: $(MANUALS:.txi=.dvi)

info: $(MANUALS:.txi=.info)

html: $(MANUALS:.txi=.html)

ps: $(MANUALS:.txi=.ps)

version: $(MANUALS) descrip.mms clean
	test -d $(DIRNAME)/texi || mkdir -p $(DIRNAME)/texi
	$(MAKE) info
	for x in descrip.mms $(MANUALS) *info* Makefile.in; do $(INSTALL) -m 644 $$x $(DIRNAME)/texi; done

# Automatic makefile rebuilding, etc.
Makefile: $(srcdir)/Makefile.in ../config.status
	(cd .. ; CONFIG_FILES=texi/Makefile ./config.status)

$(top_srcdir)/configure: $(top_srcdir)/configure.in $(top_srcdir)/aclocal.m4
	cd $(top_srcdir) && autoreconf

../config.status: $(top_srcdir)/configure
	cd .. && ./config.status --recheck
