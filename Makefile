# -*- mode: make; -*-

system_name = org.wobh.common-lisp.games.wizards-castle
system_path = ${XDG_DATA_HOME}/common-lisp/source
system_files = org.wobh.common-lisp.games.wizards-castle.asd wizards-castle.lisp # wizards-castle-test.lisp wizards-castle-user.lisp

# see https://asdf.common-lisp.dev/asdf.html#Configuring-ASDF-to-find-your-systems
installdir = $(system_path)/$(system_name)

default :
	@echo "targets: all install clean"

# TODO: every lisp will build differently somewhat differently. May
# need to expand options in asd. May need a tool like roswell.
all :
	@echo "TODO"

installdirs :
	mkdir -p $(installdir)

install : installdirs
	install $(system_files) $(installdir)

uninstall :
	find $(installdir) -print -delete

clean :
	find . -maxdepth 1 -type f \( \
		-iname "*.fasl" \
	\) -print -delete
