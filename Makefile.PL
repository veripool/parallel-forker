# $Revision: #5 $$Date: 2004/08/26 $$Author: ws150726 $
# DESCRIPTION: Perl ExtUtils: Type 'perl Makefile.PL' to create a Makefile for this package
#
# Copyright 2003-2005 by Wilson Snyder.  This program is free software;
# you can redistribute it and/or modify it under the terms of either the GNU
# General Public License or the Perl Artistic License.
######################################################################

require 5.005;
use ExtUtils::MakeMaker;
use Carp;

sub MY::postamble {
"
DIST_DEFAULT += manifest

README: readme.texi
	-rm \$@
	makeinfo \$< --output \$@ --no-headers --no-validate

tag:
	cvs tag -F \$(DISTNAME)_\$(VERSION_SYM) .

maintainer-diff:
	c4 diff ...@\$(DISTNAME)_\$(VERSION_SYM) 

maintainer-dist: dist tag
	cp \$(DISTVNAME).tar.gz \$(HOME)/src/kits
	mv \$(DISTVNAME).tar.gz \$(HOME)/backpack

maintainer-clean: distclean
	-rm README vtrace

maintainer-copy: README

clean::
	-rm -rf test_dir
";
}

WriteMakefile(
	      VERSION_FROM => 'Forker.pm',
	      DISTNAME  => 'Parallel-Forker',
	      NAME      => 'Parallel::Forker',
	      AUTHOR	=> 'Wilson Snyder <wsnyder@wsnyder.org>',
	      ABSTRACT  => 'Parallel job forking and management on objects and system calls',
	      PREREQ_PM => {'Time::HiRes' => 0,
			},
	      'dist'    => {COMPRESS => 'gzip -9f',
			    SUFFIX   => '.gz', 
			    DIST_DEFAULT => 'README all tardist',
			},
	      );

