#!/usr/bin/perl -w
# $Revision: #4 $$Date$$Author$
# DESCRIPTION: Perl ExtUtils: Type 'make test' to test this package
#
# Copyright 2003-2005 by Wilson Snyder.  This program is free software;
# you can redistribute it and/or modify it under the terms of either the GNU
# General Public License or the Perl Artistic License.
######################################################################

use Test;
use strict;

BEGIN { plan tests => 9 }
BEGIN { require "t/test_utils.pl"; }

BEGIN { $Parallel::Forker::Debug = 1; }

use Parallel::Forker;
ok(1);

######################################################################

a_test(0);
a_test(1);

sub a_test {
    my $failit = shift;

    my $fork = new Parallel::Forker;
    $SIG{CHLD} = sub { Parallel::Forker::sig_child($fork); };
    $SIG{TERM} = sub { $fork->kill_tree_all('TERM') if $fork; die "Quitting...\n"; };
    ok(1);

    # Test use of -'s in run_afters
    my %Didit;

    $fork->schedule(name => 'a',
		    run_on_start => sub {
			if ($failit) {exit(13);} # Intentional bad status
			exit(0);
		    },
		    run_on_finish => sub {
			my ($procref, $status) = @_;
			#print "Stat = $status\n";
			if ($failit) {
			    if (($status>>8) == 13) { $Didit{a} = 1 }
			} else { $Didit{a} = 1; }
		    },
		    run_after => ['-doesnt_exist'],
		    label => 'a',
		    );
    $fork->schedule(name => 'b',
		    run_on_start => sub { },
		    run_on_finish => sub { $Didit{b} = 1 },
		    run_after => ['| a'],
		    label => 'd2',
		    );
    my $na =
    $fork->schedule(name => 'c',
		    run_on_start => sub { },
		    run_on_finish => sub { $Didit{c} = 1 },
		    run_after => ['!a'],
		    label => 'd3',
		    );
    $fork->schedule(name => 'd',
		    run_on_start => sub { },
		    run_on_finish => sub { $Didit{d} = 1 },
		    run_after => ['^a'],
		    );
    $fork->schedule(name => 'e',
		    run_on_start => sub { },
		    run_on_finish => sub { $Didit{e} = 1 },
		    run_after => [$na],
		    );
    $fork->schedule(name => 'e2',
		    run_on_start => sub { },
		    run_on_finish => sub { $Didit{e2} = 1 },
		    run_after => ['e'],
		    );
    $fork->schedule(name => 'f',
		    run_on_start => sub { },
		    run_on_finish => sub { $Didit{f} = 1 },
		    run_after => ["d2 | d3"],
		    );

    # Run them
    $fork->ready_all();
    $fork->wait_all();

    # Check right procs died
    print " Didit: ", (join ' ',(sort (keys %Didit))), "\n";
    if ($failit) {
	ok($Didit{a} && !$Didit{b} && $Didit{c} && $Didit{d} && $Didit{f});
    } else {
	ok($Didit{a} && $Didit{b} && !$Didit{c} && $Didit{d} && $Didit{f});
    }
    ok( (($Didit{e}||-1) == ($Didit{c}||-1))
	&& (($Didit{e}||-1) == ($Didit{e2}||-1)));

    # Check all marked
    my $o=1;
    foreach my $procref ($fork->processes) {
	if (!$procref->is_done && !$procref->is_parerr) {
	    print " %Error: process $procref->{name} is $procref->{_state} not done\n";
	}
    }
    ok($o);
}
