#!/usr/bin/perl -w
# $Id$
# DESCRIPTION: Perl ExtUtils: Type 'make test' to test this package
#
# Copyright 2003-2005 by Wilson Snyder.  This program is free software;
# you can redistribute it and/or modify it under the terms of either the GNU
# Lesser General Public License or the Perl Artistic License.
######################################################################

use Test;
use strict;
use Time::HiRes qw (gettimeofday usleep tv_interval);

BEGIN { plan tests => 31 }
BEGIN { require "t/test_utils.pl"; }

BEGIN { $Parallel::Forker::Debug = 1; }

use Parallel::Forker;
ok(1);

######################################################################

my $fork = new Parallel::Forker;
ok(1);

$SIG{CHLD} = sub { Parallel::Forker::sig_child($fork); };  # Not method, as is less stuff for a handler to do
$SIG{TERM} = sub { $fork->kill_tree_all('TERM') if $fork; die "Quitting...\n"; };
ok(1);

{
    my $Didit;
    $fork->schedule(run_on_start => sub { },
		    run_on_finish => sub { $Didit = 1 },
		    ) ->run();
    $fork->wait_all();   # Wait for all children to finish
    ok($Didit);
}

run_a_test(run_it=>1);
run_a_test(wait_it=>1);
run_a_test(wait_it=>1, wait_label=>1);

######################################################################

our $WTN;
sub run_a_test {
    my %params = (run_it => 0,
		  wait_it => 0,
		  wait_label => 0,
		  @_);

    print "   A test\n";
    my $p1 = $fork->schedule(
			     label => 'after_p1_p2',
			     run_on_start => sub { usleep(300*1000); },
			     run_on_finish => sub {
				 my ($procref, $status) = @_;
				 $procref->{my_done_time} = [gettimeofday()];
			     },);
    $p1->run() if $params{run_it};
    $p1->ready() if $params{wait_it};
    ok(1);

    my $p2 = $fork->schedule(
			     label => 'after_p1_p2',
			     run_on_start => sub { usleep(200*1000); },
			     run_on_finish => sub {
				 my ($procref, $status) = @_;
				 $procref->{my_done_time} = [gettimeofday()];
			     },);
    $p2->run() if $params{run_it};
    $p2->run_after($p1) if $params{wait_it};
    $p2->ready() if $params{wait_it};
    ok(1);

    my $p3 = $fork->schedule(run_on_start => sub { usleep(100*1000); },
			     run_on_finish => sub {
				 my ($procref, $status) = @_;
				 $procref->{my_done_time} = [gettimeofday()];
			     },);
    $p3->run() if $params{run_it};
    if ($params{wait_label}) {
	$p3->run_after('after_p1_p2');
    } elsif ($params{wait_it}) {
	$p3->run_after($p2->{name});
    }
    $p3->ready() if $params{wait_it};
    ok(1);

    $fork->wait_all();   # Wait for all children to finish
    ok(1);

    ok($p1->{my_done_time});   # Check actually ran at some point
    ok($p2->{my_done_time});
    ok($p3->{my_done_time});
    # Check we executed in parallel (p1&p2), or with appropriate ordering (p1 then p2)
    ok(tv_interval($p1->{my_done_time},$p2->{my_done_time}) < 0) if $params{run_it};
    ok(tv_interval($p1->{my_done_time},$p2->{my_done_time}) > 0) if $params{wait_it};

    ok(tv_interval($p2->{my_done_time},$p3->{my_done_time}) < 0) if $params{run_it};
    ok(tv_interval($p2->{my_done_time},$p3->{my_done_time}) > 0) if $params{wait_it};

    $WTN++;
    $fork->write_tree(filename=>"test_dir/10_write_tree_$WTN.log");
}

