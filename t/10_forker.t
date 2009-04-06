#!/usr/bin/perl -w
# DESCRIPTION: Perl ExtUtils: Type 'make test' to test this package
#
# Copyright 2003-2009 by Wilson Snyder.  This program is free software;
# you can redistribute it and/or modify it under the terms of either the GNU
# Lesser General Public License Version 3 or the Perl Artistic License Version 2.0.
######################################################################

use Test;
use strict;
use Time::HiRes qw (gettimeofday usleep tv_interval sleep time);

BEGIN { plan tests => 50 }
BEGIN { require "t/test_utils.pl"; }

BEGIN { $Parallel::Forker::Debug = 1; }

use Parallel::Forker;
ok(1);

######################################################################

my $fork = new Parallel::Forker;
ok(1);
ok($fork->in_parent);

$SIG{CHLD} = sub { Parallel::Forker::sig_child($fork); };  # Not method, as is less stuff for a handler to do
$SIG{TERM} = sub { $fork->kill_tree_all('TERM') if $fork && $fork->in_parent; die "Quitting...\n"; };
$fork->use_sig_child(1);
ok(1);

{
    my $Didit;
    my ($start_pid, $finish_pid);
    $fork->schedule (
		     run_on_start => sub { $start_pid = $$; },
		     run_on_finish => sub { $Didit = 1; $finish_pid = $$; },
		     ) ->run();
    $fork->wait_all();   # Wait for all children to finish
    ok($Didit);
    ok(not defined $start_pid); # runs in child
    ok($finish_pid, $$); # runs in parent (us)
}

sub restarting_sleep {
    my ($seconds) = @_;
    my $start = time;
    my $time_left;
    while ($time_left = (time - $start) < $seconds) {
	sleep($seconds - $time_left);
    }
}

# poll() services multiple completed children and starts correct number of new
#   workers
# Method:
#  - Cap processes at 3
#  - Schedule 6 processes, the first two of which will exit quickly
#  - Wait long enough for first two to finish
#  - poll() should fire both run_on_finish callbacks
#  - Current running process count should be 3 again (3rd original process
#    plus two new ones)
{
    my @done;
    sub quick_sleep { sleep 1 }
    sub slow_sleep { sleep 7 }
    sub finish_func { push @done, $_[0]{name} }

    $fork->max_proc(3);
    $fork->schedule (
		     name => 'p1',
		     run_on_start => \&quick_sleep,
		     run_on_finish => \&finish_func,
		     );
    $fork->schedule (
		     name => 'p2',
		     run_on_start => \&quick_sleep,
		     run_on_finish => \&finish_func,
		     );
    $fork->schedule (
		     name => 'p3',
		     run_on_start => \&slow_sleep,
		     run_on_finish => \&finish_func,
		     );
    $fork->schedule (
		     name => 'p4',
		     run_on_start => \&quick_sleep,
		     run_on_finish => \&finish_func,
		     run_after => ['p1 | p2 | p3'],
		     );
    $fork->schedule (
		     name => 'p5',
		     run_on_start => \&quick_sleep,
		     run_on_finish => \&finish_func,
		     run_after => ['p1 | p2 | p3'],
		     );
    $fork->schedule (
		     name => 'p6',
		     run_on_start => \&quick_sleep,
		     run_on_finish => \&finish_func,
		     run_after => ['p1 | p2 | p3'],
		     );

    # Nothing should have started running yet.
    my $running_count = $fork->running;
    ok( $running_count, 0 );

    $fork->ready_all;

    # should have 3 runable and 3 waiting on dependencies (ready)
    ok( scalar(grep { $_->is_ready } $fork->processes), 3 );
    ok( scalar(grep { $_->is_runable } $fork->processes), 3 );

    restarting_sleep(1);

    # Still nothing running...
    $running_count = $fork->running;
    ok( $running_count, 0 );

    $fork->poll;
    print "#  Should have fired off 3 by now...\n";
    restarting_sleep(3);
    print "#  First two should have exited by now...\n";

    # First three should have spawned
    $running_count = scalar $fork->running;
    ok( $running_count, 3 );
    ok( scalar(@done), 0 );  # no done call-backs fired yet

    $fork->poll;

    ok( scalar(grep { $_->is_running } $fork->processes), 3 );
    ok( scalar(grep { $_->is_runable } $fork->processes), 1 );
    ok( scalar(@done), 2 ); # called both finished processes' callbacks

    $fork->wait_all;

    ok( scalar(@done), 6 ); # sanity check
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

# White-box test to ensure that poll() short-circuits and does less work IF
#   you have use_sig_child set to true and _activity is false.
{
    # sanity-check precondition:
    ok( $fork->use_sig_child );

    my $done;
    $fork->schedule(
		    name => 'f1',
		    run_on_start => sub {},
		    run_on_finish => sub { $done = 1 },
		    );

    $fork->ready_all;
    $fork->poll;

    # should be 1 running process now
    ok( scalar(grep { $_->is_running } $fork->processes), 1 );

    sleep 2;
    $fork->{_activity} = 0;

    # because use_sig_child is 1 and _activity is 0, poll() does no work:
    $fork->poll;

    # still have one "running" process
    ok( scalar(grep { $_->is_running } $fork->processes), 1 );
    ok( not $done );

    # but if we have activity (like the sig_child() makes true), we do work:
    $fork->sig_child;
    $fork->poll;

    ok( scalar(grep { $_->is_running } $fork->processes), 0 );
    ok( $done, 1 );
}
