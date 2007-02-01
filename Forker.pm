# Fork.pm -- Parallel management
# $Id$
######################################################################
#
# This program is Copyright 2002-2007 by Wilson Snyder.
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of either the GNU General Public License or the
# Perl Artistic License.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
######################################################################

package Parallel::Forker;
require 5.006;
use Time::HiRes qw (usleep);
use Proc::ProcessTable;
use IO::File;

use Parallel::Forker::Process;
use strict;
use Carp;
use vars qw($Debug $VERSION);

$VERSION = '1.213';

######################################################################
#### CONSTRUCTOR

sub new {
    my $class = shift;
    my $self = {
	_activity => 1,		# Optionally set true when a sig_child comes
	_processes => {},	# All process objects, keyed by id
	_labels => {},		# List of process objects, keyed by label
	_runable => {},		# Process objects runable now, keyed by id
	_running => {},		# Process objects running now, keyed *PID*
	_in_child => 0,		# In a child process, don't allow forking
	_run_after_eqn => undef,# Equation to eval to determine if ready to launch
	max_proc => undef,	# Number processes to launch, <1=any, +=that number
	@_
    };
    bless $self, ref($class)||$class;
    return $self;
}

#### ACCESSORS

sub max_proc {
    my $self = shift;
    $self->{max_proc} = shift if $#_>=0;
    return $self->{max_proc};
}

sub running {
    my $self = shift;
    return (values %{$self->{_running}});
}

sub processes {
    my $self = shift;
    return (values %{$self->{_processes}});
}

sub processes_sorted {
    my $self = shift;
    return (sort {$a->{name} cmp $b->{name}} values %{$self->{_processes}});
}

#### METHODS

sub schedule {
    my $class = shift;
    return Parallel::Forker::Process->_new(_forkref=>$class,
					   @_);
}

sub sig_child {
    # Keep minimal to avoid coredumps
    return if !$_[0];
    $_[0]->{_activity} = 1;
}

sub wait_all {
    my $self = shift;
    while ($self->is_any_left) {
	#print "NRUNNING ", scalar ( (keys %{$self->{_running}}) ), "\n";
	$self->poll;
	usleep 100*1000;
    };
}

sub is_any_left {
    my $self = shift;
    return 1 if ( (keys %{$self->{_runable}}) > 0 );
    return 1 if ( (keys %{$self->{_running}}) > 0 );
}

sub find_proc_name {
    my $self = shift;
    my $name = shift;
    # Returns list of processes matching the name or label
    if (exists $self->{_processes}{$name}) {
	return ($self->{_processes}{$name});
    } elsif (exists $self->{_labels}{$name}) {
	return @{$self->{_labels}{$name}};
    }
    return undef;
}

sub poll {
    my $self = shift;
    # For backward compatibilty, we allow poll to check for processes exit,
    # without requiring calls to sig_child.
    #return if !$self->{_activity};

    # We don't have a loop around this any more, as we want to allow
    # applications to do other work.  We'd also need to be careful not to
    # set _activity with no one runnable, as it would potentially cause a
    # inifinite loop.

    $self->{_activity} = 0;
    my $nrunning = grep { not $_->poll } (values %{$self->{_running}});

    foreach my $procref (values %{$self->{_runable}}) {
	last if ($self->{max_proc} && $nrunning >= $self->{max_proc});
	$procref->run;
	$nrunning++;
    }
    $self->{_activity} = 1 if !$nrunning;  # No one running, we need to check for >run next poll()
}

sub ready_all {
    my $self = shift;
    foreach my $procref ($self->processes) {
	$procref->ready() if $procref->is_idle();
    };
}

sub kill_all {
    my $self = shift;
    my $signal = shift || 9;
    foreach my $procref (values %{$self->{_running}}) {
	$procref->kill($signal);
    };
}

sub kill_tree_all {
    my $self = shift;
    my $signal = shift || 9;
    foreach my $procref (values %{$self->{_running}}) {
	$procref->kill_tree($signal);
    };
}

sub write_tree {
    my $self = shift;
    my %params = (@_);
    defined $params{filename} or croak "%Error: filename not specified,";

    my %did_print;
    my $another_loop = 1;
    my $level = 0;
    my $line = 4;
    my @lines;
    while ($another_loop) {
	$another_loop = 0;
	$level++;
      proc:
	foreach my $procref ($self->processes_sorted) {
	    foreach my $ra (values %{$procref->{_after_parents}}) {
		next proc if (($did_print{$ra->{name}}{level}||999) >= $level);
	    }
	    if (!$did_print{$procref->{name}}{level}) {
		$did_print{$procref->{name}}{level} = $level;
		$did_print{$procref->{name}}{line} = $line;
		$another_loop = 1;
		$lines[$line][0] = $procref->_write_tree_line($level,0);
		$lines[$line+1][0] = $procref->_write_tree_line($level,1);
		foreach my $ra (values %{$procref->{_after_parents}}) {
		    $lines[$line][$did_print{$ra->{name}}{line}]
			= $procref->{_after_parents_op}{$ra->{name}};
		}
		$line+=2;
		if ($Debug) {
		    $lines[$line++][0] = $procref->_write_tree_line($level,2);
		    $lines[$line++][0] = $procref->_write_tree_line($level,3);
		    $lines[$line++][0] = $procref->_write_tree_line($level,4);
		}
		$line++;
	    }
	}
    }
    $line++;

    if (0) {
	for (my $row=1; $row<$line; $row++) {
	    for (my $col=1; $col<$line; $col++) {
		print ($lines[$row][$col]?1:0);
	    }
	    print "\n";
	}
    }

    for (my $col=1; $col<=$#lines; $col++) {
	my $col_used_row_min;
	my $col_used_row_max;
	for (my $row=1; $row<=$#lines; $row++) {
	    if ($lines[$row][$col]) {
		$col_used_row_min = min($col_used_row_min, $row);
		$col_used_row_max = max($col_used_row_max, $row);
	    }
	}
	if ($col_used_row_min) {
	    $col_used_row_min = min($col_used_row_min, $col);
	    $col_used_row_max = max($col_used_row_max, $col);
	    for (my $row=$col_used_row_min; $row<=$col_used_row_max; $row++) {
		$lines[$row][$col] ||= '<' if $row==$col;
		$lines[$row][$col] ||= '|';
	    }
	    for (my $row=1; $row<=$#lines; $row++) {
		if (($lines[$row][0]||" ") !~ /^ /) {  # Line with text on it
		    $lines[$row][$col] ||= '-';
		    #$lines[$row][$col-1] ||= '-';
		}

		$lines[$row][$col] ||= ' ';
		#$lines[$row][$col-1] ||= ' ';
	    }
	}
    }

    my $fh = IO::File->new($params{filename},"w") or die "%Error: $! $params{filename},";
    print $fh "Tree of process spawn requirements:\n";
    print $fh "  &  Indicates the program it connects to must complete with ok status\n";
    print $fh "     before the command on this row is allowed to become RUNABLE\n";
    print $fh "  E  As with &, but with error status\n";
    print $fh "  ^  As with &, but with error or ok status\n";
    print $fh "  O  Ored condition, either completing starts proc\n";
    print $fh "\n";
    for (my $row=1; $row<=$#lines; $row++) {
	my $line = "";
	for (my $col=1; $col<$#lines; $col++) {
	    $line .= ($lines[$row][$col]||"");
	}
	$line .= $lines[$row][0]||"";
	$line =~ s/\s+$//;
	print $fh "$line\n"; #if $line !~ /^\s*$/;
    }

    $fh->close();
}

sub min {
    my $rtn = shift;
    foreach my $v (@_) {
	$rtn = $v if !defined $rtn || (defined $v && $v < $rtn);
    }
    return $rtn;
}
sub max {
    my $rtn = shift;
    foreach my $v (@_) {
	$rtn = $v if !defined $rtn || (defined $v && $v > $rtn);
    }
    return $rtn;
}

######################################################################
=pod

=head1 NAME

Parallel::Forker - Parallel job forking and management

=head1 SYNOPSIS

   use Parallel::Forker;
   $Fork = new Parallel::Forker;
   $SIG{CHLD} = sub { Fork::sig_child($Fork); };
   $SIG{TERM} = sub { $Fork->kill_tree_all('TERM') if $Fork; die "Quitting...\n"; };

   $Fork->schedule(run_on_start => sub {print "child work here...";},
		   run_on_finish => sub {print "parent cleanup here...";},
		   )
	    ->run();

   $Fork->wait_all();   # Wait for all children to finish

   # More processes
   my $p1 = $Fork->schedule(...)->ready();
   my $p2 = $Fork->schedule(..., run_after=>[$p1])->ready();
   $Fork->wait_all();   # p1 will complete before p2 starts

   # Other functions
   $Fork->poll();       # Service any active children
   foreach my $proc ($Fork->running()) {   # Loop on each running child

   while ($self->is_any_left) {
       $Fork->poll;
       usleep(10*1000);
   }

=head1 DESCRIPTION

Parallel::Forker manages parallel processes that are either subroutines or
system commands.  Forker supports most of the features in all the other
little packages out there, with the addition of being able to specify
complicated expressions to determine which processes run after others, or
run when others fail.

Function names are loosely based on Parallel::ForkManager.

The unique property of Parallel::Forker is the ability to schedule
processes based on expressions that are specified when the processes are
defined. For example:

   my $p1 = $Fork->schedule(..., label=>'p1');
   my $p2 = $Fork->schedule(..., label=>'p2');
   my $p3 = $Fork->schedule(..., run_after => ["p1 | p2"]);
   my $p4 = $Fork->schedule(..., run_after => ["p1 & !p2"]);

Process p3 is specified to run after process p1 *or* p2 have completed
successfully.  Process p4 will run after p1 finishes successfully, and
process p2 has completed with bad exit status.

For more examples, see the tests.

=head1 METHODS

=over 4

=item $self->find_proc_name (<name>)

Returns one or more Parallel::Forker::Process objects for the given name (one
object returned) or label (one or more objects returned).  Returns undef if no
processes are found.

=item $self->is_any_left

Return true if any processes are running, or runnable (need to run).

=item $self->kill_all (<signal>)

Send a signal to all running children.

=item $self->kill_tree_all (<signal>)

Send a signal to all running children and their subchildren.

=item $self->max_proc

Specify the maximum number of processes to run at any one time.  Defaults
to undef, which runs all possible jobs at once.

=item $self->new (<parameters>)

Create a new manager object.  There may be more then one manager in any
application, but each manager's sig_child method should be called in the
application's SIGCHLD handler.

=item $self->poll

See if any children need work, and service them.  Non-blocking; always
returns immediately.

=item $self->processes

Return Parallel::Forker::Process objects for all processes.

=item $self->processes_sorted

Return Parallel::Forker::Process objects for all processes, sorted by name.

=item $self->ready_all

Mark all processes as ready for scheduling.

=item $self->running

Return Parallel::Forker::Process objects for an processes that are currently running.

=item $self->schedule (<parameters>)

Register a new process perhaps for later running.  Returns a
Parallel::Forker::Process object.  Parameters are passed by name as
follows:

=over 4

=item label

Optional name to use in run_after commands.  Unlike name, this may be
reused, in which case run_after will wait on all commands with the given
label.

=item name

Optional name to use in run_after commands.  Note that names MUST be
unique!  When not specified, a unique number will be assigned
automatically.

=item run_on_start

Subroutine reference to execute when the job begins.  Executes under the
forked process.

=item run_on_finish

Subroutine reference to execute when the job ends.  Executes on the master
process.

=item run_after

A list reference of processes that must be completed before this process
can be runnable.  You may pass a process object (from schedule), a process
name, or a process label.  You may use "|" or "&" in a string to run this
process after ANY processes exit, or after ALL exit (the default.)
! in front of a process name indicates to run if that process fails with
bad exit status.  ^ in front of a process indicates to run if that process
succeeds OR fails.

=back

=item $self->sig_child

Must be called in SIG{CHLD} handler by the parent process.  If there are
multiple Parallel::Forker's each of their sig_child's must be called.

=item $self->wait_all

Wait until there are no running or runable jobs left.

=item $self->write_tree (filename => <filename>)

Print a dump of the execution tree.

=back

=head1 DISTRIBUTION

The latest version is available from CPAN and from
L<http://www.veripool.com/>.

Copyright 2002-2007 by Wilson Snyder.  This package is free software; you
can redistribute it and/or modify it under the terms of either the GNU
Lesser General Public License or the Perl Artistic License.

=head1 AUTHORS

Wilson Snyder <wsnyder@wsnyder.org>

=head1 SEE ALSO

L<Parallel::Forker::Process>

=cut
######################################################################
