# Fork.pm -- Parallel management
# $Id$
######################################################################
#
# This program is Copyright 2002-2005 by Wilson Snyder.
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

use strict;
use Schedule::Load;  # Really only needed for _subprocesses.  Cleanup if release ext.
use Carp;
use vars qw($Debug $VERSION);

$VERSION = '1.200';

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
    while (defined $self->_wait_one()) {
	usleep 100*1000;
    }
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

sub _wait_one {
    my $self = shift;
    # Poll.  Return 0 if someone still has work left, else undef.
    $self->poll();
    #print "NRUNNING ", scalar ( (keys %{$self->{_running}}) ), "\n";
    return 0 if ( (keys %{$self->{_runable}}) > 0 );
    return 0 if ( (keys %{$self->{_running}}) > 0 );
    return undef;
}

sub poll {
    my $self = shift;
    while ($self->{_activity}) {
	$self->{_activity} = 0;
	my $nrunning = 0;
	foreach my $procref (values %{$self->{_running}}) {
	    if (my $doneref = $procref->poll()) {
		$self->{_activity} = 1;
		return $doneref;
	    }
	    $nrunning++;
	}
	foreach my $procref (values %{$self->{_runable}}) {
	    last if ($self->{max_proc} && $nrunning >= $self->{max_proc});
	    $procref->run;
	    $nrunning++;
	}
    }
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
    use Data::Dumper;

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

   $Fork->schedule(run_on_start => sub {print "starting...";},
		   run_on_finish => sub {print "done...";},
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

=head1 DESCRIPTION

Parallel::Forker manages parallel processes that are either subroutines or
system commands.  Forker supports most of the features in all the other
little packages out there, with the addition of being able to specify
complicated expressions to determine which processes run after others, or
run when others fail.

Function names loosely based on Parallel::ForkManager.

The unique property of Parallel::Forker is the ability to schedule
processes based on expressions that are specified when the processes are
defined. For example:

   my $p1 = $Fork->schedule(..., label=>'p1');
   my $p2 = $Fork->schedule(..., label=>'p2');
   my $p3 = $Fork->schedule(..., run_after => "p1 | p2");
   my $p4 = $Fork->schedule(..., run_after => "p1 & !p2");

Process p3 is specified to run after process p1 *or* p2 have completed
successfully.  Process p4 will run after p1 finishes successfully, and
process p2 has completed with bad exit status.

For more examples, see the tests.

=head1 METHODS

=over 4

=item $self->find_proc_name (<name>)

Return a Parallel::Forker::Process objects for the given named process, or
undef if not found.

=item $self->is_any_left

Return true if any processes are running, or runnable (need to run).

=item $self->kill_all (<signal>)

Send a kill to all running children.

=item $self->kill_tree_all (<signal>)

Send a kill to all running children and their subchildren.  Requires
the Schedule::Load package to be installed.

=item $self->max_proc

Specify the maximum number of processes to run at any one time.  Defaults
to undef, which runs all possible jobs at once.

=item $self->new (<parameters>)

Create a new manager object.  There may be more then one manager in any
application.

=item $self->poll

See if any children need work, and service them.  Non-blocking; always
returns immediately.

=item $self->processes

Return Parallel::Forker::Process objects for all processes.

=item $self->processes_sorted

Return Parallel::Forker::Process objects for all processes, in name sorted order.

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

Optional name to use in run_after commands.  Note names MUST be unique!
When not specified, a unique number will be assigned automatically.

=item run_on_start

Subroutine reference to execute when the job begins.  Executes under the
forked process.

=item run_on_finish

Subroutine reference to execute when the job ends.  Executes on the master
process.

=item run_after

Add a new (or list of) processes that must be completed before this process
can be runnable.  You may pass a process object (from schedule), a process
name, or a process label.  You may use "|" or "&" in a string to run this
process after a OR of any processes exit, or after ALL exit (the default.)
! in front of a process name indicates to run if that process fails with
bad exit status.  ^ in front of a process indicates to run if that process
succeeds OR fails.

=back

=item $self->sig_child

Must be called in SIG{CHLD} handler by the parent process.  If there are
multiple Parallel::Forker's each of their sig_child's must be called.

=item $self->wait_all

Wait for all running jobs to complete.

=item $self->write_tree (filename=><filename>)

Print a dump of the execution tree.

=back

=head1 DISTRIBUTION

The latest version is available from CPAN and from
L<http://www.veripool.com/>.

Copyright 2002-2005 by Wilson Snyder.  This package is free software; you
can redistribute it and/or modify it under the terms of either the GNU
Lesser General Public License or the Perl Artistic License.

=head1 AUTHORS

Wilson Snyder <wsnyder@wsnyder.org>

=head1 SEE ALSO

L<Parallel::Forker::Process>

=cut
######################################################################
######################################################################
######################################################################
#### Per-process Section

package Parallel::Forker::Process;
use strict;
use POSIX qw(sys_wait_h :signal_h);
use Carp;

use vars qw($HashId $Debug);

$Debug = $Parallel::Forker::Debug;
$HashId = 0;

sub _new {
    my $class = shift;
    my $self = {
	_forkref => undef,	# Upper Fork object
	name => $HashId++,	# ID for hashing.  User may override it
	label => undef,		# Label for run_after's
	_after_children => {},	# IDs that are waiting on this event
	_after_parents => {},	# IDs that we need to wait for
	_state => 'idle',	# 'idle', 'ready', 'runable', 'running', 'done', 'parerr'
	pid => undef,		# Pid # running as, undef=not running
	run_after => [],	# Process objects that are prereqs
	run_on_start => sub {confess "%Error: No run_on_start defined\n";},
	run_on_finish => sub {my ($procref,$status) = @_;},	# Routine taking child and exit status
	@_
    };
    $Debug = $Parallel::Forker::Debug;
    bless $self, ref($class)||$class;
    $self->{_forkref}{_processes}{$self->{name}} = $self;
    if (defined $self->{label}) {
	if (ref $self->{label}) {
	    foreach my $label (@{$self->{label}}) {
		push @{$self->{_forkref}{_labels}{$label}}, $self;
	    }
	} else {
	    push @{$self->{_forkref}{_labels}{$self->{label}}}, $self;
	}
    }
    $self->_calc_runable();   # Recalculate
    return $self;
}

sub DESTROY {
    my $self = shift;
    delete $self->{_forkref}{_processes}{$self->{name}};
}

##### ACCESSORS

sub pid { return $_[0]->{pid}; }
sub forkref { return $_[0]->{_forkref}; }
sub is_idle    { return $_[0]->{_state} eq 'idle'; }
sub is_ready   { return $_[0]->{_state} eq 'ready'; }
sub is_runable { return $_[0]->{_state} eq 'runable'; }
sub is_running { return $_[0]->{_state} eq 'running'; }
sub is_done    { return $_[0]->{_state} eq 'done'; }
sub is_parerr  { return $_[0]->{_state} eq 'parerr'; }

##### METHODS

sub _calc_eqns {
    my $self = shift;

    # Convert references to names of the reference
    $self->{run_after} = [map
			  {
			      if (ref $_) { $_ = $_->{name} };
			      $_;
			  } @{$self->{run_after}} ];

    my $run_after = (join " & ", @{$self->{run_after}});
    $run_after =~ s/([&\|\!\^\---\(\)])/ $1 /g;
    print "  FrkRunafter $self->{name}: $run_after\n" if ($Debug||0)>=2;

    my $runable_eqn = "";
    my $parerr_eqn  = "";
    my $ignerr;
    my $flip_op = '';     # ~ or ^ or empty
    my $between_op     = '&&';
    my $between_op_not = '||';
    my $need_op_next = 0;
    my $any_refs = 0;
    foreach my $token (split /\s+/, " $run_after ") {
	next if $token =~ /^\s*$/;
	#print "TOKE $token\n" if $Debug;
	if ($token eq '!' || $token eq '^') {
	    $flip_op = $token;
	} elsif ($token eq '-') {
	    $ignerr = 1;
	} elsif ($token eq '(' || $token eq ')') {
	    if ($token eq '(') {
		$runable_eqn .= " ${between_op}" if $need_op_next;
		$parerr_eqn  .= " ${between_op_not}" if $need_op_next;
		$need_op_next = 0;
	    }
	    $runable_eqn .= " $token ";
	    $parerr_eqn.= " $token ";
	} elsif ($token eq '&') {
	    $between_op = '&&'; $between_op_not = '||';
	} elsif ($token eq '|') {
	    $between_op = '||'; $between_op_not = '&&';
	} elsif ($token =~ /^[a-z0-9_]*$/i) {
	    # Find it
	    my @found = $self->{_forkref}->find_proc_name($token);
	    if (defined $found[0]) {
		foreach my $aftref (@found) {
		    my $aftname = $aftref->{name};
		    ($aftref ne $self) or die "%Error: Id $self->{name} has a run_after on itself; it will never start\n";
		    $runable_eqn .= " ${between_op}" if $need_op_next;
		    $parerr_eqn  .= " ${between_op_not}" if $need_op_next;
		    # _ranok, _ranfail, _nofail
		    if ($flip_op eq '!') {
			$runable_eqn .= " (_ranfail('$aftname')||_parerr('$aftname'))";
			$parerr_eqn  .= " (_ranok('$aftname'))";
		    } elsif ($flip_op eq '^') {
			$runable_eqn .= " (_ranok('$aftname')||_ranfail('$aftname')||_parerr('$aftname'))";
			$parerr_eqn  .= " (0)";
		    } else {
			$runable_eqn .= " (_ranok('$aftname'))";
			$parerr_eqn  .= " (_ranfail('$aftname')||_parerr('$aftname'))";
		    }
		    $aftref->{_after_children}{$self->{name}} = $self;
		    $self->{_after_parents}{$aftref->{name}} = $aftref;
		    my $apo = $flip_op; $apo ||= 'O' if $between_op eq '||';
		    $apo ||= '&';  $apo='E' if $apo eq '!';
		    $self->{_after_parents_op}{$aftref->{name}} = $apo;
		    $need_op_next = 1;
		    $any_refs = 1;
		}
	    } else {
		if ($ignerr) {
		    print "  FrkProc $self->{name} run_after process/label $token not found ignored.\n" if $Debug;
		} else {
		    croak "%Error: run_after process/label $token not found,";
		}
	    }
	    # Prep for next
	    $ignerr = 0;
	    $flip_op = '';
	} else {
	    croak "%Error: run_after parse error of $token in: $run_after,";
	}
    }
    $runable_eqn = "1" if !$any_refs;
    $parerr_eqn  = "0" if !$any_refs;
    $self->{_runafter_text} = $run_after;
    $self->{_runable_eqn_text} = $runable_eqn;
    $self->{_parerr_eqn_text}  = $parerr_eqn;
    my $set = ("\t\$self->{_runable_eqn} = sub { return $runable_eqn; };\n"
	       ."\t\$self->{_parerr_eqn} = sub { return $parerr_eqn; };1;\n");
    print "$set" if ($Debug||0)>=2;
    eval $set or die ("%Error: Can't eval:\n$@\n"
		      ."  $self->{_runafter_text}\n  $self->{_runable_eqn_text}\n  $self->{_parerr_eqn_text}\n");
}

sub ready {
    my $self = shift;
    # User is indicating ready.
    ($self->{_state} eq 'idle') or croak "%Error: Signalling ready to already ready process,";

    $self->_calc_eqns();

    # Transition: idle -> 'ready'
    print "  FrkProc $self->{name} $self->{_state} -> ready\n" if $Debug;
    $self->{_state} = 'ready';
    $self->_calc_runable();
}

sub parerr {
    my $self = shift;
    # Mark process as never to be run
    if ($self->is_idle || $self->is_ready) {
	print "  FrkProc $self->{name} $self->{_state} -> parerr\n" if $Debug;
	$self->{_state} = 'parerr';  # "can't run due to parent status" is more accurate
    } else {
	croak "%Error: process isn't ready\n";
    }
    # May need to spawn/kill children
    foreach my $ra (values %{$self->{_after_children}}) {
	$ra->_calc_runable();
    }
}

sub run {
    my $self = shift;
    # Transition: Any state -> 'running', ignoring run_after's
    !$self->{pid} or croak "%Error: process is already running,";
    !$self->is_running or croak "%Error: process is already running,";

    print "  FrkProc $self->{name} $self->{_state} -> running\n" if $Debug;
    $self->{_state} = 'running';
    $self->{start_time} = time();
    if (my $pid = fork()) {
	$self->{pid} = $pid;
	$self->{_forkref}{_running}{$self->{pid}} = $self;
	delete $self->{_forkref}{_runable}{$self->{name}};
    } else {
	$self->{_forkref}{_in_child} = 1;
	$self->{run_on_start}->($self);
	exit(0);	# Don't close anything
    }
    return $self;   # So can chain commands
}

sub run_after {
    my $self = shift;
    # @_ = objects to add as prereqs
    ($self->{_state} eq 'idle') or croak "%Error: Must set run_after's before marking the process ready,";
    push @{$self->{run_after}}, @_;
    return $self;   # So can chain commands
}

use vars qw($_Calc_Runable_Fork);

sub _calc_runable {
    my $self = shift;
    # @_ = objects to add as prereqs
    return if ($self->{_state} ne 'ready');
    #use Data::Dumper; print "CR ",Dumper($self),"\n";

    # Used by the callbacks
    local $_Calc_Runable_Fork = $self->{_forkref};
    sub _ranok {
	my $procref = $_Calc_Runable_Fork->{_processes}{$_[0]};
	print "   _ranok   $procref->{name}  State $procref->{_state}\n" if ($Debug||0)>=2;
	return ($procref->is_done && $procref->{status}==0);
    }
    sub _ranfail {
	my $procref = $_Calc_Runable_Fork->{_processes}{$_[0]};
	print "   _ranfail $procref->{name}  State $procref->{_state}\n" if ($Debug||0)>=2;
	return ($procref->is_done && $procref->{status}!=0);
    }
    sub _parerr {
	my $procref = $_Calc_Runable_Fork->{_processes}{$_[0]};
	print "   _parerr  $procref->{name}  State $procref->{_state}\n" if ($Debug||0)>=2;
	return ($procref->is_parerr);
    }

    if (&{$self->{_runable_eqn}}) {
	# Transition: ready -> runable
	print "  FrkProc $self->{name} $self->{_state} -> runable\n" if $Debug;
	$self->{_state} = 'runable';  # No dependencies (yet) so can launch it
	$self->{_forkref}{_runable}{$self->{name}} = $self;
    } elsif (&{$self->{_parerr_eqn}}) {
	$self->parerr;
    }
}

##### STATE TRANSITIONS

sub poll {
    my $self = shift;
    return undef if !$self->{pid};

    my $got = waitpid ($self->{pid}, WNOHANG);
    if ($got>0) {
	$self->{status} = $?;	# convert wait return to status 
	# Transition: running -> 'done'
	print "  FrkProc $self->{name} $self->{_state} -> done ($self->{status})\n" if $Debug;
	delete $self->{_forkref}{_running}{$self->{pid}};
	$self->{pid} = undef;
	$self->{_state} = 'done';
	$self->{end_time} = time();
	$self->{run_on_finish}->($self, $self->{status});
	# Transition children: ready -> runable
	foreach my $ra (values %{$self->{_after_children}}) {
	    $ra->_calc_runable();
	}
	# Done
	return $self;
    }
    return undef;
}

sub kill {
    my $self = shift;
    my $signal = shift || 9;
    CORE::kill ($signal, $self->{pid}) if $self->{pid};
    # We don't remove it's pid, we'll get a child exit that will do it
}

sub kill_tree {
    my $self = shift;
    my $signal = shift || 9;
    return if !$self->{pid};
    my @proc = (Schedule::Load::_subprocesses($self->{pid}), $self->{pid});
    foreach my $pid (@proc) {
	print "  Fork Kill -$signal $pid (child of $pid)\n" if $Debug;
	CORE::kill ($signal, $pid);
    }
    # We don't remove it's pid, we'll get a child exit that will do it
}

sub format_time {
    my $secs = shift;
    return sprintf ("%02d:%02d:%02d", int($secs/3600), int(($secs%3600)/60), $secs % 60);
}

sub format_loctime {
    my $time = shift;
    my ($sec,$min,$hour) = localtime($time);
    return sprintf ("%02d:%02d:%02d", $hour, $min, $sec);
}

sub _write_tree_line {
    my $self = shift;
    my $level = shift;
    my $linenum = shift;
    my $cmt = "";
    if (!$linenum) {
	my $state = uc $self->{_state};
	$state .= "-ok"  if $self->is_done && !$self->{status};
	$state .= "-err" if $self->is_done && $self->{status};
	return sprintf ("%s %-27s  %-8s  %s\n",
			"--", #x$level
			$self->{name},
			$state,  # DONE-err is longest
			($self->{comment}||""));
    } elsif ($linenum == 1) {
	if ($self->{start_time}) {
	    $cmt .= "Start ".format_loctime($self->{start_time});
	    if ($self->{end_time}) {
		$cmt .= ", End ".format_loctime($self->{end_time});
		$cmt .= ", Took ".format_time(($self->{end_time}-$self->{start_time}));
	    }
	}
    } elsif ($linenum == 2) {
	$cmt .= "Runaft = ".$self->{_runafter_text}    if defined $self->{_runafter_text};
    } elsif ($linenum == 3) {
	$cmt .= "RunEqn = ".$self->{_runable_eqn_text} if defined $self->{_runable_eqn_text} ;
    } elsif ($linenum == 4) {
	$cmt .= "ErrEqn = ".$self->{_parerr_eqn_text}  if defined $self->{_parerr_eqn_text} ;
    }
    return sprintf ("%s %-27s  %-8s  %s\n",
		    "  ", #x$level
		    "",
		    "",
		    $cmt);
}
######################################################################
#### Package return
1;
=pod

=head1 NAME

Parallel::Forker::Process - Single parallel fork process object

=head1 SYNOPSIS

   $obj->run;
   $obj->poll;
   $obj->kill(<"SIGNAL">);
   $obj->kill_tree(<"SIGNAL">);

=head1 DESCRIPTION

Manage a single process under the control of Parallel::Forker.

Processes transition over 6 states.  They begin in idle state, and are
transitioned by the user into ready state.  As their dependencies complete,
Parallel::Forker transitions them to the runable state.  As the max_proc
limit permits, they transition to the running state, and executed.  On
completion, they transition to the done state.  If a process depends on
another process, and that other process fails, they transition to the
parerr (parent error) state, and are never run.

=head1 METHODS

=over 4

=item forkref

Return the parent Parallel::Forker object this process belongs to.

=item is_done

Returns true if the process is in the done state.

=item is_idle

Returns true if the process is in the idle state.

=item is_parerr

Returns true if the process is in the parent error state.

=item is_ready

Returns true if the process is in the ready state.

=item is_runable

Returns true if the process is in the runable state.

=item is_running

Returns true if the process is in the running state.

=item kill

Kill the process if it is running

=item kill_tree

Kill the process and any of it's subchildren.  Requires Schedule::Load.

=item pid

Return the process ID if this job is running, else undef.

=item poll

Check the process for activity, invoking callbacks if needed.
Generally Fork->poll() is used instead.

=item ready

Mark this process as being ready for execution when all run_after's are
ready and CPU resources permit.  When that occurs, run will be called on
the process automatically.

=item kill (<signal>)

Send a kill to this child.

=item kill_tree_all (<signal>)

Send a kill to this child and its subchildren.  Requires the Schedule::Load
package to be installed.

=item run 

Start this process now.

=back

=head1 DISTRIBUTION

The latest version is available from CPAN and from
L<http://www.veripool.com/>.

Copyright 2002-2005 by Wilson Snyder.  This package is free software; you
can redistribute it and/or modify it under the terms of either the GNU
Lesser General Public License or the Perl Artistic License.

=head1 AUTHORS

Wilson Snyder <wsnyder@wsnyder.org>

=head1 SEE ALSO

L<Parallel::Forker>

=cut
######################################################################
