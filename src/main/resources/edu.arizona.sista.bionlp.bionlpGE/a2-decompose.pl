#!/usr/bin/perl
require 5.000;
use strict;

my  $opt_string = 'hv';

use Getopt::Std;
our %opt;
getopts("$opt_string", \%opt) or usage();
usage() if $opt{h};
usage() if $#ARGV < 0;

my $fstem;
foreach my $fname (@ARGV) {
    if ($fname =~ /([^\/]+)\.a2$/) {$fstem = $1}
    else {warn "unrecognizable filename: $fname.\n"; next} 

    # storage for annotations
    my @tanno = ();  # text annotation
    my %eanno = ();  # event annotation
    my %split = ();  # splitted annotation information

    # read annotation file
    open (FILE, "<", $fname) or warn "cannot open the file: $fname\n";
    while (<FILE>) {
		chomp;
		if ($_ eq '') {next}

		# text-bound annotation and relation annotation
		if (/^[TR*]/) {
		    # store the line as it is.
		    push @tanno, $_;
		} # if

		# event expression annotation
		elsif (/^E/) {
		    my ($eid, $exp) = split "\t";
		    my (@arg) = split ' ', $exp;
		    my ($etype, $tid) = split ':', shift @arg;

		    if ($#arg > 0) {
				# decompose and store
				for (my $i=0; $i<=$#arg; $i++) {
				    my ($atype, $aid) = split (':', $arg[$i]);
				    $atype =~ s/([a-zA-Z])[0-9]+$/$1/;
				    $eanno{"$eid-$i"} = [$etype, $tid, $atype, $aid];
				} # for
				$split{$eid} = $#arg + 1;
		    } # if
		    else {
				my ($atype, $aid) = split (':', $arg[$0]);
				$atype =~ s/([a-zA-Z])[0-9]+$/$1/;
				$eanno{$eid} = [$etype, $tid, $atype, $aid];
		    } # else
		} # elsif

		elsif (/^M/) {
		    my ($mid, $mtype, $aid) = split /[ \t]/;
		    $eanno{$mid} = [$mtype, 'NULL', 'Theme', $aid];
		} # elsif

		else {
		    warn "undefined type of annotation ID: $_\n";
		} # else
    } # while
    close (FILE);

	# foreach (keys %eanno) {
	# 	warn "$_\t", join (", ", @{$eanno{$_}}), "\n";
	# }
	# warn "===\n";

	# sort events so that dependent ones come later
    my @elist = ();
    my %remain = map { $_ => 1 } sort keys %eanno;
 
	while (%remain) {
		my $pick = '';

		my %remain_s = map { s/-[0-9]+$//; $_ => 1 } keys %remain;

		foreach my $eid (keys %remain) {
		    my $aid = ${$eanno{$eid}}[3];
		    if (($aid =~ /^E/) && $remain_s{$aid}) {}
		    else {$pick = $eid; last}
		} # foreach

		if ($pick) {
			push @elist, $pick;
			delete $remain{$pick};
		} # if

		else {
	    	warn "==cyclic references found in '$fstem':\n";
	    	foreach (keys %remain) {printevent($_, @{$eanno{$_}})}
	    	warn "=======================================\n";
		    push @elist, sort keys %remain;
		    %remain = ();
		} # if
    } # while

	# foreach (@elist) {
	# 	warn "$_\t", join (", ", @{$eanno{$_}}), "\n";
	# }
	# warn "===\n";

    ## transitive duplication
    my @nelist = ();
    foreach my $eid (@elist) {
		my ($etype, $tid, $atype, $aid) = @{$eanno{$eid}};
		if ($split{$aid}) {
		    my $i = 0;
		    foreach my $naid (&expand_aid($aid, \%split)) {
				$eanno{"$eid-$i"} = [$etype, $tid, $atype, $naid];
				push @nelist, "$eid-$i";
				$i++;
		    } # foreach
		    delete $eanno{$eid};
		    $split{$eid} = $i; #warn "$eid\t$i===\n";
		} # if
		else {push @nelist, "$eid"}
    } # foreach

	# foreach (@nelist) {
	# 	warn "$_\t", join (", ", @{$eanno{$_}}), "\n";
	# } # foreach
	# warn "===\n";

    # remove duplicates
    @elist = @nelist;
    @nelist = ();
    my %eventexp = (); # for checking of event duplication
    my %equiv =();
    foreach my $eid (@elist) {
		my ($etype, $tid, $atype, $aid) = @{$eanno{$eid}};
		if ($equiv{$aid}) {${$eanno{$eid}}[3] = $equiv{$aid}}
		my $eventexp = join ', ', @{$eanno{$eid}};

		# check duplication
		if (my $did = $eventexp{$eventexp}) {
		    delete $eanno{$eid};
		    $equiv{$eid} = $did; #warn "EQUIV $eid\t==> $equiv{$eid}\n";
		    if ($opt{v}) {warn "[$fstem] $eid is equivalent to $did => removed.\n"}
		} # if
		else {$eventexp{$eventexp} = $eid; push @nelist, $eid}
    } # foreach

	# foreach (@nelist) {
	# 	warn "$_\t", join (", ", @{$eanno{$_}}), "\n";
	# } # foreach
	# warn "===\n";

    # remove less meaningful regulation chains
    @elist  = @nelist;
    @nelist = ();
    foreach my $eid (@elist) {
		my ($etype, $tid, $atype, $aid) = @{$eanno{$eid}};
		if ($aid =~ /^E[0-9]/) {
		    if ($eanno{$aid} && (${$eanno{$aid}}[2] eq 'Theme')) {push @nelist, $eid}
		    else                                                 {delete $eanno{$eid}}
		} # if

		else {push @nelist, $eid}
    } # foreach

	# foreach (@nelist) {
	# 	warn "$_\t", join (", ", @{$eanno{$_}}), "\n";
	# }
	# warn "===\n";

    @elist  = @nelist;
    @nelist = sort {$a cmp $b} @elist;

    # output
    open (FILE, ">", "${fname}d") or warn "cannot open the file: ${fname}d\n";

    foreach (@tanno) {print FILE "$_\n"}

    foreach my $eid (@nelist) {
		my ($etype, $tid, $atype, $aid) = @{$eanno{$eid}};
		if    ($eid =~ /^E/) {print FILE "$eid\t$etype:$tid $atype:$aid\n"}
		elsif ($eid =~ /^M/) {print FILE "$eid\t$etype $aid\n"}
    } # foreach

    close (FILE);
} # foreach


sub printevent {
	my ($eid, $etype, $tid, $atype, $aid) = @_;
	warn "$eid\t$etype:$tid $atype:$aid\n";
}

sub expand_aid {
    my ($aid, $rh_split) = @_;
    my @naid = ();

    if ($rh_split->{$aid}) {
		# warn "$aid\t$rh_split->{$aid}\n";
		for (my $i = 0; $i < $rh_split->{$aid}; $i++) {
		    push (@naid, &expand_aid("$aid-$i", $rh_split));
		} # for
		# warn "==> ", join (", ", @naid), "\n";
		return @naid;
    } # if
    else {return $aid}
} # expand_aid


sub usage {
    warn << "EOF";

[a2-decompose]
last updated by jdkim\@dbcls.rois.ac.jp on 13 February, 2011.


<DESCRIPTION>
It is a part of the evaluation tools for
the GENIA event extracton task of BioNLP Shared Task 2011.

It reads a2 file(s), decompose the events with multiple arguments,
and produces a2d file(s) with the decomposed events.


<USAGE>
$0 [-$opt_string] a2_file(s)


<OPTIONS>
-h     show help (this) page.
-v     verbose


<REFERENCE>
https://sites.google.com/site/bionlpst/

EOF
      exit;
} # usage
