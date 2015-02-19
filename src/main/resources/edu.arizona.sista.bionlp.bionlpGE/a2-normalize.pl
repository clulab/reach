#!/usr/bin/perl
require 5.000;
use strict;

my %target_eclass = (
		     ('Gene_expression', 1),
		     ('Transcription', 1),
		     ('Protein_catabolism', 1),
		     ('Phosphorylation', 1),
		     ('Acetylation', 1),
		     ('Deacetylation', 1),
		     ('Ubiquitination', 1),
		     ('Protein_modification', 1),
		     ('Localization', 1),
		     ('Binding', 1),
		     ('Regulation', 1),
		     ('Positive_regulation', 1),
		     ('Negative_regulation',1 )
		     );

my $gDir = '.';
my $oDir = '';

my  $opt_string = 'heg:o:uv';
our %opt;
 

use Getopt::Std;
getopts("$opt_string", \%opt) or &usage;
usage() if $opt{h};
usage() if $#ARGV < 0;
if ($opt{g}) {$gDir = $opt{g}; $gDir =~ s/\/$//}

if ($opt{u} && $opt{o}) {&usage}
if (!$opt{u} && !$opt{o}) {&usage}

if ($opt{o}) {$oDir = $opt{o}}


# per-text variables referenced globally. Should be initialzed for each file.
my ($text, $textlen);

my (@panno, @tanno, @ranno, @eanno, @manno);  # annotations
my (%panno, %tanno, %ranno, %eanno, %manno);  # annotations
my @equiv; # list equivalent groups


my $fstem;
foreach my $fname (@ARGV) {
    if ($fname =~ /([^\/]+)\.a2$/) {$fstem = $1}
    else {warn "unrecognizable filename : $fname\n"; next}

    # initialize per-text variables
    $text = '';
    @panno = @tanno = @ranno = @eanno = @manno = ();
    %panno = %tanno = %ranno = %eanno = %manno = @equiv = ();

    &read_text_file("$gDir/$fstem.txt");
    &read_a1_file("$gDir/$fstem.a1");
    &read_a2_file($fname);

    foreach my $eid (@eanno) {
		my ($etype, $etid, @arg) = @{$eanno{$eid}};

		## id-linkage verification

		if (!$tanno{$etid}) {warn "invalid reference to event trigger: [$fstem] $eid => $etid\n"; next}
		if (${$tanno{$etid}}[0] ne $etype) {warn "event type mismatch: [$fstem] $eid => $etype vs ${$tanno{$etid}}[0]\n"; next}

		foreach (@arg) {
		    my ($atype, $aid) = split /:/;

		    if (($aid =~ /^T/) && !$panno{$aid} && !$tanno{$aid}) {warn "unkown reference: [$fstem] $eid => $aid\n"}
		    if (($aid =~ /^E/) && !$eanno{$aid}) {warn "unkown reference: [$fstem] $eid => $aid\n"}
		    if ($aid eq $eid) {warn "self-reference: [$fstem] $eid => $aid\n"; next}

		    if ($atype =~ /^Theme/) {
				if ($etype =~ /egulation$/) {
				    if (!$panno{$aid} && !$eanno{$aid}) {warn "Only a protein or a event can be a Theme or a Cause of a regulation event: [$fstem] $eid => $atype:$aid\n"}
				} # if
				else {
				    if (!$panno{$aid}) {warn "Only a protein can be a Theme of a non-regulation event: [$fstem] $eid => $atype:$aid\n"}
				} # else
		    } # if

		    elsif ($atype eq 'Cause') {
				if (($etype =~ /egulation$/) || ($etype =~ /^Protein_modification$/) || ($etype =~ /hosphorylation$/) || ($etype =~ /biquitination$/) || ($etype =~ /cetylation$/)){
				    if (!$panno{$aid} && !$eanno{$aid}) {warn "Only a protein or a event can be a Cause of a Protein_modification event: [$fstem] $eid => $atype:$aid\n"}
				} # if
				# else {
				#     if (!$panno{$aid}) {warn "Only a regulation or protein modification event can have a cause argument: [$fstem] $eid => $atype:$aid\n"}
				# } # else
		    } # if

		    else {
				if (!$tanno{$aid} || (${$tanno{$aid}}[0] ne 'Entity')) {warn "A secondary argument has to reference to a 'Entity' type t-entity: [$fstem] $eid => $atype:$aid\n"}
		    } # else
		} # foreach


		# canonicalize the order of arguments
		my %argorder = (('Theme', 0), ('Theme1', 1), ('Theme2', 2), ('Theme3', 3), ('Theme4', 4), ('Theme5', 5), ('Theme6', 6), ('Cause', 7),
				('Site', 10), ('Site1', 11), ('Site2', 12), ('Site3', 13), ('Site4', 14), ('Site5', 15), ('Site6', 16), ('CSite', 17), ('AtLoc', 18), ('ToLoc', 19));
		@arg = sort {$argorder{(split /:/, $a)[0]} <=> $argorder{(split /:/, $b)[0]}} @arg;


		# canonicalize the order of multiple themes for binding events
		if ($etype eq 'Binding') {
		    my (%theme, %site) = ();

		    foreach (@arg) {
				my ($atype, $aid) = split /:/;
				$aid =~ s/^T//;

				if ($atype =~ /^Theme([2-5]?)/) {
				    my $i = ($1)? $1:1;
				    if ($theme{$i}) {warn "duplicate theme numbering: [$fstem] $eid => $atype\n"}
				    else            {$theme{$i} = $aid}
				} # if
				if ($atype =~ /^Site([2-5]?)/)  {
				    my $i = ($1)? $1:1;
				    if ($site{$i}) {warn "duplicate site numbering: [$fstem] $eid => $atype\n"}
				    else           {$site{$i} = $aid}
				    $theme{$i} .= '-' . $aid;
				} # if
		    } # foreach

		    my @theme = sort {(split /-/, $theme{$a})[0] <=> (split /-/, $theme{$b})[0]} keys %theme;

		    my (@newtheme, @newsite) = ();
		    for (my $i = 0; $i <= $#theme; $i++) {
				my ($t , $s) = split /-/, $theme{$theme[$i]};
				my $j = $i? $i+1 : '';
				push @newtheme, "Theme$j:T" . $t;
				if ($s) {push @newsite, "Site$j:T" . $s}
		    } # for

		    @arg = (@newtheme, @newsite);
		} # if (Binding)

		$eanno{$eid} = ["$etype:$etid", @arg];
    } # foreach (%eanno)


    # id-linkage verification
    foreach my $rid (@ranno) {
		my ($pred, $sub, $obj) = @{$ranno{$rid}};
	    my ($stype, $sid) = split ':', $sub;
	    my ($otype, $oid) = split ':', $obj;
		if (!$panno{$sid} && !$tanno{$sid}) {warn "invalid ID reference: [$fstem] $rid: $sid\n"}
		if (!$panno{$oid} && !$tanno{$oid}) {warn "invalid ID reference: [$fstem] $rid: $oid\n"}
    } # foreach (%manno)


    # id-linkage verification
    foreach my $mid (@manno) {
		my ($mod, $aid) = @{$manno{$mid}};
		if (($aid !~ /^E/) || (!$eanno{$aid})) {
		    warn "invalid ID reference: [$fstem] $mid: $aid\n";
		} # if
    } # foreach (%manno)


    # id-linkage verification
    foreach (@equiv) {
		my @egroup = @$_;
		foreach (@egroup) {
		    if (!$panno{$_}) {warn "non-protein entity in Equiv relation: $fstem\t[", join (",", @egroup), "]\n"}
		} # foreach
    } # foreach


    # count the number of events referencing each term
    my %nref = ();
    foreach my $eid (@eanno) {
		my @aid = map {(split ':')[1]} @{$eanno{$eid}};
		foreach (@aid) {if ($nref{$_}) {$nref{$_}++} else {$nref{$_}=1}}
    } # foreach


    # find the referenced one and put it in the first place.
    foreach (@equiv) {
		my @egroup = @$_;

		my ($rterm, @others) = ();
		my $num_rterm = 0;
		foreach (@egroup) {
		    if ($nref{$_}) {$rterm = $_; $num_rterm++}
		    else           {push @others, $_}
		}
		if ($num_rterm > 1) {warn "multiple terms in a equiv group are referenced: $fstem\t[", join (",", @egroup), "]\n"}

		if ($rterm) {$_ = [$rterm, @others]}
    } # foreach

    # check duplication
    my %seen = ();
    foreach my $id (@tanno, @eanno, @manno) {
		my $anno = ($id =~ /^T/)? $tanno{$id} : (($id =~ /^E/)? $eanno{$id} : $manno{$id});
		my $exp  = join ' ', @{$anno};
		if ($seen{$exp}) {warn "duplicate events: [$fstem] $id = $seen{$exp}\n"}
		else             {$seen{$exp} = $id}
    } # foreach ($id)


    my $newfname = $fname;

    if ($oDir) {
		if (!-e $oDir) {mkdir $oDir or die " !Cannot create the directory for output, $oDir.\n"}

		$newfname =~ s/^.*\///;
		$newfname = "$oDir/$newfname";
		if ($opt{v}) {warn "$fname\t-> $newfname.\n"}
    } # if
    elsif ($opt{u}) {
		-w $newfname or die " !The target file is not writable.\n";
		if ($opt{v}) {warn "update $newfname.\n"}
    } # elsif

    if (!open (FILE, ">", $newfname)) {warn "cannot open output file: $newfname\n"; return}

    foreach (@equiv) {print FILE "*\tEquiv ", join (' ', @$_), "\n"}
    foreach my $id (@tanno) {print FILE "$id\t", join (' ', @{$tanno{$id}}[0 .. 2]); if (${$tanno{$id}}[3]) {print FILE "\t${$tanno{$id}}[3]"} print FILE "\n"}
    foreach my $id (@ranno) {print FILE "$id\t", join (' ', @{$ranno{$id}}), "\n"}
    foreach my $id (@eanno) {print FILE "$id\t", join (' ', @{$eanno{$id}}), "\n"}
    foreach my $id (@manno) {print FILE "$id\t", join (' ', @{$manno{$id}}), "\n"}
    close (FILE);
} # foreach


sub read_text_file {
    my ($fname) = @_;

    if (!open (FILE, "<", "$fname")) {warn "cannot open txt file: $fname\n"; exit}
    while (<FILE>) {$text .= $_}
    close (FILE);

    $textlen = length $text;
} # read_text_file


# t-entity : (type, beg, end)
# event    : (type, tid, arg1, arg2, ...)

sub read_a1_file {
    my ($fname) = @_;

    if (!open (FILE, "<", $fname)) {warn "cannot open a1 file: $fname\n"; exit}

    while (<FILE>) {
		chomp;
		my ($id, $anno, $extra) = split /\t/;
		if ($id !~ /^T[0-9-]+$/) {warn "invalid ID in a1 file: [$fstem] $_\n"; next}

		my ($type, $beg, $end) = split / /, $anno;
		if ($type ne 'Protein')   {warn "non-protein entity in a1 file: [$fstem] $_\n"}
		if (!&rangep($beg, $end)) {warn "invalid text range: [$fstem] $beg - $end\n"}

		if ($panno{$id}) {warn "duplicated entity ID: [$fstem] $id\n"}

		push @panno, $id;
		$panno{$id} = [$type, $beg, $end, $extra];
    } # while

    close (FILE);
} # read_a1_file


sub read_a2_file {
    my ($fname) = @_;

    if (!open (FILE, "<", $fname)) {warn "cannot open a2 file: $fname\n"; return}

    while (<FILE>) {
		chomp;

		my ($id, $anno, $extra) = split /\t/;
		if ($id !~ /^([TREM][0-9-]+|\*)$/) {
		    warn "invalide ID: $id\nAn ID has to begin with 'T', 'R', E', or 'M', being followed by digits or dash '-'.\n";
		    next;
		} # if

		if ($id =~ /^T/) {
		    my ($type, $beg, $end) = split / +/, $anno;

		    if (!$target_eclass{$type} && ($type ne 'Entity') && ($type ne 'Anaphora')) {warn "invalid entity type in a2 file: [$fstem] $id => $type\n"}
		    if (!&rangep($beg, $end)) {warn "invalid text range: [$fstem] $beg - $end\n"}
		    if ($tanno{$id}) {warn "duplicated entity ID: [$fstem] $id\n"}
		    push @tanno, $id;
		    $tanno{$id} = [$type, $beg, $end, $extra];
		} # if

		elsif ($id =~ /^R/) {
		    my ($pred, $sub, $obj) = split / +/, $anno;
		    my ($stype, $sid) = split ':', $sub;
		    my ($otype, $oid) = split ':', $obj;
			if ($sid !~ /^[TE][0-9-]+$/) {warn "invalid reference for an argument: [$fstem] $id => $sid\n"}
			if ($oid !~ /^[TE][0-9-]+$/) {warn "invalid reference for an argument: [$fstem] $id => $oid\n"}
		    if ($ranno{$id}) {warn "duplicated relation ID: [$fstem] $id\n"}
		    push @ranno, $id;
		    $ranno{$id} = [$pred, $sub, $obj];
		}

		elsif ($id =~ /^E/) {
		    my ($pred, @arg) = split / +/, $anno;
		    my ($type, $tid) = split ':', $pred;
		    if (!$target_eclass{$type}) {warn "invalid event type: [$fstem] $id => $type\n"}
		    if ($#arg < 0) {warn "event with no argument: [$fstem] $id => $_\n"}

		    my %argnum = ();
			    foreach (@arg) {
				my ($type, $aid) = split ':';
				if ($aid !~ /^[TE][0-9-]+$/) {warn "invalid reference for an argument: [$fstem] $id => $aid\n"}

				$type =~ s/^Theme[1-6]$/Theme/;
				$type =~ s/^Site[1-6]$/Site/;
				$type =~ s/^..Loc$/Loc/;
				if ($argnum{$type}) {$argnum{$type}++} else {$argnum{$type}=1}
		    } # foreach

	#	    if ($argnum{'Site'} || $argnum{'CSite'} || $argnum{'Loc'}) {warn "arguments for task 2 found: [$fstem] $_\n"}

		    if (!$argnum{'Theme'}) {warn "event with no theme: [$fstem] $_\n"}

		    if ($type eq 'Binding') {
				if ($argnum{'Site'} > $argnum{'Theme'}) {warn "more sites than themes: [$fstem] $_\n"}
				delete $argnum{'Theme'};
				delete $argnum{'Site'};
		    } # if

		    else {
			if ($argnum{'Theme'}>1) {warn "multiple themes for non-Binding event: [$fstem] $_\n"}
			delete $argnum{'Theme'};

			if ($type =~ /egulation$/) {
			    if ($argnum{'Cause'}>1) {warn "multiple causes: [$fstem] $_\n"}
			    if ($argnum{'Site'}>1)  {warn "multiple sites: [$fstem] $_\n"}
			    if ($argnum{'CSite'}>1) {warn "multiple csites: [$fstem] $_\n"}
			    if (!$argnum{'Cause'} && $argnum{'CSite'}) {warn "no Cause but CSite: [$fstem] $_\n"}
			    delete $argnum{'Cause'};
			    delete $argnum{'Site'};
			    delete $argnum{'CSite'};
			} # if

			elsif (($type =~ /^Protein_modification$/) || ($type =~ /hosphorylation$/) || ($type =~ /biquitination$/) || ($type =~ /cetylation$/)){
			    if ($argnum{'Cause'}>1) {warn "multiple causes: [$fstem] $_\n"}
			    if ($argnum{'Site'}>1)  {warn "multiple sites: [$fstem] $_\n"}
			    delete $argnum{'Cause'};
			    delete $argnum{'Site'};
			} # if

			elsif ($type =~ /^Localization$/) {
			    if ($argnum{'Loc'}>1)  {warn "multiple location arguments: [$fstem] $_\n"}
			    delete $argnum{'Loc'};
			} # if

		    } # else

		    if (%argnum) {warn "invalid argument(s) for $type type: [$fstem] " , join (', ', keys %argnum) , "\n"}

		    if ($eanno{$id}) {warn "duplicated event ID: [$fstem] $_\n"}
		    push @eanno, $id;
		    $eanno{$id} = [$type, $tid, @arg];
		} # if

		elsif ($id =~ /^M/) {
		    my ($mod, $aid) = split ' ', $anno;
	#	    warn "task 3: [$fstem] $_\n";
		    if (($mod ne 'Negation') && ($mod ne 'Speculation')) {warn "invalid type of event modification: [$fstem] $_\n"}
		    if ($manno{$id}) {warn "duplicated modifier ID: [$fstem] $_\n"}
		    push @manno, $id;
		    $manno{$id} = [$mod, $aid];
		} # elsif

		elsif ($id eq '*') {
	#	    warn "Equiv annotation: [$fstem] $_\n";

		    my ($type, @pid) = split ' ', $anno;
		    if ($type ne 'Equiv') {warn "invalid type of relation: [$fstem] $_\n"}
		    push @equiv, [@pid];
		} # elsif

    } # while

    close (FILE);
} # read_a2_file


sub rangep {
    my ($beg, $end) = @_;

    if (($beg =~ /^\d+$/) && ($end =~ /^\d+$/)
	&& ($beg >= 0) && ($end <= $textlen) && ($beg < $end)) {return 1}

    else {return 0}
} # rangep


sub usage() {
    warn << "EOF";

[a2-normalize]
last updated by jdkim\@dbcls.rois.ac.jp on 13 February, 2011.


<DESCRIPTION>
It is a part of the evaluation tools for
the GENIA event extracton task of BioNLP Shared Task 2011.

It checks on the format of a2 files, and
normalizes the order of the event arguments.

It has to be applied to the predicted a2 files to be evaluated.


<USAGE>
$0 [-$opt_string] a2_file(s)


<OPTIONS>
-h             this (help) message.
-g gold_dir    specifies the 'gold' directory (default = $gDir).
-o output_dir  specifies the output directory.
-u             tells it update the original files.
               Note that one of -o or -u has to be specified, but not both.
-v             verbose mode


<REFERENCE>
https://sites.google.com/site/bionlpst/

EOF
      exit;
}
