#!/usr/bin/perl
require 5.000;
use strict;

my @target_eclass = ('Gene_expression', 'Transcription', 'Protein_catabolism', 'Localization', 'Protein_modification', 'Phosphorylation', 'Ubiquitination', 'Acetylation', 'Deacetylation');
my @target_rclass = ('Regulation', 'Positive_regulation', 'Negative_regulation');
my @target_mclass = ('Negation', 'Speculation');
my @target_class  = (@target_eclass, 'Binding', @target_rclass, @target_mclass);


my $gdir = '.';
my $task = 1;

my  $opt_string = 'g:t:sSmpvxdh';
our %opt;

use Getopt::Std;
getopts("$opt_string", \%opt) or &usage;
&usage if $opt{h};
&usage if $#ARGV < 0;
if ($opt{g}) {$gdir = $opt{g}; $gdir =~ s/\/$//}
if ($opt{t}) {$task = $opt{t}}
if ($task !~ /[123]/) {&usage}

## default functions for equivalency checking
my $fn_eq_span  = \&eq_span_hard;;
my $fn_eq_class = \&eq_class_hard;
my $fn_eq_args  = \&eq_args_hard;
my $fn_eq_rargs = \&eq_args_hard;


## for total scoring
#  - initialized only once.
my (%tnum_gold, %tnum_mgold, %tnum_answer, %tnum_manswer); # number of golds/matched golds/answers/matched answers
foreach (@target_class) {$tnum_gold{$_} = $tnum_answer{$_} = $tnum_mgold{$_} = $tnum_manswer{$_} = 0}

my ($num_t2arg, $num_t3stat) = (0, 0); # number of task 2 arguments / task 3 statements

## for FPs and FNs
my (@FP, @FN) = ((), ());


## Variables which are file-specific.
#  - they are referenced globally.
#  - should be initialized for every file.

## for storing annotation
my ($text, $textlen, @textpic);
my (%protein, %gold, %gold_site, %equiv, %answer, %answer_site);
my (%rgold, %ranswer);   # raw data of gold and answer annotations

## for local scoring
my ($num_gold, $num_mgold, $num_answer, $num_manswer);
my (%num_gold, %num_mgold, %num_answer, %num_manswer); # number of golds/matched golds/answers/matched answers


my $fstem; # $fstem is globally referenced to know what exact file is under processing.
foreach my $fname (@ARGV) {
    if ($fname =~ /([^\/]+)\.a2$/) {$fstem = $1}
    else {warn "unrecognizable filename: $fname.\n"; next} 

    ## initialization of file-specific global variables
    ($text, $textlen, @textpic) = ('', 0, ());
    (%protein, %gold, %gold_site, %equiv, %answer, %answer_site) = ();
    (%rgold, %ranswer) = ();
    ($num_gold, $num_mgold, $num_answer, $num_manswer) = (0, 0, 0, 0);
    foreach (@target_class) {$num_gold{$_} = $num_answer{$_} = $num_mgold{$_} = $num_manswer{$_} = 0}

    ## event loading
    if (!($textlen = &read_text_file("$gdir/$fstem.txt", $text))) {next}
    if (!&read_a1_file("$gdir/$fstem.a1", \%protein)) {next}
    if (($num_gold   = &read_a2_file("$gdir/$fstem.a2", 'G', $task)) < 0) {next}
    if (($num_answer = &read_a2_file($fname,          , 'A', $task)) < 0) {next}

    ## set matching methods
    if ($opt{s}) {$fn_eq_span  = \&eq_span_soft}
    if ($opt{S}) {$fn_eq_span  = \&eq_span_soft_notrigger}
    if ($opt{m}) {$fn_eq_class = \&eq_class_soft}
    if ($opt{p}) {$fn_eq_rargs = \&eq_args_soft}

    ## Event matching
    &count_match;

    # debugging message
    if ($opt{d}) {
    	foreach (@target_class) {
    	    if ($num_manswer{$_} != $num_mgold{$_})  {warn "inconsistent number of matched events: [$fstem] [$_]\t$num_manswer{$_} vs. $num_mgold{$_}\n"}
    	    if ($num_manswer{$_} != $num_answer{$_}) {warn "not perfect precision: [$fstem] [$_] $num_manswer{$_} / $num_answer{$_}\n"}
    	    if ($num_mgold{$_}   != $num_gold{$_})   {warn "not perfect recall   : [$fstem] [$_] $num_mgold{$_} / $num_gold{$_}\n"}
    	} # foreach
    } # if

    ## adjustment for duplication
    foreach (@target_class) {
    	if ($num_manswer{$_} > $num_mgold{$_}) {
    	    my $num_danswer = $num_manswer{$_} - $num_mgold{$_};
    	    $num_answer{$_}  -= $num_danswer;
    	    $num_manswer{$_} -= $num_danswer;
    	    if ($opt{v}) {warn "$num_danswer event(s) have been discarded due to an equivanlency by the approximate matching: [$fstem]\n"}
    	} # if
    } # foreach

    ## totalling
    foreach (@target_class) {
    	$tnum_gold{$_}    += $num_gold{$_};
    	$tnum_answer{$_}  += $num_answer{$_};
    	$tnum_mgold{$_}   += $num_mgold{$_};
    	$tnum_manswer{$_} += $num_manswer{$_};
    } # foreach
} # foreach

if (($task == 2) && ($num_t2arg == 0)) {die "no argument that belongs to task 2 found.\n"}

if ($task =~ /[12]/) {

    my ($tnum_gold, $tnum_mgold, $tnum_answer, $tnum_manswer) = (0, 0, 0, 0);

    foreach (@target_eclass) {
    	&report ($_, $tnum_gold{$_}, $tnum_mgold{$_}, $tnum_answer{$_}, $tnum_manswer{$_});
    	$tnum_gold += $tnum_gold{$_}; $tnum_mgold += $tnum_mgold{$_};
    	$tnum_answer += $tnum_answer{$_}; $tnum_manswer += $tnum_manswer{$_};
    } # foreach
    &report ('=[SVT-TOTAL]=', $tnum_gold, $tnum_mgold, $tnum_answer, $tnum_manswer);

    foreach (('Binding')) {
    	&report ($_, $tnum_gold{$_}, $tnum_mgold{$_}, $tnum_answer{$_}, $tnum_manswer{$_});
    	$tnum_gold += $tnum_gold{$_}; $tnum_mgold += $tnum_mgold{$_};
    	$tnum_answer += $tnum_answer{$_}; $tnum_manswer += $tnum_manswer{$_};
    } # foreach

    &report ('==[EVT-TOTAL]==', $tnum_gold, $tnum_mgold, $tnum_answer, $tnum_manswer);
    print STDOUT "------------------------------------------------------------------------------------\n";

    my ($gnum_gold, $gnum_mgold, $gnum_answer, $gnum_manswer) = ($tnum_gold, $tnum_mgold, $tnum_answer, $tnum_manswer);

    $tnum_gold = $tnum_mgold = $tnum_answer = $tnum_manswer = 0;
    foreach (@target_rclass) {
    	&report ($_, $tnum_gold{$_}, $tnum_mgold{$_}, $tnum_answer{$_}, $tnum_manswer{$_});
    	$tnum_gold += $tnum_gold{$_}; $tnum_mgold += $tnum_mgold{$_};
    	$tnum_answer += $tnum_answer{$_}; $tnum_manswer += $tnum_manswer{$_};
    } # foreach
    &report ('==[REG-TOTAL]==', $tnum_gold, $tnum_mgold, $tnum_answer, $tnum_manswer);
    print STDOUT "------------------------------------------------------------------------------------\n";

    $gnum_gold += $tnum_gold; $gnum_mgold += $tnum_mgold; $gnum_answer += $tnum_answer; $gnum_manswer += $tnum_manswer;

    &report ('==[ALL-TOTAL]==', $gnum_gold, $gnum_mgold, $gnum_answer, $gnum_manswer);
    print STDOUT "------------------------------------------------------------------------------------\n";
} # if (task == 1 or 2)


if ($task == 3) {
    if ($num_t3stat == 0) {print "no statement that belongs to task 3 found.\n"; exit}

    my ($tnum_gold, $tnum_mgold, $tnum_answer, $tnum_manswer) == (0, 0, 0, 0);
    foreach (@target_mclass) {
    	&report ($_, $tnum_gold{$_}, $tnum_mgold{$_}, $tnum_answer{$_}, $tnum_manswer{$_});
    	$tnum_gold += $tnum_gold{$_}; $tnum_mgold += $tnum_mgold{$_};
    	$tnum_answer += $tnum_answer{$_}; $tnum_manswer += $tnum_manswer{$_};
    } # foreach
    &report ('==[MOD-TOTAL]==', $tnum_gold, $tnum_mgold, $tnum_answer, $tnum_manswer);
    print STDOUT "------------------------------------------------------------------------------------\n";
} # if

if ($opt{x}) {
    if (@FP) {print "\n"}
    foreach (@FP) {print "[FP]  $_\n"}
    if (@FN) {print "\n"}
    foreach (@FN) {print "[FN]  $_\n"}
} # if

exit;


my ($c, $g, $mg, $w, $mw, $r, $p, $f);

format STDOUT_TOP =
------------------------------------------------------------------------------------
       Event Class          gold (match)   answer (match)   recall    prec.   fscore  
------------------------------------------------------------------------------------
.

format STDOUT =
  @||||||||||||||||||||    @#### (@####)    @#### (@####)   @##.##   @##.##   @##.##  
$c, $g, $mg, $w, $mw, $r, $p, $f
.

sub report ($$$$$) {
    ($c, $g, $mg, $w, $mw) = @_;
    ($r, $p, $f) = &accuracy ($g, $mg, $w, $mw);
    write ();
} # report

sub accuracy {
    my ($gold, $mgold, $answer, $manswer) = @_;

    my $rec = ($gold)?   $mgold   /   $gold : 0;
    my $pre = ($answer)? $manswer / $answer : 0;
    my $f1s = ($pre + $rec)? (2 * $pre * $rec) / ($pre + $rec) : 0;
    
    return ($rec * 100, $pre * 100, $f1s * 100);
} # accuracy

sub storeFP {
    my $tanno = $ranswer{$_[0]};
    while ($tanno =~ /:(T[0-9]+)/) {my $tspan = &tspan($1, \%answer); $tanno =~ s/:$1/:$tspan/}
    push @FP, "$fstem#$tanno";
} # storeFP

sub storeFN {
    my $tanno = $rgold{$_[0]};
    while ($tanno =~ /:(T[0-9]+)/) {my $tspan = &tspan($1, \%gold); $tanno =~ s/:$1/:$tspan/}
    push @FN, "$fstem#$tanno";
} # storeFN

sub tspan {
    my ($id, $rh_anno) = @_;
    my ($beg, $end);
    if    (($id =~ /^T/) && $protein{$id})   {($beg, $end) = (${$protein{$id}}[1],   ${$protein{$id}}[2])}
    elsif (($id =~ /^T/) && $rh_anno->{$id}) {($beg, $end) = (${$rh_anno->{$id}}[1], ${$rh_anno->{$id}}[2])}
    else  {return $id}

    return '"' . substr ($text, $beg, $end-$beg) . '"'. "[$beg-$end]";
} # tspan

sub count_match {
    my %cnt_manswer = (); # count matches of answer annotation instances.
    my %cnt_mgold   = (); # count matches of gold annotation instances.

    my @answer = (); foreach (keys %answer) {if (/^[EM]/) {push @answer, $_; $cnt_manswer{$_} = 0}}
    my @gold   = (); foreach (keys %gold)   {if (/^[EM]/) {push @gold,   $_; $cnt_mgold{$_} = 0}}

    #  for each answer,
    foreach my $aid (@answer) {
        # search for golds which match it.
    	foreach my $gid (@gold) {
    	    # when found,
    	    if (eq_event($aid, $gid)) {
        		$cnt_manswer{$aid}++;
        		$cnt_mgold{$gid}++;
    	    } # if
    	} # foreach
    } # foreach

    # update per-class statistics & store
    foreach (@answer) {if ($cnt_manswer{$_} > 0) {$num_manswer{${$answer{$_}}[0]}++}}
    foreach (@gold)   {if ($cnt_mgold{$_}   > 0) {$num_mgold{${$gold{$_}}[0]}++}}

    # store FPs and FNs
    foreach (@answer) {if ($cnt_manswer{$_} < 1) {&storeFP($_)}}
    foreach (@gold)   {if ($cnt_mgold{$_}   < 1) {&storeFN($_)}}
} # count_match


sub eq_event {
    my ($aeid, $geid) = @_;

    if (($aeid =~ /^E/) && ($geid =~ /^E/)) {
    	if ($fn_eq_class->($aeid, $geid) &&
    	    $fn_eq_span->($aeid, $geid) &&
    	    $fn_eq_args->($aeid, $geid)) {return 1}
    } # if

    elsif (($aeid =~ /^M/) && ($geid =~ /^M/)) {
    	if ($fn_eq_class->($aeid, $geid) &&
    	    $fn_eq_args->($aeid, $geid)) {return 1}
    } # elsif

    else {return 0}
} # eq_event


sub eq_revent {
    my ($aeid, $geid) = @_;
    if ($aeid !~ /^E/) {warn "non-event annotation: $aeid.\n"; return 0}
    if ($geid !~ /^E/) {warn "non-event annotation: $geid.\n"; return 0}

    if ($fn_eq_class->($aeid, $geid) &&
	    $fn_eq_span->($aeid, $geid) &&
	    $fn_eq_rargs->($aeid, $geid)) {return 1}

    else {return 0}
} # eq_event


sub eq_entity {
    my ($aeid, $geid) = @_;
    if ($aeid !~ /^T/) {warn "[eq_entity] non-entity annotation: $aeid.\n"; return 0}
    if ($geid !~ /^T/) {warn "[eq_entity] non-entity annotation: $geid.\n"; return 0}

    if ($fn_eq_class->($aeid, $geid) && $fn_eq_span->($aeid, $geid)) {return 1}
    else {return 0}
} # eq_entity


sub eq_span_hard {
    my ($aid, $gid) = @_;
    my ($abeg, $aend, $gbeg, $gend) = (-1, -1, -1, -1);

    if (($aid =~ /^T/) && $protein{$aid}) {return ($aid eq $gid)}

    if    ($aid =~ /^T/) {$abeg = ${$answer{$aid}}[1]; $aend = ${$answer{$aid}}[2]}
    elsif ($aid =~ /^E/) {$abeg = ${$answer{${$answer{$aid}}[1]}}[1]; $aend = ${$answer{${$answer{$aid}}[1]}}[2]}

    if    ($gid =~ /^T/) {$gbeg = ${$gold{$gid}}[1];   $gend = ${$gold{$gid}}[2]}
    elsif ($gid =~ /^E/) {$gbeg = ${$gold{${$gold{$gid}}[1]}}[1];     $gend = ${$gold{${$gold{$gid}}[1]}}[2]}

    if (($abeg < 0) || ($gbeg < 0)) {warn "failed to find the span: $fstem ($aid, $gid)\n"; return ''}

    return (($abeg == $gbeg) && ($aend == $gend));
} # eq_span_hard


sub eq_span_soft {
    my ($aid, $gid) = @_;
    my ($abeg, $aend, $gbeg, $gend) = (-1, -1, -2, -2);

    if (($aid =~ /^T/) && $protein{$aid}) {return ($aid eq $gid)}

    if    ($aid =~ /^T/) {$abeg = ${$answer{$aid}}[1]; $aend = ${$answer{$aid}}[2]}
    elsif ($aid =~ /^E/) {$abeg = ${$answer{${$answer{$aid}}[1]}}[1]; $aend = ${$answer{${$answer{$aid}}[1]}}[2]}

    if    ($gid =~ /^T/) {$gbeg = ${$gold{$gid}}[1];   $gend = ${$gold{$gid}}[2]}
    elsif ($gid =~ /^E/) {$gbeg = ${$gold{${$gold{$gid}}[1]}}[1];     $gend = ${$gold{${$gold{$gid}}[1]}}[2]}

    if (($abeg < 0) || ($gbeg < 0)) {warn "failed to find the span: $fstem ($aid, $gid)\n"; return ''}

    ($gbeg, $gend) = &expand_span($gbeg, $gend);
    return (($abeg >= $gbeg) && ($aend <= $gend));
} # eq_span_soft


sub eq_span_soft_notrigger {
    my ($aid, $gid) = @_;
    if (($aid =~ /^E/) && ($gid =~ /^E/)) {return 1}
    return &eq_span_soft($aid, $gid);
} # eq_span_soft_notrigger


# expand an entity span
# it refers to global variables $text and @entity 
sub expand_span  {
    my ($beg, $end) = @_;

    my $ebeg = $beg - 2;
    while (($ebeg >= 0)        && (substr ($text, $ebeg, 1) !~ /[ .!?,"']/) && ($textpic[$ebeg] ne 'E')) {$ebeg--} # '"
    $ebeg++;

    my $eend = $end + 2;
    while (($eend <= $textlen) && (substr ($text, $eend-1, 1) !~ /[ .!?,"']/) && ($textpic[$eend-1] ne 'E')) {$eend++} # '"
    $eend--;

#    warn "\n", substr ($text, $ebeg-5, $eend-$ebeg+10), "\n";
#    for(my $i = $ebeg-5; $i< $eend+5; $i++) {
#	if ($textpic[$i]) {warn $textpic[$i]}
#	else {warn ' '}
#    } # for ($i)
#    warn "\n";
#    warn substr ($text, $beg, $end-$beg), "  ===> ", substr ($text, $ebeg, $eend-$ebeg), "\n";

    return ($ebeg, $eend);
} # expand_span


sub eq_class_hard {
    my ($aid, $gid) = @_;
    if    ($protein{$aid}) {return ($aid eq $gid)}
    elsif ($answer{$aid})  {return (${$answer{$aid}}[0] eq ${$gold{$gid}}[0])}
    else  {return 0}
} # eq_class_hard


sub eq_class_soft {
    my ($aid, $gid) = @_;
    if    ($protein{$aid}) {return ($aid eq $gid)}
    elsif ($answer{$aid})  {
    	my $aclass = ${$answer{$aid}}[0];
    	my $gclass = ${$gold{$gid}}[0];

    	$aclass =~ s/^Positive_r/R/; $gclass =~ s/^Positive_r/R/;
    	$aclass =~ s/^Negative_r/R/; $gclass =~ s/^Negative_r/R/;

    	$aclass =~ s/^Transcription$/Gene_expression/; $gclass =~ s/^Transcription$/Gene_expression/;

        $aclass =~ s/^Phosphorylation$/Protein_modification/; $gclass =~ s/^Phosphorylation$/Protein_modification/;
        $aclass =~ s/^Ubiquitination$/Protein_modification/; $gclass =~ s/^Ubiquitination$/Protein_modification/;
        $aclass =~ s/^Acetylation$/Protein_modification/; $gclass =~ s/^Acetylation$/Protein_modification/;
        $aclass =~ s/^Deacetylation$/Protein_modification/; $gclass =~ s/^Deacetylation$/Protein_modification/;

    	return ($aclass eq $gclass);
    } # elsif
    else  {return 0}
} # eq_class_soft


sub eq_args_hard {
    my ($aeid, $geid) = @_;

    my @answer_arg = @{$answer{$aeid}};
    my $aetype = shift @answer_arg;
    my $atid   = shift @answer_arg;
    
    my @gold_arg =   @{$gold{$geid}};
    my $getype = shift @gold_arg;
    my $gtid   = shift @gold_arg;

    if ($#answer_arg != $#gold_arg) {return ''}

#    if (($aeid eq 'E3') && ($geid eq 'E4')) {
#	warn " (", join (", ", @answer_arg), ")\n";
#	warn " (", join (", ", @gold_arg), ")\n";
#   }

    ## compare argument lists as ordered lists.
    for (my $i = 0; $i <= $#answer_arg; $i++) {
    	my ($aatype, $aaid) = split /:/, $answer_arg[$i];
    	my ($gatype, $gaid) = split /:/, $gold_arg[$i];

        # merge AtLoc, ToLoc, FromLoc into Loc
        $aatype =~ s/^.+Loc$/Loc/;
        $gatype =~ s/^.+Loc$/Loc/;

    	if ($aatype ne $gatype) {return ''}

    	# both have to be either t-entities or events
    	if (substr($aaid, 0, 1) ne substr($gaid, 0, 1))  {return ''}
    	if (($aaid =~ /^E/) && !&eq_revent($aaid, $gaid)) {return ''}
    	if (($aaid =~ /^T/) && !&eq_entity($aaid, $gaid)) {return ''}
    } # for

    return 1;
} # eq_args_hard


sub eq_args_soft {
    my ($aeid, $geid) = @_;

    my @answer_arg = @{$answer{$aeid}};
    my $aetype = shift @answer_arg;
    shift @answer_arg;
    while ($answer_arg[-1] !~ /^Theme:/) {pop @answer_arg}
    
    my @gold_arg =   @{$gold{$geid}};
    my $getype = shift @gold_arg;
    shift @gold_arg;
    while ($gold_arg[-1] !~ /^Theme:/) {pop @gold_arg}

    ## compare argument lists as ordered lists.
    if ($#answer_arg != $#gold_arg) {return ''}
    for (my $i = 0; $i <= $#answer_arg; $i++) {
    	my ($aatype, $aaid) = split /:/, $answer_arg[$i];
    	my ($gatype, $gaid) = split /:/, $gold_arg[$i];

    	# both have to be either t-entities or events
    	if (substr($aaid, 0, 1) ne substr($gaid, 0, 1))  {return ''}
    	if (($aaid =~ /^E/) && !&eq_revent($aaid, $gaid)) {return ''}
    	if (($aaid =~ /^T/) && !&eq_entity($aaid, $gaid)) {return ''}
    } # for

    return 1;
} # eq_args_soft


## representation of annotations
# t-entities:          TID (entity_type, beg, end)
#
# event annotation:    EID (event-type, event_entity_id, arg_type:arg_id, arg_type:arg_id, ...)
#                     * order of arguments: theme, cause, site, csite, AtLoc, ToLoc
#                     * linear order between themes
#                     * site may be numbered to indicate the specific theme
#
#  Modifier:           MID (mod_type, '', (Theme, $arg))

sub read_text_file ($$) {
    my ($fname) = $_[0];
    $_[1] = '';     # text, output variable

    if (!open (FILE, "<", $fname)) {warn "cannot open the file: $fname\n"; return ''}
    while (<FILE>) {$_[1] .= $_}
    close (FILE);

    return length $_[1];
} # read_text_file


sub read_a1_file ($$) {
    my ($fname, $rh_anno) = @_;   # rh: reference to hash

    if (!open (FILE, "<", $fname)) {warn "cannot open the a1 file: $fname\n"; return ''}
    my @line = <FILE>; chomp (@line);
    close (FILE);

    foreach (@line) {
	my ($id, $exp) = split /\t/;

	if (/^T/) {
	    my ($type, $beg, $end) = split ' ', $exp;

	    # for text picture
	    for (my $i = $beg; $i < $end; $i++) {$textpic[$i] = 'E'}

	    $rh_anno->{$id} = [$type, $beg, $end];
	} # if
	else {
	    warn "invalid annotation in a1 file: [$fstem] $_\n";
	} # else
    } # foreach

    return $#line + 1;
} # read_a1_file


sub read_a2_file ($$$) {
    my ($fname, $mode, $task) = @_; # rh: reference to hash

    if (!open (FILE, "<", $fname)) {warn "cannot open the a2 file: $fname\n"; return -1}
    my @line = <FILE>; chomp (@line);
    close (FILE);

    my ($rh_anno, $rh_site, $rh_ranno, $rh_num_event); # reference to hash
    if ($mode eq 'G') {$rh_anno = \%gold;   $rh_site = \%gold_site;   $rh_ranno = \%rgold;   $rh_num_event = \%num_gold}
    else              {$rh_anno = \%answer; $rh_site = \%answer_site; $rh_ranno = \%ranswer; $rh_num_event = \%num_answer}

    foreach (@line) {
    	my ($id, $exp) = split /\t/;
    	$rh_ranno->{$id} = $_;

    	if (/^T/) {
    	    my ($type, $beg, $end) = split ' ', $exp;

    	    # for text picture
    	    if ($mode eq 'G') {for (my $i = $beg; $i < $end; $i++) {$textpic[$i] = 'E'}}

    	    $rh_anno->{$id} = [$type, $beg, $end];
    	} # if

    	elsif (/^E/) {
    	    my @arg = split ' ', $exp;
    	    my ($type, $tid) = split ':', shift @arg;

    	    my @newarg = ();
    	    foreach (@arg) {
    		my ($atype, $aid) = split ':';

    		$atype =~ s/^Theme[2-6]$/Theme/;
    		if ($equiv{$aid}) {$aid = $equiv{$aid}}

    		if (($atype =~ /Site/) || ($atype =~ /Loc/)) {
    		    if ($task == 2) {if ($mode eq 'A') {$num_t2arg++}}
    		    else            {next}
    		} # if

    		push @newarg, "$atype:$aid";
    	    } # foreach

    	    $rh_anno->{$id} = [$type, $tid, @newarg];
    	} # elsif

    	elsif (/^M/) {
    	    my ($type, $aid) = split ' ', $exp;

    	    if ($task == 3) {if ($mode eq 'A') {$num_t3stat++}}
    	    else            {next}

    	    $rh_anno->{$id} = [$type, '', ("Theme:$aid")];
    	} # elsif

    	elsif (/^\*/) {
    	    my ($rel, @pid) = split ' ', $exp;
    	    my ($rep, @other) = @pid;
    	    foreach (@other) {$equiv{$_} = $rep}
    	} # elsif

    } # foreach


    my @elist = grep /^[EM]/, keys %{$rh_anno};


    # detect and remove duplication by Simplication
    if ($mode eq 'G') {
    	## sort events
    	my @newelist = ();
    	my %added = ();
    	my %remain = ();
    	foreach (@elist) {$remain{$_} = 1}
    	while (%remain) {
    	    my $changep = 0;
    	    foreach (keys %remain) {
        		my @earg = grep /:E[0-9-]+$/, @{$rh_anno->{$_}};
        		my @eaid = map {(split /:/)[1]} @earg;
        		my $danglingp = 0;
        		foreach (@eaid) {
        		    if (!$added{$_}) {$danglingp = 1; last}
        		} # foreach
        		if (!$danglingp) {push @newelist, $_; $added{$_} = 1; delete $remain{$_}; $changep = 1}
    	    } # foreach
    	    if (!$changep) {
        		if ($opt{v}) {warn "circular reference: [$fstem] ", join (', ', keys %remain), "\n"}
        		push @newelist, keys %remain;
        		%remain = ();
    	    } # if
    	} # while

    	@elist = @newelist;

    	my %equiv = ();		# 'equiv' locally defined only for gold data
    	my %eventexp = (); # for checking of event duplication
    	foreach my $eid (@elist) {
    	    # get event expression
    	    foreach (@{$rh_anno->{$eid}}) {
        		if (!/:/) {next}
        		my ($atype, $aid) = split /:/;
        		if ($equiv{$aid}) {$aid = $equiv{$aid}}
        		$_ = "$atype:$aid";
    	    } # foreach

    	    my $eventexp = join ',', @{$rh_anno->{$eid}};

    	    # check duplication
    	    if (my $did = $eventexp{$eventexp}) {
        		delete $rh_anno->{$eid};
        		$equiv{$eid} = $did;
        		if ($opt{v}) {warn "[$fstem] $eid is equivalent to $did => removed.\n"}
    	    } # if
    	    else {$eventexp{$eventexp} = $eid}
    	} # foreach

    	@elist = grep /^[EM]/, keys %{$rh_anno};
    } # if (mode eq 'G')


    # detect and remove duplication by Equiv
    if ($mode eq 'A') {
    	## sort events
    	my @newelist = ();
    	my %added = ();
    	my %remain = ();
    	foreach (@elist) {$remain{$_} = 1}
    	while (%remain) {
    	    my $changep = 0;
    	    foreach (keys %remain) {
        		my @earg = grep /:E[0-9-]+$/, @{$rh_anno->{$_}};
        		my @eaid = map {(split /:/)[1]} @earg;
        		my $danglingp = 0;
        		foreach (@eaid) {
        		    if (!$added{$_}) {$danglingp = 1; last}
        		} # foreach
        		if (!$danglingp) {push @newelist, $_; $added{$_} = 1; delete $remain{$_}; $changep = 1}
    	    } # foreach
    	    if (!$changep) {
        		if ($opt{v}) {warn "circular reference: [$fstem] ", join (', ', keys %remain), "\n"}
        		push @newelist, keys %remain;
        		%remain = ();
    	    } # if
    	} # while

    	@elist = @newelist;

    	my %eventexp = (); # for checking of event duplication
    	foreach my $eid (@elist) {
    	    # get event expression
    	    foreach (@{$rh_anno->{$eid}}) {
        		if (!/:/) {next}
        		my ($atype, $aid) = split /:/;
        		if ($equiv{$aid}) {$aid = $equiv{$aid}}
        		$_ = "$atype:$aid";
    	    } # foreach

    	    my $eventexp = join ',', @{$rh_anno->{$eid}};

    	    # check duplication
    	    if (my $did = $eventexp{$eventexp}) {
        		delete $rh_anno->{$eid};
        		$equiv{$eid} = $did;
        		if ($opt{v}) {warn "[$fstem] $eid is equivalent to $did => removed.\n"}
    	    } # if
    	    else {$eventexp{$eventexp} = $eid}
    	} # foreach

    	@elist = grep /^[EM]/, keys %{$rh_anno};
    } # if (mode eq 'A')

    # get statistics
    my $num_event = 0;
    foreach my $eid (@elist) {
    	my $type = ${$rh_anno->{$eid}}[0];
    	$rh_num_event->{$type}++; $num_event++;
    } # foreach

#    foreach (@elist) {
#	warn "$_\t", join (", ", @{$rh_anno->{$_}}), "\n";
#    } # foreach
#    warn "==========\n";

    return $num_event;
} # read_a2_file


sub usage {
    warn << "EOF";

[a2-evaluate]
last updated by jdkim\@dbcls.rois.ac.jp on 13 February, 2011. 


<DESCRIPTION>
It is a part of the evaluation tools for
the GENIA event extracton task of BioNLP Shared Task 2011.

It takes a2 files and evaluate the accuracy by comparing to the 'gold' ones.

a2-normalize has to be first applied to the a2 files to be evaluated,
for format checking and normalization.


<USAGE>
$0 [-$opt_string] a2_file(s)


<OPTIONS>
-g gold_dir specifies the 'gold' directory (default = $gdir)
-t task     specifies the task type (1, 2, or 3; default = $task)
-s          tells it to perform a soft matching for the boundary of triggers and entities.
-S          tells it to ignore event triggers and perform a soft matching the boundary of entities.
-p          tells it to perform an approximate recursive matching.
-v          verbose output.
-x          output false positives/negatives
-d          debugging message.


<REFERENCE>
https://sites.google.com/site/bionlpst/

EOF
      exit;
} # usage
