#!/usr/bin/perl
require 5.000;
use strict;

my @target_theme = ('Theme-Gene_expression',
                    'Theme-Transcription',
                    'Theme-Protein_catabolism',
                    'Theme-Binding',
                    'Theme-Localization',
                    'Theme-Protein_modification',
                    'Theme-Phosphorylation',
                    'Theme-Ubiquitination',
                    'Theme-Acetylation',
                    'Theme-Deacetylation',
                    'Theme-Regulation',
                    'Theme-Positive_regulation',
                    'Theme-Negative_regulation');

my @target_cause = ('Cause-Regulation',
                    'Cause-Positive_regulation',
                    'Cause-Negative_regulation',
                    'Cause-Protein_modification',
                    'Cause-Phosphorylation',
                    'Cause-Ubiquitination',
                    'Cause-Acetylation',
                    'Cause-Deacetylation');

my @target_site  = ('Site-Binding',
                    'Site-Protein_modification',
                    'Site-Phosphorylation',
                    'Site-Ubiquitination',
                    'Site-Acetylation',
                    'Site-Deacetylation',
                    'Site-Regulation',
                    'Site-Positive_regulation',
                    'Site-Negative_regulation',
        		    'CSite-Regulation',
                    'CSite-Positive_regulation',
                    'CSite-Negative_regulation');
my @target_loc   = ('AtLoc-Localization',
                    'ToLoc-Localization',
                    'FromLoc-Localization');

my @target_class = (@target_theme, @target_cause, @target_site, @target_loc);

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

## functions for equivalency checking
my $fn_eq_span  = \&eq_span_hard;;
my $fn_eq_class = \&eq_class_hard;
my $fn_eq_args  = \&eq_args_hard;
my $fn_eq_rargs = \&eq_args_hard;


## for total scoring
#  - initialized only once.
my (%tnum_gold, %tnum_mgold, %tnum_answer, %tnum_manswer); # number of golds/matched golds/answers/matched answers
foreach (@target_class) {$tnum_gold{$_} = $tnum_answer{$_} = $tnum_mgold{$_} = $tnum_manswer{$_} = 0}

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


my ($fstem); # $fstem will be globally referenced to know what file is under processing.
foreach my $fname (@ARGV) {
    if ($fname =~ /([^\/]+)\.a2d$/) {$fstem = $1}
    else {warn "unrecognizable filename: $fname.\n"; next} 

    ## initialization of file-specific global variables
    ($text, $textlen, @textpic) = ('', 0, ());
    (%protein, %gold, %gold_site, %equiv, %answer, %answer_site) = ((), (), (), (), (), ());
    (%rgold, %ranswer) = ((), ());
    ($num_gold, $num_mgold, $num_answer, $num_manswer) = (0, 0, 0, 0);
    foreach (@target_class) {$num_gold{$_} = $num_answer{$_} = $num_mgold{$_} = $num_manswer{$_} = 0}

    ## event loading
    if (!($textlen = &read_text_file("$gdir/$fstem.txt", $text))) {next}
    if (!&read_a1_file("$gdir/$fstem.a1", \%protein)) {next}
    if (($num_gold   = &read_a2d_file("$gdir/$fstem.a2d", 'G')) < 0) {next}
    if (($num_answer = &read_a2d_file($fname,               , 'A')) < 0) {next}

    ## set matching methods
    if ($opt{s}) {$fn_eq_span  = \&eq_span_soft}
    if ($opt{S}) {$fn_eq_span  = \&eq_span_soft_notrigger}
#    if ($opt{m}) {$fn_eq_class = \&eq_class_soft}
    if ($opt{p}) {$fn_eq_rargs = \&eq_args_hard}

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
    	    if ($opt{v}) {warn "According to approximate mathing criteria, $num_danswer equivalent event(s) in your submission is (are) detected, and discarded: [$fstem]\n"}
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


## Report

if ($task == 1) {

    $tnum_gold{'Theme'} = $tnum_mgold{'Theme'} = $tnum_answer{'Theme'} = $tnum_manswer{'Theme'} = 0; # sub-total
    foreach (@target_theme) {
    	&report ($_, $tnum_gold{$_}, $tnum_mgold{$_}, $tnum_answer{$_}, $tnum_manswer{$_});
    	$tnum_gold{'Theme'} += $tnum_gold{$_}; $tnum_mgold{'Theme'} += $tnum_mgold{$_};
    	$tnum_answer{'Theme'} += $tnum_answer{$_}; $tnum_manswer{'Theme'} += $tnum_manswer{$_};
    } # foreach
    print STDOUT "--------------------------------------------------------------------------------------\n";
    &report ('=[Theme-TOTAL]=', $tnum_gold{'Theme'}, $tnum_mgold{'Theme'}, $tnum_answer{'Theme'}, $tnum_manswer{'Theme'});
    print STDOUT "--------------------------------------------------------------------------------------\n";

    $tnum_gold{'Cause'} = $tnum_mgold{'Cause'} = $tnum_answer{'Cause'} = $tnum_manswer{'Cause'} = 0; # sub-total
    foreach (@target_cause) {
    	&report ($_, $tnum_gold{$_}, $tnum_mgold{$_}, $tnum_answer{$_}, $tnum_manswer{$_});
    	$tnum_gold{'Cause'} += $tnum_gold{$_}; $tnum_mgold{'Cause'} += $tnum_mgold{$_};
    	$tnum_answer{'Cause'} += $tnum_answer{$_}; $tnum_manswer{'Cause'} += $tnum_manswer{$_};
    } # foreach
    print STDOUT "--------------------------------------------------------------------------------------\n";
    &report ('=[Cause-TOTAL]=', $tnum_gold{'Cause'}, $tnum_mgold{'Cause'}, $tnum_answer{'Cause'}, $tnum_manswer{'Cause'});
    print STDOUT "--------------------------------------------------------------------------------------\n";

    # grand total
    my $tnum_gold    = $tnum_gold{'Theme'}    + $tnum_gold{'Cause'};
    my $tnum_mgold   = $tnum_mgold{'Theme'}   + $tnum_mgold{'Cause'};
    my $tnum_answer  = $tnum_answer{'Theme'}  + $tnum_answer{'Cause'};
    my $tnum_manswer = $tnum_manswer{'Theme'} + $tnum_manswer{'Cause'};
    &report ('==[ALL-TOTAL]==', $tnum_gold, $tnum_mgold, $tnum_answer, $tnum_manswer);
    print STDOUT "--------------------------------------------------------------------------------------\n";
} # if


if ($task == 2) {

    my $num_task2_answers = 0;
    foreach (@target_site, @target_loc) {$num_task2_answers += $tnum_answer{$_}}
    if ($num_task2_answers == 0) {die "no argument that belongs to task 2 found.\n"}

    else {
	$tnum_gold{'Site'} = $tnum_mgold{'Site'} = $tnum_answer{'Site'} = $tnum_manswer{'Site'} = 0; # sub-total
	foreach (@target_site) {
	    &report ($_, $tnum_gold{$_}, $tnum_mgold{$_}, $tnum_answer{$_}, $tnum_manswer{$_});
	    $tnum_gold{'Site'} += $tnum_gold{$_}; $tnum_mgold{'Site'} += $tnum_mgold{$_};
	    $tnum_answer{'Site'} += $tnum_answer{$_}; $tnum_manswer{'Site'} += $tnum_manswer{$_};
	} # foreach
	print STDOUT "--------------------------------------------------------------------------------------\n";
	&report ('=[Site-TOTAL]=', $tnum_gold{'Site'}, $tnum_mgold{'Site'}, $tnum_answer{'Site'}, $tnum_manswer{'Site'});
	print STDOUT "--------------------------------------------------------------------------------------\n";

	$tnum_gold{'Loc'} = $tnum_mgold{'Loc'} = $tnum_answer{'Loc'} = $tnum_manswer{'Loc'} = 0; # sub-total
	foreach (@target_loc) {
	    &report ($_, $tnum_gold{$_}, $tnum_mgold{$_}, $tnum_answer{$_}, $tnum_manswer{$_});
	    $tnum_gold{'Loc'} += $tnum_gold{$_}; $tnum_mgold{'Loc'} += $tnum_mgold{$_};
	    $tnum_answer{'Loc'} += $tnum_answer{$_}; $tnum_manswer{'Loc'} += $tnum_manswer{$_};
	} # foreach
	print STDOUT "--------------------------------------------------------------------------------------\n";
	&report ('=[Loc-TOTAL]=', $tnum_gold{'Loc'}, $tnum_mgold{'Loc'}, $tnum_answer{'Loc'}, $tnum_manswer{'Loc'});
	print STDOUT "--------------------------------------------------------------------------------------\n";

	# grand total
	my $tnum_gold    = $tnum_gold{'Site'}    + $tnum_gold{'Loc'};
	my $tnum_mgold   = $tnum_mgold{'Site'}   + $tnum_mgold{'Loc'};
	my $tnum_answer  = $tnum_answer{'Site'}  + $tnum_answer{'Loc'};
	my $tnum_manswer = $tnum_manswer{'Site'} + $tnum_manswer{'Loc'};
	&report ('==[ALL-TOTAL]==', $tnum_gold, $tnum_mgold, $tnum_answer, $tnum_manswer);
	print STDOUT "--------------------------------------------------------------------------------------\n";
    } # else
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
--------------------------------------------------------------------------------------
     Event-Arg Type            gold (match)   answer (match)   recall    prec.   fscore  
--------------------------------------------------------------------------------------
.

format STDOUT =
@<<<<<<<<<<<<<<<<<<<<<<<<<    @#### (@####)    @#### (@####)   @##.##   @##.##   @##.##  
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
    foreach (@answer) {if ($cnt_manswer{$_} > 0) {$num_manswer{"${$answer{$_}}[2]-${$answer{$_}}[0]"}++}}
    foreach (@gold)   {if ($cnt_mgold{$_}   > 0) {$num_mgold{"${$gold{$_}}[2]-${$gold{$_}}[0]"}++}}

    # store FPs and FNs
    foreach (@answer) {if ($cnt_manswer{$_} < 1) {&storeFP($_)}}
    foreach (@gold)   {if ($cnt_mgold{$_}   < 1) {&storeFN($_)}}
} # count_match


sub eq_event {
    my ($aeid, $geid) = @_;

    if    (($aeid =~ /^E/) && ($geid =~ /^E/)) {
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
	$aclass =~ s/^Transcription$/Gene_expression/; $gclass =~ s/^Transcription/Gene_expression/;
	return ($aclass eq $gclass);
    } # elsif
    else  {return 0}
} # eq_class_soft


sub eq_span_soft_notrigger {
    my ($aid, $gid) = @_;
    if (($aid =~ /^E/) && ($gid =~ /^E/)) {return 1}
    return &eq_span_soft($aid, $gid);
} # eq_span_soft_notrigger


sub eq_args_hard {
    my ($aeid, $geid) = @_;

    my ($aetype, $atid, $aatype, $aaid) = @{$answer{$aeid}};
    my ($getype, $gtid, $gatype, $gaid) = @{$gold{$geid}};

#    if (($aeid eq 'E14') && ($geid eq 'E14')) {
#	warn " (", join (", ", @answer_arg), ")\t";
#	warn " (", join (", ", @gold_arg), ")\n";
#    }

    ## compare the argument type and id.
    if ($aatype ne $gatype) {return ''}

    # both have to be either t-entities or events
    if (substr($aaid, 0, 1) ne substr($gaid, 0, 1))  {return ''}
    if (($aaid =~ /^E/) && !&eq_revent($aaid, $gaid)) {return ''}
    if (($aaid =~ /^T/) && !&eq_entity($aaid, $gaid)) {return ''}

    return 1;
} # eq_args_hard


## representation of annotations
# t-entities:          TID (entity_type, beg, end)
#
# event annotation:    EID (event-type, event_entity_id, arg_type, arg_id)
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


sub read_a2d_file ($$) {
    my ($fname, $mode) = @_; # rh: reference to hash

    if (!open (FILE, "<", $fname)) {warn "cannot open the a2d file: $fname\n"; return -1}
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
    	    if ($#arg != 1) {warn "format error: $fname\n"; return -1}

    	    my ($type, $tid)  = split ':', $arg[0];
    	    my ($atype, $aid) = split ':', $arg[1];
    	    if ($equiv{$aid}) {$aid = $equiv{$aid}}

    	    $rh_anno->{$id} = [$type, $tid, $atype, $aid];
    	} # elsif

    	elsif (/^M/) {
    	    my ($type, $aid) = split ' ', $exp;
    	    $rh_anno->{$id} = [$type, '', 'Theme', $aid];
    	} # elsif

    	elsif (/^\*/) {
    	    my ($rel, @pid) = split ' ', $exp;
    	    my ($rep, @other) = @pid;
    	    foreach (@other) {$equiv{$_} = $rep}
    	} # elsif

    } # foreach


    my @elist = grep /^[EM]/, keys %{$rh_anno};


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
        		my $aid = ${$rh_anno->{$_}}[3];
        		if (($aid =~ /^T/) || $added{$aid}) {push @newelist, $_; $added{$_} = 1; delete $remain{$_}; $changep = 1}
    	    } # foreach
    	    if (!$changep) {
        		if ($opt{v}) {warn "circular reference: [$fstem] ", join (', ', keys %remain), "\n"}
        		push @newelist, keys %remain;
        		%remain = ();
    	    } # if
    	} # while

    	@elist = @newelist;

    	# foreach (@elist) {
    	#     warn "$_\t", join (", ", @{$rh_anno->{$_}}), "\n";
    	# } # foreach

    	my %eventexp = (); # for checking of event duplication
    	foreach my $eid (@elist) {
    	    my $aid = ${$rh_anno->{$eid}}[3];
    	    if ($equiv{$aid}) {${$rh_anno->{$eid}}[3] = $equiv{$aid}}

    	    my $eventexp = join ',', @{$rh_anno->{$eid}};

    	    # check duplication
    	    if (my $did = $eventexp{$eventexp}) {
        		delete $rh_anno->{$eid};
        		$equiv{$eid} = $did;
        		if ($opt{v}) {warn "[$fstem] $eid is equivalent to $did => removed.\n"}
    	    } # if
    	    else {$eventexp{$eventexp} = $eid}
    	} # foreach
    } # else

    # get statistics
    my $num_event = 0;
    foreach my $eid (@elist) {
    	my $etype = ${$rh_anno->{$eid}}[0];
    	my $atype = ${$rh_anno->{$eid}}[2];
    	$rh_num_event->{"$atype-$etype"}++; 
    	$num_event++;
    } # foreach

    return $num_event;
} # read_a2d_file



sub usage {
    warn << "EOF";

[a2d-evaluate]
last updated by jdkim\@dbcls.rois.ac.jp on 13 February, 2011.


<DESCRIPTION>
It is a part of the evaluation tools for
the GENIA event extracton task of BioNLP Shared Task 2011.

It takes a2d files which are generated from a2 files by a2-decompose.pl,
and evaluates the accuracy by comparing to the 'gold' a2d files.

Note that the gold a2d files have be generated from gold a2 files
before evaluation.


<USAGE>
$0 [-$opt_string] a2d_file(s)


<OPTIONS>
-g gold_dir specifies the 'gold' directory.
-t task     specifies the task type (1, 2, or 3; default = 1).
-s          tells it to perform a soft matching for the boundary of triggers.
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
