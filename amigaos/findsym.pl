#! /bin/perl
# parses file src/amiga_dump.cands and nm output for temacs
# prints out symbols list
#
# Author: Bert Winkelmann <bertw@in-brb.de>
# Date: 15-Oct-97 
#
# requires: tool nm(1),
#             files: temacs + amiga_dump.cands in directory 
#                    /build/emacs-20.2/src/
#
#

use strict;

# sorted temacs nm.out file/pipe
my $nm_out_file = "nm -Cn $ARGV[0] |";
#my $nm_out_file = "/build/emacs-20.2/src/nm.out";


# containing a offset list which first two fields are OFFSET and [TBD]
# segment id
#my $cand_out_file = "/build/emacs-20.2/src/amiga_dump.cands";
my $cand_out_file = $ARGV[1];

#print "$nm_out_file\n";
#print "$cand_out_file\n";

my $last_data;
my $last_bss;
my $last_text;

my $curr_data;
my $curr_bss;
my $curr_text;

my $last_data_sym;
my $last_bss_sym;
my $last_text_sym;

my $curr_data_sym;
my $curr_bss_sym;
my $curr_text_sym;

my @list_data;
my @list_bss;
my @list_text;

my @list_data_comment;
my @list_bss_comment;
my @list_text_comment;

my $list_data_idx;
my $list_bss_idx;
my $list_text_idx;


#parse cand list and fill numeric offsets to tables @list_xxx
sub parse_cand_out {
    while (<>) {
	if (/^([0-9A-Fa-f]{8}) \b([TDB])\b ?(.*)$/) {
	    if ("T" eq $2) {
		$list_text[$#list_text + 1] = hex ($1);
		$list_text_comment[$#list_text_comment + 1] = $3;
	    } elsif ("D" eq $2) {
		$list_data[$#list_data + 1] = hex ($1);
		$list_data_comment[$#list_data_comment + 1] = $3;
	    } elsif ("B" eq $2) {
		$list_bss[$#list_bss + 1] = hex ($1);
		$list_bss_comment[$#list_bss_comment + 1] = $3;
	    }
	}
    }
}

#parse a line of nm.out file and fill in $curr_xxx and $last_xxx 
sub parse_nm_out_line {
    if (/^([0-9A-Fa-f]{8}) ([TDB]) ([_A-Za-z][_A-Za-z0-9]+)$/) {
	if ("T" eq $2) {
	    $last_text = $curr_text;
	    $last_text_sym = $curr_text_sym;
	    $curr_text = hex ($1);
	    $curr_text_sym = $3;
	} elsif ("D" eq $2) {
	    $last_data = $curr_data;
	    $last_data_sym = $curr_data_sym;
	    $curr_data = hex ($1);
	    $curr_data_sym = $3;
	} elsif ("B" eq $2) {
	    $last_bss = $curr_bss;
	    $last_bss_sym = $curr_bss_sym;
	    $curr_bss = hex ($1);
	    $curr_bss_sym = $3;
	}
    }
}


#main
open (CAND_OUT, $cand_out_file) || die "cannot open cand.out file";
&parse_cand_out;

#stop fields
$list_text[$#list_text + 1] = 99999999;
$list_data[$#list_data + 1] = 99999999;
$list_bss[$#list_bss + 1] = 99999999;

open (NM_OUT, $nm_out_file) || die "cannot open nm.out file";
while (<NM_OUT>) {
    &parse_nm_out_line;

#FIXME-bw: correct?
    ++$list_text_idx while ($list_text[$list_text_idx] < $last_text);
    ++$list_data_idx while ($list_data[$list_data_idx] < $last_data);
    ++$list_bss_idx while ($list_bss[$list_bss_idx] < $last_bss);

    if ($list_text[$list_text_idx] < $curr_text) {
	printf ("%08x %s < %08x %s > + %d (%s)\n",
		$list_text[$list_text_idx],
		"T",
		$last_text, $last_text_sym,
		$list_text[$list_text_idx] - $last_text,
		$list_text_comment[$list_text_idx],
		);
    }
    if ($list_data[$list_data_idx] < $curr_data) {
	printf ("%08x %s < %08x %s > + %d (%s)\n",
		$list_data[$list_data_idx],
		"D",
		$last_data, $last_data_sym,
		$list_data[$list_data_idx] - $last_data,
		$list_data_comment[$list_data_idx],
		);
    }
    if ($list_bss[$list_bss_idx] < $curr_bss) {
	printf ("%08x %s < %08x %s > + %d (%s)\n",
		$list_bss[$list_bss_idx],
		"B",
		$last_bss, $last_bss_sym,
		$list_bss[$list_bss_idx] - $last_bss,
		$list_bss_comment[$list_bss_idx],
		);
    }
}


