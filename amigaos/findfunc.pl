#! /bin/perl
# findfunc.pl - print caller chains in human readable form
#
# It takes text (aka code) segment offsets and returns the embracing function.
# (Probably there is already a shell command for this functionality, but I don't know.)
#
# It requires: 
#     - The examined binary must contain debug information (compiled with -g)
#     - installed perl(1) (version 4 should suffice)
#     - installed nm(1) from gnu-binutils.
# 
#  Why not (ab)use gccfindhit:
#    - the output would "sorted" for offsets, which is not the right thing for
#       function call chains.
#    - I'm more interested on functions names than on line numbers, especially
#        then the source has changed.

$#ARGV >= 1 || die "Usage: $0 Executable Offset ...\n";

open (INPUT, "nm -Cn $ARGV[0] |");

@soffs;
for ($i = 1; $i <= $#ARGV; ++$i) {
    if ($ARGV[$i] =~ /^0x/) {
	$soffs[$i] = hex ($ARGV[$i]);
    } elsif ($ARGV[$i] =~ /^0/) {
	$soffs[$i] = oct ($ARGV[$i]);
    } else {
	$soffs[$i] = int ($ARGV[$i]);
    }
}

@result_name;
@result_offs;

while (<INPUT>) {
    next unless /^(........) T (.*)$/;
    $offs = hex ("0x" . $1);

    for ($i = 1; $i <= $#soffs; ++$i) {
	if ($offs <= $soffs[$i]) {
	    $result_name[$i] = $2;
	    $result_offs[$i] = $offs;
	}
    }
}

close INPUT;

print "\n";
print "hunk-offs.  func-entry           func-name\n";
print "==========  ==========  ================================\n";

for ($i = 1; $i <= $#soffs; ++$i) {
    printf ("0x%0.8x  0x%0.8x  $result_name[$i] \n",
	    $soffs[$i], $result_offs[$i]);
}
print "==========  ==========  ================================\n";

