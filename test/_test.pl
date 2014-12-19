#!/usr/bin/env perl
use strict;

use File::Basename;
use Test::Simple tests => 1;

my $dir = dirname $0;
$dir .= "/..";
my $bin = "$dir/bin";

my ($file) = @ARGV
    or die "no args";

open FH, "<", $file
    or die "cannot open file '$file'";

undef $/;
my $content = <FH>;

my ($expected) = $content =~ m{/\*\n(|.*?\n)\*/}s
    or die "expected output not given in '$file'";

system "$bin/ucc $file 2> /dev/null";

if ($? != 0) {
    ok $expected eq "DEAD\n", "ok";
    exit;
}

(my $outfile = $file) =~ s/^(.*)\.c$/$1.out/;

open PIPE, "$bin/sim $outfile -write raw 2> /dev/null |"
    or die "could not run simulator with $outfile";

undef $/;
my $output = <PIPE>;

ok $output eq $expected, 'ok';
