#!/usr/bin/perl

use Time::HiRes qw( gettimeofday tv_interval );

for (my $ssize = 1; $ssize<=30; $ssize++) {
    my $str = 'a' x $ssize;
    my $pat = "(a?)\{$ssize\}a\{$ssize\}";

    print "$str =~ /$pat/\n";

    my $t0 = [gettimeofday];

    $str =~ /$pat/;

    my $elapsed = tv_interval ( $t0 );

    print "$ssize,$elapsed\n";
}
