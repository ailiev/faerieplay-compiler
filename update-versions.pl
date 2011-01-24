#!/usr/bin/perl

# Check the SVN versions of all the files given as args, and replace a token
# <versions>...</versions> with a summary of the versions. Actually the output
# of svnversion for each source directory (inferred from the argument files)

use warnings;

use English;

use File::Basename;

sub get_version_info($);


my %versions = % { get_version_info ($ARGV[0]) };

# map { print "$_=$versions{$_}\n" } (keys %versions);

while (<STDIN>) {
  s/ \$ \{ ([^}]+) \} /$versions{$1}/exg;
  print;
}

sub get_version_info($) {
    my $srcDir = shift;
    my $info = `git log -n 1 --pretty="format:fullCommit=%H%ndate=%ai%nsubject=%s%n" -- $srcDir`;
#    return $info;
    my @matches = $info =~ m/([^=]+)=([^\n]+)\n/gs;
    # create a hash from array of key, value element pairs
    return {@matches};
}

