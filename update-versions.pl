#!/usr/bin/perl

# Check the SVN versions of all the files given as args, and replace a token
# <versions>...</versions> with a summary of the versions. Actually the output
# of svnversion for each source directory (inferred from the argument files)

use warnings;

use English;

use File::Basename;

sub get_versions(@);
sub exp_chomp ($);

my $versions = get_versions (@ARGV);

# print $versions, "\n";


$INPUT_RECORD_SEPARATOR = undef;

$ARG = <STDIN>;
s+(<versions>\s*).*(\s*</versions>)+$1$versions\n$2+s;

print $ARG;

sub get_versions(@) {
  # get the unique dirs
  my @dirs = sort (map { dirname $_ } @_);

  my @uniq_dirs;
  my $last = "";
  foreach my $d (@dirs) {
    if ($d ne $last) {
      push @uniq_dirs, $d;
    }
    $last = $d;
  }

#  print ("unique dirs: ", (join ':', @uniq_dirs), "\n");

  my @versions = map { "$_/: " . exp_chomp(`svnversion $_`) . ';' } @uniq_dirs;

  return (join "\n", @versions);
}


# chomp a value and return the chomped value.
sub exp_chomp ($) {
  my $val = shift;

  chomp $val;

  return $val;
}
