#!/usr/bin/perl -w

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
