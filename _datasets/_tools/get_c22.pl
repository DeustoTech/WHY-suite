#!/usr/bin/perl

for (my $ii = 1; $ii <= 30000; $ii = $ii + 500) {
   print "$ii\n";
   system("Rscript", "get_c22_features.R", $ii)
}