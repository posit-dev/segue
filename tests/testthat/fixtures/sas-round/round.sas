/*
  Computes the SAS ROUND() function for the cases in cases.csv so that the
  results can be compared to segue::sas_round().

  1. Set `dir` below to the directory containing this file.
  2. Run this program. It writes sas-results.csv to the same directory.
  3. Run the segue tests. The test "sas_round() matches SAS output" in
     test-sas_round.R compares the results bit-for-bit and is skipped when
     sas-results.csv does not exist.

  Numbers are read and written with the HEX16. informat/format so that SAS and
  R use the exact same doubles. If every case fails in R, check the byte order
  of the HEX16. values on your system.
*/

%let dir = /path/to/segue/tests/testthat/fixtures/sas-round;

data cases;
  infile "&dir/cases.csv" dsd firstobs=2 truncover;
  length source $20 x_hex unit_hex $16;
  input id source $ x_hex $ unit_hex $;
  x = input(x_hex, hex16.);
  unit = input(unit_hex, hex16.);
run;

data results;
  set cases;
  length result_hex $16 sas_version $40;
  result = round(x, unit);
  result_hex = put(result, hex16.);
  sas_version = "&sysvlong";
  format result best32.;
  keep id result_hex result sas_version;
run;

proc export data=results outfile="&dir/sas-results.csv" dbms=csv replace;
run;
