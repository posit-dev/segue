/*
  Exact binomial test of a proportion for every events/total combination,
  null value, and alternative, using PROC FREQ for every test.
*/

%let resdir = /home/topepo/r-to-sas-comparisons/results;

libname res "&resdir";

/* ---------------------------------------------------------------------------
   Test cases: events 1:50, totals 2:50, events <= totals
--------------------------------------------------------------------------- */

data cases;
  do events = 1 to 50;
    do totals = 2 to 50;
      if events <= totals then do;
        id + 1;
        output;
      end;
    end;
  end;
run;

/* Full data set: one row per observation */
data long;
  set cases;
  length resp $1;
  do i = 1 to totals;
    if i <= events then resp = "a";
    else resp = "b";
    output;
  end;
  keep id resp;
run;

/* ---------------------------------------------------------------------------
   One PROC FREQ run per (null value, alternative) condition
--------------------------------------------------------------------------- */

%macro run_freq(p, alt, pvar);
  proc freq data=long noprint;
    by id;
    tables resp / binomial(p=&p level="a");
    exact binomial;
    output out=_one binomial;
  run;

  data _one;
    length alternative $9;
    set _one;
    alternative = "&alt";
    p           = &p;
    p_value     = &pvar;
    estimate    = _bin_;
    keep id estimate p_value alternative p;
  run;

  proc append base=freq_all data=_one force;
  run;
%mend run_freq;

proc datasets library=work nolist;
  delete freq_all;
quit;

%macro run_all;
  %local i j;
  %let ps   = 0.01 0.25 0.50;
  %let alts = two.sided less greater;
  %let vars = xp2_bin xpl_bin xpr_bin;
  %do i = 1 %to 3;
    %do j = 1 %to 3;
      %run_freq(%scan(&ps, &i, %str( )),
                %scan(&alts, &j, %str( )),
                %scan(&vars, &j, %str( )));
    %end;
  %end;
%mend run_all;

%run_all;

/* ---------------------------------------------------------------------------
   Attach the case information
--------------------------------------------------------------------------- */

proc sql;
  create table results as
  select f.estimate,
         c.events as statistic,
         f.p_value,
         f.alternative,
         c.events as num_events,
         c.totals as num_total,
         f.p
  from freq_all as f
  inner join cases as c on f.id = c.id
  order by f.p, f.alternative, c.totals, c.events;
quit;

/* ---------------------------------------------------------------------------
   Save as a SAS data set (results/bin_prop_test_sas.sas7bdat)

   Numeric variables are stored as 8-byte doubles, so the values are kept
   bit-for-bit, and R can read them with haven::read_sas() without parsing
   any decimal text. A CSV is not used because best32. truncates to 14
   decimal places (or 14 significant digits in E notation).
--------------------------------------------------------------------------- */

data res.bin_prop_test_sas;
  set results;
run;
