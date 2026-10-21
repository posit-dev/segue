# code is written in SAS order with defaults

    Code
      sas_proc("mixed", data = "mtcars", lsmeans = "cyl / diff cl", by = "am", class = "cyl",
        options = c(method = "reml"))
    Output
      proc mixed data=mtcars method=reml;
        class cyl;
        model &outcome = &trt;
        lsmeans cyl / diff cl;
        by am;
      run;
    Code
      sas_proc("logistic", data = "mtcars", model = "am(event='1') = wt hp / clodds=pl",
        estimate = "'wt/hp' wt 1 hp -1 / exp", estimate = "'wt' wt 1")
    Output
      proc logistic data=mtcars;
        model am(event='1') = wt hp / clodds=pl;
        estimate 'wt/hp' wt 1 hp -1 / exp;
        estimate 'wt' wt 1;
      run;
    Code
      sas_proc("means", data = "mtcars", class = c("cyl", "am"), var = "mpg",
      options = c("n", alpha = 0.05))
    Output
      proc means data=mtcars n alpha=0.05;
        class cyl am;
        var mpg;
      run;
    Code
      sas_proc("REG")
    Output
      proc reg data=&dsn;
        model &outcome = &x;
      quit;
    Code
      sas_proc("sgplot", data = NULL)
    Output
      proc sgplot;
      run;

# printing uses syntax colors

    Code
      sas_proc("logistic", model = "am(event='1') = wt / alpha=0.05")
    Output
      [1m[34mproc[39m[22m [1m[34mlogistic[39m[22m [34mdata[39m=[35m&dsn[39m;
        [34mmodel[39m am(event=[32m'1'[39m) = wt [90m/[39m alpha=[33m0.05[39m;
      [1m[34mrun[39m[22m;

# bad input gives informative errors

    Code
      sas_proc("freq", "cyl*am")
    Condition
      Error in `sas_proc()`:
      ! All statements in `...` must be named.
      i For example: `lsmeans = "&trt / diff"`.
    Code
      sas_proc("freq", tables = 1)
    Condition
      Error in `sas_proc()`:
      ! The `tables` statement must be a character vector, not the number 1.
    Code
      sas_proc(c("freq", "means"))
    Condition
      Error in `sas_proc()`:
      ! `proc` must be a single string, not a character vector.
    Code
      sas_proc("freq", options = list("chisq"))
    Condition
      Error in `sas_proc()`:
      ! `options` must be a character or numeric vector, not a list.

