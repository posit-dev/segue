# printing uses syntax colors

    Code
      sas_proc("logistic", model = "&y(event='1') = age / alpha=0.05")
    Output
      [1m[34mproc[39m[22m [1m[34mlogistic[39m[22m [34mdata[39m=[35m&dsn[39m;
        [34mmodel[39m [35m&y[39m(event=[32m'1'[39m) = age [90m/[39m alpha=[33m0.05[39m;
      [1m[34mrun[39m[22m;

# bad input gives informative errors

    Code
      sas_proc("freq", "&a*&b")
    Condition
      Error in `sas_proc()`:
      ! All statements in `...` must be named.
      i For example: `model = "&y = &x"`.
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

