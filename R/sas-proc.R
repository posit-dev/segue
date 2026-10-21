#' Write SAS code for a procedure
#'
#' @description
#' `sas_proc()` writes the SAS code for one procedure (proc) as text. Each
#' named argument in `...` is one statement: the name is the SAS keyword, and
#' the value is the rest of the line, written as you would write it in SAS.
#'
#' Statements are printed in a fixed order: `class` first, then the main
#' statement (such as `tables`, `var`, `model`, or `time`), then other
#' statements in the order you give them, then `by`, `where`, `weight`,
#' `freq`, and `id`, and `output` last. The code ends with `quit;` for
#' interactive procs (`reg`, `glm`, `anova`, `sql`) and with `run;` for all
#' other procs.
#'
#' @section Placeholders:
#' Placeholders use SAS macro variables, such as `&dsn` or `&outcome`. This
#' means that the code is valid SAS. Set the values with `%let` statements
#' before you run it:
#'
#' ```sas
#' %let dsn = adsl;
#' %let row = trt01p;
#' %let col = sex;
#' ```
#'
#' @section Default statements:
#' If you leave out a statement that the proc needs, `sas_proc()` adds it with
#' placeholders:
#'
#' | Proc                   | Default statements                         |
#' |------------------------|--------------------------------------------|
#' | `freq`                 | `tables &row*&col`                         |
#' | `means`, `univariate`  | `var &var`                                 |
#' | `ttest`                | `class &group`, `var &var`                 |
#' | `reg`                  | `model &outcome = &x`                      |
#' | `glm`                  | `class &group`, `model &outcome = &group`  |
#' | `logistic`             | `model &outcome(event='1') = &x`           |
#' | `mixed`                | `class &trt &subj`, `model &outcome = &trt`|
#' | `lifetest`             | `time &time*&censor(0)`                    |
#' | `phreg`                | `model &time*&censor(0) = &trt`            |
#'
#' To leave out a default statement, set it to `NULL`, for example
#' `class = NULL`.
#'
#' @param proc A single string with the name of the proc, such as `"freq"`
#'   or `"mixed"`.
#' @param ... Named statements. Each value is a string, or a character vector
#'   that is joined with spaces. Use a slash for statement options, as in SAS,
#'   for example `model = "&y = &x / solution"`. Use the same name more than
#'   once to repeat a statement. A `NULL` or empty value leaves the statement
#'   out.
#' @param data A single string with the data set name, or `NULL` to leave out
#'   `data=`.
#' @param options Options for the `proc` line. Unnamed values are written as
#'   they are, and named values are written as `name=value`. For example,
#'   `c("n", "mean", alpha = 0.05)` gives `n mean alpha=0.05`.
#'
#' @returns An object of class `sas_proc`. Printing it shows the code with
#'   syntax colors. Use `as.character()` to get the code as plain text, for
#'   example to write it to a `.sas` file.
#'
#' @examples
#' # Default statements only
#' sas_proc("freq")
#'
#' sas_proc(
#'   "freq",
#'   tables = "&row*&col / chisq exact",
#'   weight = "&count"
#' )
#'
#' sas_proc(
#'   "means",
#'   var = "&outcome",
#'   class = "&trt",
#'   options = c("n", "mean", "std", "clm", alpha = 0.05)
#' )
#'
#' sas_proc(
#'   "mixed",
#'   class    = c("&trt", "&visit", "&subj"),
#'   model    = "&outcome = &trt &visit &trt*&visit / solution ddfm=kr",
#'   repeated = "&visit / subject=&subj type=un",
#'   lsmeans  = "&trt*&visit / diff cl",
#'   options  = c(method = "reml")
#' )
#'
#' # Plain text
#' cat(as.character(sas_proc("reg", model = "&outcome = &x1 &x2 / clb")))
#' @export
sas_proc <- function(proc, ..., data = "&dsn", options = NULL) {
  check_string(proc)
  check_string(data, allow_null = TRUE)
  if (!is.null(options) && (!is.atomic(options) || is.logical(options))) {
    cli::cli_abort(
      "{.arg options} must be a character or numeric vector, not
       {obj_type_friendly(options)}."
    )
  }

  proc_name <- tolower(proc)
  user_stmts <- list(...)
  keywords <- names2(user_stmts)
  if (any(keywords == "")) {
    cli::cli_abort(
      c(
        "All statements in {.arg ...} must be named.",
        "i" = "For example: {.code model = \"&y = &x\"}."
      )
    )
  }
  bad <- purrr::detect_index(
    user_stmts,
    function(x) !is.null(x) && !is.character(x)
  )
  if (bad > 0) {
    cli::cli_abort(
      "The {.arg {keywords[bad]}} statement must be a character vector, not
       {obj_type_friendly(user_stmts[[bad]])}."
    )
  }

  # Add the default statements that the user left out.
  defaults <- sas_proc_defaults[[proc_name]]
  unset_defaults <- defaults[!names(defaults) %in% tolower(keywords)]
  stmts <- purrr::compact(c(unset_defaults, user_stmts))

  # Named options become name=value. Unnamed options are written as they are.
  proc_options <- paste(
    sub("^=", "", paste0(names2(options), "=", options)),
    collapse = " "
  )

  proc_stmts <- purrr::map2(names(stmts), stmts, sas_stmt)

  structure(
    list(
      proc = proc_name,
      data = data,
      options = proc_options,
      statements = proc_stmts
    ),
    class = "sas_proc"
  )
}

#' @export
format.sas_proc <- function(x, indent = "  ", color = FALSE, ...) {
  data <- ""
  if (!is.null(x$data) && nzchar(x$data)) {
    data <- glue::glue(
      " {sas_paint('data', 'keyword', color)}={sas_paint_text(x$data, color)}"
    )
  }
  options <- ""
  if (nzchar(x$options)) {
    options <- paste0(" ", sas_paint_text(x$options, color))
  }
  head <- glue::glue(
    "{sas_paint('proc', 'proc', color)} ",
    "{sas_paint(x$proc, 'proc', color)}{data}{options};"
  )

  # order() keeps the given order when ranks are equal.
  rank <- purrr::map_dbl(
    x$statements,
    function(stmt) sas_stmt_order[[stmt$keyword]] %||% 3
  )
  body <- purrr::map_chr(x$statements[order(rank)], sas_format_stmt, color)
  if (length(body) > 0) {
    body <- paste0(indent, body)
  }

  end_word <- "run"
  if (x$proc %in% sas_quit_procs) {
    end_word <- "quit"
  }
  end <- glue::glue("{sas_paint(end_word, 'proc', color)};")

  paste(c(head, body, end), collapse = "\n")
}

#' @export
print.sas_proc <- function(x, ...) {
  cli::cat_line(format(x, color = TRUE, ...))
  invisible(x)
}

#' @export
as.character.sas_proc <- function(x, ...) {
  format(x, color = FALSE, ...)
}

# ---- Proc settings -----------------------------------------------------------

# Procs that end with quit instead of run.
sas_quit_procs <- c("reg", "glm", "anova", "sql")

# Order of statements inside a proc. Statements that are not in this list
# get rank 3, so they go after the main statements, in the order given.
sas_stmt_order <- list(
  class = 1,
  tables = 2,
  var = 2,
  model = 2,
  time = 2,
  paired = 2,
  by = 4,
  where = 4,
  weight = 4,
  freq = 4,
  id = 4,
  output = 5
)

# Statements each proc needs. `sas_proc()` adds them when they are left out.
# Keep this list the same as the table in the `sas_proc()` docs.
sas_proc_defaults <- list(
  freq = list(tables = "&row*&col"),
  means = list(var = "&var"),
  univariate = list(var = "&var"),
  ttest = list(class = "&group", var = "&var"),
  reg = list(model = "&outcome = &x"),
  glm = list(class = "&group", model = "&outcome = &group"),
  logistic = list(model = "&outcome(event='1') = &x"),
  mixed = list(class = "&trt &subj", model = "&outcome = &trt"),
  lifetest = list(time = "&time*&censor(0)"),
  phreg = list(model = "&time*&censor(0) = &trt")
)

# ---- Statements --------------------------------------------------------------

# Make one statement from a keyword and its text. Text after the first slash
# that is not inside quotes becomes the options.
sas_stmt <- function(keyword, text) {
  text <- trimws(paste(text, collapse = " "))
  parts <- regmatches(
    text,
    regexec("^((?:[^'\"/]|'[^']*'|\"[^\"]*\")*)/(.*)$", text, perl = TRUE)
  )[[1]]
  if (length(parts) == 0) {
    args <- text
    options <- ""
  } else {
    args <- trimws(parts[2])
    options <- trimws(parts[3])
  }
  list(keyword = tolower(keyword), args = args, options = options)
}

sas_format_stmt <- function(stmt, color = FALSE) {
  args <- ""
  if (nzchar(stmt$args)) {
    args <- paste0(" ", sas_paint_text(stmt$args, color))
  }
  options <- ""
  if (nzchar(stmt$options)) {
    options <- glue::glue(
      " {sas_paint('/', 'slash', color)} {sas_paint_text(stmt$options, color)}"
    )
  }
  as.character(
    glue::glue("{sas_paint(stmt$keyword, 'keyword', color)}{args}{options};")
  )
}

# ---- Syntax colors -----------------------------------------------------------

sas_colors <- list(
  proc = function(x) cli::style_bold(cli::col_blue(x)),
  keyword = cli::col_blue,
  macro = cli::col_magenta,
  string = cli::col_green,
  number = cli::col_yellow,
  slash = cli::col_grey
)

sas_paint <- function(x, type, color) {
  if (color) as.character(sas_colors[[type]](x)) else x
}

# Color quoted strings, macro variables, and numbers in free text. All three
# are matched in one pass, so the color codes added for one token are never
# matched again.
sas_paint_text <- function(x, color) {
  if (!color || !nzchar(x)) {
    return(x)
  }
  pattern <- paste(
    "'[^']*'|\"[^\"]*\"", # quoted strings
    "&[A-Za-z_][A-Za-z0-9_]*\\.?", # macro variables
    "\\b[0-9]+(\\.[0-9]+)?\\b", # numbers
    sep = "|"
  )
  m <- gregexpr(pattern, x, perl = TRUE)
  tokens <- regmatches(x, m)[[1]]
  if (length(tokens) == 0) {
    return(x)
  }
  type <- ifelse(
    grepl("^['\"]", tokens),
    "string",
    ifelse(startsWith(tokens, "&"), "macro", "number")
  )
  regmatches(x, m) <- list(purrr::map2_chr(tokens, type, sas_paint, color))
  x
}
