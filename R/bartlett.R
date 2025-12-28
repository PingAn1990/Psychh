#' Bartlett's test (reported vs computed)
bartlett <- function(data, varname, spec2 = spec2(),
                     which = c("both","reported","computed"),
                     use = c("complete.obs","pairwise.complete.obs"),
                     check_items = TRUE) {
  which <- match.arg(which)
  use <- match.arg(use)
  call <- list(varname=varname, which=which, use=use, check_items=check_items)

  key <- NA_character_
  items <- character(0)
  match <- list(type="construct", input=varname, normalized=NA, required_items=character(0),
                observed_items=character(0), exact_items_ok=NA, reason=NA_character_)

  if (is.character(varname) && length(varname)==1) {
    key <- .norm_construct(varname, spec2)
    match$normalized <- key
    match$required_items <- spec2$schema$items_by_construct[[key]] %||% character(0)
    items <- .get_items(data, key, spec2)
    match$observed_items <- items
    if (check_items && spec2$match$require_exact_items) {
      match$exact_items_ok <- setequal(match$required_items, items)
      if (!isTRUE(match$exact_items_ok)) {
        match$reason <- "items mismatch"
        key <- NA_character_
      } else {
        match$reason <- "matched by construct key"
      }
    } else {
      match$reason <- "matched by construct key (non-strict)"
    }
  } else if (is.character(varname) && length(varname) > 1) {
    items <- intersect(varname, colnames(data))
    match$required_items <- items
    match$observed_items <- items
    match$reason <- "matched by explicit item set"
  } else {
    match$reason <- "unsupported varname type"
  }

  reported <- NULL
  matched <- FALSE
  if (!is.na(key)) {
    r <- spec2$reported$table2[[key]]
    if (!is.null(r)) {
      reported <- list(chisq = r$bartlett$chisq, df = r$bartlett$df, Sig = r$bartlett$sig,
                       items = spec2$schema$items_by_construct[[key]],
                       source=list(table="table2", fields=c("chisq","df","sig")))
      matched <- TRUE
    }
  }

  computed <- NULL
  diag <- list(n_used=NA_integer_, na_action=use, warnings=character(), errors=character())
  if (which != "reported") {
    if (length(items) >= 2) {
      X <- data[, items, drop=FALSE]
      X <- as.data.frame(lapply(X, function(z) .as_numeric(z)))
      try({
        if (use == "complete.obs") {
          Xc <- X[stats::complete.cases(X), , drop=FALSE]
          n_used <- nrow(Xc)
          R <- stats::cor(Xc)
        } else {
          n_used <- nrow(X)  # approximate
          R <- stats::cor(X, use="pairwise.complete.obs")
          diag$warnings <- c(diag$warnings, "pairwise.complete.obs: n_used is approximate; Bartlett test may be unstable under missingness.")
        }
        bt <- .bartlett_from_cor(R, n_used)
        computed <- list(chisq = bt$chisq, df = bt$df, p = bt$p, detR = bt$det, items=items, n_used=n_used,
                         method=list(use=use))
        diag$n_used <- n_used
      }, silent=TRUE)
      if (is.null(computed)) diag$errors <- c(diag$errors, "Failed to compute Bartlett test (possibly singular correlation matrix).")
    } else {
      diag$errors <- c(diag$errors, "Too few matched item columns to compute Bartlett test.")
    }
  }

  if (which == "reported") computed <- NULL
  if (which == "computed") { reported <- NULL; matched <- FALSE }

  diff <- NULL
  if (!is.null(reported) && !is.null(computed)) {
    diff <- list(chisq_abs = abs(reported$chisq - computed$chisq), df_match = (reported$df == computed$df))
  }

  .psychh_result("bartlett", spec2, key = key, matched = matched, match = match, call = call,
                 reported = reported, computed = computed, diff = diff, diag = diag)
}
