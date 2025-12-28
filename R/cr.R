#' Composite Reliability (CR) + loadings (reported vs computed)
cr <- function(data, varname, spec2 = spec2(),
               which = c("both","reported","computed"),
               loading_method = c("factanal_1factor"),
               na_action = c("listwise","pairwise"),
               check_items = TRUE) {
  which <- match.arg(which)
  loading_method <- match.arg(loading_method)
  na_action <- match.arg(na_action)

  call <- list(varname=varname, which=which, loading_method=loading_method, na_action=na_action, check_items=check_items)

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
      match$exact_items_ok <- NA
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
      reported <- list(
        CR = r$CR,
        loadings = r$loadings,
        items = spec2$schema$items_by_construct[[key]],
        source=list(table="table2", fields=c("CR","loadings"))
      )
      matched <- TRUE
    }
  }

  computed <- NULL
  diag <- list(n_used=NA_integer_, na_action=na_action, warnings=character(), errors=character())
  if (which != "reported") {
    if (length(items) >= 2) {
      X <- data[, items, drop=FALSE]
      X <- as.data.frame(lapply(X, function(z) .as_numeric(z)))
      try({
        lf <- .onefactor_loadings(X)
        crv <- .CR_from_loadings(lf$loadings[items], lf$uniqueness[items])
        computed <- list(
          CR = crv$CR,
          AVE = crv$AVE,
          loadings = as.numeric(lf$loadings[items]),
          uniqueness = as.numeric(lf$uniqueness[items]),
          items = items,
          n_used = lf$n_used,
          method = list(loading_method=loading_method, cr_formula="(sum(lambda))^2 / ((sum(lambda))^2 + sum(theta))")
        )
        names(computed$loadings) <- items
        names(computed$uniqueness) <- items
        diag$n_used <- lf$n_used
      }, silent=TRUE)
      if (is.null(computed)) {
        diag$errors <- c(diag$errors, "Failed to compute CR/loadings (factor analysis failed or insufficient data).")
      }
    } else {
      diag$errors <- c(diag$errors, "Too few matched item columns to compute CR.")
    }
  }

  if (which == "reported") computed <- NULL
  if (which == "computed") { reported <- NULL; matched <- FALSE }

  diff <- NULL
  if (!is.null(reported) && !is.null(computed)) {
    # align by position
    rl <- reported$loadings
    cl <- unname(computed$loadings)
    diff <- list(
      CR_abs = abs(reported$CR - computed$CR),
      loadings_abs_max = max(abs(rl - cl), na.rm=TRUE)
    )
  }

  .psychh_result("cr", spec2, key = key, matched = matched, match = match, call = call,
                 reported = reported, computed = computed, diff = diff, diag = diag)
}
