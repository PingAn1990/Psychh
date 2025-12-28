#' Cronbach's Alpha (reported vs computed)
alpha <- function(data, varname, spec2 = spec2(),
                  which = c("both","reported","computed"),
                  na_action = c("listwise","pairwise"),
                  check_items = TRUE) {
  which <- match.arg(which)
  na_action <- match.arg(na_action)

  call <- list(varname=varname, which=which, na_action=na_action, check_items=check_items)

  # resolve construct key and items
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

  # reported
  reported <- NULL
  matched <- FALSE
  if (!is.na(key)) {
    r <- spec2$reported$table2[[key]]
    if (!is.null(r)) {
      reported <- list(CA = r$CA, items = spec2$schema$items_by_construct[[key]],
                       source=list(table="table2", field="CA"))
      matched <- TRUE
    }
  }

  # computed
  computed <- NULL
  diag <- list(n_used=NA_integer_, na_action=na_action, warnings=character(), errors=character())
  if (which != "reported") {
    if (length(items) >= 2) {
      X <- data[, items, drop=FALSE]
      X <- as.data.frame(lapply(X, function(z) .as_numeric(z)))
      try({
        res <- .cronbach_alpha(X)
        computed <- list(CA = res$alpha, items = items, n_used = res$n_used, method="cronbach_alpha")
        diag$n_used <- res$n_used
      }, silent=TRUE)
      if (is.null(computed)) {
        diag$errors <- c(diag$errors, "Failed to compute alpha (insufficient complete cases or non-numeric items).")
      }
    } else {
      diag$errors <- c(diag$errors, "Too few matched item columns to compute alpha.")
    }
  }

  if (which == "reported") computed <- NULL
  if (which == "computed") { reported <- NULL; matched <- FALSE }

  diff <- NULL
  if (!is.null(reported) && !is.null(computed)) {
    diff <- list(CA_abs = abs(reported$CA - computed$CA), CA_signed = computed$CA - reported$CA)
  }

  .psychh_result("alpha", spec2, key = key, matched = matched, match = match, call = call,
                 reported = reported, computed = computed, diff = diff, diag = diag)
}
