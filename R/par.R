#' Discriminant Validity Table (Table 3): reported vs computed
#' NOTE: function name 'par' matches the user's required interface (Psychh::par).
par <- function(data, spec2 = spec2(),
                which = c("both","reported","computed"),
                score_method = c("mean"),
                cor_method = c("pearson"),
                ave_source = c("reported","computed"),
                use = c("complete.obs","pairwise.complete.obs")) {
  which <- match.arg(which)
  score_method <- match.arg(score_method)
  cor_method <- match.arg(cor_method)
  ave_source <- match.arg(ave_source)
  use <- match.arg(use)

  call <- list(which=which, score_method=score_method, cor_method=cor_method, ave_source=ave_source, use=use)
  order <- spec2$reported$table3$order

  # reported
  reported <- NULL
  if (which != "computed") {
    raw <- spec2$reported$table3$matrix_raw
    # sanitize into numeric
    sanitize_map <- spec2$reported$table3$sanitize
    raw2 <- raw
    for (from in names(sanitize_map)) {
      raw2[raw2 == from] <- sanitize_map[[from]]
    }
    num <- suppressWarnings(matrix(as.numeric(raw2), nrow=nrow(raw2), ncol=ncol(raw2), dimnames=dimnames(raw2)))
    rownames(num) <- rownames(raw2)
    colnames(num) <- colnames(raw2)
    reported <- list(
      table3_raw = raw,
      table3_num = num,
      order = order,
      note = spec2$reported$table3$note,
      source = list(table="table3")
    )
  }

  # computed
  computed <- NULL
  diag <- list(n_used=NA_integer_, na_action=use, warnings=character(), errors=character())
  if (which != "reported") {
    constructs <- order
    # construct scores
    try({
      sc <- .score_constructs(data, constructs, spec2, na_rm = TRUE)
      # remove row_id
      sc2 <- sc[, constructs, drop=FALSE]
      if (use == "complete.obs") {
        sc2c <- sc2[stats::complete.cases(sc2), , drop=FALSE]
        n_used <- nrow(sc2c)
        C <- stats::cor(sc2c, method=cor_method)
      } else {
        n_used <- nrow(sc2) # approximate
        C <- stats::cor(sc2, use="pairwise.complete.obs", method=cor_method)
        diag$warnings <- c(diag$warnings, "pairwise.complete.obs: n_used is approximate for computed Table3.")
      }

      # AVE/sqrt(AVE)
      sqrt_ave <- setNames(rep(NA_real_, length(constructs)), constructs)
      ave <- setNames(rep(NA_real_, length(constructs)), constructs)

      for (k in constructs) {
        items <- spec2$schema$items_by_construct[[k]]
        if (ave_source == "reported") {
          lam <- spec2$reported$table2[[k]]$loadings
          ave[k] <- mean(lam^2)
          sqrt_ave[k] <- sqrt(ave[k])
        } else {
          X <- data[, items, drop=FALSE]
          X <- as.data.frame(lapply(X, function(z) .as_numeric(z)))
          lf <- .onefactor_loadings(X)
          crv <- .CR_from_loadings(lf$loadings[items], lf$uniqueness[items])
          ave[k] <- crv$AVE
          sqrt_ave[k] <- sqrt(ave[k])
        }
      }

      # build table3 numeric matrix: diagonal sqrt_ave, lower triangle correlations, upper triangle NA
      M <- matrix(NA_real_, nrow=length(constructs), ncol=length(constructs),
                  dimnames=list(constructs, constructs))
      for (i in seq_along(constructs)) {
        M[i,i] <- sqrt_ave[i]
        for (j in seq_len(i-1)) {
          M[i,j] <- C[i,j]
        }
      }

      computed <- list(
        cor = C,
        ave = ave,
        sqrt_ave = sqrt_ave,
        table3 = M,
        n_used = n_used,
        score_method = score_method,
        ave_source = ave_source
      )
      diag$n_used <- n_used
    }, silent=TRUE)

    if (is.null(computed)) diag$errors <- c(diag$errors, "Failed to compute Table3 (missing item columns or non-numeric data).")
  }

  if (which == "reported") computed <- NULL
  if (which == "computed") reported <- NULL

  diff <- NULL
  if (!is.null(reported) && !is.null(computed)) {
    # compare on numeric lower triangle + diagonal
    Rnum <- reported$table3_num
    Cnum <- computed$table3
    # align dims
    common <- intersect(rownames(Rnum), rownames(Cnum))
    Rnum <- Rnum[common, common, drop=FALSE]
    Cnum <- Cnum[common, common, drop=FALSE]
    diff <- list(
      max_abs = max(abs(Rnum - Cnum), na.rm=TRUE)
    )
  }

  .psychh_result("par", spec2, key = NA_character_, matched = !is.null(reported),
                 match = list(type="table", reason="Table3 is keyed by spec2 (no varname)."),
                 call = call, reported = reported, computed = computed, diff = diff, diag = diag)
}
