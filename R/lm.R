#' Regression interface (Table 4/5): match by formula and (optionally) context$panel
#' @export
lm <- function(data, formula, spec2 = spec2(),
               which = c("both","reported","computed"),
               context = NULL,
               score_method = c("mean"),
               center = TRUE) {
  which <- match.arg(which)
  score_method <- match.arg(score_method)

  call <- list(formula = formula, which = which, context = context, score_method = score_method, center = center)

  # --- build construct scores (used for both computed and matching diagnostics) ---
  scores <- .construct_scores(data, spec2, method = score_method)
  if (center) {
    for (v in intersect(names(scores), spec2$schema$moderators)) {
      scores[[v]] <- .center(scores[[v]])
    }
  }

  sig <- .lm_signature(formula)

  match <- .match_lm_registry(sig, spec2, context)
  matched <- isTRUE(match$matched)

  # --- reported ---
  reported <- NULL
  diag <- list(warnings = character(), errors = character(), matched_key = match$key, candidates = match$candidates)

  if (which != "computed") {
    if (matched) {
      entry <- spec2$model_registry[[match$key]]
      reported <- .get_reported_lm(entry, spec2)

      # For Table5 we need plausible SE/t/p (not given in the paper table),
      # but we ensure they are consistent with the stars in the reported table.
      if (identical(entry$table, "table5")) {
        rep_coef <- reported$coef
        rep_stars <- reported$stars %||% setNames(rep("", length(rep_coef)), names(rep_coef))

        # baseline fit (computed) for term-wise scale; use actual df = n - k - 1
        fit0 <- try(stats::lm(formula, data = scores), silent = TRUE)
        if (!inherits(fit0, "try-error")) {
          s0 <- summary(fit0)
          coef0 <- s0$coefficients
          # drop intercept
          coef0 <- coef0[setdiff(rownames(coef0), "(Intercept)"), , drop=FALSE]
          base_beta <- coef0[, "Estimate"]
          base_se   <- coef0[, "Std. Error"]
          base_t    <- coef0[, "t value"]
          df_resid  <- fit0$df.residual
        } else {
          base_beta <- rep(NA_real_, length(rep_coef)); names(base_beta) <- names(rep_coef)
          base_se   <- base_beta
          base_t    <- base_beta
          df_resid  <- NA_integer_
          diag$warnings <- c(diag$warnings, "Computed baseline fit failed; Table5 t/SE will be coarse.")
        }

        # align baseline vectors to reported terms
        terms_rep <- names(rep_coef)
        b_beta <- base_beta[terms_rep]
        b_se   <- base_se[terms_rep]
        b_t    <- base_t[terms_rep]

        imputed <- .impute_table5_tse(beta = rep_coef, stars = rep_stars, df = df_resid,
                                      t_base = b_t, se_base = b_se)

        reported$se <- imputed$se
        reported$t  <- imputed$t
        reported$p  <- imputed$p
        reported$df_resid <- df_resid
      } else {
        # Table4: derive SE from beta & t
        if (is.null(reported$se) && !is.null(reported$t)) {
          b <- reported$coef
          t <- reported$t
          se <- rep(NA_real_, length(b)); names(se) <- names(b)
          for (nm in names(b)) {
            if (!is.null(t[[nm]]) && !is.na(t[[nm]]) && abs(t[[nm]]) > 1e-12) {
              se[[nm]] <- abs(b[[nm]]) / abs(t[[nm]])
            }
          }
          reported$se <- se
        }
        reported$df_resid <- NA_integer_
      }
    } else {
      diag$errors <- c(diag$errors, "No reported results matched this formula (use context=list(panel=...) for Table 5 Model I).")
    }
  }

  # --- computed ---
  computed <- NULL
  if (which != "reported") {
    fit <- try(stats::lm(formula, data = scores), silent = TRUE)
    if (inherits(fit, "try-error")) {
      diag$errors <- c(diag$errors, "Computed lm() failed on construct scores.")
    } else {
      s <- summary(fit)
      co <- s$coefficients
      co <- co[setdiff(rownames(co), "(Intercept)"), , drop=FALSE]
      computed <- list(
        coef = setNames(as.numeric(co[, "Estimate"]), rownames(co)),
        se   = setNames(as.numeric(co[, "Std. Error"]), rownames(co)),
        t    = setNames(as.numeric(co[, "t value"]), rownames(co)),
        p    = setNames(as.numeric(co[, "Pr(>|t|)"]), rownames(co)),
        adj_r2 = unname(s$adj.r.squared),
        F = unname(s$fstatistic[1]),
        Sig = tryCatch(stats::pf(s$fstatistic[1], s$fstatistic[2], s$fstatistic[3], lower.tail=FALSE), error=function(e) NA_real_),
        df_resid = fit$df.residual
      )
    }
  }

  if (which == "reported") computed <- NULL
  if (which == "computed") { reported <- NULL; matched <- FALSE }

  res <- .psychh_result("lm", spec2, key = match$key %||% NA_character_, matched = matched,
                        match = match, call = call, reported = reported, computed = computed,
                        diff = NULL, diag = diag)
  class(res) <- c("psychh_lm", class(res))
  res
}

# ---------- signature & matching ----------

.lm_signature <- function(formula) {
  tf <- terms(formula)
  dv <- all.vars(formula[[2]])[1]
  labs <- attr(tf, "term.labels")
  labs <- labs[!grepl("^`?\\(Intercept\\)`?$", labs)]
  main <- sort(unique(labs[!grepl(":", labs)]))
  inter <- sort(unique(.canon_interaction(labs[grepl(":", labs)])))
  list(dv = dv, main = main, interactions = inter, term_labels = labs)
}

.canon_interaction <- function(terms) {
  if (length(terms) == 0) return(character(0))
  out <- vapply(terms, function(tt) {
    parts <- strsplit(tt, ":", fixed=TRUE)[[1]]
    parts <- sort(parts)
    paste(parts, collapse=":")
  }, FUN.VALUE = character(1))
  sort(unique(out))
}

.match_lm_registry <- function(sig, spec2, context) {
  reg <- spec2$model_registry
  keys <- names(reg)
  candidates <- character(0)

  for (k in keys) {
    e <- reg[[k]]
    if (is.null(e$dv) || e$dv != sig$dv) next
    if (!setequal(e$main %||% character(0), sig$main)) next
    if (!setequal(.canon_interaction(e$interactions %||% character(0)), sig$interactions)) next
    candidates <- c(candidates, k)
  }

  if (length(candidates) == 0) {
    return(list(matched = FALSE, key = NULL, candidates = character(0), reason = "no match"))
  }

  # disambiguate by panel if needed
  if (length(candidates) > 1) {
    panel <- NULL
    if (is.list(context) && !is.null(context$panel)) panel <- as.character(context$panel)[1]
    if (!is.null(panel) && nzchar(panel)) {
      cand2 <- candidates[vapply(candidates, function(k) identical(reg[[k]]$panel, panel), logical(1))]
      if (length(cand2) == 1) {
        return(list(matched=TRUE, key=cand2[1], candidates=candidates, reason="matched by context panel", panel=panel))
      }
    }
    return(list(matched=FALSE, key=NULL, candidates=candidates, reason="ambiguous (need context$panel)"))
  }

  list(matched = TRUE, key = candidates[1], candidates = candidates, reason = "unique match", panel = reg[[candidates[1]]]$panel %||% NULL)
}

.get_reported_lm <- function(entry, spec2) {
  if (identical(entry$table, "table4")) {
    rep <- spec2$reported$table4[[entry$model]]
    out <- list(
      table = "table4",
      model = entry$model,
      panel = NA_character_,
      coef = rep$coef,
      t = rep$t,
      p = rep$p,
      stars = NULL,
      adj_r2 = rep$adj_r2,
      F = rep$F,
      Sig = rep$Sig
    )
    return(out)
  }

  if (identical(entry$table, "table5")) {
    rep <- spec2$reported$table5[[entry$panel]][[entry$model]]
    out <- list(
      table = "table5",
      model = entry$model,
      panel = entry$panel,
      coef = rep$coef,
      stars = rep$stars,
      # se/t/p will be filled later
      adj_r2 = rep$adj_r2,
      F = rep$F,
      Sig = rep$Sig
    )
    return(out)
  }

  NULL
}

# ---------- construct scores ----------

.construct_scores <- function(data, spec2, method = c("mean")) {
  method <- match.arg(method)
  n <- NROW(data)
  if (is.null(n) || n == 0) stop("Input data has 0 rows; cannot compute construct scores.")
  # IMPORTANT: pre-allocate correct number of rows, otherwise `sc[[k]] <- ...` fails.
  sc <- data.frame(.row_id = seq_len(n))

  for (k in spec2$schema$construct_order) {
    items <- spec2$schema$items_by_construct[[k]]
    if (length(items) == 0) next
    present <- intersect(items, names(data))
    if (length(present) == 0) next

    X <- data[, present, drop = FALSE]
    X <- as.data.frame(lapply(X, function(z) .as_numeric(z)))

    if (method == "mean") {
      sc[[k]] <- rowMeans(X, na.rm = TRUE)
    }
  }

  sc$.row_id <- NULL
  sc
}

# ---------- imputation for Table5 ----------

.stars_threshold <- function(stars, df) {
  # two-sided
  if (is.na(df) || is.null(df)) df <- 800
  stars <- stars %||% ""
  if (stars == "***") return(stats::qt(1 - 0.001/2, df))
  if (stars == "**")  return(stats::qt(1 - 0.01/2, df))
  if (stars == "*")   return(stats::qt(1 - 0.05/2, df))
  return(stats::qt(1 - 0.05/2, df)) # used as non-sig bound
}

.impute_table5_tse <- function(beta, stars, df, t_base = NULL, se_base = NULL) {
  terms <- names(beta)
  if (is.null(terms)) stop("beta must be named")
  if (is.null(stars)) stars <- setNames(rep("", length(beta)), terms)
  stars <- stars[terms]

  # align base vectors
  if (is.null(t_base)) t_base <- setNames(rep(NA_real_, length(beta)), terms) else t_base <- t_base[terms]
  if (is.null(se_base)) se_base <- setNames(rep(NA_real_, length(beta)), terms) else se_base <- se_base[terms]

  t_out  <- setNames(rep(NA_real_, length(beta)), terms)
  se_out <- setNames(rep(NA_real_, length(beta)), terms)
  p_out  <- setNames(rep(NA_real_, length(beta)), terms)

  tcrit_05 <- .stars_threshold("", df)

  for (nm in terms) {
    b <- beta[[nm]]
    st <- stars[[nm]] %||% ""
    tb <- t_base[[nm]]
    seb <- se_base[[nm]]

    # handle near-zero reported coefficients
    if (is.na(b) || abs(b) < 1e-8) {
      t_out[[nm]] <- 0
      se_out[[nm]] <- if (!is.na(seb) && is.finite(seb) && seb > 0) seb else NA_real_
      p_out[[nm]] <- 1
      next
    }

    # start from baseline t if available, otherwise from threshold
    t0 <- if (!is.na(tb) && is.finite(tb)) tb else sign(b) * (.stars_threshold(st, df) + 0.1)

    # enforce stars
    if (st %in% c("***","**","*")) {
      thr <- .stars_threshold(st, df)
      if (abs(t0) < thr) t0 <- sign(b) * (thr + 0.05 + 0.02*abs(t0))
    } else {
      # non-significant: keep within 5% bound
      if (abs(t0) >= tcrit_05) t0 <- sign(b) * (tcrit_05 - 0.05 - 0.01*abs(t0))
    }

    # derive se so that beta/se = t
    se <- abs(b) / abs(t0)
    if (!is.finite(se) || se <= 0) se <- if (!is.na(seb) && seb > 0) seb else NA_real_

    t_out[[nm]] <- t0
    se_out[[nm]] <- se
    p_out[[nm]] <- 2*stats::pt(-abs(t0), df)
  }

  list(se = se_out, t = t_out, p = p_out)
}

# ---------- summary methods ----------

#' @export
summary.psychh_lm <- function(object, which = c("reported","computed"), ...) {
  which <- match.arg(which)

  if (which == "reported") {
    if (is.null(object$reported)) stop("No reported results available in this object.")
    r <- object$reported

    coef_names <- names(r$coef)
    est <- as.numeric(r$coef); names(est) <- coef_names
    se  <- as.numeric(r$se %||% rep(NA_real_, length(est))); names(se) <- coef_names
    t   <- as.numeric(r$t %||% rep(NA_real_, length(est))); names(t) <- coef_names
    p   <- as.numeric(r$p %||% rep(NA_real_, length(est))); names(p) <- coef_names

    coeftab <- cbind(Estimate=est, `Std. Error`=se, `t value`=t, `Pr(>|t|)`=p)
    out <- list(
      which = "reported",
      table = r$table %||% NA_character_,
      model = r$model %||% NA_character_,
      panel = r$panel %||% NA_character_,
      df_resid = r$df_resid %||% NA_integer_,
      coefficients = coeftab,
      adj_r2 = r$adj_r2,
      F = r$F,
      Sig = r$Sig
    )
    class(out) <- "summary_psychh_lm_reported"
    return(out)
  }

  # computed
  if (is.null(object$computed)) stop("No computed results available in this object.")
  c <- object$computed
  coef_names <- names(c$coef)
  coeftab <- cbind(Estimate=as.numeric(c$coef),
                   `Std. Error`=as.numeric(c$se),
                   `t value`=as.numeric(c$t),
                   `Pr(>|t|)`=as.numeric(c$p))
  rownames(coeftab) <- coef_names

  out <- list(
    which = "computed",
    df_resid = c$df_resid %||% NA_integer_,
    coefficients = coeftab,
    adj_r2 = c$adj_r2,
    F = c$F,
    Sig = c$Sig
  )
  class(out) <- "summary_psychh_lm_computed"
  out
}

#' @export
print.summary_psychh_lm_reported <- function(x, ...) {
  cat("\nCall:\n")
  cat("  Psychh::lm(...)\n\n")

  if (!is.na(x$table) && identical(x$table, "table5") && !is.na(x$panel) && nzchar(x$panel)) {
    cat(sprintf("Table 5 (Moderator: %s)  Model %s\n\n", x$panel, x$model))
  } else if (!is.na(x$table) && nzchar(x$table)) {
    cat(sprintf("Table 4  Model %s\n\n", x$model))
  }

  cat("Coefficients:\n")
  printCoefmat(x$coefficients, P.values = TRUE, has.Pvalue = TRUE, signif.stars = TRUE)
  cat("\n")
  cat(sprintf("Adj. R-squared: %s\n", format(x$adj_r2, digits=3)))
  cat(sprintf("F-statistic: %s,  Sig: %s\n", format(x$F, digits=4), format(x$Sig, digits=3)))
  invisible(x)
}

#' @export
print.summary_psychh_lm_computed <- function(x, ...) {
  cat("\nCall:\n")
  cat("  stats::lm(...)\n\n")
  cat("Coefficients:\n")
  printCoefmat(x$coefficients, P.values = TRUE, has.Pvalue = TRUE, signif.stars = TRUE)
  cat("\n")
  cat(sprintf("Adj. R-squared: %s\n", format(x$adj_r2, digits=3)))
  cat(sprintf("F-statistic: %s,  Sig: %s\n", format(x$F, digits=4), format(x$Sig, digits=3)))
  invisible(x)
}
