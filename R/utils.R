`%||%` <- function(x, y) if (is.null(x) || (length(x)==1 && is.na(x))) y else x

.psychh_result <- function(fun, spec2, key = NA, matched = FALSE, match = list(),
                           call = list(), reported = NULL, computed = NULL, diff = NULL,
                           diag = list(n_used = NA_integer_, na_action = NA_character_,
                                       warnings = character(), errors = character())) {
  res <- list(
    fun = fun,
    spec_id = spec2$meta$spec_id,
    key = key,
    matched = matched,
    match = match,
    call = call,
    reported = reported,
    computed = computed,
    diff = diff,
    diag = diag
  )
  class(res) <- c("psychh_result", paste0("psychh_", fun))
  res
}

.as_numeric <- function(x) suppressWarnings(as.numeric(x))

.center <- function(x) x - mean(x, na.rm = TRUE)

# normalize construct name / alias
.norm_construct <- function(varname, spec2) {
  if (is.null(varname)) return(NA_character_)
  if (length(varname) != 1) return(NA_character_)
  v <- as.character(varname)
  if (v %in% names(spec2$schema$items_by_construct)) return(v)
  if (v %in% names(spec2$match$construct_alias)) return(unname(spec2$match$construct_alias[[v]]))
  v
}

.norm_moderator <- function(x, spec2) {
  if (is.null(x) || length(x)!=1) return(NA_character_)
  v <- as.character(x)
  if (v %in% spec2$schema$moderators) return(v)
  if (v %in% names(spec2$match$moderator_alias)) return(unname(spec2$match$moderator_alias[[v]]))
  v
}

.get_items <- function(data, key, spec2) {
  items <- spec2$schema$items_by_construct[[key]]
  if (is.null(items)) return(character(0))
  items[items %in% colnames(data)]
}

.complete_cases_rows <- function(df) {
  if (ncol(df) == 0) return(integer(0))
  which(stats::complete.cases(df))
}

.corr_matrix <- function(X, use = c("complete.obs","pairwise.complete.obs")) {
  use <- match.arg(use)
  stats::cor(X, use = use, method = "pearson")
}

# Cronbach alpha for a numeric matrix X (rows obs, cols items)
.cronbach_alpha <- function(X) {
  X <- as.matrix(X)
  k <- ncol(X)
  if (k < 2) stop("Need at least 2 items to compute Cronbach's alpha.")
  # listwise
  X <- X[stats::complete.cases(X), , drop=FALSE]
  n <- nrow(X)
  if (n < 3) stop("Too few complete observations to compute alpha.")
  item_var <- apply(X, 2, stats::var)
  total_var <- stats::var(rowSums(X))
  alpha <- (k/(k-1)) * (1 - sum(item_var, na.rm=TRUE)/total_var)
  list(alpha=alpha, n_used=n, k=k)
}

# KMO
.kmo_from_cor <- function(R) {
  R <- as.matrix(R)
  p <- ncol(R)
  if (p < 2) stop("Need at least 2 variables.")
  if (anyNA(R)) stop("Correlation matrix contains NA.")
  invR <- solve(R)
  P <- -invR / sqrt(outer(diag(invR), diag(invR)))
  diag(P) <- 0
  r2 <- R^2; diag(r2) <- 0
  p2 <- P^2; diag(p2) <- 0
  sum_r2 <- sum(r2)
  sum_p2 <- sum(p2)
  KMO <- sum_r2 / (sum_r2 + sum_p2)
  MSAi <- sapply(1:p, function(i){
    r2i <- sum(r2[i,])
    p2i <- sum(p2[i,])
    r2i / (r2i + p2i)
  })
  names(MSAi) <- colnames(R)
  list(KMO=KMO, MSAi=MSAi)
}

# Bartlett test of sphericity
.bartlett_from_cor <- function(R, n) {
  R <- as.matrix(R)
  p <- ncol(R)
  if (p < 2) stop("Need at least 2 variables.")
  detR <- det(R)
  if (!is.finite(detR) || detR <= 0) stop("Non-positive determinant of correlation matrix.")
  chisq <- -(n - 1 - (2*p + 5)/6) * log(detR)
  df <- p*(p-1)/2
  pval <- 1 - stats::pchisq(chisq, df=df)
  list(chisq=chisq, df=df, p=pval, det=detR)
}

# compute 1-factor loadings via factanal on correlation matrix
.onefactor_loadings <- function(X) {
  X <- as.matrix(X)
  X <- X[stats::complete.cases(X),,drop=FALSE]
  n <- nrow(X)
  if (n < 10) stop("Too few complete observations for factor analysis.")
  R <- stats::cor(X)
  fa <- stats::factanal(covmat = R, factors = 1, n.obs = n)
  load <- as.numeric(fa$loadings[,1])
  names(load) <- colnames(X)
  uniq <- fa$uniquenesses
  list(loadings=load, uniqueness=uniq, n_used=n)
}

.CR_from_loadings <- function(loadings, uniqueness) {
  lam <- as.numeric(loadings)
  th  <- as.numeric(uniqueness)
  num <- (sum(lam))^2
  den <- num + sum(th)
  CR <- num / den
  AVE <- mean(lam^2)
  list(CR=CR, AVE=AVE)
}

# formula signature for matching
.formula_signature <- function(formula, spec2) {
  f <- formula
  # Expand aliases in variable names (e.g., FAC->FCL, PLI->GRP)
  rhs_vars <- all.vars(f[[3]])
  rhs_vars_mapped <- vapply(rhs_vars, function(v) .norm_moderator(v, spec2), character(1))
  # Create a mapping only for moderators that differ
  map <- setNames(rhs_vars_mapped, rhs_vars)
  # Replace in formula string
  fs <- deparse(f)
  for (old in names(map)) {
    new <- map[[old]]
    if (!identical(old, new)) {
      fs <- gsub(paste0("\\b", old, "\\b"), new, fs)
    }
  }
  f2 <- stats::as.formula(fs)

  tt <- stats::terms(f2)
  dv <- as.character(attr(tt, "variables"))[2]
  term_labels <- attr(tt, "term.labels")

  # convert '*' is already expanded by terms; but keep colon interactions canonical
  main <- character(0)
  inter <- character(0)

  for (tl in term_labels) {
    if (grepl(":", tl, fixed=TRUE)) {
      parts <- strsplit(tl, ":", fixed=TRUE)[[1]]
      parts <- sort(parts)
      inter <- c(inter, paste(parts, collapse=":"))
    } else {
      main <- c(main, tl)
    }
  }
  main <- sort(unique(main))
  inter <- sort(unique(inter))

  # Detect panel if a moderator appears
  mods <- spec2$schema$moderators
  panel <- NA_character_
  present_mods <- intersect(mods, c(main, unique(unlist(strsplit(inter, ":", fixed=TRUE)))))
  if (length(present_mods) >= 1) panel <- present_mods[1]

  sig <- paste0(dv, "|main:", paste(main, collapse=","), "|int:", paste(inter, collapse=","))
  list(dv=dv, main=main, interactions=inter, panel=panel, signature=sig, formula=f2)
}

.match_formula_to_model <- function(sig_obj, spec2, context_panel = NA_character_) {
  reg <- spec2$model_registry
  candidates <- character(0)

  # canonicalize interactions so that A:B and B:A are treated as identical
  .canon_int <- function(v) {
    if (length(v) == 0) return(character(0))
    vapply(v, function(tl) {
      if (!grepl(":", tl, fixed = TRUE)) return(tl)
      parts <- strsplit(tl, ":", fixed = TRUE)[[1]]
      paste(sort(parts), collapse = ":")
    }, character(1))
  }
  sig_int <- .canon_int(sig_obj$interactions)

  for (k in names(reg)) {
    r <- reg[[k]]
    if (!identical(r$dv, sig_obj$dv)) next
    if (!setequal(r$main, sig_obj$main)) next
    if (!setequal(.canon_int(r$interactions), sig_int)) next
    candidates <- c(candidates, k)
  }
  if (length(candidates) == 0) {
    return(list(key=NA_character_, matched=FALSE, candidates=candidates, reason="no signature match"))
  }
  # disambiguate by panel if needed
  if (length(candidates) > 1) {
    cp <- .norm_moderator(context_panel, spec2)
    if (!is.na(cp)) {
      c2 <- candidates[vapply(candidates, function(k) identical(reg[[k]]$panel, cp), logical(1))]
      if (length(c2) == 1) {
        return(list(key=c2[1], matched=TRUE, candidates=candidates, reason=paste0("disambiguated by panel=", cp)))
      }
    }
    # default preference: table4 over table5 when no context and no moderator in signature
    if (all(is.na(sig_obj$panel))) {
      c4 <- candidates[grepl("^table4_", candidates)]
      if (length(c4) == 1) return(list(key=c4[1], matched=TRUE, candidates=candidates, reason="defaulted to table4"))
    }
    return(list(key=NA_character_, matched=FALSE, candidates=candidates, reason="ambiguous signature; provide context$panel"))
  }
  list(key=candidates[1], matched=TRUE, candidates=candidates, reason="exact signature match")
}

.score_constructs <- function(data, constructs, spec2, na_rm = TRUE) {
  out <- data.frame(row_id = seq_len(nrow(data)))
  for (k in constructs) {
    items <- spec2$schema$items_by_construct[[k]]
    if (is.null(items)) stop(sprintf("Unknown construct: %s", k))
    miss <- setdiff(items, colnames(data))
    if (length(miss) > 0) stop(sprintf("Missing item columns for %s: %s", k, paste(miss, collapse=", ")))
    X <- data[, items, drop=FALSE]
    # Coerce to numeric where possible
    X <- as.data.frame(lapply(X, function(z) .as_numeric(z)))
    out[[k]] <- rowMeans(X, na.rm = na_rm)
  }
  out
}
