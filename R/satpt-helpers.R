# Internal helpers for satpt(). Not exported.

# Best-effort name for an unevaluated argument expression.
# Handles bare names (`x`), `$`-accessors (`d$col`), and wrapping calls
# (`factor(d$col)`, `as.factor(d$col[1:10])`, ...) by stripping any prefix
# up to the last `$` and then keeping only the leading R identifier. Falls
# back to `default` when no usable identifier can be recovered.
extract_var_name <- function(expr_obj, default = "y") {
  txt <- deparse(expr = expr_obj)[1]
  txt <- sub(pattern = ".*\\$", replacement = "", x = txt, perl = FALSE)
  m <- regmatches(
    x = txt,
    m = regexpr(pattern = "^[A-Za-z._][A-Za-z0-9._]*", text = txt)
  )
  if (length(m) == 1L && nzchar(m)) m else default
}

# Coerce y to a character matrix, with a fallback variable name for
# atomic input. Standardizes the error message so callers see the same
# guidance regardless of which char_matrix method failed.
coerce_responses <- function(y, var_name) {
  result <- try(
    expr = {
      if (is.atomic(x = y)) {
        satpt::char_matrix(y = y, cname = var_name)
      } else {
        satpt::char_matrix(y = y)
      }
    },
    silent = TRUE
  )
  if (inherits(x = result, what = "try-error")) {
    stop(
      "y must be a vector, matrix, data.frame, data.table, tibble,",
      " factor, or list."
    )
  }
  result
}

# Coerce by to a character vector. Mirrors coerce_responses' error contract.
coerce_grouping <- function(by) {
  result <- try(
    expr = as.character(x = satpt::char_matrix(y = by)),
    silent = TRUE
  )
  if (inherits(x = result, what = "try-error")) {
    stop(
      "by must be a vector, matrix, data.frame, data.table, tibble,",
      " factor, or list."
    )
  }
  result
}

# Disambiguate multi-column y. NULL warns (the common-mistake case),
# TRUE silently accepts (intentional select-all-apply), FALSE errors
# (the user meant separate questions, which would produce wrong results).
check_select_all_apply <- function(y, select_all_apply) {
  if (ncol(y) < 2L) {
    return(invisible(NULL))
  }
  if (isTRUE(select_all_apply)) {
    return(invisible(NULL))
  }
  if (isFALSE(select_all_apply)) {
    stop(
      "y has ", ncol(y), " columns but select_all_apply = FALSE.\n",
      "If each column is a separate survey question, use satpt_survey()",
      " to analyze them individually.\n",
      "If the columns are the response items of one select-all-that-apply",
      " question, set select_all_apply = TRUE."
    )
  }
  warning(
    "y has ", ncol(y), " columns; satpt() will treat them as response",
    " items of one select-all-that-apply question.\n",
    "  - If each column is a separate question, use satpt_survey().\n",
    "  - If this is intentional, pass select_all_apply = TRUE to silence",
    " this warning.",
    call. = FALSE
  )
  invisible(NULL)
}

# Warn about degenerate inputs that satpt() will technically handle but
# whose results are misleading. The function continues either way; users
# may suppress these via suppressWarnings() if intentional.
sanity_check_inputs <- function(y, by) {
  if (!is.null(by)) {
    by_levels <- unique(by[!is.na(by)])
    if (length(by_levels) <= 1L) {
      warning(
        "`by` has ", length(by_levels),
        " unique non-NA level(s); the test for response bias requires",
        " at least 2. The analysis will proceed as if `by` were not",
        " specified.",
        call. = FALSE
      )
    }
  }
  for (j in seq_len(ncol(y))) {
    col <- y[, j]
    n_unique <- length(unique(col[!is.na(col)]))
    if (n_unique <= 1L) {
      col_label <- if (ncol(y) > 1L) {
        paste0("Column '", colnames(y)[j], "'")
      } else {
        "y"
      }
      warning(
        col_label, " has ", n_unique,
        " unique non-NA value(s); saturation will fire trivially and",
        " the analysis will not be informative.",
        call. = FALSE
      )
    }
    flag_pipe_encoded(col = col, col_label_fn = function() {
      if (ncol(y) > 1L) paste0("Column '", colnames(y)[j], "'") else "y"
    })
  }
  invisible(NULL)
}

# Warn when a single response column appears to be a select-all-that-apply
# question encoded as one '|'-delimited string per row. Pipes are rare in
# free text so a high prevalence is a reliable signal; ',' and ';' are
# noisier and not flagged here.
flag_pipe_encoded <- function(col, col_label_fn) {
  non_na <- col[!is.na(col)]
  if (length(non_na) == 0L) {
    return(invisible(NULL))
  }
  pipe_rate <- mean(grepl(pattern = "|", x = non_na, fixed = TRUE))
  if (pipe_rate > 0.1) {
    warning(
      col_label_fn(), " has '|' in ",
      round(pipe_rate * 100), "% of non-NA values; this often indicates",
      " a select-all-that-apply question encoded as one string. Convert",
      " it with split_select_all_apply() (or pass split = list(<col> =",
      " '|') to satpt_survey()) to analyze each response item",
      " separately.",
      call. = FALSE
    )
  }
  invisible(NULL)
}

validate_alpha <- function(alpha) {
  if (!is.numeric(x = alpha) || alpha >= 1 || alpha <= 0) {
    stop("alpha must be numeric between 0 and 1.")
  }
  invisible(NULL)
}

validate_threshold <- function(threshold) {
  if (!is.numeric(x = threshold) || threshold >= 0.25 || threshold <= 0) {
    stop("threshold must be numeric between 0 and 0.25.")
  }
  invisible(NULL)
}

validate_dimnames <- function(dimnames, by) {
  if (is.null(dimnames)) {
    return(invisible(NULL))
  }
  if (!is.character(x = dimnames)) {
    stop("dimnames must be a character vector.")
  }
  if (length(dimnames) == 1L && !is.null(by)) {
    stop("dimnames must be of length two when by is specified.")
  }
  ndimns <- names(dimnames)
  if (!is.null(ndimns) && !all(c("y", "by") %in% ndimns)) {
    stop("For a named dimnames vector, 'y' and 'by' must be included.")
  }
  invisible(NULL)
}

# Run all input validators for satpt(); separated so each rule is a small,
# single-purpose function with low cyclomatic complexity.
validate_args <- function(y, by, alpha, threshold, dimnames) {
  if (!is.null(by) && (nrow(y) != length(by))) {
    stop("y and by must have the same number of observations.")
  }
  validate_alpha(alpha)
  validate_threshold(threshold)
  validate_dimnames(dimnames, by)
  invisible(NULL)
}

# Apply user-specified dimnames (or fall back to colnames/var_name_by) to
# the attributes of an ftable so the resulting matrix has informative axes.
apply_dimnames <- function(attrs, dimnames, by, var_name_by, col_name) {
  ndimns <- names(dimnames)
  if (!is.null(dimnames)) {
    if (is.null(ndimns)) {
      names(attrs$col.vars) <- paste0("y: ", dimnames[1])
      if (!is.null(by)) {
        names(attrs$row.vars) <- paste0("by: ", dimnames[2])
      }
    } else {
      names(attrs$col.vars) <- paste0("y: ", unname(dimnames["y"]))
      if (!is.null(by)) {
        names(attrs$row.vars) <- paste0("by: ", unname(dimnames["by"]))
      }
    }
  } else {
    names(attrs$col.vars) <- paste0("y: ", col_name)
    if (!is.null(by)) {
      names(attrs$row.vars) <- paste0("by: ", var_name_by)
    }
  }
  attrs
}

# Build a contingency table for one column of y, optionally stratified by
# `by`. Sorts the input alphabetically before tabulation to keep the column
# order deterministic across calls.
build_counts <- function(y_col, by, exclude, dimnames, var_name_by, col_name) {
  if (is.null(by)) {
    out <- y_col[order(y_col)]
    out <- stats::ftable(y = out)
  } else {
    ord <- order(y_col, by)
    out <- stats::ftable(by[ord], y_col[ord], exclude = exclude)
  }
  attributes(out) <- apply_dimnames(
    attrs = attributes(out),
    dimnames = dimnames,
    by = by,
    var_name_by = var_name_by,
    col_name = col_name
  )
  as.matrix(x = out)
}

# Per-row standard errors of the row-wise sample proportions of a counts matrix.
calc_se <- function(counts_mat) {
  nn <- rowSums(x = counts_mat)
  phat <- base::proportions(x = counts_mat, margin = 1)
  out <- phat
  for (i in seq_len(nrow(out))) {
    out[i, ] <- sqrt((phat[i, ] * (1 - phat[i, ])) / nn[i])
  }
  out
}

# Run a chi-squared or Fisher's exact test for independence on a 2D
# contingency table. Switches to Fisher's when more than 20% of the
# expected cell counts fall below 5. Forwards user-supplied `dots` to
# the chosen test, defensively dropping `x` and `y` so the contingency
# matrix is always the operand.
run_independence_test <- function(counts_mat, dots) {
  expected <- outer(X = rowSums(counts_mat), Y = colSums(counts_mat)) /
    sum(counts_mat)
  pct_less5 <- mean(expected < 5)

  if (pct_less5 < 0.2) {
    test_args <- list(x = counts_mat, correct = FALSE)
    test_fun <- "chisq.test"
  } else {
    test_args <- list(x = counts_mat)
    if (any(dim(counts_mat) > 2L)) {
      test_args$simulate.p.value <- TRUE
    }
    test_fun <- "fisher.test"
  }

  user_args <- dots[setdiff(names(dots), c("x", "y"))]
  if (length(user_args) > 0L) {
    test_args[names(user_args)] <- user_args
  }

  out <- do.call(what = test_fun, args = test_args)
  out$data.name <- paste0(
    names(dimnames(counts_mat))[2],
    " given ",
    names(dimnames(counts_mat))[1]
  )
  out
}

# Calculate the per-category overall (total) sample statistics. Standard
# errors switch between the unpooled binomial form and the pooled,
# wave-weighted form depending on whether the independence test detected
# response bias.
calc_total <- function(counts_mat, test, se_mat, pooled) {
  out <- data.frame(
    categories = dimnames(counts_mat)[[2]],
    counts = NA,
    phat = NA,
    se = NA
  )
  out$counts <- as.integer(colSums(x = counts_mat))
  total_obs <- sum(out$counts)
  out$phat <- out$counts / total_obs
  if (!is.null(test)) {
    if (isTRUE(pooled)) {
      weights <- rowSums(counts_mat) / sum(rowSums(counts_mat))
      for (j in seq_len(ncol(se_mat))) {
        out$se[j] <- sqrt(sum(weights^2 * se_mat[, j]^2))
      }
    } else {
      out$se <- sqrt((out$phat * (1 - out$phat)) / total_obs)
    }
  } else {
    out$se <- as.vector(se_mat)
  }
  out
}

# Approximate count of additional responses needed to bring max(SE) under
# the saturation threshold, assuming current proportions hold. Both the
# unpooled and pooled SE shrink as 1/sqrt(N) under proportional extension
# of the sample, so the same scaling formula applies to both regimes.
# Returns 0L when saturation is already achieved.
calc_n_to_saturation <- function(max_se, n, threshold) {
  if (max_se <= threshold) {
    return(0L)
  }
  required <- ceiling(n * (max_se / threshold)^2)
  as.integer(required - n)
}

# Heterogeneity index (mean absolute deviation of wave proportions from
# the overall sample proportion) for each response category in one column.
calc_hindex <- function(counts_mat, phat_mat, total_df) {
  out <- rep(x = NA_real_, length.out = nrow(total_df))
  names(out) <- total_df$categories
  n_waves <- nrow(counts_mat)
  for (j in seq_len(ncol(phat_mat))) {
    out[j] <- sum(abs(phat_mat[, j] - total_df$phat[j])) / n_waves
  }
  out
}
