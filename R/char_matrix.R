#' Coerce Input to Character Matrix with Meaningful Column Names
#'
#' Converts various *R* object types, such as vectors, factors, data frames,
#' data tables, tibbles, and lists, into a character matrix, preserving or
#' assigning column names intelligently.
#'
#' @param y An object to be converted to a character matrix. Acceptable types
#' include `vector`, `factor`, `matrix`, `data.frame`, `data.table`, `tibble`,
#' or `list`.
#' @param cname A character vector of column names whose length must match the
#' number of output columns. Default is `NULL`, which means `factor`s and
#' `atomic` vectors will not be assigned a column name and `list`s,
#' `data.frame`s, and `matrix` inputs will fall back to their existing column
#' names (or synthesized `V1`, `V2`, ... names for unnamed lists).
#'
#' @details The transformation of the *R* objects into a character matrix is
#' done through [base::as.character] and using [base::as.matrix] when the
#' method is defined for the object type. For `list`s, `NA` are appended to the
#' elements that have an object length less than the maximum object length.
#'
#' @return A `character` matrix with meaningful column names when possible.
#'
#' @seealso [base::as.matrix] [data.table::data.table] [tibble::tibble]
#'
#' @examples
#' char_matrix(y = 1:3)
#' char_matrix(y = factor(x = c("a", "b", "c")), cname = "ex_name")
#' char_matrix(y = data.frame(a = 1:3, b = letters[1:3]))
#' char_matrix(y = list(one = 1:3, two = 4:6))
#'
#' @export
#' @rdname char_matrix
char_matrix <- function(y, cname = NULL) {
  UseMethod("char_matrix")
}

#' @export
#' @rdname char_matrix
char_matrix.default <- function(y, cname = NULL) {
  out <- matrix(data = as.character(x = y), ncol = 1)
  if (!is.null(cname)) {
    if (length(cname) != ncol(out)) {
      stop(
        "cname must have length ", ncol(out),
        " (one name per output column)."
      )
    }
    colnames(x = out) <- cname
  }

  # Returning character matrix ####
  return(out)
}

#' @export
#' @rdname char_matrix
char_matrix.factor <- function(y, cname = NULL) {
  # Handle factor to character vector ####
  y <- as.character(x = y)

  # Character vector to character matrix ####
  out <- char_matrix.default(y = y, cname = cname)

  # Returning character matrix ####
  return(out)
}

#' @export
#' @rdname char_matrix
char_matrix.list <- function(y, cname = NULL) {
  # Creating padded character list ####
  max_len <- max(sapply(X = y, FUN = length))
  padded <- lapply(
    X = y,
    FUN = function(yy) {
      col_char <- as.character(x = yy)
      length(x = col_char) <- max_len # pads with NA automatically
      return(col_char)
    }
  )

  # Creating character matrix ####
  out <- do.call(what = "cbind", args = padded)
  if (!is.null(cname)) {
    if (length(cname) != ncol(out)) {
      stop(
        "cname must have length ", ncol(out),
        " (one name per output column)."
      )
    }
    colnames(x = out) <- cname
  } else {
    colnames(x = out) <- names(x = y) %||% paste0("V", seq_along(padded))
  }

  # Returning character matrix ####
  return(out)
}

#' @export
#' @rdname char_matrix
char_matrix.data.frame <- function(y, cname = NULL) {
  # Obtaining character matrix ####
  out <- as.matrix(
    x = data.frame(
      lapply(X = y, FUN = as.character),
      stringsAsFactors = FALSE
    )
  )

  # Assigning name ####
  if (!is.null(cname)) {
    if (length(cname) != ncol(out)) {
      stop(
        "cname must have length ", ncol(out),
        " (one name per output column)."
      )
    }
    colnames(x = out) <- cname
  } else {
    colnames(x = out) <- colnames(x = y)
  }

  # Returning character matrix ####
  return(out)
}

#' @export
#' @rdname char_matrix
char_matrix.data.table <- function(y, cname = NULL) {
  # Getting character matrix based on data.frame ####
  out <- char_matrix.data.frame(y = y, cname = cname)

  # Returning character matrix ####
  return(out)
}

#' @export
#' @rdname char_matrix
char_matrix.matrix <- function(y, cname = NULL) {
  # Obtaining character matrix ####
  out <- apply(X = y, MARGIN = c(1, 2), FUN = as.character)

  # Assigning names ####
  if (!is.null(cname)) {
    if (length(cname) != ncol(out)) {
      stop(
        "cname must have length ", ncol(out),
        " (one name per output column)."
      )
    }
    colnames(x = out) <- cname
  } else {
    colnames(x = out) <- colnames(x = y)
  }

  # Returning character matrix ####
  return(out)
}


# Helper: Null-coalescing operator
`%||%` <- function(a, b) {
  if (!is.null(x = a)) a else b
}
