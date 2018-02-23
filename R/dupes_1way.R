#' Remove previously documented publication records
#'
#' Takes output of \code{dupes_find_1way}, and returns a set of combined records
#' with duplicate entries removed.
#'
#' After running \code{dupes_find_1way}, records from \code{old} and \code{new}
#' are combined and pairs of entries matched between \code{old} and \code{new}
#' are marked with identical match IDs specified in the \code{match_ID} column.
#' This function removes the duplicate entry that originated from the \code{new}
#' set of records.
#'
#' @param x The output of \code{dupes_find_1way}
#' @param ignore_IDs A set of match IDs that will be ignored when removing
#' duplicate records. These should be identified by manually inspecting
#' the output of \code{dupes_return}.
#' @return An updated version of \code{x}, with duplicate records removed.
#' @examples
#' \dontrun{
#' old <- form_mm_recs[1:10, ]
#' new <- form_mm_recs[8:12, ]
#' test <- dupes_find_1way(old, new, c(1, 3))
#' dupes <- dupes_return(test)
#' out <- dupes_rm_1way(test)
#' }
#' @export
dupes_rm_1way <- function(x, ignore_IDs = NULL) {
    UseMethod("dupes_rm_1way")
}

#' @export
dupes_rm_1way.default <- function(x, ...) {
    stop("x must be of class data.frame")
}

#' @export
dupes_rm_1way.data.frame <- function(x, ignore_IDs = NULL) {
    old_matchIDs <- x$match_ID[!x$is_new]
    x <- x[!(x$is_new & (x$match_ID %in% old_matchIDs)), ]
    x[, c("match_ID", "matching_col", "is_new")] <- NULL
    return(x)
}


#' Identify previously documented publication records
#'
#' For two sets of publication records, one old and one new, combines old and
#' new records and marks instances where new records are duplicated in the old
#' set of records.
#'
#' @param old The previous set of publication records
#' @param new The new set of publication records
#' @param match_cols Column(s) that will be used to search for duplicate
#' records
#' @param approx_match Whether to perform a duplicate search using
#' string distances or exact values
#' @param string_dist When using approximate matching, the string
#' distance cutoff at which records will be assumed duplicated
#' @param min_length The minimum string length for \code{match_cols} at which
#' a record will be considered when searching for duplicates
#' @param simplify_match Whether to perform duplicate searches after removing
#' all non alpha-numeric characters from the reference string generated from
#' \code{match_cols}
#' @return The combined records from \code{old} and \code{new} records from new
#' found in old, indicated by paired match_IDs.
#' duplicates (\code{match_ID}).
#' @examples
#' \dontrun{
#' old <- form_mm_recs[1:10, ]
#' new <- form_mm_recs[8:12, ]
#' test <- dupes_find_1way(old, new, c(1, 3))
#' dupes <- dupes_return(test)
#' out <- dupes_rm_1way(test)
#' }
#' @export
dupes_find_1way <- function(old,
                            new,
                            match_cols,
                            approx_match = FALSE,
                            string_dist = 5,
                            min_length = 10,
                            simplify_match = TRUE) {
    UseMethod("dupes_find_1way")
}

#' @export
dupes_find_1way.default <- function(old, new, ...) {
    stop("x must be of class data.frame")
}

#' @export
dupes_find_1way.data.frame <- function(old,
                                       new,
                                       match_cols,
                                       approx_match = FALSE,
                                       string_dist = 10,
                                       min_length = 10,
                                       simplify_match = TRUE) {

    # Matching column -------------------------------------------------------
    new <- add_matching_col(new, match_cols, simplify_match = simplify_match)
    old <- add_matching_col(old, match_cols, simplify_match = simplify_match)

    # Flag rows based on string length --------------------------------------
    if (!is.null(min_length)) {
        new <- protect_min_length_1way(new, match_cols, min_length)
    }

    # Add matching IDs based on exact or fuzzy matching ---------------------
    x <- matching_1way(old, new, string_dist, approx_match)

    return(x)
}


# Helper functions -----------------------------------------------------------

matching_1way <- function(old, new, string_dist, approx_match) {

    old$match_ID <- paste0("m", 1:nrow(old))

    for (i in 1:nrow(new)) {
        if (!is.na(new[i, "match_ID"])) next
        if (approx_match) {
            m.idx <-
                which(as.vector(utils::adist(new$matching_col[i],
                                             old$matching_col) <=
                                    string_dist))
        } else {
            m.idx <- which(new$matching_col[i] == old$matching_col)
        }
        if (length(m.idx) > 0) {
            new$match_ID[i] <- paste0("m", m.idx[1])
        }
    }

    not_match_idx <- which(is.na(new$match_ID))
    new$match_ID[not_match_idx] <- paste0("nm", not_match_idx)
    new$is_new <- TRUE
    old$is_new <- FALSE

    return(rbind(old, new))
}

# Returns logical index indicating if row should be flagged based on a minimum
# string length
protect_min_length_1way <- function(new, match_cols, min_length) {
    # If only one length is provided, apply that length to the combined string
    # If more than one is provided, allow each value to be applied to the
    # appropriate column based upon position in the argument
    if (length(min_length) == 1) {
        if(length(match_cols == 1)) {
            idx <- nchar(new[, match_cols]) < min_length
        } else {
            idx <- nchar(apply(new[, match_cols], 1, paste0, collapse = "")) <
                min_length
        }
    } else if (length(min_length) > 1) {
        if (length(min_length) != length(match_cols)) {
            stop(paste0("min_length must be one of the following: set to NULL,",
                        " of length one, or of the same length as match_cols."))
        }
        # This will check to see that each entry in each column meets the
        # minimum number of characters specified in min_length, and will return
        # a logical index specifying any row that did not meet these
        # expectations
        sums <- rowSums(matrix(mapply(function(x, y) {nchar(x) < y},
                                      x = as.matrix(new[, match_cols]),
                                      y = sapply(min_length, rep, nrow(new))),
                               nrow = nrow(new),
                               byrow = FALSE))
        idx <- sums > 0
    }
    new[idx, "match_ID"] <- paste0("p", 1:nrow(new))[idx]
    return(new)
}
