#' Update a revised publication table with previously extracted information
#'
#' Takes a new set of publication records populated and updates with
#' previously recorded (often manually acquired) data.
#'
#' @param empty The new set of publication records
#' @param populated Previously recorded publication data
#' @param match_cols Column(s) that will be used to match
#' \code{empty} and \code{populated}
#' @param approx_match Whether to use string distances or exact values when
#' matching records.
#' @param string_dist When using approximate matching, the string
#' distance cutoff at which records will be matched.
#' @param min_length The minimum string length for \code{match_cols} at which
#' a record will be considered when matching records.
#' @param simplify_match Whether to perform matching on strings composed from
#' \code{match_cols}, but with non alpha-numeric values removed.
#' @return An updated version of \code{empty}, which will be updated where
#' matches to \code{populated} (based on \code{match_cols}) are made.
#' @examples
#' \dontrun{
#' empty <- data.frame(a = c("Apples", "Oranges", "Bananas"),
#'                     b = c("Granny", "Florida", "Chiquita"),
#'                     c = c("", "", ""),
#'                     d = c("", "", ""),
#'                     stringsAsFactors = FALSE)
#'
#' update <- data.frame(a = c("Apples", "Oranges"),
#'                      b = c("Granny", "Florida"),
#'                      c = c("Red", ""),
#'                      d = c("Green", ""),
#'                      stringsAsFactors = FALSE)
#'
#' update_data(empty, update, c("a", "b"), min_length = 5)
#' }
#' @export
update_data <- function(empty,
                        populated,
                        match_cols,
                        approx_match = FALSE,
                        string_dist = 10,
                        min_length = 20,
                        simplify_match = TRUE) {
    UseMethod("update_data")
}

#' @export
update_data.default <- function(empty, ...) {
    stop("x must be of class data.frame")
}

#' @export
update_data.data.frame <- function(empty,
                                   populated,
                                   match_cols,
                                   approx_match = FALSE,
                                   string_dist = 10,
                                   min_length = 20,
                                   simplify_match = TRUE) {

    # Make matching columns for both the blank dataset and the one that will
    # be used to update it.
    empty <- add_matching_col(empty, match_cols, simplify_match)
    populated <- add_matching_col(populated, match_cols, simplify_match)

    if (approx_match) {
        idx <- get_matching_index_approx(empty, populated, string_dist)
    } else {
        # For each row in 'empty', gets a vector of rows in 'populated' that form
        # a perfect match
        idx <- get_matching_index_exact(empty, populated)
    }

    # Remove matches where the string length for the matching length is shorter
    # than desired
    idx <- remove_short_string_matches(idx, empty, min_length)

    if (sum(unlist(lapply(idx, length))) > 0) {
        # Make sure there are not matches between rows in 'populated' and 'empty'.
        # Send a warning for rows with multiple matches and remove the extra
        # matches.
        # Afterwards, make sure there are not duplicate refernces to a single
        # row in populated for each row in empty
        idx <- remove_multi_matches(idx)

        # Take update empty with populated using the finalized index
        final <- finalize_matches(empty, populated, idx)

    } else {
        final <- empty
    }

    # Remove the internal columns
    final$match_ID <- NULL
    final$matching_col <- NULL

    return(final)

}

# Helper functions ------------------------------------------------------------

# For each row in 'empty', gets a vector of rows in 'populated' that form a
# perfect match
get_matching_index_exact <- function(empty, populated) {
    return(lapply(empty$matching_col,
                  function(x) {which(populated$matching_col == x)}))
}

get_matching_index_approx <- function(empty, populated, string_dist) {
    return(lapply(empty$matching_col,
           function(x) {which(as.vector(utils::adist(x,
                                                     populated$matching_col) <=
                                            string_dist))}))
}

# First removes instances where multiple columns in populated are referenced
# for a single column of empty
# Second, removes duplicates within the index
remove_multi_matches <- function(idx) {
    multi_match_idx <- which(sapply(idx, length) > 1)
    if (length(multi_match_idx) > 0) {
        warning(paste0("Rows ",
                       paste(which(multi_match_idx), sep = ", "),
                       " of 'empty' have multiple matches to rows in ",
                       "'populated'. Using only the first match."))
        for (i in multi_match_idx) {
            idx[[i]] <- idx[[i]][1]
        }
    }
    duplicated_idx <- which(duplicated(idx))
    if (length(duplicated_idx) > 0) {
        warning(paste0("Duplicate matches in rows of 'empty' detected.",
                       " Will only match first instance of each reference",
                       " to 'populated'."))
        for (i in duplicated_idx) {
            idx[[i]] <- numeric()
        }
    }
    return(idx)
}

remove_short_string_matches <- function(idx, empty, min_length) {
    short_idx <- which(nchar(empty$matching_col) < min_length)
    for (i in short_idx) {
        idx[[i]] <- numeric()
    }
    return(idx)
}

finalize_matches <- function(empty, populated, idx) {
    for (i in 1:length(idx)) {
        if (length(idx[[i]]) > 0) {
            empty[i, ] <- populated[idx[[i]], ]
        }
    }
    return(empty)
}
