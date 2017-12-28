# Helper functions --------------------------------------------------------

# Remove all non alpha-numeric characters (including
# punctuation) and to convert all characters to lower-case.
simplify_string <- function(x) {
    x <- gsub("[^a-zA-z0-9]", "", x)
    x <- gsub("[[:punct:]]", "", x)
    x <- tolower(x)
    return(x)
}

# Takes a data.frame and a list of columns and creates a new column composed of
# the pasted contents of the specified columns. Additionally, allows
# string simplification. The purpose is to create a single column on which
# to eventually compute string distances for the purpose of fuzzy matching.
add_matching_col <- function(x, cols, simplify_match = TRUE) {

    if (simplify_match) {
        x$matching_col <-
            do.call(paste,
                    data.frame(sapply(x[, cols], simplify_string)),
                    sep = "")
    } else {
        x$matching_col <- do.call(paste, x[, cols], sep = "")
    }

    return(x)
}

# Returns logical index indicating if row should be flagged based on a minimum
# string length
create_min_str_leng_idx <- function(x, cols, min_length) {
    # If only one length is provided, apply that length to the combined string
    # If more than one is provided, allow each value to be applied to the
    # appropriate column based upon position in the argument
    if (length(min_length) == 1) {
        return(nchar(x[, "matching_col"] < min_length))
    } else if (length(min_length) > 1) {
        if (length(min_length) != length(cols)) {
            stop(paste0("min_length must be one of the following: set to NULL,",
                        " of length one, or of the same length as cols."))
        }
        # This will check to see that each entry in each column meets the
        # minimum number of characters specified in min_length, and will return
        # a logical index specifying any row that did not meet these
        # expectations
        sums <- rowSums(matrix(mapply(function(x, y) {nchar(x) < y},
                                      x = as.matrix(x[, cols]),
                                      y = sapply(min_length, rep, nrow(test))),
                               nrow = nrow(test),
                               byrow = FALSE))
        return(sums > 0)
    }
}


fuzzy_matching <- function(x, string_dist) {

    # Add an empty match_ID to identify matching rows
    x$match_ID <- NA
    # Add an interal ID to uniquely identify rows for this function
    x$internal_ID <- 1:nrow(x)

    for (i in 1:nrow(x)) {
        # Skip row if it has already been assigned a match ID
        if (!is.na(x$match_ID[i])) next
        # Index entries still missing a match_ID
        na.idx <- is.na(x$match_ID)
        # Among those entries still that have not been used for a pair-wise
        # search and which have not already been matched
        # (match_ID still set to NA), make an index to to pull entries which
        # match based upon a certain string distance
        m.idx <-
            as.vector(utils::adist(x$matching_col[i],
                                   x$matching_col[na.idx]) <=
                          string_dist)
        # Get the IDs of those that matched
        internal_ids <- x[na.idx, "internal_ID"][m.idx]
        # Set the match ID of those entries to a value based upon the current
        # entry used for the search.
        x[x$internal_ID %in% internal_ids, "match_ID"] <-
            paste0("m", i)
    }

    # Remove the internal ID
    x$internal_ID <- NULL

    return(x)
}

# test <- data.frame(match_ID = NA,
#                    internal_ID = c(1, 2, 3),
#                    matching_col = c("Apple", "Apple", "Apple"),
#                    stringsAsFactors = FALSE)
# test <- data.frame(match_ID = NA,
#                    internal_ID = c(1, 2, 3),
#                    matching_col = c("Apple", "Apple", "Pear"),
#                    stringsAsFactors = FALSE)
#
# to_name(test, 0)

strict_matching <- function(x) {

    # Add an empty match_ID to identify matching rows
    x$match_ID <- NA
    # Add an interal ID to uniquely identify rows for this function
    x$internal_ID <- 1:nrow(x)

    for (i in 1:nrow(x)) {
        # Skip row if it has already been assigned a match ID
        if (!is.na(x$match_ID[i])) next
        # This will still look at things upstream and downstream that have
        # already been paired, but will not use these elements for pairwise
        # matching if they have already been assigned
        x[x$matching_col == x$matching_col[i] &
              is.na(x$match_ID), "match_ID"] <-
            paste0("m", i)
    }

    # Remove the internal ID
    x$internal_ID <- NULL

    return(x)
}

add_match_ID <- function(x,
                         cols,
                         approx_match = FALSE,
                         string_dist = 10,
                         min_length = 20,
                         simplify_match = TRUE) {

    # Matching column -------------------------------
    x <- add_matching_col(x, cols, simplify_match = simplify_match)

    # Flag rows based on string length ----------------------------
    if (!is.null(min_length)) {
        x[, "minimum_length_flag"] <-
            create_min_str_leng_idx(x, cols, min_length)
    }

    # Add matching IDs based on exact or fuzzy matching -----------------
    if (approx_match) {
        x <- fuzzy_matching(x, string_dist)
    } else {
        x <- strict_matching(x)
    }

    return(x)
}

get_matches <- function(x) {
    matched_match_IDs <- unique(x[duplicated(x[,"match_ID"]), "match_ID"])
    x <- x[x[,"match_ID"] %in% matched_match_IDs,]
    idx <- order(x[, "match_ID"])
    return(x[idx,])
}

remove_matches_screened <- function(x, db_pref = NULL, ignore_IDs = NULL) {

    #########################
    #########################
    # Remove matching column here
    # Remove minimum length flag here

    # Add unique ID to x
    x$internal_ID <- 1:nrow(x)

    # Create a search variable if database precedence is specified
    if(!is.null(db_pref)) {
        db_search <- paste(db_pref, collapse = "|")
    }

    duplicated_match_IDs <- unique(x$match_ID[duplicated(x$match_ID)])
    if (!is.null(ignore_IDs)) {
        duplicated_match_IDs <- setdiff(duplicated_match_IDs, ignore_IDs)
    }

    for (id in duplicated_match_IDs) {

        ############### Create duplicates df here #########################
        duplicate.df <- x[x[, "match_ID"] == id, ]
        ###################################################################

        # Skip if there aren't any duplicates
        if (nrow(duplicate.df) == 1) next

        # Give precedence to certain databases if they are contained
        # within the duplicates dataframe
        if (!is.null(db_pref) &&
            sum(grepl(db_search, duplicate.df[,"DATABASE"])) > 0) {

            # Find which IDs to remove
            removal_ids <- duplicate.df[!(grepl(db_search,
                                                duplicate.df[,"DATABASE"])),
                                        "internal_ID"]

            # Remove IDs from duplicate df and original df
            duplicate.df <-
                duplicate.df[!(duplicate.df[, "internal_ID"] %in% removal_ids), ]
            x <- x[!(x[, "internal_ID"] %in% removal_ids), ]

            if (nrow(duplicate.df) == 1) next

        }

        # Choose entry with the most available data
        idx <- which.max(nchar(simplify_string(apply(duplicate.df,
                                                     1,
                                                     paste,
                                                     collapse = ""))))
        removal_ids <- duplicate.df[-idx, "internal_ID"]
        x <- x[!(x[, "internal_ID"] %in% removal_ids), ]
    }

    # Remove unique ID
    x$internal_ID <- NULL
    # Remove match_ID
    x$match_ID <- NULL

    return(x)
}

