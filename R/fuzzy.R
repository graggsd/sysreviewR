
#' @export
fuzzy_unique <- function(x, string_dist = 10, protect_length = NULL) {
    # Remove NA values
    x <- as.character(na.omit(x))
    # Remove alpha-numerics and subset x for some minimum length
    x_simplified <- simplify_string(x)

    # Create a vector to indicate elements that need to be remove
    remove <- rep(NA, length(x_simplified))

    if (!is.null(protect_length)) {

        idx <- which(nchar(x_simplified) <= protect_length)
        remove[idx] <- FALSE
    }

    remove[is.na(remove)][which(duplicated(x_simplified[is.na(remove)]))] <-
        TRUE

    for (i in 1:length(x_simplified)) {
        if (!is.na(remove[i])) next
        remove[i] <- FALSE
        na_idx <- which(is.na(remove))
        if (length(na_idx) == 0) next
        match_idx <- which(utils::adist(x_simplified[i],
                                        x_simplified[na_idx]) < string_dist)
        if(length(match_idx) == 0) next
        remove[na_idx][match_idx] <- TRUE
    }
    if (length(which(remove)) != 0) {
        x <- x[-which(remove)]
    }
    return(x)
}

#' @export
fuzzy_in <- function(x, y, string_dist = 10, protect_length = NULL) {

    # Remake the simplified x vector
    x_simplified <- simplify_string(x)
    y_simplified <- simplify_string(y)

    is_in <- rep(NA, length(x_simplified))

    if (!is.null(protect_length)) {
        y_simplified <-
            y_simplified[which(nchar(y_simplified) > protect_length)]
        is_in[which(nchar(x_simplified) <= protect_length)] <-
            FALSE
    }

    is_in[is.na(is_in)][which(x_simplified[is.na(is_in)] %in% y_simplified)] <-
        TRUE

    for (i in 1:length(x_simplified)) {
        if (!is.na(is_in[i])) next
        is_in[i] <- sum(utils::adist(x_simplified[i],
                                     y_simplified) < string_dist) > 0
    }
    return(is_in)
}

