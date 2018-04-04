#' Create a list of \code{data.frame}s to screen abstracts
#'
#' Creates a list of \code{data.frame}s to screen abstracts
#'
#' @param x A set of publication records without duplicates
#' @param reviewers Names of people that will screen abstracts
#' @param title_col Column name containing the publication title
#' @param abstract_col Column name containing the publication abstract
#' @return A list of of \code{data.frame}s, formatted for each reviewer to
#' screen abstracts
#' @examples
#' \dontrun{
#' screening_list <- screen_list_abst(form_mm_recs, c("you", "me"))
#' }
#' @export
screen_list_abst <- function(x,
                             reviewers,
                             title_col = "TITLE",
                             abstract_col = "ABSTRACT") {
    UseMethod("screen_list_abst")
}

#' @export
screen_list_abst.default <- function(x, ...) {
    stop("x must be of class data.frame")
}

#' @export
screen_list_abst.data.frame <- function(x,
                                        reviewers,
                                        title_col = "TITLE",
                                        abstract_col = "ABSTRACT") {

    base_sheet <- x[, c("UNIQUE_ID", title_col, abstract_col)]
    out <- lapply(reviewers,
                  function(x) {
                      base_sheet[, x] <- "not vetted"
                      return(base_sheet)
                  })
    names(out) <- reviewers
    return(out)
}

#' Create a list to screen full texts
#'
#' Creates a list of \code{data.frame}s to screen full texts
#'
#' @param x A set of publications records for full-text screening
#' @param reviewers Names of people that will screen full texts
#' @param cols Columns from x to include in the full-text screeners
#' @return A list of of \code{data.frame}s, formatted for each reviewer to
#' screen abstracts
#' @examples
#' \dontrun{
#' screening_list <- screen_list_fulltxt(form_mm_recs, c("you", "me"))
#' }
#' @export
screen_list_fulltxt <- function(x,
                                reviewers,
                                cols = "TITLE") {
    UseMethod("screen_list_fulltxt")
}

#' @export
screen_list_fulltxt.default <- function(x, ...) {
    stop("x must be of class data.frame")
}

#' @export
screen_list_fulltxt.data.frame <- function(x,
                                           reviewers,
                                           cols = "TITLE") {
    base_sheet <- x[, unique(c("UNIQUE_ID", cols))]
    new_cols <- c("OBTAINABLE", "INCLUDE", "EXCLUSION_RATIONALE")
    out <- lapply(reviewers,
                  function(x) {
                      base_sheet[, paste0(x, "_", new_cols)] <- "not vetted"
                      return(base_sheet)
                  })
    names(out) <- reviewers
    return(out)
}

#' Create .csv files for abstract screening
#'
#' Takes a the output of \code{screen_list_abst} and creates a series of
#' .csv files that may be distributed to the individuals that will screen
#' abstracts
#'
#' @param x The list created by \code{screen_list_abst}
#' @param dir The desired directory into which .csv files will be placed
#' @return NULL
#' @examples
#' \dontrun{
#' screening_list <- screen_list_abst(form_mm_recs, c("you", "me"))
#' screen_write_abst(screening_list, dir = "./")
#' }
#' @export
screen_write_abst <- function(x, dir = "../intermediate_data/") {
    screen_write(x = x, dir = dir, abstract = TRUE)
}


#' Create .csv files for full-text screening
#'
#' Takes a the output of \code{screen_list_fulltxt} and creates a series of
#' .csv files that may be distributed to the individuals that will screen
#' full-text articles
#'
#' @param x The list created by \code{screen_list_fulltxt}
#' @param dir The desired directory into which .csv files will be placed
#' @return NULL
#' @examples
#' \dontrun{
#' screening_list <- screen_list_fulltxt(form_mm_recs, c("you", "me"))
#' screen_write_fulltxt(screening_list, dir = "./")
#' }
#' @export
screen_write_fulltxt <- function(x, dir = "../intermediate_data/") {
    screen_write(x = x, dir = dir, abstract = FALSE)
}

# Helper function:
screen_write <- function(x, dir = "../intermediate_data/", abstract) {

    if (abstract) {
        prefix <- "AbstScreener_"
    } else {
        prefix <- "FulltxtScreener_"
    }

    for (reviewer in names(x)) {
        utils::write.csv(x[[reviewer]],
                         file = paste0(dir, prefix, reviewer, ".csv"),
                         row.names = FALSE)
    }
}

#' Reads completed abstract screening .csv files
#'
#' Takes a series of completed abstract screening forms and assembles them
#' into a \code{data.frame}
#'
#' @param dir The directory containing complete screening forms
#' @param ref_table The original set of records from which screening sheets
#' were collected. Note: this must be unaltered from the point at which
#' screening forms were first generated with \code{screen_list_abst}.
#' @return A \code{data.frame} with the combined results of all screening
#' sheets, plus the publication data from \code{ref_table}
#' @examples
#' \dontrun{
#' screening_list <- screen_list_abst(form_mm_recs, c("you", "me"))
#' screen_write_abst(screening_list, dir = "./")
#' # Abstracts should be screened at this point
#' combined_screening_forms <- screen_read("./", form_mm_recs)
#' }
#' @export
screen_read_abst <- function(ref_table, dir = "../intermediate_data/") {
    screen_read(ref_table = ref_table, dir = dir, abstract = TRUE)
}

#' Reads completed full-text screening .csv files
#'
#' Takes a series of completed full-text screening forms and assembles them
#' into a \code{data.frame}
#'
#' @param dir The directory containing complete screening forms
#' @param ref_table The original set of records from which screening sheets
#' were collected. Note: this must be unaltered from the point at which
#' screening forms were first generated with \code{screen_list_abst}.
#' @return A \code{data.frame} with the combined results of all screening
#' sheets, plus the publication data from \code{ref_table}
#' @examples
#' \dontrun{
#' screening_list <- screen_list_fulltxt(form_mm_recs, c("you", "me"))
#' screen_list_fulltxt(screening_list, dir = "./")
#' # Full-text articles should be screened at this point
#' combined_screening_forms <- screen_read_fulltxt("./", form_mm_recs)
#' }
#' @export
screen_read_fulltxt <- function(ref_table, dir = "../intermediate_data/") {
    screen_read(ref_table = ref_table, dir = dir, abstract = FALSE)
}

# Helper function
screen_read <- function(ref_table, dir = "../intermediate_data/", abstract) {

    # Decide on prefix of files
    if (abstract) {
        prefix <- "AbstScreener_"
    } else {
        prefix <- "FulltxtScreener_"
    }

    # Fined file names and reviewer names
    files <- paste0(dir, list.files(path = dir, pattern = prefix))
    names <- gsub(paste0("^.*", prefix, "|\\.csv$"), "", files)

    # Decide on which columns to keep
    if (abstract) {
        keeper_cols <- c(names, "UNIQUE_ID")
    } else {
        keeper_cols <-
            apply(expand.grid(names,
                              c("OBTAINABLE", "INCLUDE", "EXCLUSION_RATIONALE")),
                  1, paste, collapse = "_")
        keeper_cols <- c("UNIQUE_ID", keeper_cols)
    }

    # Read all files into a list, combine the lists into a data.frame
    # based on UNIQUE_ID, then combine with the reference table
    return(
        lapply(files,
               function(x) {
                   out <- utils::read.csv(x, stringsAsFactors = FALSE)
                   idx <- which(colnames(out) %in% keeper_cols)
                   return(out[, idx])}) %>%
            purrr::reduce(dplyr::full_join, by = "UNIQUE_ID") %>%
            dplyr::full_join(ref_table, by = "UNIQUE_ID")
    )
}