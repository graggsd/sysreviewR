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
#' screen_write(screening_list, dir = "./")
#' }
#' @export
screen_write <- function(x, dir = "../intermediate_data/") {
    for (reviewer in names(x)) {
        utils::write.csv(x[[reviewer]],
                  file = paste0(dir, "AbstScreener_", reviewer, ".csv"),
                  row.names = FALSE)
    }
}

#' Reads completed abstract screening .csv
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
#' screen_write(screening_list, dir = "./")
#' # Abstracts should be screened at this point
#' combined_screening_forms <- screen_read("./", form_mm_recs)
#' }
#' @export
screen_read <- function(dir = "../intermediate_data/", ref_table) {
    files <- paste0(dir, list.files(path = dir, pattern = "AbstScreener"))
    names <- gsub("^.*AbstScreener_|\\.csv$", "", files)
    keeper_cols <-
        apply(expand.grid(names,
                          c("OBTAINABLE", "INCLUDE", "EXCLUSION_RATIONALE")),
              1, paste, collapse = "_")
    keeper_cols <- c(names, "UNIQUE_ID", keeper_cols)
    abs_scr_list <-
        lapply(files,
               function(x) {
                   out <- utils::read.csv(x, stringsAsFactors = FALSE)
                   idx <- which(colnames(out) %in% keeper_cols)
                   out <- out[, idx]
               })
    return(
        purrr::reduce(abs_scr_list, dplyr::full_join, by = "UNIQUE_ID") %>%
            dplyr::full_join(ref_table, by = "UNIQUE_ID")
    )
}
