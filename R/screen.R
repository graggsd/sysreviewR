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
#' screening_list <- screen_abstract_list(form_mm_recs, c("you", "me"))
#' }
#' @export
screen_abstract_list <- function(x,
                                 reviewers,
                                 title_col = "TITLE",
                                 abstract_col = "ABSTRACT") {
    UseMethod("screen_abstract_list")
}

#' @export
screen_abstract_list.default <- function(x, ...) {
    stop("x must be of class data.frame")
}

#' @export
screen_abstract_list.data.frame <- function(x,
                                          reviewers,
                                          title_col = "TITLE",
                                          abstract_col = "ABSTRACT") {

    base_sheet <- x[, c("UNIQUE_ID", title_col, abstract_col)]
    idx <- na.omit(match(c(title_col, abstract_col), colnames(base_sheet)))
    colnames(base_sheet)[idx] <- c("TITLE", "ABSTRACT")
    out <- lapply(reviewers,
                  function(x) {
                      base_sheet[, x] <- "not vetted"
                      return(base_sheet)
                  })
    names(out) <- reviewers
    return(out)
}

#' Create .csv files for abstract screening
#'
#' Takes a the output of \code{screen_abstract_list} and creates a series of
#' .csv files that may be distributed to the individuals that will screen
#' abstracts
#'
#' @param x The list created by \code{screen_abstract_list}
#' @param dir The desired directory into which .csv files will be placed
#' @return NULL
#' @examples
#' \dontrun{
#' screening_list <- screen_abstract_list(form_mm_recs, c("you", "me"))
#' screen_write(screening_list, dir = "./")
#' }
#' @export
screen_write <- function(x, dir = "../intermediate_data/") {
    for (reviewer in names(x)) {
        write.csv(x[[reviewer]],
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
#' screening forms were first generated with \code{screen_abstract_list}.
#' @return A \code{data.frame} with the combined results of all screening
#' sheets, plus the publication data from \code{ref_table}
#' @examples
#' \dontrun{
#' screening_list <- screen_abstract_list(form_mm_recs, c("you", "me"))
#' screen_write(screening_list, dir = "./")
#' # Abstracts should be screened at this point
#' combined_screening_forms <- screen_read("./", form_mm_recs)
#' }
#' @export
screen_read <- function(dir = "../intermediate_data/", ref_table) {
    files <- paste0(dir, list.files(path = dir, pattern = "AbstScreener"))
    abs_scr_list <-
        lapply(files,
               function(x) {
                   out <- read.csv(x, stringsAsFactors = FALSE)
                   idx <- which(colnames(out) %in% c("TITLE", "ABSTRACT"))
                   out <- out[, -idx]
               })
    return(
        purrr::reduce(abs_scr_list, dplyr::full_join, by = "UNIQUE_ID") %>%
            dplyr::full_join(ref_table, by = "UNIQUE_ID") %>%
            dplyr::select(-SCREENING_ID)
    )
}
