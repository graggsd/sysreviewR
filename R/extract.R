
#' @export
extract_studychar <- function(chars, ref_tbl, ref_tbl_cols = c("TITLE", "PUB")) {
    ref_tbl <- ref_tbl[, c("UNIQUE_ID", ref_tbl_cols)]
    ref_tbl[, chars] <- "not vetted"
    return(ref_tbl)
}

#' @export
extract_findings <- function(chars) {
    out <- data.frame(as.matrix(rep("not vetted", length(chars)), nrow = 1),
                      stringsAsFactors = FALSE)
    colnames(out) <- chars
    return(out)
}


