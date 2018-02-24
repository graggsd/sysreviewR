#' @export
bibs_extract_titles <- function(x) {
    x <- x$CR
    x <- strsplit(x, split = "; ")
    return(lapply(x, extract_title))
}

title_start <- function(x){
    # Find the start point in a string that ends with "., ", then has a series
    # of characters lacking "., ", then ends with "(".
    return(regexpr("\\.\\,\\s((?!\\.\\,).)*\\(", x, perl = TRUE)  + 3)
}

title_stop <- function(x){
    # Find the position in the string where the date begins
    return(regexpr("\\([0-9]{4}\\)", x) - 2)
}

extract_title <- function(x) {
    substring(x, title_start(x), title_stop(x))
}

