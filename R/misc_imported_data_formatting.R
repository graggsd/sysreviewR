# -------------------------------------------------------------------------
# To create a shorthand name for the publication based on first author name,
# initial, and year of publication
add_pubname <- function(x) {
    x[, "PUB"] <- paste0(x[, "FIRSTAUTHLN"],
                         " ",
                         x[, "FIRSTAUTHINIT"],
                         ", ",
                         x[, "YEAR"])
    return(x)
}
