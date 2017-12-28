

format_Scopus <- function(x) {

    # Change NA's to blanks
    x[is.na(x)] <- ""

    # Authors -----------------------------------------------
    auth_idx <- grep("Authors", colnames(x), ignore.case = TRUE)
    if (length(auth_idx) == 1) {
        # Remove instances of ", Jr." from the list of names
        x[, auth_idx] <- gsub("\\, Jr\\.", "", x[, auth_idx])
        # Change no author names to blanks
        x[x[, auth_idx] == "[No author name available]", auth_idx] <- ""
        # Add columns that specify author last names and initials
        x <- cbind(x,
                   auth_matrix_scopus(x, auth_idx))
    }

    # Abstracts -----------------------------------------------
    abst_idx <- grep("Abstract", colnames(x), ignore.case = TRUE)
    if (length(abst_idx) == 1) {
        # Change no author names to blanks
        x[x[, abst_idx] == "[No abstract available]", abst_idx] <- ""
        # Change to upper case
        colnames(x)[abst_idx] <- "ABSTRACT"
    }

    # Titles -----------------------------------------------
    title_idx <- grep("Title\\>", colnames(x), ignore.case = TRUE)
    if (length(title_idx) == 1) {
        # Change to upper case
        colnames(x)[title_idx] <- "TITLE"
    }

    # Year -----------------------------------------------
    year_idx <- grep("Year", colnames(x), ignore.case = TRUE)
    if (length(year_idx) == 1) {
        # Change to upper case
        colnames(x)[year_idx] <- "YEAR"
    }

    # Source -----------------------------------------------
    source_idx <- grep("Source.title", colnames(x), ignore.case = TRUE)
    if (length(source_idx) == 1) {
        # Change to upper case
        colnames(x)[source_idx] <- "SOURCE"
    }

    # Volume -----------------------------------------------
    vol_idx <- grep("Volume", colnames(x), ignore.case = TRUE)
    if (length(vol_idx) == 1) {
        # Change to upper case
        colnames(x)[vol_idx] <- "Volume"
    }

    # Issue -----------------------------------------------
    iss_idx <- grep("Issue", colnames(x), ignore.case = TRUE)
    if (length(iss_idx) == 1) {
        # Change to upper case
        colnames(x)[iss_idx] <- "ISSUE"
    }

    # Pages -----------------------------------------------
    ps_idx <- grep("Page.start", colnames(x), ignore.case = TRUE)
    pe_idx <- grep("Page.end", colnames(x), ignore.case = TRUE)
    if (length(ps_idx) == 1 & length(pe_idx) == 1) {
        # Create PAGES column
        x[, "PAGES"] <- paste0(x[, ps_idx], "-", x[, pe_idx])
        # If there is a leading or lagging "-", remove it
        idx <- grepl("^-|-$", x[, "PAGES"])
        if (length(idx > 0)) {
            x[idx, "PAGES"] <- sub("^-|-$", "", x[idx, "PAGES"])
        }
    }

    # Source -----------------------------------------------
    source_idx <- grep("Source", colnames(x), ignore.case = TRUE)
    if (length(source_idx) == 1) {
        # Change name
        colnames(x)[source_idx] <- "DATABASE"
    }

    return(add_pubname(x))

}

# ----------------------------------------------------------------------------
# Helpers to get first name and initials

# Input is a vector of names with the format "Last Init.Init."

# Get last name and initial of nth author
ln_init_scopus <- function(x, idx) {
    # Divide based on spaces
    tmp <- strsplit(x, " ")[[idx]]
    # If last chunk contains initials, set those aside as initials and
    # everything else is interpreted as the last name
    len_tmp <- length(tmp)
    if (len_tmp > 1 & grepl("\\.", tmp[len_tmp])) {
        LN <- paste(tmp[-len_tmp], collapse = " ")
        INIT <- gsub("\\.|\\-", "", tmp[len_tmp])
    } else {
        LN <- x[idx]
        INIT <- ""
    }
    return(c(LN, INIT))
}


# -----------------------------------------------------------------------------
# Helper to make vector of first and last author last name and initials

# Input is a string of names with the format "Last Init.Init.," where the ", "
# separates author names

fl_auth_scopus <- function(x) {
    # Check that length is not zero
    if (nchar(x) == 0) {
        first <- c("", "")
        last <- c("", "")
    } else {
        # # Separate by ", "
        tmp <- strsplit(x, ", ")[[1]]
        # Get ln and init of first auth
        first <- ln_init_scopus(tmp, 1)
        # Get ln and init of last auth if more than one auth exists
        last_idx <- length(tmp)
        if (last_idx > 1) {
            last <- ln_init_scopus(tmp, last_idx)
        } else {
            last <- c("", "")
        }
    }

    out <- c(first, last)
    names(out) <- c("FIRSTAUTHLN",
                    "FIRSTAUTHINIT",
                    "LASTAUTHLN",
                    "LASTAUTHINIT")
    return(out)
}

# -----------------------------------------------------------------------------

# Helper to make matrix of first and last author last name and initials
# Input: is data.frame created from a manual download of a scopus query
# Output: a matrix with first and last author last names and initials in four
# columns
auth_matrix_scopus <- function(x, idx) {
    x[, idx] <- pre_edit_auth_nms_scopus(x[, idx])
    return(t(sapply(x[, idx], fl_auth_scopus)))
}
