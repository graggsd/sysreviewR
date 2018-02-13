context("test-remove_duplicates.R")
test_dat <- form_mm_recs[1:5, c(1, 3, 4)]

test_dat_2 <- rbind(test_dat, test_dat)
test_dat_2[1, 1] <- paste0(test_dat_2[1, 1], "sadhfhkjkjhsdfhkjkhjsdf")
test_dat_2[9, 1] <- sub("dexamethasone", "dexamethasign", test_dat_2[9, 1])
test_dat_2[5, 1] <- "purple_rain"
test_dat_2[10, 1] <- "purple_raining"
test_dat_2_match <- add_matching_col(test_dat_2, "TITLE", TRUE)

test_that("fuzzy_matching is appropriately reponsive to different string distances", {

    expect_equal(fuzzy_matching(test_dat_2_match, 10)$match_ID,
                 c("m1", "m2", "m3", "m4", "m5",
                   "m6", "m2", "m3", "m4", "m5"))
    expect_equal(fuzzy_matching(test_dat_2_match, 1)$match_ID,
                 c("m1", "m2", "m3", "m4", "m5",
                   "m6", "m2", "m3", "m9", "m10"))

})

test_that("strict_matching works as expected", {
    expect_equal(strict_matching(test_dat_2_match)$match_ID,
                 c("m1", "m2", "m3", "m4", "m5",
                   "m6", "m2", "m3", "m9", "m10"))
})

test_that("protect_min_length works for single columns", {
    cols <- "TITLE"
    expect_equal(sum(is.na(protect_min_length(test_dat_2_match,
                                              cols, 1000)[, "match_ID"])),
                 0)
    expect_equal(sum(is.na(protect_min_length(test_dat_2_match,
                                              cols, 1)[, "match_ID"])),
                 10)
})

test_that("protect_min_length works for multiple columns", {
    cols <- c("TITLE", "FIRSTAUTHLN")
    match_2_cols <- add_matching_col(test_dat, cols, TRUE)
    expect_equal(sum(is.na(protect_min_length(match_2_cols,
                                              cols, c(1, 1))[, "match_ID"])),
                 5)
    expect_equal(sum(is.na(protect_min_length(match_2_cols,
                                              cols, c(1000, 1))[, "match_ID"])),
                 0)
    expect_equal(sum(is.na(protect_min_length(match_2_cols,
                                              cols, c(1, 1000))[, "match_ID"])),
                 0)
    expect_equal(sum(is.na(protect_min_length(match_2_cols,
                                              cols, c(1000, 1000))[, "match_ID"])),
                 0)
})


