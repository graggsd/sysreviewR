context("test-dupes.R")

bat_ipsum1 <-
    data.frame(TITLE = c("Accomplice? I'm gonna tell them the whole thing was your idea.",
                         "I seek the means to fight injustice. To turn fear against those who prey on the fearful.",
                         "Well, you see... I'm buying this hotel and setting some new rules about the pool area."),
               AUTHOR = c("Batman", "Bruce", "Mr Wayne"),
               DATABASE = rep("Batman Ipsum 1", 3),
               stringsAsFactors = FALSE)

test_that("approx_match == FALSE produces exact matching", {
    btc1 <- data.frame(TITLE = c("Bread", "Tree", "Car"),
                       AUTHOR = c("Fred", "Ted", "Ed"),
                       stringsAsFactors = FALSE)
    btc2 <- data.frame(TITLE = c("Bread", "Tree", "Car"),
                       AUTHOR = c("Fred", "Ted", "Eddy"),
                       stringsAsFactors = FALSE)
    expected_out <- data.frame(UNIQUE_ID = 1:4,
                               TITLE = c("Bread", "Tree", "Car", "Car"),
                               AUTHOR = c("Fred", "Ted", "Ed", "Eddy"),
                               stringsAsFactors = FALSE)
    expect_equivalent(dupes_rm(dupes_find(x = rbind(btc1, btc2),
                                          match_cols = c("TITLE", "AUTHOR"),
                                          min_length = 5)),
                      expected_out)
})


test_that("approx_match == TRUE produces exact matching", {
    bat_ipsum1 <-
        data.frame(TITLE = c("Accomplice? I'm gonna tell them the whole thing was your idea.",
                             "I seek the means to fight injustice. To turn fear against those who prey on the fearful.",
                             "Car"),
                   AUTHOR = c("Batman", "Bruce", "Ed"),
                   stringsAsFactors = FALSE)

    expect_equivalent(dupes_rm(dupes_find(x = rbind(bat_ipsum1, bat_ipsum1),
                                          match_cols = c("TITLE", "AUTHOR"),
                                          approx_match = FALSE,
                                          string_dist = 3,
                                          min_length = 1)),
                      data.frame(UNIQUE_ID = 1:3,
                                 bat_ipsum1,
                                 stringsAsFactors = FALSE))
})

test_that("min_length protects short strings", {
    bat_ipsum1 <-
        data.frame(TITLE = c("Accomplice? I'm gonna tell them the whole thing was your idea.",
                             "I seek the means to fight injustice. To turn fear against those who prey on the fearful.",
                             "Car"),
                   AUTHOR = c("Batman", "Bruce", "Ed"),
                   stringsAsFactors = FALSE)

    expect_equivalent(dupes_rm(dupes_find(x = rbind(bat_ipsum1, bat_ipsum1),
                                          match_cols = c("TITLE", "AUTHOR"),
                                          approx_match = FALSE,
                                          string_dist = 3,
                                          min_length = 1)),
                      data.frame(UNIQUE_ID = 1:3,
                                 bat_ipsum1,
                                 stringsAsFactors = FALSE))

    expect_equivalent(dupes_rm(dupes_find(x = rbind(bat_ipsum1, bat_ipsum1),
                                          match_cols = c("TITLE", "AUTHOR"),
                                          approx_match = FALSE,
                                          string_dist = 3,
                                          min_length = 6)),
                      dupes_rm(dupes_find(x = rbind(bat_ipsum1, bat_ipsum1),
                                          match_cols = c("TITLE", "AUTHOR"),
                                          approx_match = TRUE,
                                          string_dist = 3,
                                          min_length = 6)),
                      data.frame(UNIQUE_ID = 1:4,
                                 rbind(bat_ipsum1,
                                       data.frame(TITLE = "Car",
                                                  AUTHOR = "Ed",
                                                  stringsAsFactors = FALSE)),
                                 stringsAsFactors = FALSE))
})



