context("test-fuzzy.R")

test_that("fuzzy_unique does exact matching when string_dist = 0", {
    strings <- c("Apples", "Oranges", "Bananas", "Apple")
    expect_equivalent(fuzzy_unique(strings, string_dist = 0),
                      strings)
})

test_that("fuzzy_unique eliminates close matches", {
    strings <- c("Apples", "Oranges", "Bananas", "Apple")
    expect_equivalent(fuzzy_unique(strings,
                                   string_dist = 2,
                                   protect_length = 2),
                      strings[-4])

    bat_ipsum1 <- c("Accomplice? I'm gonna tell them the whole thing was your idea.",
                   "I seek the means to fight injustice. To turn fear against those who prey on the fearful.",
                   "Well, you see... I'm buying this hotel and setting some new rules about the pool area.")
    bat_ipsum2 <- c("Accomplice? I'm gonna tell them the whole thing was your idea.",
                    "I seek the means to fight injustice. To turn fear against those who prey on the fearful.",
                    "Well, you see... I'm buying this motel and writing some new rules about the pool area.")

    expect_equivalent(fuzzy_unique(c(bat_ipsum1, bat_ipsum2)),
                      bat_ipsum1)

})

test_that("fuzzy_unique protects short strings", {
    strings <- c("", "Oranges", "Bananas")
    expect_equivalent(fuzzy_unique(c(strings, strings),
                                   string_dist = 0,
                                   protect_length = 3),
                      c(strings, ""))
})

test_that("fuzzy_in does exact matching when string_dist = 0", {
    strings1 <- c("Apples", "Oranges", "Bananas")
    strings2 <- c("Apple", "Oranges", "Bananas")
    expect_equivalent(fuzzy_in(strings1, strings2, string_dist = 0),
                      c(FALSE, TRUE, TRUE))
})

test_that("fuzzy_in does fuzzy matching when string_dist != 0", {
    strings1 <- c("Apples", "Oranges", "Bananas")
    strings2 <- c("Apple", "Oranges", "Bananas")
    expect_equivalent(fuzzy_in(strings1, strings2,
                               string_dist = 2, protect_length = 2),
                      c(TRUE, TRUE, TRUE))

    bat_ipsum1 <- c("Accomplice? I'm gonna tell them the whole thing was your idea.",
                    "I seek the means to fight injustice. To turn fear against those who prey on the fearful.",
                    "Well, you see... I'm buying this hotel and setting some new rules about the pool area.")
    bat_ipsum2 <- c("Accomplice? I'm gonna tell them the whole thing was your idea.",
                    "I seek the means to fight injustice. To turn fear against those who prey on the fearful.",
                    "Well, you see... I'm buying this motel and writing some new rules about the pool area.")
    expect_equivalent(fuzzy_in(bat_ipsum1, bat_ipsum2),
                      c(TRUE, TRUE, TRUE))
})

test_that("fuzzy_in protects short strings", {
    strings <- c("", "Oranges", "Bananas")
    expect_equivalent(fuzzy_in(strings,
                               strings,
                               string_dist = 0,
                               protect_length = 3),
                      c(FALSE, TRUE, TRUE))
})