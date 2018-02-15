context("test-remove_dupes_1way.R")

new <- data.frame(a = c("Apples", "Oranges", "Bananas"),
                  b = c("Granny", "Florida", "Chiquita"),
                  c = c("", "", ""),
                  d = c("", "", ""),
                  stringsAsFactors = FALSE)

old <- data.frame(a = c("Apples", "Oranges"),
                  b = c("Granny", "Florida"),
                  c = c("Red", "Orange"),
                  d = c("Green", "Orange"),
                  stringsAsFactors = FALSE)

expected_out <- data.frame(a = c("Apples", "Oranges", "Bananas"),
                           b = c("Granny", "Florida", "Chiquita"),
                           c = c("Red", "Orange", ""),
                           d = c("Green", "Orange", ""),
                           stringsAsFactors = FALSE)



test_that("One-way duplicate removal works as expected", {
    x <- find_dupes_1way(old, new, c("a", "b"))
    expect_equal(remove_dupes_1way(x), expected_out)
})
