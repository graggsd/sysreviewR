context("test-remove_dupes_1way.R")

test_that("One-way duplicate removal works as expected", {
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

    expected_out <- data.frame(a = c("Apples", "Oranges", "Apples", "Bananas"),
                               b = c("Granny", "Florida", "Granny", "Chiquita"),
                               c = c("Red", "Orange", "", ""),
                               d = c("Green", "Orange", "", ""),
                               stringsAsFactors = FALSE)

    x <- find_dupes_1way(old, new, c("a", "b"), min_length = c(7, 1))
    expect_equivalent(remove_dupes_1way(x), expected_out)
})
