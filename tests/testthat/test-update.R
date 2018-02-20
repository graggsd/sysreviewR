context("test-update.R")

empty <- data.frame(a = c("Apples", "Oranges", "Bananas"),
                    b = c("Granny", "Florida", "Chiquita"),
                    c = c("", "", ""),
                    d = c("", "", ""),
                    stringsAsFactors = FALSE)
update <- data.frame(a = c("Apples", "Oranges"),
                     b = c("Granny", "Florida"),
                     c = c("Red", ""),
                     d = c("Green", ""),
                     stringsAsFactors = FALSE)

test_that("update_data exact matching works appropriately", {
    expected_out <- data.frame(a = c("Apples", "Oranges", "Bananas"),
                               b = c("Granny", "Florida", "Chiquita"),
                               c = c("Red", "", ""),
                               d = c("Green", "", ""),
                               stringsAsFactors = FALSE)
    update2 <- data.frame(a = c("Applesss", "Oranges"),
                         b = c("Granny", "Florida"),
                         c = c("Red", ""),
                         d = c("Green", ""),
                         stringsAsFactors = FALSE)
    expected_out2 <- data.frame(a = c("Apples", "Oranges", "Bananas"),
                                b = c("Granny", "Florida", "Chiquita"),
                                c = c("", "", ""),
                                d = c("", "", ""),
                                stringsAsFactors = FALSE)

    expect_equal(update_data(empty, update, c("a", "b"), min_length = 5),
                 expected_out)
    expect_equal(update_data(empty, update2, c("a", "b"), min_length = 5),
                 expected_out2)

})

test_that("update_data protects short matching strings", {
    expected_out <- data.frame(a = c("Apples", "Oranges", "Bananas"),
                               b = c("Granny", "Florida", "Chiquita"),
                               c = c("", "", ""),
                               d = c("", "", ""),
                               stringsAsFactors = FALSE)

    expect_equal(update_data(empty, update, c("a", "b"), min_length = 13),
                 expected_out)

})


test_that("update_data approximate matching works appropriately", {
    expected_out <- data.frame(a = c("Apples", "Oranges", "Bananas"),
                               b = c("Granny", "Florida", "Chiquita"),
                               c = c("Red", "", ""),
                               d = c("Green", "", ""),
                               stringsAsFactors = FALSE)
    update2 <- data.frame(a = c("Applesss", "Oranges"),
                          b = c("Granny", "Florida"),
                          c = c("Red", ""),
                          d = c("Green", ""),
                          stringsAsFactors = FALSE)
    expected_out2 <- data.frame(a = c("Apples", "Oranges", "Bananas"),
                               b = c("Granny", "Florida", "Chiquita"),
                               c = c("Red", "", ""),
                               d = c("Green", "", ""),
                               stringsAsFactors = FALSE)

    # Works for exact matches
    expect_equal(update_data(empty = empty,
                             populated = update,
                             match_cols = c("a", "b"),
                             min_length = 5,
                             replace_cols = c("c", "d"),
                             approx_match = TRUE,
                             string_dist = 1),
                 expected_out)

    # Works for inexact matches
    expect_equal(update_data(empty = empty,
                             populated = update2,
                             match_cols = c("a", "b"),
                             min_length = 5,
                             replace_cols = c("c", "d"),
                             approx_match = TRUE,
                             string_dist = 3),
                 expected_out)

    # Doesn't match when string distances is longer than assigned
    expect_equal(update_data(empty = empty,
                             populated = update2,
                             match_cols = c("a", "b"),
                             min_length = 5,
                             replace_cols = c("c", "d"),
                             string_dist = 1,
                             approx_match = TRUE),
                 expected_out2)

})