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

test_that("approx_match == FALSE, matches exactly", {
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

    expect_equivalent(
        update_data(empty = empty,
                    populated = update,
                    match_cols = c("a", "b"),
                    replace_cols = c("c", "d"),
                    min_length = 5),
        expected_out)

    expect_equal(update_data(empty = empty,
                             populated = update2,
                             match_cols = c("a", "b"),
                             replace_cols = c("c", "d"),
                             min_length = 5),
                 expected_out2)

})

test_that("match_cols and replace_cols can be colnames or indices", {

    expect_equivalent(
        update_data(empty = empty,
                    populated = update,
                    match_cols = 1:2,
                    replace_cols = c("c", "d"),
                    min_length = 5),
        update_data(empty = empty,
                    populated = update,
                    match_cols = c("a", "b"),
                    replace_cols = 3:4,
                    min_length = 5),
        update_data(empty = empty,
                    populated = update,
                    match_cols = 1:2,
                    replace_cols = 3:4,
                    min_length = 5))

})


test_that("match_cols and replace_cols may be specified separately for empty and populated", {
    update2 <- update
    colnames(update2) <- c("d", "c", "b", "a")

    expect_equivalent(update_data(empty = empty,
                                  populated = update2,
                                  match_cols = list(c("a", "b"), c("d", "c")),
                                  replace_cols = 3:4,
                                  min_length = 5),
                      update_data(empty = empty,
                                  populated = update2,
                                  match_cols = list(1:2, 1:2),
                                  replace_cols = 3:4,
                                  min_length = 5),
                      update_data(empty = empty,
                                  populated = update2,
                                  match_cols = 1:2,
                                  replace_cols = 3:4,
                                  min_length = 5))

    expect_equivalent(update_data(empty = empty,
                                  populated = update2,
                                  match_cols = 1:2,
                                  replace_cols = list(3:4, 3:4),
                                  min_length = 5),
                      update_data(empty = empty,
                                  populated = update2,
                                  match_cols = 1:2,
                                  replace_cols = list(c("c", "d"), c("b", "a")),
                                  min_length = 5),
                      update_data(empty = empty,
                                  populated = update2,
                                  match_cols = 1:2,
                                  replace_cols = 3:4,
                                  min_length = 5))

})

test_that("update_data protects short matching strings", {
    expected_out <- data.frame(a = c("Apples", "Oranges", "Bananas"),
                               b = c("Granny", "Florida", "Chiquita"),
                               c = c("", "", ""),
                               d = c("", "", ""),
                               stringsAsFactors = FALSE)

    expect_equal(update_data(empty = empty,
                             populated = update,
                             match_cols = c("a", "b"),
                             replace_cols = c("c", "d"),
                             min_length = 13),
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
                               c = c("", "", ""),
                               d = c("", "", ""),
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
                             min_length = 13,
                             replace_cols = c("c", "d"),
                             string_dist = 3,
                             approx_match = TRUE),
                 expected_out2)

})

