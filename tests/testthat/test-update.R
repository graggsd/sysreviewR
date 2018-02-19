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
