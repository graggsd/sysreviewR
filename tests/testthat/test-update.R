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

expected_out <- data.frame(a = c("Apples", "Oranges", "Bananas"),
                           b = c("Granny", "Florida", "Chiquita"),
                           c = c("Red", "", ""),
                           d = c("Green", "", ""),
                           stringsAsFactors = FALSE)

test_that("exact matching works appropriately", {

    expect_equal(update_dataframe(empty, update, c("a", "b")),
                 expected_out)

})

