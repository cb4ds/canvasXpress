context("canvasXpress Web Charts - Treemap")


test_that("cXtreemap1", {
    check_ui_test(cXtreemap1())
    fail("Ordering of boxes is different")
})

test_that("cXtreemap2", {
    check_ui_test(cXtreemap2())
    fail("Ordering of boxes is different")
})

test_that("cXtreemap3", {
    check_ui_test(cXtreemap3())
    fail("Ordering of boxes is different")
})
