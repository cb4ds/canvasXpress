context("canvasXpress Web Charts - ParallelCoordinates")


test_that("cXparallelcoordinates1", {
    check_ui_test(cXparallelcoordinates1())
    fail("Lines don't highlight when hovering over")
})

test_that("cXparallelcoordinates2", {
    check_ui_test(cXparallelcoordinates2())
})

