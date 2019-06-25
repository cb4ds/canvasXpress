context("canvasXpress Web Charts - Bubble")


test_that("cXbubble1", {
    check_ui_test(cXbubble1())
})

test_that("cXbubble2", {
    check_ui_test(cXbubble2())
    warning("missing order legend box as in web example")
})

test_that("cXbubble3", {
    check_ui_test(cXbubble3())
})
