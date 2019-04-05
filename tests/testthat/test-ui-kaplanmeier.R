context("canvasXpress Web Charts - Kaplanmeier")


test_that("cXkaplanmeier1", {
    check_ui_test(cXkaplanmeier1())
    fail("plot doesn't highlight when hovered over")
})

test_that("cXkaplanmeier2", {
    check_ui_test(cXkaplanmeier2())
    fail("plot does not select when hovered over")
})

test_that("cXkaplanmeier3", {
    check_ui_test(cXkaplanmeier3())
})

test_that("cXkaplanmeier4", {
    check_ui_test(cXkaplanmeier4())
})

test_that("cXkaplanmeier5", {
    check_ui_test(cXkaplanmeier5())

    warning("The subcharts are in a different layout order compared to web example")
    warning("Colors for drug are different, two pieces are green, but one should be orange; in Clin4, green portion should be orange")
})
