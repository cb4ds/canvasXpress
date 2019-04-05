context("canvasXpress Web Charts - Heatmap")


test_that("cXheatmap1", {
    check_ui_test(cXheatmap1())
})

test_that("cXheatmap2", {
    check_ui_test(cXheatmap2())
})

test_that("cXheatmap3", {
    check_ui_test(cXheatmap3())
    fail("In overlay, dose is 0 on online version, and NaN here")
})

test_that("cXheatmap4", {
    check_ui_test(cXheatmap4())
    fail("In overlay, dose is 0 on online version, and NaN here; colors don't match for low doses, pink instead of purple")

})

test_that("cXheatmap5", {
    check_ui_test(cXheatmap5())
    fail("In overlay, colors don't match for low doses, pink instead of purple")

})

test_that("cXheatmap6", {
    check_ui_test(cXheatmap6())
    fail("In overlay, colors don't match for dose, sample13probe26 is red here and maroon online")

})

test_that("cXheatmap7", {
    check_ui_test(cXheatmap7())
    fail("In overlay, colors don't match for dose, sample8probe20 is blue here, dark blue online")
})

test_that("cXheatmap8", {
    check_ui_test(cXheatmap8())
    fail("In overlay, colors don't match for dose, sample7probe32 is pink here, purple online")

})

test_that("cXheatmap9", {
    check_ui_test(cXheatmap9())
    fail("In overlay, colors don't match for dose, sample15probe33 is yellow here, orange online")

})

test_that("cXheatmap10", {
    check_ui_test(cXheatmap10())
})

test_that("cXheatmap11", {
    check_ui_test(cXheatmap11())
})

test_that("cXheatmap12", {
    check_ui_test(cXheatmap12())

    message('middle section too small at default height - working as designed')
})

