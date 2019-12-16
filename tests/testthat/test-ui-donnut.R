context("canvasXpress Web Charts - Donnut")


test_that("cXdonut1", {
    check_ui_test(cXdonnut1())

    warning("Hover effect results in a tooltip value being displayed at the center if the plot is not resized.")
})

test_that("cXdonut2", {
    check_ui_test(cXdonnut2())

    warning("Hover effect results in a tooltip value being displayed at the center if the plot is not resized.")
})
