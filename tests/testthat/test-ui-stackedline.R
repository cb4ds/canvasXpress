context("canvasXpress Web Charts - StackedLine")


test_that("cXstackedline1", {
    check_ui_test(cXstackedline1())
    fail("Variable3 and Variable4 do not highlight when hovered over")
})

test_that("cXstackedline2", {
    check_ui_test(cXstackedline2())
})
