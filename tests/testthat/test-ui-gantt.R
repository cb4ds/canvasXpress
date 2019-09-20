context("canvasXpress Web Charts - Gantt")


test_that("cXgantt1", {
    check_ui_test(cXgantt1())
})

test_that("cXgantt2", {
    check_ui_test(cXgantt2())
    warning("background coloring for dependencies field in table is red when null, blue when not null, and in web example, is blue when null and red when not null")
})

test_that("cXgantt3", {
    check_ui_test(cXgantt3())
    warning("table colors for dependency field may differ from web example")
})

test_that("cXgantt4", {
    check_ui_test(cXgantt4())
    warning("table colors for dependency field may differ from web example")
})

test_that("cXgantt5", {
    check_ui_test(cXgantt5())
    warning("table colors for dependency fields may differ from web example")
})

test_that("cXgantt6", {
    check_ui_test(cXgantt6())
    warning("table colors for dependency fields may differ from web example")
})
