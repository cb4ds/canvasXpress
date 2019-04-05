context("canvasXpress Web Charts - Chord")


test_that("cXchord1", {
    check_ui_test(cXchord1())
    fail("Legend is missing")

})

test_that("cXchord2", {
    check_ui_test(cXchord2())
})

test_that("cXchord3", {
    check_ui_test(cXchord3())
})
