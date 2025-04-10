context("canvasXpress Web Charts - Map")


if (interactive()) {
    test_that("cXmap1", {
        check_ui_test(cXmap1())
    })

    test_that("cXmap2", {
        check_ui_test(cXmap2())
    })

    test_that("cXmap3", {
        check_ui_test(cXmap3())
    })

    test_that("cXmap4", {
        check_ui_test(cXmap4())
    })

    test_that("cXmap5", {
        check_ui_test(cXmap5())
    })

    test_that("cXmap6", {
        check_ui_test(cXmap6())
    })

    test_that("cXmap7", {
        check_ui_test(cXmap7())
    })

    # the test generates "Row names in smpAnnot are different from column names in data"
    # It won't be fixed by Issac at this time
    # It is commented in CX v55.8
    # test_that("cXmap8", {
    #     check_ui_test(cXmap8())
    # })

    test_that("cXmap9", {
        check_ui_test(cXmap9())
    })

    test_that("cXmap10", {
        check_ui_test(cXmap10())
    })

    test_that("cXmap11", {
        check_ui_test(cXmap11())
    })

    test_that("cXmap12", {
        check_ui_test(cXmap12())
    })

    test_that("cXmap13", {
        check_ui_test(cXmap13())
    })

    test_that("cXmap14", {
        check_ui_test(cXmap14())
    })

    test_that("cXmap15", {
        check_ui_test(cXmap15())
    })

    # the test generates "duplicate 'row.names' are not allowed"
    # It won't be fixed by Issac at this time
    # It is commented in CX v55.8
    # test_that("cXmap16", {
    #     check_ui_test(cXmap16())
    # })
    #
    # test_that("cXmap17", {
    #     check_ui_test(cXmap17())
    # })
} else {
    message("Non-interactive ui-map tests skipped")
}
