context("canvasXpress data-url")

if (interactive()) {
    test_that("Barplot URL data", {
        result <- canvasXpress(data             = "https://www.canvasxpress.org/data/r/cX-generic-dat.txt",
                               graphType        = "Bar",
                               graphOrientation = "vertical",
                               plotByVariable   = TRUE,
                               smpLabelInterval = 2,
                               smpTextRotate    = 45,
                               smpTitle         = "Sample Title",
                               title            = "Barplot: loading data from URL",
                               xAxis2Show       = FALSE)

        check_ui_test(result)
    })
} else {
    message("Non-interactive data-url tests skipped")
}
