context("shiny_example1")

library(shinytest)

test_that("shiny example 1", {
    skip_on_cran()
    appdir <- system.file(package = "canvasXpress", "shiny-examples/example1")
    expect_pass(testApp(appdir, compareImages = FALSE))
})
