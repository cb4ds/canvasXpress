context("shiny_example1")

library(shinytest)

test_that("shiny example 1 - initial_state", {
    skip_on_cran()
    appdir <- system.file(package = "canvasXpress", "shiny-examples/example1")
    expect_pass(testApp(appdir, "initial_state", compareImages = FALSE))
})

test_that("shiny example 1 - coloring_petal_length", {
    skip_on_cran()
    appdir <- system.file(package = "canvasXpress", "shiny-examples/example1")
    expect_pass(testApp(appdir, "coloring_petal_length", compareImages = FALSE))
})

test_that("shiny example 1 - coloring_and_shaping", {
    skip_on_cran()
    appdir <- system.file(package = "canvasXpress", "shiny-examples/example1")
    expect_pass(testApp(appdir, "coloring_and_shaping", compareImages = FALSE))
})
