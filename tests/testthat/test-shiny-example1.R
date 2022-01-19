context("shiny_example1")

library(shinytest)

test_that("shiny example 1 - initial_state", {
    skip_on_cran()
    appdir <- system.file(package = "canvasXpress", "shiny-examples/example1")
    expect_pass(testApp(appdir, "initial_state", compareImages = FALSE))
})

test_that("shiny example 1 - basic_select_only", {
    skip_on_cran()
    appdir <- system.file(package = "canvasXpress", "shiny-examples/example1")
    expect_pass(testApp(appdir, "basic_select_only", compareImages = FALSE))
})

test_that("shiny example 1 - select_and_coloring_only", {
    skip_on_cran()
    appdir <- system.file(package = "canvasXpress", "shiny-examples/example1")
    expect_pass(testApp(appdir, "select_and_coloring_only", compareImages = FALSE))
})

test_that("shiny example 1 - select_and_shaping_only", {
    skip_on_cran()
    appdir <- system.file(package = "canvasXpress", "shiny-examples/example1")
    expect_pass(testApp(appdir, "select_and_shaping_only", compareImages = FALSE))
})

test_that("shiny example 1 - coloring_and_shaping", {
    skip_on_cran()
    appdir <- system.file(package = "canvasXpress", "shiny-examples/example1")
    expect_pass(testApp(appdir, "coloring_and_shaping", compareImages = FALSE))
})
