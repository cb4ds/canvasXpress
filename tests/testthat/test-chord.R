context("canvasXpress Charts - Chord")


# -- From Isaac, web examples --
chord.y <- readRDS(system.file("extdata", "cX-chord-dat.RData", package = "canvasXpress"))

test_that("cX-chord-1", {
    result <- canvasXpress(
        data = chord.y,
        circularArc = 360,
        circularRotate = 0,
        circularType = "chord",
        colors = c("#000000", "#FFDD89", "#957244", "#F26223"),
        graphType = "Circular",
        higlightGreyOut = TRUE,
        rAxisTickFormat = c("%sK", "val / 1000"),
        showTransition = TRUE,
        title = "Simple Chord Graph"
    )
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-chord-2", {
    result <- canvasXpress(
        data = chord.y,
        circularArc = 360,
        circularRotate = 180,
        circularType = "chord",
        colors = c("#000000", "#FFDD89", "#957244", "#F26223"),
        graphType = "Circular",
        higlightGreyOut = TRUE,
        rAxisTickFormat = c("%sK", "val / 1000"),
        showTransition = TRUE,
        title = "Rotated Chord Graph"
    )
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-chord-3", {
    result <- canvasXpress(
        data = chord.y,
        circularArc = 180,
        circularRotate = -90,
        circularType = "chord",
        colors = c("#000000", "#FFDD89", "#957244", "#F26223"),
        graphType = "Circular",
        higlightGreyOut = TRUE,
        rAxisTickFormat = c("%sK", "val / 1000"),
        showLegend = FALSE,
        showTransition = TRUE,
        title = "Rotated Half Chord Graph"
    )
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})
