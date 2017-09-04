context("canvasXpress Charts - ParallelCoordinates")


irist.y <- readRDS(system.file("extdata", "cX-irist-dat.RData", package = "canvasXpress"))
irist.z <- readRDS(system.file("extdata", "cX-irist-var.RData", package = "canvasXpress"))

test_that("cX-parallelcoordinates-1", {
    result <- canvasXpress(
        data = irist.y,
        varAnnot = irist.z,
        colorBy = "Species",
        graphOrientation = "vertical",
        graphType = "ParallelCoordinates",
        lineDecoration = FALSE,
        showTransition = TRUE,
        smpLabelRotate = 90,
        title = "Iris flower data set"
    )
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-parallelcoordinates-2", {
    result <- canvasXpress(
        data = irist.y,
        varAnnot = irist.z,
        colorBy = "Species",
        graphOrientation = "vertical",
        graphType = "ParallelCoordinates",
        lineDecoration = FALSE,
        smpLabelRotate = 90,
        title = "Iris flower data set"
    )
    warning('parallelcoordinates - graph incorrect - Missing Species (far right)')
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

