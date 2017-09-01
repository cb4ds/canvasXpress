context("canvasXpress Charts - StackedPercentLine")


generic.y   <- readRDS(system.file("extdata", "cX-generic-dat.RData", package = "canvasXpress"))
generic.x   <- readRDS(system.file("extdata", "cX-generic-smp.RData", package = "canvasXpress"))
generic.z   <- readRDS(system.file("extdata", "cX-generic-var.RData", package = "canvasXpress"))

test_that("cX-stackedpercentline-1", {
    result <- canvasXpress(
        data = generic.y,
        smpAnnot = generic.x,
        varAnnot = generic.z,
        graphOrientation = "vertical",
        graphType = "StackedPercentLine",
        lineThickness = 3,
        lineType = "spline",
        showShadow = TRUE,
        showTransition = TRUE,
        smpTitle = "Collection of Samples",
        smpTitleFontStyle = "italic",
        subtitle = "Random Data",
        title = "Stacked-Line Graphs",
        xAxis = c("Variable1", "Variable2"),
        xAxis2 = c("Variable3", "Variable4")
    )
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-stackedpercentline-2", {
    result <- canvasXpress(
        data = generic.y,
        smpAnnot = generic.x,
        varAnnot = generic.z,
        coordinateLineColor = TRUE,
        graphOrientation = "horizontal",
        graphType = "StackedPercentLine",
        legendBox = FALSE,
        lineThickness = 3,
        lineType = "spline",
        showShadow = TRUE,
        smpTitle = "Collection of Samples",
        smpTitleFontStyle = "italic",
        subtitle = "Random Data",
        title = "Stacked-Line Graphs",
        xAxis = c("Variable1", "Variable2"),
        xAxis2 = c("Variable3", "Variable4")
    )
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})
