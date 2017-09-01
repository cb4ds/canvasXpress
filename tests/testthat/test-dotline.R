context("canvasXpress Charts - DotLine")


generic.y  <- readRDS(system.file("extdata", "cX-generic-dat.RData",  package = "canvasXpress"))
generic.x  <- readRDS(system.file("extdata", "cX-generic-smp.RData",  package = "canvasXpress"))
generic.z  <- readRDS(system.file("extdata", "cX-generic-var.RData",  package = "canvasXpress"))

test_that("cX-dotline-1", {
    result <- canvasXpress(
        data = generic.y,
        smpAnnot = generic.x,
        varAnnot = generic.z,
        backgroundGradient1Color = "rgb(226,236,248)",
        backgroundGradient2Color = "rgb(112,179,222)",
        backgroundType = "gradient",
        graphOrientation = "vertical",
        graphType = "DotLine",
        legendBackgroundColor = FALSE,
        legendBox = FALSE,
        legendColumns = 2,
        legendPosition = "bottom",
        lineThickness = 2,
        lineType = "spline",
        showShadow = TRUE,
        smpLabelRotate = 45,
        smpTitle = "Collection of Samples",
        smpTitleFontStyle = "italic",
        subtitle = "Random Data",
        title = "Dot-Line Graphs",
        xAxis = c("Variable1", "Variable2"),
        xAxis2 = c("Variable3", "Variable4"),
        xAxisTickColor = "rgb(0,0,0)"
    )
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})
test_that("cX-dotline-2", {
    result <- canvasXpress(
        data = generic.y,
        smpAnnot = generic.x,
        varAnnot = generic.z,
        coordinateLineColor = TRUE,
        graphOrientation = "vertical",
        graphType = "DotLine",
        lineThickness = 3,
        lineType = "spline",
        showShadow = TRUE,
        smpTitle = "Collection of Samples",
        smpTitleFontStyle = "italic",
        subtitle = "Random Data",
        title = "Dot-Line Graphs",
        xAxis = c("Variable1", "Variable2"),
        xAxis2 = c("Variable3", "Variable4")
    )
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})
test_that("cX-dotline-3 ", {
    result <- canvasXpress(
        data = generic.y,
        smpAnnot = generic.x,
        varAnnot = generic.z,
        coordinateLineColor = TRUE,
        graphOrientation = "horizontal",
        graphType = "DotLine",
        legendBox = FALSE,
        legendColumns = 2,
        legendPosition = "bottom",
        lineThickness = 3,
        lineType = "spline",
        showShadow = TRUE,
        smpTitle = "Collection of Samples",
        smpTitleFontStyle = "italic",
        subtitle = "Random Data",
        title = "Dot-Line Graphs",
        xAxis = c("Variable1", "Variable2"),
        xAxis2 = c("Variable3", "Variable4")
    )
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})
