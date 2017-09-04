context("canvasXpress Charts - BarLine")


generic.y  <- readRDS(system.file("extdata", "cX-generic-dat.RData",  package = "canvasXpress"))
generic.x  <- readRDS(system.file("extdata", "cX-generic-smp.RData",  package = "canvasXpress"))
generic.z  <- readRDS(system.file("extdata", "cX-generic-var.RData",  package = "canvasXpress"))

test_that("cX-barline-1", {
    result <- canvasXpress(
        data = generic.y,
        smpAnnot = generic.x,
        varAnnot = generic.z,
        backgroundGradient1Color = "rgb(226,236,248)",
        backgroundGradient2Color = "rgb(112,179,222)",
        backgroundType = "gradient",
        graphOrientation = "vertical",
        graphType = "BarLine",
        legendBackgroundColor = FALSE,
        legendBox = FALSE,
        legendColumns = 2,
        legendPosition = "bottom",
        lineThickness = 2,
        lineType = "spline",
        showShadow = TRUE,
        showTransition = TRUE,
        smpLabelRotate = 45,
        smpTitle = "Collection of Samples",
        smpTitleFontStyle = "italic",
        subtitle = "Random Data",
        title = "Bar-Line Graphs",
        xAxis = list("Variable1", "Variable2"),
        xAxis2 = list("Variable3", "Variable4"),
        xAxis2TickFormat = "%.0f T",
        xAxisTickColor = "rgb(0,0,0)",
        xAxisTickFormat = "%.0f M"
    )
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-barline-2", {
    result <- canvasXpress(
        data = generic.y,
        smpAnnot = generic.x,
        varAnnot = generic.z,
        coordinateLineColor = TRUE,
        graphOrientation = "vertical",
        graphType = "BarLine",
        lineThickness = 3,
        lineType = "spline",
        showShadow = TRUE,
        smpTitle = "Collection of Samples",
        smpTitleFontStyle = "italic",
        subtitle = "Random Data",
        title = "Bar-Line Graphs",
        xAxis = list("Variable1", "Variable2"),
        xAxis2 = list("Variable3", "Variable4")
    )
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-barline-3", {
    result <- canvasXpress(
        data = generic.y,
        smpAnnot = generic.x,
        varAnnot = generic.z,
        coordinateLineColor = TRUE,
        graphOrientation = "horizontal",
        graphType = "BarLine",
        legendBox = FALSE,
        legendColumns = 4,
        legendPosition = "bottom",
        lineThickness = 3,
        lineType = "spline",
        showShadow = TRUE,
        smpTitle = "Collection of Samples",
        smpTitleFontStyle = "italic",
        subtitle = "Random Data",
        title = "Bar-Line Graphs",
        xAxis = list("Variable1", "Variable2"),
        xAxis2 = list("Variable3", "Variable4") 
    )
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})
