context("canvasXpress Charts - StackedPercent")


diverging.y <- readRDS(system.file("extdata", "cX-diverging-dat.RData", package = "canvasXpress"))
generic.y   <- readRDS(system.file("extdata", "cX-generic-dat.RData", package = "canvasXpress"))
generic.x   <- readRDS(system.file("extdata", "cX-generic-smp.RData", package = "canvasXpress"))
generic.z   <- readRDS(system.file("extdata", "cX-generic-var.RData", package = "canvasXpress"))

test_that("cX-stackedpercent-1", {
    result <- canvasXpress(
        data = generic.y,
        smpAnnot = generic.x,
        varAnnot = generic.z,
        graphOrientation = "horizontal",
        graphType = "StackedPercent",
        legendBackgroundColor = FALSE,
        sampleSeparationFactor = 1,
        showShadow = TRUE,
        showTransition = TRUE,
        smpLabelScaleFontFactor = 0.8,
        smpTitle = "Collection of Samples",
        smpTitleFontStyle = "italic",
        title = "Random Data"
    )
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-stackedpercent-2", {
    result <- canvasXpress(
        data = generic.y,
        smpAnnot = generic.x,
        varAnnot = generic.z,
        gradient = TRUE,
        graphOrientation = "vertical",
        graphType = "StackedPercent",
        legendBackgroundColor = FALSE,
        showShadow = TRUE,
        smpLabelScaleFontFactor = 0.8,
        smpTitle = "Collection of Samples",
        smpTitleFontStyle = "italic",
        title = "Random Data"
    )
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-stackedpercent-3", {
    result <- canvasXpress(
        data = generic.y,
        smpAnnot = generic.x,
        varAnnot = generic.z,
        graphOrientation = "horizontal",
        graphType = "StackedPercent",
        legendBackgroundColor = FALSE,
        sampleSeparationFactor = 1.5,
        showShadow = TRUE,
        smpLabelScaleFontFactor = 0.8,
        smpTitle = "Collection of Samples",
        smpTitleFontStyle = "italic",
        title = "Random Data"
    )
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-stackedpercent-4", {
    result <- canvasXpress(
        data = diverging.y,
        axisAlgorithm = "wilkinson",
        colorScheme = "RdYlBu",
        graphOrientation = "horizontal",
        graphType = "StackedPercent",
        legendColumns = 3,
        legendPosition = "bottom",
        marginRight = 20,
        title = "Diverging Stacked Percent Graph",
        xAxisTickFormat = "%s%%",
        xAxis = list("Pants on Fire", "False", "Mostly False",
                     "Half True", "Mostly True", "True"),
        xAxisTitle = ""
    )
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})
