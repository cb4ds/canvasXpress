context("canvasXpress Charts - Area")

iris.data <- t(iris[, 1:4])

test_that("Area Chart 1 normal", {
    result <- canvasXpress(iris.data,
                           graphType = "Area")
    result
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})


test_that("Area Chart 1 percent", {
    result <- canvasXpress(iris.data,
                           graphType = "Area",
                           areaType = "percent")
    result
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})


test_that("Area Chart 1 stacked", {
    result <- canvasXpress(iris.data,
                           graphType = "Area",
                           areaType = "stacked")
    result
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})


# -- From Isaac, web examples --
area1.y <- readRDS(system.file("extdata", "cX-area-dat.RData", package = "canvasXpress"))
area2.y <- readRDS(system.file("extdata", "cX-area2-dat.RData", package = "canvasXpress"))
area3.y <- readRDS(system.file("extdata", "cX-area3-dat.RData", package = "canvasXpress"))

test_that("cX-area-1", {
    result <- canvasXpress(
        data = area3.y,
        areaType = "stacked",
        colorScheme = "ColorSpectrum",
        colorSpectrum = c("blue", "cyan", "yellow", "red"),
        graphOrientation = "vertical",
        graphType = "Area",
        lineType = "spline",
        showLegend = FALSE,
        showSampleNames = FALSE,
        showTransition = TRUE,
        subtitle = "http://menugget.blogspot.com/2013/12/data-mountains-and-streams-stacked-area.html",
        title = "Steam Plot"
    )
    result
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-area-2", {
    result <- canvasXpress(
        data = area1.y,
        colorScheme = "RlatticeShingle",
        graphOrientation = "vertical",
        graphType = "Area",
        legendPosition = "right",
        lineType = "spline",
        showTransition = TRUE,
        smpLabelInterval = 20,
        smpLabelRotate = 45,
        smpTitle = "Year",
        subtitle = "gcookbook - uspopage",
        title = "Age distribution of population in the United State",
        transparency = 0.5,
        xAxis2Show = FALSE,
        xAxisTitle = "Number of People (1000's)"
    )
    result
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-area-3", {
    result <- canvasXpress(
        data = area1.y,
        areaType = "percent",
        colorScheme = "Basic",
        graphOrientation = "vertical",
        graphType = "Area",
        legendPosition = "right",
        lineType = "spline",
        showTransition = TRUE,
        smpLabelInterval = 20,
        smpLabelRotate = 45,
        smpTitle = "Year",
        subtitle = "gcookbook - uspopage",
        title = "Age distribution of population in the United State",
        xAxis2Show = FALSE,
        xAxisTitle = "Normalized Percentage of People"
    )
    result
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-area-4", {
    result <- canvasXpress(
        data = area1.y,
        areaType = "stacked",
        colorScheme = "Blues",
        graphOrientation = "vertical",
        graphType = "Area",
        legendPosition = "right",
        lineType = "spline",
        smpLabelInterval = 20,
        smpLabelRotate = 45,
        smpTitle = "Year",
        subtitle = "gcookbook - uspopage",
        title = "Age distribution of population in the United State",
        xAxis2Show = FALSE,
        xAxisTitle = "Number of People (1000's)"
    )
    result
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-area-5", {
    result <- canvasXpress(
        data = area1.y,
        colorScheme = "RlatticeShingle",
        graphOrientation = "vertical",
        graphType = "Area",
        legendPosition = "right",
        lineType = "spline",
        smpLabelInterval = 20,
        smpLabelRotate = 45,
        smpTitle = "Year",
        subtitle = "gcookbook - uspopage",
        title = "Age distribution of population in the United State",
        transformAxis = "samples",
        transformType = "zscore",
        transformedData = TRUE,
        transparency = 0.5,
        xAxis2Show = FALSE,
        xAxisTitle = "Normalized Scores of Number of People"
    )
    result
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})


test_that("cX-area-6", {
    result <- canvasXpress(
        data = area2.y,
        areaType = "stacked",
        axisAlgorithm = "wilkinsonExtended",
        colorScheme = "ColorSpectrum",
        colorSpectrum = c("blue", "cyan", "yellow", "red"),
        graphOrientation = "vertical",
        graphType = "Area",
        lineType = "spline",
        showLegend = FALSE,
        showSampleNames = FALSE,
        subtitle = "http://menugget.blogspot.com/2013/12/data-mountains-and-streams-stacked-area.html",
        title = "Data Mountain"
    )
    result
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})
