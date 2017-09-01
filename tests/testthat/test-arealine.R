context("canvasXpress Charts - AreaLine")


arealine.y <- readRDS(system.file("extdata", "cX-arealine-dat.RData", package = "canvasXpress"))

test_that("cX-arealine-1", {
    result <- canvasXpress(
        data = arealine.y,
        colorScheme = "Basic",
        graphOrientation = "vertical",
        graphType = "AreaLine",
        legendPosition = "right",
        lineThickness = 3,
        lineType = "spline",
        smpLabelInterval = 20,
        smpLabelRotate = 45,
        smpTitle = "Year",
        subtitle = "gcookbook - uspopage",
        title = "Age distribution of population in the United State",
        xAxis = c("<5", "5-14", "15-24", "25-34"),
        xAxis2 = c("35-44", "45-54", "55-64", ">64"),
        xAxisTitle = "Number of People (1000's)"
    )
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-arealine-2", {
    result <- canvasXpress(
        data = arealine.y,
        areaType = "stacked",
        colorScheme = "Spectral",
        graphOrientation = "vertical",
        graphType = "AreaLine",
        legendPosition = "right",
        lineThickness = 3,
        lineType = "spline",
        smpLabelInterval = 20,
        smpLabelRotate = 45,
        smpTitle = "Year",
        subtitle = "gcookbook - uspopage",
        title = "Age distribution of population in the United State",
        xAxis = c("<5", "5-14", "15-24", "25-34"),
        xAxis2 = c("35-44", "45-54", "55-64", ">64"),
        xAxisTitle = "Number of People (1000's)"
    )
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-arealine-3", {
    result <- canvasXpress(
        data = arealine.y,
        colorScheme = "Paired",
        coordinateLineColor = TRUE,
        graphOrientation = "vertical",
        graphType = "AreaLine",
        legendPosition = "right",
        lineThickness = 3,
        lineType = "spline",
        smpLabelInterval = 20,
        smpLabelRotate = 45,
        smpTitle = "Year",
        subtitle = "gcookbook - uspopage",
        title = "Age distribution of population in the United State",
        xAxis = c("<5", "5-14", "15-24", "25-34"),
        xAxis2 = c("35-44", "45-54", "55-64", ">64"),
        xAxisTitle = "Number of People (1000's)"
    )
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})
