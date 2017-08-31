context("canvasXpress Charts - AreaLine")


data <- t(iris[, 1:4])
smpAnnot <- as.matrix(iris[, 5])
colnames(data) <- make.names(rep("S", ncol(data)), unique = T)
rownames(smpAnnot) <- colnames(data)
colnames(smpAnnot) <- "Species"
varAnnot <- t(data.frame(color = c("red", "blue", "green", "yellow")))
colnames(varAnnot) <- rownames(data)


test_that("AreaLine Chart - basic 1", {
    result <- canvasXpress(
        data,
        varAnnot = varAnnot,
        graphOrientation = "vertical",
        graphType = "AreaLine",
        colorBy = 'Species'
    )
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})


test_that("AreaLine Chart - basic 2", {
    result <- canvasXpress(
        data,
        smpAnnot = smpAnnot,
        graphOrientation = "vertical",
        graphType = "AreaLine",
        colorBy = 'Species'
    )
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})


# -- From Isaac, web examples --
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
