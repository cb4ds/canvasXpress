context("canvasXpress Charts - Pie")


data <- t(iris[,1:4])


test_that("Bar Chart - basic 1", {
    result <- canvasXpress(data, 
                           graphType = "Pie")
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})


test_that("Bar Chart - basic 2", {
    result <- canvasXpress(data, 
                           graphType = "Pie")
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})


# -- From Isaac, web examples --
generic.y  <- readRDS(system.file("extdata", "cX-generic-dat.RData", package = "canvasXpress"))
generic.x  <- readRDS(system.file("extdata", "cX-generic-smp.RData", package = "canvasXpress"))
generic.z  <- readRDS(system.file("extdata", "cX-generic-var.RData", package = "canvasXpress"))

test_that("cX-pie-1", {
    result <- canvasXpress(
        data = generic.y,
        smpAnnot = generic.x,
        varAnnot = generic.z,
        graphType = "Pie",
        layout = "2X3",
        pieSegmentLabels = "inside",
        pieSegmentPrecision = 0,
        pieSegmentSeparation = 1,
        showPieGrid = TRUE,
        showPieSampleLabel = TRUE,
        showTransition = TRUE,
        xAxis = c(
            "Sample1",
            "Sample2",
            "Sample3",
            "Sample4",
            "Sample5",
            "Sample6"
        )
    )
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-pie-2", {
    result <- canvasXpress(
        data = generic.y,
        smpAnnot = generic.x,
        varAnnot = generic.z,
        graphType = "Pie",
        pieSegmentLabels = "outside",
        pieSegmentPrecision = 1,
        pieSegmentSeparation = 2,
        pieType = "solid",
        showTransition = TRUE
    )
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

