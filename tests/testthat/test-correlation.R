context("canvasXpress Charts - Correlation")


data <- t(iris[,1:4])


test_that("Correlation - basic 1", {
    result <- canvasXpress(data, 
                           graphType = "Correlation")
    result
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})


# -- From Isaac, web examples --
generic.y  <- readRDS(system.file("extdata", "cX-generic-dat.RData", package = "canvasXpress"))
generic.x  <- readRDS(system.file("extdata", "cX-generic-smp.RData", package = "canvasXpress"))
generic.z  <- readRDS(system.file("extdata", "cX-generic-var.RData", package = "canvasXpress"))

test_that("cX-correlation-1", {
    result <- canvasXpress(
        data = generic.y,
        smpAnnot = generic.x,
        varAnnot = generic.z,
        correlationAxis = "samples",
        gradient = TRUE,
        graphType = "Correlation",
        showTransition = TRUE,
        title = "Correlation Plot",
        yAxisTitle = "Correlation Title"
    )
    result
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-correlation-2", {
    result <- canvasXpress(
        data = generic.y,
        smpAnnot = generic.x,
        varAnnot = generic.z,
        correlationAnchorLegend = TRUE,
        correlationAnchorLegendAlignWidth = 20,
        correlationAxis = "variables",
        graphType = "Correlation",
        title = "Correlation Plot",
        yAxisTitle = "Correlation Title"
    )
    result
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})
