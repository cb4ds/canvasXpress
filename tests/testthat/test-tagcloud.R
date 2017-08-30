context("canvasXpress Charts - TagCloud")


data <- t(iris[,1:4])


test_that("Tag Cloud - basic 1", {
    result <- canvasXpress(data,
                           graphType = "TagCloud")
    result
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})


test_that("Tag Cloud - basic 2", {
    result <- canvasXpress(data, 
                           graphType = "TagCloud")
    result
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})


# -- From Isaac, web examples --
cars.y  <- readRDS(system.file("extdata", "cX-cars-dat.RData", package = "canvasXpress"))
cars.x  <- readRDS(system.file("extdata", "cX-cars-smp.RData", package = "canvasXpress"))
cars.z  <- readRDS(system.file("extdata", "cX-cars-var.RData", package = "canvasXpress"))

test_that("cX-tagcloud-1", {
    result <- canvasXpress(
        data = cars.y,
        smpAnnot = cars.x,
        varAnnot = cars.z,
        colorBy = "Country",
        graphType = "TagCloud",
        showTransition = TRUE
    )
    result
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})
