context("canvasXpress Charts - TagCloud")


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
    warning('tagcloud - not appearing correctly - color')
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

