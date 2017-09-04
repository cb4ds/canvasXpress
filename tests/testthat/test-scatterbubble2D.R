context("canvasXpress Charts - ScatterBubble2D")


bubble.y <- readRDS(system.file("extdata", "cX-bubble-dat.RData", package = "canvasXpress"))
bubble.z <- readRDS(system.file("extdata", "cX-bubble-var.RData", package = "canvasXpress"))
generic.y  <- readRDS(system.file("extdata", "cX-generic-dat.RData", package = "canvasXpress"))
generic.x  <- readRDS(system.file("extdata", "cX-generic-smp.RData", package = "canvasXpress"))
generic.z  <- readRDS(system.file("extdata", "cX-generic-var.RData", package = "canvasXpress"))

test_that("cX-scatterbubble2d-1", {
    result <- canvasXpress(
        data = bubble.y,
        varAnnot = bubble.z,
        colorBy = "Continent",
        graphType = "ScatterBubble2D",
        showTransition = TRUE,
        xAxis = list("LifeExpectancy"),
        yAxis = list("GDPPerCapita"),
        yAxisTransform = "log2",
        zAxis = list("Population")
    )
    warning('scatterbubble2d - not appearing correctly - loading')
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-scatterbubble2d-2", {
    result <- canvasXpress(
        data = generic.y,
        smpAnnot = generic.x,
        varAnnot = generic.z,
        graphType = "ScatterBubble2D",
        xAxis = list("Sample1", "Sample4"),
        yAxis = list("Sample2", "Sample5"),
        zAxis = list("Sample3", "Sample6")
    )
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-scatterbubble2d-3", {
    result <- canvasXpress(
        data = bubble.y,
        varAnnot = bubble.z,
        colorBy = "Continent",
        graphType = "ScatterBubble2D",
        motionBy = "Year",
        xAxis = list("LifeExpectancy"),
        yAxis = list("GDPPerCapita"),
        yAxisTransform = "log2",
        zAxis = list("Population")
    )
    warning('scatterbubble2d - not appearing correctly - loading')
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})
