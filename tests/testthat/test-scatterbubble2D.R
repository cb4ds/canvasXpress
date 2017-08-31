context("canvasXpress Charts - ScatterBubble2D")


data <- t(iris[, 1:4])
smpAnnot <- as.matrix(iris[, 5])
colnames(data) <- make.names(rep("S", ncol(data)), unique = T)
rownames(smpAnnot) <- colnames(data)
colnames(smpAnnot) <- "Species"
varAnnot <- t(data.frame(color = c("red", "blue", "green", "yellow")))
colnames(varAnnot) <- rownames(data)


test_that("Scatter2D Chart - basic 1", {
    result <- canvasXpress(data, 
                           varAnnot = varAnnot, 
                           colorBy = 'Species',
                           graphType = 'ScatterBubble2D')
    print(result)
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})


# -- From Isaac, web examples --
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
        xAxis = c("LifeExpectancy"),
        yAxis = c("GDPPerCapita"),
        yAxisTransform = "log2",
        zAxis = c("Population")
    )
    warning('scatterbubble2d - not appearing correctly - loading')
    print(result)
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-scatterbubble2d-2", {
    result <- canvasXpress(
        data = generic.y,
        smpAnnot = generic.x,
        varAnnot = generic.z,
        graphType = "ScatterBubble2D",
        xAxis = c("Sample1", "Sample4"),
        yAxis = c("Sample2", "Sample5"),
        zAxis = c("Sample3", "Sample6")
    )
    print(result)
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
        xAxis = c("LifeExpectancy"),
        yAxis = c("GDPPerCapita"),
        yAxisTransform = "log2",
        zAxis = c("Population")
    )
    warning('scatterbubble2d - not appearing correctly - loading')
    print(result)
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})
