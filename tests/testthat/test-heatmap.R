context("canvasXpress Charts - Heatmap")


data <- t(iris[, 1:4])
smpAnnot <- as.matrix(iris[, 5])
colnames(data) <- make.names(rep("S", ncol(data)), unique = T)
rownames(smpAnnot) <- colnames(data)
colnames(smpAnnot) <- "Species"
varAnnot <- t(data.frame(color = c("red", "blue", "green", "yellow")))
colnames(varAnnot) <- rownames(data)


test_that("Heatmap - basic 1", {
    result <- canvasXpress(data, 
                           varAnnot = varAnnot, 
                           groupingFactors = list('Species'),
                           graphType = "Heatmap")
    print(result)
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})


test_that("Heatmap - basic 2", {
    result <- canvasXpress(data, 
                           smpAnnot = smpAnnot, 
                           groupingFactors = list('Species'),
                           graphType = "Heatmap")
    print(result)
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})


# -- From Isaac, web examples --
heatmapR.y <- readRDS(system.file("extdata", "cX-heatmapR-dat.RData", package = "canvasXpress"))
heatmapR.x <- readRDS(system.file("extdata", "cX-heatmapR-smp.RData", package = "canvasXpress"))
heatmapR.z <- readRDS(system.file("extdata", "cX-heatmapR-var.RData", package = "canvasXpress"))
multidimensionalheatmap.y  <- readRDS(system.file("extdata", "cX-multidimensionalheatmap-dat.RData", package = "canvasXpress"))
multidimensionalheatmap.y2 <- readRDS(system.file("extdata", "cX-multidimensionalheatmap-dat2.RData", package = "canvasXpress"))
multidimensionalheatmap.y3 <- readRDS(system.file("extdata", "cX-multidimensionalheatmap-dat3.RData", package = "canvasXpress"))
multidimensionalheatmap.y4 <- readRDS(system.file("extdata", "cX-multidimensionalheatmap-dat4.RData", package = "canvasXpress"))
multidimensionalheatmap.x  <- readRDS(system.file("extdata", "cX-multidimensionalheatmap-smp.RData", package = "canvasXpress"))
multidimensionalheatmap.z  <- readRDS(system.file("extdata", "cX-multidimensionalheatmap-var.RData", package = "canvasXpress"))

test_that("cX-heatmap-1", {
    result <- canvasXpress(
        data = heatmapR.y,
        smpAnnot = heatmapR.x,
        varAnnot = heatmapR.z,
        colorSpectrum = c("blue", "white", "red"),
        graphType = "Heatmap",
        title = "Simple Heatmap"
    )
    print(result)
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-heatmap-2", {
    result <- canvasXpress(
        data = heatmapR.y,
        smpAnnot = heatmapR.x,
        varAnnot = heatmapR.z,
        colorSpectrum = c("blue", "white", "red"),
        graphType = "Heatmap",
        heatmapCellBoxColor = "rgb(255,255,255)",
        samplesClustered = TRUE,
        showTransition = TRUE,
        title = "Clustered data",
        variablesClustered = TRUE
    )
    print(result)
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-heatmap-3", {
    result <- canvasXpress(
        data = heatmapR.y,
        smpAnnot = heatmapR.x,
        varAnnot = heatmapR.z,
        colorSpectrum = c("blue", "white", "red"),
        colorSpectrumBreaks = c(1, 2, 10),
        graphType = "Heatmap",
        heatmapCellBoxColor = "rgb(255,255,255)",
        samplesClustered = TRUE,
        showSmpDendrogram = FALSE,
        showVarDendrogram = FALSE,
        title = "Custom color breaks",
        variablesClustered = TRUE
    )
    print(result)
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-heatmap-4", {
    result <- canvasXpress(
        data = heatmapR.y,
        smpAnnot = heatmapR.x,
        varAnnot = heatmapR.z,
        colorSpectrum = c("magenta", "blue", "black", "red", "gold"),
        graphType = "Heatmap",
        heatmapCellBox = FALSE,
        samplesClustered = TRUE,
        showSmpDendrogram = FALSE,
        showVarDendrogram = FALSE,
        title = "Cluster Heatmap Without Trees",
        variablesClustered = TRUE
    )
    print(result)
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-heatmap-5", {
    result <- canvasXpress(
        data = heatmapR.y,
        smpAnnot = heatmapR.x,
        varAnnot = heatmapR.z,
        colorSpectrum = c("magenta", "blue", "black", "red", "gold"),
        colorSpectrumZeroValue = 0,
        graphType = "Heatmap",
        samplesClustered = TRUE,
        showSmpDendrogram = FALSE,
        showVarDendrogram = FALSE,
        title = "Symetrical Colors in Heatmap",
        variablesClustered = TRUE
    )
    print(result)
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-heatmap-6", {
    result <- canvasXpress(
        data = heatmapR.y,
        smpAnnot = heatmapR.x,
        varAnnot = heatmapR.z,
        colorSmpDendrogramBy = "Treatment",
        colorSpectrum = c("magenta", "blue", "black", "red", "gold"),
        colorSpectrumZeroValue = 0,
        graphType = "Heatmap",
        heatmapIndicatorHeight = 50,
        heatmapIndicatorHistogram = TRUE,
        heatmapIndicatorPosition = "topLeft",
        heatmapIndicatorWidth = 60,
        samplesClustered = TRUE,
        title = "R Heatmap",
        variablesClustered = TRUE
    )
    warning('heatmap - not appearing correctly - dendrogram not colored')
    print(result)
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-heatmap-7", {
    result <- canvasXpress(
        data = heatmapR.y,
        smpAnnot = heatmapR.x,
        varAnnot = heatmapR.z,
        colorSmpDendrogramBy = "Treatment",
        colorSpectrum = c("magenta", "blue", "black", "red", "gold"),
        colorSpectrumZeroValue = 0,
        graphType = "Heatmap",
        heatmapIndicatorHeight = 50,
        heatmapIndicatorHistogram = TRUE,
        heatmapIndicatorPosition = "topLeft",
        heatmapIndicatorWidth = 60,
        heatmapSmpSeparateBy = "Treatment",
        samplesClustered = TRUE,
        smpOverlays = c("Treatment", "Site"),
        title = "Overlays in Heatmap",
        variablesClustered = TRUE
    )
    warning('heatmap - not appearing correctly - dendrogram not colored, overlays missing')
    print(result)
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-heatmap-8", {
    result <- canvasXpress(
        data = heatmapR.y,
        smpAnnot = heatmapR.x,
        varAnnot = heatmapR.z,
        colorSpectrum = c("magenta", "blue", "black", "red", "gold"),
        colorSpectrumZeroValue = 0,
        graphType = "Heatmap",
        heatmapIndicatorHeight = 50,
        heatmapIndicatorHistogram = TRUE,
        heatmapIndicatorPosition = "topLeft",
        heatmapIndicatorWidth = 60,
        heatmapSmpSeparateBy = "Treatment",
        highlightSmp = c("Sample1", "Sample2", "Sample3", "Sample4", "Sample5"),
        highlightVar = c("Probe18", "Probe19", "Probe20"),
        samplesClustered = TRUE,
        showTransition = TRUE,
        smpOverlays = c("Treatment", "Site"),
        title = "Highlight cells in Heatmap",
        variablesClustered = TRUE
    )
    print(result)
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-heatmap-9", {
    result <- canvasXpress(
        data = heatmapR.y,
        smpAnnot = heatmapR.x,
        varAnnot = heatmapR.z,
        colorKey = list(
            Sens = c("white", "green"),
            Site = "Pastel1",
            Treatment = "Accent",
            Type = "YlGn"
        ),
        colorSpectrum = c("magenta", "blue", "black", "red", "gold"),
        colorSpectrumZeroValue = 0,
        graphType = "Heatmap",
        heatmapIndicatorHeight = 50,
        heatmapIndicatorHistogram = TRUE,
        heatmapIndicatorPosition = "topLeft",
        heatmapIndicatorWidth = 60,
        samplesClustered = TRUE,
        showTransition = TRUE,
        smpOverlayProperties = list(
            Dose = list(
                position = "right",
                thickness = 50,
                type = "Dotplot"
            ),
            Site = list(position = "left"),
            Treatment = list(position = "right")
        ),
        smpOverlays = c("Treatment", "Site", "Dose"),
        title = "Advanced Overlays in Heatmaps",
        varOverlayProperties = list(
            Sens = list(
                color = "red",
                position = "bottom",
                thickness = 20,
                type = "Bar"
            ),
            Type = list(position = "top")
        ),
        varOverlays = c("Type", "Sens"),
        variablesClustered = TRUE
    )
    warning('heatmap - not appearing correctly - dendrogram not colored, overlays missing')
    print(result)
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-heatmap-10", {
    result <- canvasXpress(
        data = multidimensionalheatmap.y,
        smpAnnot = multidimensionalheatmap.x,
        varAnnot = multidimensionalheatmap.z,
        graphType = "Heatmap",
        guides = TRUE,
        outlineBy = "Outline",
        outlineByData = "data2",
        shapeBy = "Shape",
        shapeByData = "data3",
        sizeBy = "Size",
        sizeByData = "data4"
    )
    warning('heatmap - not appearing correctly - multidimensionality missing')
    print(result)
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-heatmap-11", {
    result <- canvasXpress(
        data = multidimensionalheatmap.y,
        colorSpectrum = c("#f0f0f0", "#bdbdbd", "#636363", "#000000"),
        graphType = "Heatmap",
        showHeatmapIndicator = FALSE,
        showLegend = FALSE,
        sizeBy = "Size",
        sizeByContinuous = TRUE,
        sizeByData = "data",
        title = "A good old Northern Blot"
    )
    print(result)
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})
