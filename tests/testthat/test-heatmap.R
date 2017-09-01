context("canvasXpress Charts - Heatmap")


heatmapR.y   <- readRDS(system.file("extdata", "cX-heatmapR-dat.RData", package = "canvasXpress"))
heatmapR.x   <- readRDS(system.file("extdata", "cX-heatmapR-smp.RData", package = "canvasXpress"))
heatmapR.z   <- readRDS(system.file("extdata", "cX-heatmapR-var.RData", package = "canvasXpress"))
oncoprint.y  <- readRDS(system.file("extdata", "cX-oncoprint-dat.RData", package = "canvasXpress"))
oncoprint.y4 <- readRDS(system.file("extdata", "cX-oncoprint-dat4.RData", package = "canvasXpress"))
oncoprint.x  <- readRDS(system.file("extdata", "cX-oncoprint-smp.RData", package = "canvasXpress"))
oncoprint.z  <- readRDS(system.file("extdata", "cX-oncoprint-var.RData", package = "canvasXpress"))
volcano.y    <- readRDS(system.file("extdata", "cX-volcano-dat.RData", package = "canvasXpress"))
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
    if (interactive()) { print(result) }
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
    if (interactive()) { print(result) }
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
    if (interactive()) { print(result) }
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
    if (interactive()) { print(result) }
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
    if (interactive()) { print(result) }
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
    if (interactive()) { print(result) }
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
    if (interactive()) { print(result) }
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
    if (interactive()) { print(result) }
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
    if (interactive()) { print(result) }
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
    if (interactive()) { print(result) }
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
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})


test_that("cX-oncoprint-1", {
    result <- canvasXpress(
        data = oncoprint.y,
        smpAnnot = oncoprint.x,
        varAnnot = oncoprint.z,
        graphType = "Heatmap",
        isOncoprint = "mutation",
        showTransition = TRUE
    )
    warning('heatmap - not appearing correctly - multidimensionality missing')
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-oncoprint-2", {
    result <- canvasXpress(
        data = oncoprint.y,
        smpAnnot = oncoprint.x,
        varAnnot = oncoprint.z,
        graphType = "Heatmap",
        isOncoprint = "mutation",
        smpOverlayProperties = list(
            Annt2 = list(position = "right", type = "Bar"),
            Annt3 = list(type = "Stacked"),
            Annt4 = list(type = "Stacked"),
            Annt5 = list(type = "Stacked")
        ),
        smpOverlays = c("Annt1", "-", "Annt2", "Annt3", "Annt4", "Annt5")
    )
    warning('heatmap - not appearing correctly - multidimensionality missing')
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-oncoprint-3", {
    result <- canvasXpress(
        data = oncoprint.y,
        smpAnnot = oncoprint.x,
        varAnnot = oncoprint.z,
        graphType = "Heatmap",
        isOncoprint = "mutation",
        outlineBy = "Color",
        outlineByData = "expression",
        patternBy = "Pattern",
        patternByData = "data4",
        shapeBy = "Shape",
        shapeByData = "data5",
        smpOverlayProperties = list(
            Annt2 = list(position = "right", type = "Bar"),
            Annt3 = list(type = "Stacked"),
            Annt4 = list(type = "Stacked"),
            Annt5 = list(type = "Stacked")
        ),
        smpOverlays = c("Annt1", "-", "Annt2", "Annt3", "Annt4", "Annt5")
    )
    warning('heatmap - not appearing correctly - multidimensionality missing')
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-contour-1", {
    result <- canvasXpress(
        data = volcano.y,
        graphType = "Heatmap",
        heatmapCellBox = FALSE,
        showSampleNames = FALSE,
        showVariableNames = FALSE,
        subtitle = "datasets - volcano",
        title = "Topographic Information on Auckland's Maunga Whau Volcano"
    )
    warning('heatmap - graph incorrect - lines missing')
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})
