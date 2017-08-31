context("canvasXpress Charts - Stacked")


data <- t(iris[, 1:4])
smpAnnot <- as.matrix(iris[, 5])
colnames(data) <- make.names(rep("S", ncol(data)), unique = T)
rownames(smpAnnot) <- colnames(data)
colnames(smpAnnot) <- "Species"
varAnnot <- t(data.frame(color = c("red", "blue", "green", "yellow")))
colnames(varAnnot) <- rownames(data)


test_that("Stacked Chart - basic 1", {
    result <- canvasXpress(data, 
                           varAnnot = varAnnot, 
                           colorBy = 'Species',
                           graphType = "Stacked")
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})


test_that("Stacked Chart - basic 2", {
    result <- canvasXpress(data, 
                           smpAnnot = smpAnnot, 
                           colorBy = 'Species',
                           graphType = "Stacked")
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})


test_that("Stacked Chart - grouped", {
    result <- canvasXpress(data, 
                           smpAnnot = smpAnnot, 
                           groupingFactors = list('Species'),
                           graphType = "Stacked",
                           graphOrientation = "vertical",
                           smpLabelRotate = 90,
                           legendPosition = "bottom",
                           legendColumns = 2,
                           xAxis2Show = FALSE,
                           showTransition = TRUE)
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})


# -- From Isaac, web examples --
stacked2.y  <- readRDS(system.file("extdata", "cX-stacked2-dat.RData", package = "canvasXpress"))
stacked2.x  <- readRDS(system.file("extdata", "cX-stacked2-smp.RData", package = "canvasXpress"))
diverging.y <- readRDS(system.file("extdata", "cX-diverging-dat.RData", package = "canvasXpress"))
generic.y   <- readRDS(system.file("extdata", "cX-generic-dat.RData", package = "canvasXpress"))
generic.x   <- readRDS(system.file("extdata", "cX-generic-smp.RData", package = "canvasXpress"))
generic.z   <- readRDS(system.file("extdata", "cX-generic-var.RData", package = "canvasXpress"))

test_that("cX-stacked-1", {
    result <- canvasXpress(
        data = stacked2.y,
        smpAnnot = stacked2.x,
        colorScheme = "Blues",
        foreground = "rgb(0,0,0)",
        graphOrientation = "vertical",
        graphType = "Stacked",
        groupingFactors = c("Factor1"),
        sampleSeparationFactor = 1,
        showTransition = TRUE,
        title = "Random Data",
        treemapBy = c("Factor2", "Factor3")
    )
    warning('stacked - not appearing correctly - treemap missing')
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-stacked-2", {
    result <- canvasXpress(
        data = generic.y,
        smpAnnot = generic.x,
        varAnnot = generic.z,
        graphOrientation = "horizontal",
        graphType = "Stacked",
        legendBackgroundColor = FALSE,
        sampleSeparationFactor = 1,
        showShadow = TRUE,
        smpLabelScaleFontFactor = 0.8,
        smpTitle = "Collection of Samples",
        smpTitleFontStyle = "italic",
        title = "Random Data"
    )
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-stacked-3", {
    result <- canvasXpress(
        data = generic.y,
        smpAnnot = generic.x,
        varAnnot = generic.z,
        gradient = TRUE,
        graphOrientation = "vertical",
        graphType = "Stacked",
        legendBackgroundColor = FALSE,
        showShadow = TRUE,
        smpLabelScaleFontFactor = 0.8,
        smpTitle = "Collection of Samples",
        smpTitleFontStyle = "italic",
        title = "Random Data"
    )
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-stacked-4", {
    result <- canvasXpress(
        data = generic.y,
        smpAnnot = generic.x,
        varAnnot = generic.z,
        graphOrientation = "horizontal",
        graphType = "Stacked",
        legendBackgroundColor = FALSE,
        sampleSeparationFactor = 1.5,
        showShadow = TRUE,
        smpLabelScaleFontFactor = 0.8,
        smpTitle = "Collection of Samples",
        smpTitleFontStyle = "italic",
        title = "Random Data"
    )
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-stacked-5", {
    result <- canvasXpress(
        data = diverging.y,
        axisAlgorithm = "wilkinson",
        colorScheme = "RdYlBu",
        graphOrientation = "horizontal",
        graphType = "Stacked",
        legendColumns = 3,
        legendPosition = "bottom",
        marginRight = 20,
        title = "Diverging Stacked Graph",
        xAxisTickFormat = "%s%%"
    )
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})
