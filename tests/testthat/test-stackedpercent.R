 context("canvasXpress Charts - StackedPercent")


data <- t(iris[, 1:4])
smpAnnot <- as.matrix(iris[, 5])
colnames(data) <- make.names(rep("S", ncol(data)), unique = T)
rownames(smpAnnot) <- colnames(data)
colnames(smpAnnot) <- "Species"
varAnnot <- t(data.frame(color = c("red", "blue", "green", "yellow")))
colnames(varAnnot) <- rownames(data)


test_that("StackedPercent Chart - basic 1", {
    result <- canvasXpress(data, 
                           varAnnot = varAnnot, 
                           colorBy = 'Species',
                           graphType = "StackedPercent")
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})


test_that("StackedPercent Chart - basic 2", {
    result <- canvasXpress(data, 
                           smpAnnot = smpAnnot, 
                           colorBy = 'Species',
                           graphType = "StackedPercent")
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})


test_that("StackedPercent Chart - grouped", {
    result <- canvasXpress(data, 
                           smpAnnot = smpAnnot, 
                           groupingFactors = list('Species'),
                           graphType = "StackedPercent",
                           graphOrientation = "vertical",
                           smpLabelRotate = 90,
                           legendPosition = "bottom",
                           legendColumns = 2)
    warning('stackedpercent - graph incorrect - axis issue')
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})


# -- From Isaac, web examples --
diverging.y <- readRDS(system.file("extdata", "cX-diverging-dat.RData", package = "canvasXpress"))
generic.y   <- readRDS(system.file("extdata", "cX-generic-dat.RData", package = "canvasXpress"))
generic.x   <- readRDS(system.file("extdata", "cX-generic-smp.RData", package = "canvasXpress"))
generic.z   <- readRDS(system.file("extdata", "cX-generic-var.RData", package = "canvasXpress"))

test_that("cX-stackedpercent-1", {
    result <- canvasXpress(
        data = generic.y,
        smpAnnot = generic.x,
        varAnnot = generic.z,
        graphOrientation = "horizontal",
        graphType = "StackedPercent",
        legendBackgroundColor = FALSE,
        sampleSeparationFactor = 1,
        showShadow = TRUE,
        showTransition = TRUE,
        smpLabelScaleFontFactor = 0.8,
        smpTitle = "Collection of Samples",
        smpTitleFontStyle = "italic",
        title = "Random Data"
    )
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-stackedpercent-2", {
    result <- canvasXpress(
        data = generic.y,
        smpAnnot = generic.x,
        varAnnot = generic.z,
        gradient = TRUE,
        graphOrientation = "vertical",
        graphType = "StackedPercent",
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

test_that("cX-stackedpercent-3", {
    result <- canvasXpress(
        data = generic.y,
        smpAnnot = generic.x,
        varAnnot = generic.z,
        graphOrientation = "horizontal",
        graphType = "StackedPercent",
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

test_that("cX-stackedpercent-4", {
    result <- canvasXpress(
        data = generic.y,
        axisAlgorithm = "wilkinson",
        colorScheme = "RdYlBu",
        graphOrientation = "horizontal",
        graphType = "StackedPercent",
        legendColumns = 3,
        legendPosition = "bottom",
        marginRight = 20,
        title = "Diverging Stacked Percent Graph",
        xAxisTickFormat = "%s%%"
    )
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})
