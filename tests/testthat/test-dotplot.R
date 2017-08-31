context("canvasXpress Charts - Dotplot")


data <- t(iris[, 1:4])
smpAnnot <- as.matrix(iris[, 5])
colnames(data) <- make.names(rep("S", ncol(data)), unique = T)
rownames(smpAnnot) <- colnames(data)
colnames(smpAnnot) <- "Species"
varAnnot <- t(data.frame(color = c("red", "blue", "green", "yellow")))
colnames(varAnnot) <- rownames(data)


test_that("Dotplot Chart - basic 1", {
    result <- canvasXpress(data, 
                           varAnnot = varAnnot, 
                           colorBy = 'Species',
                           graphType = "Dotplot")
    result
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})


test_that("Dotplot Chart - basic 2", {
    result <- canvasXpress(data, 
                           smpAnnot = smpAnnot, 
                           colorBy = 'Species',
                           graphType = "Dotplot")
    result
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})


test_that("Dotplot Chart - grouped", {
    result <- canvasXpress(data, 
                           smpAnnot = smpAnnot, 
                           groupingFactors = list('Species'),
                           graphType = "Dotplot",
                           graphOrientation = "vertical",
                           smpLabelRotate = 90,
                           legendPosition = "bottom",
                           legendColumns = 2)
    result
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})


# -- From Isaac, web examples --
generic.y  <- readRDS(system.file("extdata", "cX-generic-dat.RData",  package = "canvasXpress"))
generic.x  <- readRDS(system.file("extdata", "cX-generic-smp.RData",  package = "canvasXpress"))
generic.z  <- readRDS(system.file("extdata", "cX-generic-var.RData",  package = "canvasXpress"))
iris.y     <- readRDS(system.file("extdata", "cX-iris-dat.RData",  package = "canvasXpress"))
iris.x     <- readRDS(system.file("extdata", "cX-iris-smp.RData",  package = "canvasXpress"))
cars.y     <- readRDS(system.file("extdata", "cX-cars-dat.RData",  package = "canvasXpress"))
cars.x     <- readRDS(system.file("extdata", "cX-cars-smp.RData",  package = "canvasXpress"))
cars.z     <- readRDS(system.file("extdata", "cX-cars-var.RData",  package = "canvasXpress"))
dumbbell.y <- readRDS(system.file("extdata", "cX-dumbbell-dat.RData",  package = "canvasXpress"))
dumbbell.z <- readRDS(system.file("extdata", "cX-dumbbell-var.RData",  package = "canvasXpress"))

test_that("cX-dotplot-1", {
    result <- canvasXpress(
        data = generic.y,
        smpAnnot = generic.x,
        varAnnot = generic.z,
        colorScheme = "basic",
        graphOrientation = "vertical",
        graphType = "Dotplot",
        legendColumns = 2,
        lineType = "spline",
        showAnimation = FALSE,
        showShadow = TRUE,
        showTransition = TRUE,
        smpLabelRotate = 45,
        smpOverlays = c("Factor1", "Factor2", "Factor3"),
        smpTitle = "Collection of Samples",
        smpTitleFontStyle = "italic",
        subtitle = "Random Data",
        title = "Dotplot Graph",
        xAxisTickFormat = "%.0f Mil."
    )
    warning('dotplot - not appearing correctly - no overlays')
    result
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-dotplot-2", {
    result <- canvasXpress(
        data = iris.y,
        smpAnnot = iris.x,
        axisTickFontStyle = "bold",
        axisTitleFontStyle = "italic",
        citation = "R. A. Fisher (1936). The use of multiple measurements in taxonomic problems. Annals of Eugenics 7 (2) => 179-188.",
        citationFontStyle = "italic",
        decorations = list(marker = c(
            list(
                sample = "setosa",
                text = "Species with\nlowest petal\nwidth",
                variable = "Petal.Width",
                x = 0.4,
                y = 0.85
            )
        )),
        fontStyle = "italic",
        graphOrientation = "vertical",
        graphType = "Dotplot",
        jitter = TRUE,
        legendBox = FALSE,
        marginBottom = 30,
        showShadow = TRUE,
        smpLabelFontStyle = "italic",
        smpLabelRotate = 90,
        smpTitle = "Species",
        title = "Iris flower data set",
        xAxis2Show = FALSE
    )
    warning('dotplot - not appearing correctly - not grouped')
    result
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-dotplot-3", {
    result <- canvasXpress(
        data = cars.y,
        smpAnnot = cars.x,
        varAnnot = cars.z,
        citation = "Henderson, H. V. and Velleman, P. F. (1981), Building Regression Models Interactively. Biometrics, 37, 391-411.",
        citationFontStyle = "italic",
        graphType = "Dotplot",
        legendColumns = 2,
        legendInside = TRUE,
        legendPosition = "rightBottom",
        showTransition = TRUE,
        title = "Measurements on 38 1978-79 model automobiles.\nThe gas mileage in miles per gallon as measured by Consumers Union on a test track."
    )
    warning('dotplot - not appearing correctly - not grouped')
    result
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-dotplot-4", {
    result <- canvasXpress(
        data = dumbbell.y,
        varAnnot = dumbbell.z,
        axisAlgorithm = "wilkinson",
        connectBy = "Connect",
        dotplotType = "stacked",
        graphType = "Dotplot",
        showTransition = TRUE,
        smpTitle = "School",
        sortDir = "descending",
        title = "Gender Earnings Disparity",
        xAxis2Title = "Annual Salary",
        xAxisMinorTicks = FALSE,
        xAxisShow = FALSE,
        xAxisTickFormat = "\\$%sK",
        xAxisTitle = "Annual Salary"
    )
    warning('dotplot - not appearing correctly - no lines')
    result
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})
