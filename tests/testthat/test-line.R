context("canvasXpress Charts - Line")


data <- t(iris[, 1:4])
smpAnnot <- as.matrix(iris[, 5])
colnames(data) <- make.names(rep("S", ncol(data)), unique = T)
rownames(smpAnnot) <- colnames(data)
colnames(smpAnnot) <- "Species"
varAnnot <- t(data.frame(color = c("red", "blue", "green", "yellow")))
colnames(varAnnot) <- rownames(data)


test_that("Line Chart - basic 1", {
    result <- canvasXpress(data, 
                           varAnnot = varAnnot, 
                           graphOrientation = "vertical",
                           graphType = "Line", 
                           colorBy = 'Species')
    print(result)
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})


test_that("Line Chart - basic 2", {
    result <- canvasXpress(data, 
                           smpAnnot = smpAnnot, 
                           graphOrientation = "vertical",
                           graphType = "Line", 
                           colorBy = 'Species')
    
    warning('line - graph incorrect - no lines, shaped points')
    print(result)
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})         


# -- From Isaac, web examples --
generic.y <- readRDS(system.file("extdata", "cX-generic-dat.RData", package = "canvasXpress"))
generic.x <- readRDS(system.file("extdata", "cX-generic-smp.RData", package = "canvasXpress"))
generic.z <- readRDS(system.file("extdata", "cX-generic-var.RData", package = "canvasXpress"))
line.y    <- readRDS(system.file("extdata", "cX-line-dat.RData", package = "canvasXpress"))
line.x    <- readRDS(system.file("extdata", "cX-line-smp.RData", package = "canvasXpress"))

test_that("cX-line-1", {
    result <- canvasXpress(
        data = generic.y,
        smpAnnot = generic.x,
        varAnnot = generic.z,
        colorScheme = "basic",
        graphOrientation = "vertical",
        graphType = "Line",
        lineType = "spline",
        showAnimation = FALSE,
        showShadow = TRUE,
        showTransition = TRUE,
        smpLabelRotate = 45,
        smpOverlays = c("Factor1", "Factor2", "Factor3"),
        smpTitle = "Collection of Samples",
        smpTitleFontStyle = "italic",
        subtitle = "Random Data",
        title = "Line Graphs"
    )
    warning('line - graph incorrect - missing overlays')
    print(result)
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-line-2", {
    result <- canvasXpress(
        data = generic.y,
        smpAnnot = generic.x,
        varAnnot = generic.z,
        background = "rgb(226,236,248)",
        backgroundType = "window",
        blockContrast = TRUE,
        evenColor = "rgb(226,236,248)",
        graphOrientation = "horizontal",
        graphType = "Line",
        legendBackgroundColor = FALSE,
        legendInside = TRUE,
        legendPosition = "right",
        showAnimation = TRUE,
        showShadow = TRUE,
        smpOverlays = c("Factor1", "Factor2", "Factor3"),
        smpTitle = "Collection of Samples",
        smpTitleFontStyle = "italic",
        title = "Random Data"
    )
    warning('line - graph incorrect - missing overlays, incorrect direction')
    print(result)
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-line-3", {
    result <- canvasXpress(
        data = line.y,
        smpAnnot = line.x,
        graphOrientation = "vertical",
        graphType = "Line",
        lineErrorType = "area",
        lineType = "spline",
        showTransition = TRUE
    )
    warning('line - graph incorrect - missing smoothing/grouping')
    print(result)
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})
