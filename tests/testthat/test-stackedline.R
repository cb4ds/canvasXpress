context("canvasXpress Charts - StackedLine")


data <- t(iris[, 1:4])
smpAnnot <- as.matrix(iris[, 5])
colnames(data) <- make.names(rep("S", ncol(data)), unique = T)
rownames(smpAnnot) <- colnames(data)
colnames(smpAnnot) <- "Species"
varAnnot <- t(data.frame(color = c("red", "blue", "green", "yellow")))
colnames(varAnnot) <- rownames(data)


test_that("StackedLine Chart - basic 1", {
    result <- canvasXpress(data, 
                           varAnnot = varAnnot, 
                           colorBy = 'Species',
                           graphType = "StackedLine")
    print(result)
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})


test_that("StackedLine Chart - basic 2", {
    result <- canvasXpress(data, 
                           smpAnnot = smpAnnot, 
                           colorBy = 'Species',
                           graphType = "StackedLine")
    print(result)
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})


test_that("StackedLine Chart - grouped", {
    result <- canvasXpress(data, 
                           smpAnnot = smpAnnot, 
                           groupingFactors = list('Species'),
                           graphType = "StackedLine",
                           graphOrientation = "vertical",
                           smpLabelRotate = 90,
                           legendPosition = "bottom",
                           legendColumns = 2)
    print(result)
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})


# -- From Isaac, web examples --
generic.y   <- readRDS(system.file("extdata", "cX-generic-dat.RData", package = "canvasXpress"))
generic.x   <- readRDS(system.file("extdata", "cX-generic-smp.RData", package = "canvasXpress"))
generic.z   <- readRDS(system.file("extdata", "cX-generic-var.RData", package = "canvasXpress"))


test_that("cX-stackedline-1", {
    result <- canvasXpress(
        data = generic.y,
        smpAnnot = generic.x,
        varAnnot = generic.z,
        graphOrientation = "vertical",
        graphType = "StackedLine",
        lineThickness = 3,
        lineType = "spline",
        showShadow = TRUE,
        showTransition = TRUE,
        smpTitle = "Collection of Samples",
        smpTitleFontStyle = "italic",
        subtitle = "Random Data",
        title = "Stacked-Line Graphs",
        xAxis = c("Variable1", "Variable2"),
        xAxis2 = c("Variable3", "Variable4")
    )
    print(result)
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-stackedline-2", {
    result <- canvasXpress(
        data = generic.y,
        smpAnnot = generic.x,
        varAnnot = generic.z,
        coordinateLineColor = TRUE,
        graphOrientation = "horizontal",
        graphType = "StackedLine",
        legendInside = TRUE,
        legendPosition = "right",
        lineThickness = 3,
        lineType = "spline",
        showShadow = TRUE,
        smpTitle = "Collection of Samples",
        smpTitleFontStyle = "italic",
        subtitle = "Random Data",
        title = "Stacked-Line Graphs",
        xAxis = c("Variable1", "Variable2"),
        xAxis2 = c("Variable3", "Variable4")
    )
    print(result)
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})
