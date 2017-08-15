context("canvasXpress")


test_that("StackedLine Chart - basic 1", {
    data <- t(iris[,1:4])
    varAnnot <- as.matrix(iris[,5])
    colnames(varAnnot) <- "Species"
    
    result <- canvasXpress(t(data), 
                           varAnnot = varAnnot, 
                           colorBy = 'Species',
                           graphType = "StackedLine")
    
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})


test_that("StackedPercent Chart - basic 2", {
    data <- t(iris[,1:4])
    smpAnnot <- t(as.matrix(iris[,5]))
    colnames(smpAnnot) <- colnames(data)
    rownames(smpAnnot) <- "Species"
    
    result <- canvasXpress(data, 
                           smpAnnot = smpAnnot, 
                           colorBy = 'Species',
                           graphType = "StackedLine")
    
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})


test_that("StackedPercent Chart - grouped", {
    data <- t(iris[,1:4])
    smpAnnot <- t(as.matrix(iris[,5]))
    colnames(smpAnnot) <- colnames(data)
    rownames(smpAnnot) <- "Species"
    
    result <- canvasXpress(data, 
                           smpAnnot = smpAnnot, 
                           groupingFactors = list('Species'),
                           graphType = "StackedLine",
                           graphOrientation = "vertical",
                           smpLabelRotate = 90,
                           legendPosition = "bottom",
                           legendColumns = 2,
                           xAxis2Show = FALSE,
                           showTransition = TRUE)
    
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

