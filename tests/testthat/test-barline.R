context("canvasXpress")


test_that("BarLine Chart - basic 1", {
    data <- t(iris[,1:4])
    varAnnot <- as.matrix(iris[,5])
    colnames(varAnnot) <- "Species"
    
    result <- canvasXpress(t(data), 
                           varAnnot = varAnnot, 
                           graphOrientation = "vertical",
                           graphType = "BarLine", 
                           colorBy = 'Species')
    
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})


test_that("BarLine Chart - basic 2", {
    data <- t(iris[,1:4])
    smpAnnot <- t(as.matrix(iris[,5]))
    colnames(smpAnnot) <- colnames(data)
    rownames(smpAnnot) <- "Species"
    
    result <- canvasXpress(data, 
                           smpAnnot = smpAnnot, 
                           graphOrientation = "vertical",
                           graphType = "BarLine", 
                           colorBy = 'Species')
    
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})


test_that("BarLine Chart - x2specified", {
    warning('not done - need a implemented')
})

