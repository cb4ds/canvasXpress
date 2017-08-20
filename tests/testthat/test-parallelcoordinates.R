context("canvasXpress Charts - ParallelCoordinates")


data <- t(iris[,1:4])
smpAnnot <- t(as.matrix(iris[,5]))
colnames(smpAnnot) <- colnames(data)
rownames(smpAnnot) <- "Species"
varAnnot <- as.matrix(iris[,5])
colnames(varAnnot) <- "Species"


test_that("ParallelCoordinates - basic 1", {
    result <- canvasXpress(t(data), 
                           varAnnot = varAnnot, 
                           colorBy = 'Species',
                           graphType = "ParallelCoordinates")
    
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})


test_that("Bar Chart - basic 2", {
    result <- canvasXpress(data, 
                           smpAnnot = smpAnnot, 
                           colorBy = 'Species',
                           graphType = "ParallelCoordinates")
    
    warning('line - graph incorrect - no lines, shaped points')
    
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

