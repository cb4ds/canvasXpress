context("canvasXpress Charts - DotLine")


data <- t(iris[,1:4])
smpAnnot <- t(as.matrix(iris[,5]))
colnames(smpAnnot) <- colnames(data)
rownames(smpAnnot) <- "Species"
varAnnot <- as.matrix(iris[,5])
colnames(varAnnot) <- "Species"


test_that("DotLine Chart - basic 1", {
    result <- canvasXpress(t(data), 
                           varAnnot = varAnnot, 
                           colorBy = 'Species',
                           graphType = "DotLine")
    result
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})


test_that("DotLine Chart - basic 2", {
    result <- canvasXpress(data, 
                           smpAnnot = smpAnnot, 
                           colorBy = 'Species',
                           graphType = "DotLine")
    result
    warning('dotline - graph incorrect - no lines')
    
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})


test_that("DotLine Chart - grouped", {
    result <- canvasXpress(data, 
                           smpAnnot = smpAnnot, 
                           groupingFactors = list('Species'),
                           graphType = "DotLine",
                           graphOrientation = "vertical",
                           smpLabelRotate = 90,
                           legendPosition = "bottom",
                           legendColumns = 2)
    result
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

