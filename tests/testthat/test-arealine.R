context("canvasXpress Charts - AreaLine")


data <- t(iris[,1:4])
smpAnnot <- t(as.matrix(iris[,5]))
colnames(smpAnnot) <- colnames(data)
rownames(smpAnnot) <- "Species"
varAnnot <- as.matrix(iris[,5])
colnames(varAnnot) <- "Species"


test_that("AreaLine Chart - basic 1", {
    result <- canvasXpress(t(data), 
                           varAnnot = varAnnot, 
                           graphOrientation = "vertical",
                           graphType = "AreaLine", 
                           colorBy = 'Species')
    result
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})


test_that("AreaLine Chart - basic 2", {
    result <- canvasXpress(data, 
                           smpAnnot = smpAnnot, 
                           graphOrientation = "vertical",
                           graphType = "AreaLine", 
                           colorBy = 'Species')
    result
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})         


test_that("AreaLine Chart - x2specified", {
    warning('arealine - need <a> implemented')
})
