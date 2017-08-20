context("canvasXpress Charts - Scatter2D")


data <- t(iris[,1:4])
smpAnnot <- t(as.matrix(iris[,5]))
colnames(smpAnnot) <- colnames(data)
rownames(smpAnnot) <- "Species"
varAnnot <- as.matrix(iris[,5])
colnames(varAnnot) <- "Species"


test_that("Scatter2D Chart - basic 1", {
    result <- canvasXpress(t(data), 
                           varAnnot = varAnnot, 
                           colorBy = 'Species')
    result
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

