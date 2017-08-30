context("canvasXpress Charts - ParallelCoordinates")


data <- t(iris[, 1:4])
smpAnnot <- as.matrix(iris[, 5])
colnames(data) <- make.names(rep("S", ncol(data)), unique = T)
rownames(smpAnnot) <- colnames(data)
colnames(smpAnnot) <- "Species"
varAnnot <- t(data.frame(color = c("red", "blue", "green", "yellow")))
colnames(varAnnot) <- rownames(data)


test_that("ParallelCoordinates - basic 1", {
    result <- canvasXpress(data, 
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

