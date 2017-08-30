context("canvasXpress Charts - Line")


data <- t(iris[, 1:4])
smpAnnot <- as.matrix(iris[, 5])
colnames(data) <- make.names(rep("S", ncol(data)), unique = T)
rownames(smpAnnot) <- colnames(data)
colnames(smpAnnot) <- "Species"
varAnnot <- t(data.frame(color = c("red", "blue", "green", "yellow")))
colnames(varAnnot) <- rownames(data)


test_that("Line Chart - basic 1", {
    result <- canvasXpress(t(data), 
                           varAnnot = varAnnot, 
                           graphOrientation = "vertical",
                           graphType = "Line", 
                           colorBy = 'Species')
    
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
    
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})         


test_that("AreaLine Chart - x2specified", {
    warning('arealine - need <a> implemented')
})
