context("canvasXpress Charts - Stacked")


data <- t(iris[, 1:4])
smpAnnot <- as.matrix(iris[, 5])
colnames(data) <- make.names(rep("S", ncol(data)), unique = T)
rownames(smpAnnot) <- colnames(data)
colnames(smpAnnot) <- "Species"
varAnnot <- t(data.frame(color = c("red", "blue", "green", "yellow")))
colnames(varAnnot) <- rownames(data)


test_that("Stacked Chart - basic 1", {
    result <- canvasXpress(data, 
                           varAnnot = varAnnot, 
                           colorBy = 'Species',
                           graphType = "Stacked")
    
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})


test_that("Stacked Chart - basic 2", {
    result <- canvasXpress(data, 
                           smpAnnot = smpAnnot, 
                           colorBy = 'Species',
                           graphType = "Stacked")
    
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})


test_that("Stacked Chart - grouped", {
    result <- canvasXpress(data, 
                           smpAnnot = smpAnnot, 
                           groupingFactors = list('Species'),
                           graphType = "Stacked",
                           graphOrientation = "vertical",
                           smpLabelRotate = 90,
                           legendPosition = "bottom",
                           legendColumns = 2,
                           xAxis2Show = FALSE,
                           showTransition = TRUE)
    
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

