context("canvasXpress Charts - Scatter3D")


data <- t(iris[, 1:4])
smpAnnot <- as.matrix(iris[, 5])
colnames(data) <- make.names(rep("S", ncol(data)), unique = T)
rownames(smpAnnot) <- colnames(data)
colnames(smpAnnot) <- "Species"
varAnnot <- t(data.frame(color = c("red", "blue", "green", "yellow")))
colnames(varAnnot) <- rownames(data)


test_that("Scatter3D Chart - basic 1", {
    result <- canvasXpress(data, 
                           varAnnot = varAnnot, 
                           colorBy = 'Species',
                           graphType = 'Scatter3D')
    result
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

