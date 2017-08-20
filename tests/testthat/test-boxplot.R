context("canvasXpress Charts - Boxplot")


data <- t(iris[,1:4])
smpAnnot <- t(as.matrix(iris[,5]))
colnames(smpAnnot) <- colnames(data)
rownames(smpAnnot) <- "Species"
varAnnot <- as.matrix(iris[,5])
colnames(varAnnot) <- "Species"


test_that("Boxplot - basic 1", {
    result <- canvasXpress(data, 
                           smpAnnot = smpAnnot, 
                           groupingFactors = list('Species'),
                           graphType = "Boxplot")
    result
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})


test_that("Boxplot - summarized input", {
    data <- data.frame(iqr1   = c(3,  25),
                       iqr3   = c(10, 30),
                       qtl1   = c(6,  25),
                       qtl3   = c(10, 29),
                       median = c(8,  27))

    data.box <- t(data)

    data.box.out <- data
    data.box.out$outliers <- c("2, 40", NA)
    data.box.out <- t(data.box.out)

    
    result <- canvasXpress(graphType = "Boxplot",
                               data = data.box,
                               boxplotGroupData = "TESTING")
    result
    warning('boxplot - summarized data not handled yet')
    
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")

    result <- canvasXpress(graphType = "Boxplot",
                           data = data.box.out,
                           boxplotGroupData = "TESTING")
    result
    warning('boxplot - summarized data not handled yet')
    
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})
