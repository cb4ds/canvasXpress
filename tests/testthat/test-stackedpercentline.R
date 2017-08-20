context("canvasXpress Charts - StackedPercentLine")


data <- t(iris[,1:4])
smpAnnot <- t(as.matrix(iris[,5]))
colnames(smpAnnot) <- colnames(data)
rownames(smpAnnot) <- "Species"
varAnnot <- as.matrix(iris[,5])
colnames(varAnnot) <- "Species"


test_that("StackedPercentLine Chart - basic 1", {
    result <- canvasXpress(t(data), 
                           varAnnot = varAnnot, 
                           colorBy = 'Species',
                           graphType = "StackedPercentLine")
    result
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})


test_that("StackedPercentLine Chart - basic 2", {
    result <- canvasXpress(data, 
                           smpAnnot = smpAnnot, 
                           colorBy = 'Species',
                           graphType = "StackedPercentLine")
    result
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})


test_that("StackedPercentLine Chart - grouped", {
    result <- canvasXpress(data, 
                           smpAnnot = smpAnnot, 
                           groupingFactors = list('Species'),
                           graphType = "StackedPercentLine",
                           graphOrientation = "vertical",
                           smpLabelRotate = 90,
                           legendPosition = "bottom",
                           legendColumns = 2)
    result
    warning('stackedpercentline - graph incorrect - axis issue')
    
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

