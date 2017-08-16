context("canvasXpress")


data <- t(iris[,1:4])


test_that("Correlation - basic 1", {
    result <- canvasXpress(t(data), 
                           graphType = "Correlation")
    
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})
