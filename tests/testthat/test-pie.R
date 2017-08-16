context("canvasXpress")


data <- t(iris[,1:4])


test_that("Bar Chart - basic 1", {
    result <- canvasXpress(t(data), 
                           graphType = "Pie")
    
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})


test_that("Bar Chart - basic 2", {
    result <- canvasXpress(data, 
                           graphType = "Pie")
    
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

