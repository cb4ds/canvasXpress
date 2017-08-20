context("canvasXpress Charts - TagCloud")


data <- t(iris[,1:4])


test_that("Tag Cloud - basic 1", {
    result <- canvasXpress(data,
                           graphType = "TagCloud")
    result
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})


test_that("Tag Cloud - basic 2", {
    result <- canvasXpress(t(data), 
                           graphType = "TagCloud")
    result
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

