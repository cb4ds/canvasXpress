context("canvasXpress Charts - Area")

data <- t(iris[,1:4])


test_that("Area Chart 1 normal", {
    result <- canvasXpress(data, 
                           graphType = "Area")
    result
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})


test_that("Area Chart 1 percent", {
    result <- canvasXpress(data, 
                           graphType = "Area",
                           areaType = "percent")
    result
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})


test_that("Area Chart 1 stacked", {
    result <- canvasXpress(data, 
                           graphType = "Area",
                           areaType = "stacked")
    result
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

