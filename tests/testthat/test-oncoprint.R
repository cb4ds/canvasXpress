context("canvasXpress Charts - Oncoprint")
ifelse(interactive(), source("tests/cX-function.R"), source("../cX-function.R"))


test_that("cXoncoprint1", {
    stop('data issue')
    
    result <- cXoncoprint1()
    if (interactive()) { print(result) }
    
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cXoncoprint2", {
    stop('data issue')
    
    result <- cXoncoprint2()
    if (interactive()) { print(result) }
    
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cXoncoprint3", {
    stop('data issue')
    
    result <- cXoncoprint3()
    if (interactive()) { print(result) }
    
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})
