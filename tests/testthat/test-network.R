context("canvasXpress Charts - Network")
ifelse(interactive(), source("tests/cX-function.R"), source("../cX-function.R"))


test_that("cXnetwork1", {
    stop('not yet implemented')
    
    result <- cXnetwork1()
    if (interactive()) { print(result) }
    
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cXnetwork2", {
    stop('not yet implemented')
    
    result <- cXnetwork2()
    if (interactive()) { print(result) }
    
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})


test_that("cXnetwork3", {
    stop('not yet implemented')
    
    result <- cXnetwork3()
    if (interactive()) { print(result) }
    
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})


test_that("cXnetwork4", {
    stop('not yet implemented')

    result <- cXnetwork4()
    if (interactive()) { print(result) }
    
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

