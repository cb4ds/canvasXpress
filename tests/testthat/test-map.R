context("canvasXpress Charts - Map")
ifelse(interactive(), source("tests/cX-function.R"), source("../cX-function.R"))


test_that("cXmap1", {
    result <- cXmap1()
    if (interactive()) { print(result) }
    
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cXmap2", {
    result <- cXmap2()
    if (interactive()) { print(result) }
    
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cXmap3", {
    stop('data issue - no data required but not yet handled')
    
    result <- cXmap3()
    if (interactive()) { print(result) }
    
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cXmap4", {
    stop('data issue - no data required but not yet handled')
    
    result <- cXmap4()
    if (interactive()) { print(result) }
    
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cXmap5", {
    stop('data issue - no data required but not yet handled')
    
    result <- cXmap5()
    if (interactive()) { print(result) }
    
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})
