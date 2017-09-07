context("canvasXpress Web Charts - Venn")
ifelse(interactive(), source("tests/cX-function.R"), source("../cX-function.R"))


test_that("cXvenn1", {
    result <- cXvenn1()
    if (interactive()) { print(result) }
    
    fail('disappears after showing - implementation is a tria.')
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cXvenn2", {
    stop('not yet implemented')
    
    result <- cXvenn2()
    if (interactive()) { print(result) }
    
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cXvenn3", {
    stop('not yet implemented')
    
    result <- cXvenn1()
    if (interactive()) { print(result) }
    
    fail('disappears after showing')
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})
