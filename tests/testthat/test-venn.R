context("canvasXpress Charts - Venn")
ifelse(interactive(), source("tests/cX-function.R"), source("../cX-function.R"))


test_that("cXvenn1", {
    stop('data issue')
    
    result <- cXvenn1()
    if (interactive()) { print(result) }
    
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cXvenn2", {
    stop('data issue')
    
    result <- cXvenn2()
    if (interactive()) { print(result) }
    
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cXvenn3", {
    stop('data issue')
    
    result <- cXvenn1()
    if (interactive()) { print(result) }
    
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})
