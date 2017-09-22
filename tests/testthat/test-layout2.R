context("canvasXpress Charts - Layout")

y <- read.table(system.file("extdata", "cX-generic-dat.txt", package = "canvasXpress"), header = TRUE, sep = "\t", quote = "", row.names = 1, fill = TRUE, check.names = FALSE, stringsAsFactors = FALSE)
x <- read.table(system.file("extdata", "cX-generic-smp.txt", package = "canvasXpress"), header = TRUE, sep = "\t", quote = "", row.names = 1, fill = TRUE, check.names = FALSE, stringsAsFactors = FALSE)
z <- read.table(system.file("extdata", "cX-generic-var.txt", package = "canvasXpress"), header = TRUE, sep = "\t", quote = "", row.names = 1, fill = TRUE, check.names = FALSE, stringsAsFactors = FALSE)


test_that("segregation layout change - Samples", {
    result <- canvasXpress(data      = y,
                           smpAnnot  = x,
                           varAnnot  = z,
                           graphType = "Bar",
                           segregateSamplesBy = list("Factor1"),
                           layout = "1X3"
    )
                        
    if (interactive()) { print(result) }
    
    fail("layout is not 1x3")
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("segregation layout change - Variables", {
    result <- canvasXpress(data      = y,
                           smpAnnot  = x,
                           varAnnot  = z,
                           graphType = "Bar",
                           segregateVariablesBy = list("Annt1"),
                           layout = "1X4"
    )
    
    if (interactive()) { print(result) }
    
    fail("layout is not 1x4")
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})
