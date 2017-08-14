context("canvasXpress")


test_that("Area Chart 1 normal", {
    data <- t(iris[,1:4])
    result <- canvasXpress(data, 
                           graphType = "Area")
    
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})


test_that("Area Chart 1 percent", {
    data <- t(iris[,1:4])
    result <- canvasXpress(data, 
                           graphType = "Area",
                           areaType = "percent")
    
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})


test_that("Area Chart 1 stacked", {
    data <- t(iris[,1:4])
    result <- canvasXpress(data, 
                           graphType = "Area",
                           areaType = "stacked")
    
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})


test_that("Area Chart - steam plot", {
    m <- 200
    n <- 10
    x <- seq(m)
    y <- matrix(0, nrow=m, ncol=n)
    colnames(y) <- seq(n)
    for(i in seq(ncol(y))){
        mu <- runif(1, min=.25*m, max=.75*m)
        SD <- runif(1, min=5, max=10)
        TMP <- rnorm(1000, mean=mu, sd=SD)
        HIST <- hist(TMP, breaks=c(0,x), plot=FALSE)
        fit <- smooth.spline(HIST$counts ~ HIST$mids)
        y[,i] <- fit$y
    }
    y <- replace(y, abs(y)<0.1, 0)
    
    data <- data.frame(x = x, y = y)
    
    result <- canvasXpress(data,
                           graphType        = "Area",
                           areaType         = "Stacked",
                           lineType         = "Area",
                           graphOrientation = "vertical",
                           colorScheme      = "ColorSpectrum",
                           colorSpectrum    = list("blue", "cyan", "yellow", "red"),
                           showLegend       = FALSE,
                           subtitle         = "TEST subtitle",
                           title            = "Steam Plot",
                           xAxisTitle       = "" )
    
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})
