context("canvasXpress Charts - Boxplot")

precalc.data <- data.frame(iqr1   = c(45,   7.3, 8),     qtl1     = c(109, 14.9, 4),
                           median = c(159, 20.1, 7.25),  qtl3     = c(249, 26.2, 11.3),
                           iqr3   = c(337, 40.6, 17.4),  outliers = c("", "44.5, 46", ""),
                           type   = c("Assault", "Rape", "Murder"),
                           stringsAsFactors = F)



test_that("precalculated boxplot", {
    result <- canvasXpress(data                  =  as.list(precalc.data),
                           graphType             = "Boxplot",
                           precalculatedBoxplot  = TRUE,
                           groupingFactors       = list("type"),
                           smpLabelFontStyle     = "italic",
                           smpLabelRotate        = 90,
                           showLegend            = FALSE,
                           title                 = "US Arrests by Type",
                           titleScaleFontFactor  = 0.5)
    
    if (interactive()) { print(result) }
    
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})
