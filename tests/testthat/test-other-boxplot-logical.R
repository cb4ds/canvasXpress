context("canvasXpress Charts - boxplots logical groups")

test_that("boxplot values are logical", {
    vals <- c(0.41,0.39,0.49,0.34,0.34,0.38)
    vars <- c("QC_PercentDuplication")
    smps <- c("1","2","3","4","5","6")
    data <- as.data.frame(matrix(vals, nrow = 1, ncol = 6, byrow = TRUE, dimnames = list(vars, smps)))
    varx <- c("PlatformType")
    valx <- c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE)
    datx <- as.data.frame(matrix(valx, nrow = 1, ncol = 6, byrow = TRUE, dimnames = list(varx, smps)))

    result <- canvasXpress(
        data             = data,
        smpAnnot         = datx,
        graphType        = "Boxplot",
        graphOrientation = "vertical",
        groupingFactors  = list("PlatformType"),
        colorBy          = "PlatformType",
        title            = "BoxPlot uses logical True and False")

    check_ui_test(result)
})
