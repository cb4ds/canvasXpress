context("canvasXpress Charts - Boxplot")


data <- t(iris[, 1:4])
smpAnnot <- as.matrix(iris[, 5])
colnames(data) <- make.names(rep("S", ncol(data)), unique = T)
rownames(smpAnnot) <- colnames(data)
colnames(smpAnnot) <- "Species"
varAnnot <- t(data.frame(color = c("red", "blue", "green", "yellow")))
colnames(varAnnot) <- rownames(data)


test_that("Boxplot - basic 1", {
    result <- canvasXpress(
        data,
        smpAnnot = smpAnnot,
        groupingFactors = list('Species'),
        graphType = "Boxplot"
    )
    result
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})


test_that("Boxplot - summarized input", {
    data <- data.frame(
        iqr1   = c(3,  25),
        iqr3   = c(10, 30),
        qtl1   = c(6,  25),
        qtl3   = c(10, 29),
        median = c(8,  27)
    )
    
    data.box <- t(data)
    
    data.box.out <- data
    data.box.out$outliers <- c("2, 40", NA)
    data.box.out <- t(data.box.out)
    
    
    result <- canvasXpress(graphType = "Boxplot",
                           data = data.box,
                           boxplotGroupData = "TESTING")
    result
    warning('boxplot - summarized data not handled yet')
    
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
    
    result <- canvasXpress(graphType = "Boxplot",
                           data = data.box.out,
                           boxplotGroupData = "TESTING")
    result
    warning('boxplot - summarized data not handled yet')
    
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})


# -- From Isaac, web examples --
iris.y           <- readRDS(system.file("extdata", "cX-iris-dat.RData", package = "canvasXpress"))
iris.x           <- readRDS(system.file("extdata", "cX-iris-smp.RData", package = "canvasXpress"))
boxplot.y        <- readRDS(system.file("extdata", "cX-boxplot-dat.RData", package = "canvasXpress"))
boxplot.x        <- readRDS(system.file("extdata", "cX-boxplot-smp.RData", package = "canvasXpress"))
boxplot1.y       <- readRDS(system.file("extdata", "cX-boxplot1-dat.RData", package = "canvasXpress"))
boxplot1.x       <- readRDS(system.file("extdata", "cX-boxplot1-smp.RData", package = "canvasXpress"))
scents.y         <- readRDS(system.file("extdata", "cX-scents-dat.RData", package = "canvasXpress"))
scents.x         <- readRDS(system.file("extdata", "cX-scents-smp.RData", package = "canvasXpress"))
cancersurvival.y <- readRDS(system.file("extdata", "cX-cancersurvival-dat.RData", package = "canvasXpress"))
cancersurvival.x <- readRDS(system.file("extdata", "cX-cancersurvival-smp.RData", package = "canvasXpress"))
cancersurvival.z <- readRDS(system.file("extdata", "cX-cancersurvival-var.RData", package = "canvasXpress"))

test_that("cX-boxplot-1", {
    result <- canvasXpress(
        data = iris.y,
        smpAnnot = iris.x,
        axisTickFontStyle = "bold",
        axisTitleFontStyle = "italic",
        citation = "R. A. Fisher (1936). The use of multiple measurements in taxonomic problems. Annals of Eugenics 7 (2) => 179-188.",
        citationFontStyle = "italic",
        decorations = list(marker = c(
            list(
                sample = "setosa",
                text = "Species with\nlowest petal\nwidth",
                variable = "Petal.Width",
                x = 0.4,
                y = 0.85
            )
        )),
        fontStyle = "italic",
        graphOrientation = "vertical",
        graphType = "Boxplot",
        legendBox = FALSE,
        showShadow = TRUE,
        showTransition = TRUE,
        smpLabelFontStyle = "italic",
        smpLabelRotate = 90,
        smpTitle = "Species",
        title = "Iris flower data set",
        xAxis2Show = FALSE
    )
    result
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-boxplot-2", {
    result <- canvasXpress(
        data = iris.y,
        smpAnnot = iris.x,
        axisTickFontStyle = "bold",
        axisTitleFontStyle = "italic",
        citation = "R. A. Fisher (1936). The use of multiple measurements in taxonomic problems. Annals of Eugenics 7 (2) => 179-188.",
        citationFontStyle = "italic",
        colorScheme = "White",
        decorations = list(marker = c(
            list(
                sample = "setosa",
                text = "Species with\nlowest petal\nwidth",
                variable = "Petal.Width",
                x = 0.4,
                y = 0.85
            )
        )),
        fontStyle = "italic",
        graphOrientation = "vertical",
        graphType = "Boxplot",
        legendBox = FALSE,
        showShadow = TRUE,
        smpLabelFontStyle = "italic",
        smpLabelRotate = 90,
        smpTitle = "Species",
        title = "Iris flower data set",
        xAxis2Show = FALSE
    )
    result
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-boxplot-3", {
    result <- canvasXpress(
        data = iris.y,
        smpAnnot = iris.x,
        axisTickFontStyle = "bold",
        axisTitleFontStyle = "italic",
        citation = "R. A. Fisher (1936). The use of multiple measurements in taxonomic problems. Annals of Eugenics 7 (2) => 179-188.",
        citationFontStyle = "italic",
        decorations = list(marker = c(
            list(
                sample = "setosa",
                text = "Species with\nlowest petal\nwidth",
                variable = "Petal.Width",
                x = 0.4,
                y = 0.85
            )
        )),
        fontStyle = "italic",
        graphOrientation = "vertical",
        graphType = "Boxplot",
        legendBox = FALSE,
        showShadow = TRUE,
        showViolinBoxplot = TRUE,
        smpLabelFontStyle = "italic",
        smpLabelRotate = 90,
        smpTitle = "Species",
        title = "Iris flower data set",
        xAxis2Show = FALSE
    )
    result
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-boxplot-4", {
    result <- canvasXpress(
        data = iris.y,
        smpAnnot = iris.x,
        axisTickFontStyle = "bold",
        axisTitleFontStyle = "italic",
        citation = "R. A. Fisher (1936). The use of multiple measurements in taxonomic problems. Annals of Eugenics 7 (2) => 179-188.",
        citationFontStyle = "italic",
        colorScheme = "White",
        decorations = list(marker = c(
            list(
                sample = "setosa",
                text = "Species with\nlowest petal\nwidth",
                variable = "Petal.Width",
                x = 0.4,
                y = 0.85
            )
        )),
        fontStyle = "italic",
        graphOrientation = "vertical",
        graphType = "Boxplot",
        legendBox = FALSE,
        showShadow = TRUE,
        showViolinBoxplot = TRUE,
        smpLabelFontStyle = "italic",
        smpLabelRotate = 90,
        smpTitle = "Species",
        title = "Iris flower data set",
        xAxis2Show = FALSE
    )
    result
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-boxplot-5", {
    result <- canvasXpress(
        data = boxplot.y,
        smpAnnot = boxplot.x,
        axisTitleFontStyle = "italic",
        graphType = "Boxplot",
        jitter = TRUE,
        legendInside = TRUE,
        legendPosition = "right",
        showBoxplotOriginalData = TRUE,
        showTransition = TRUE,
        subtitle = "Boxplot Graph",
        title = "Random Data",
        xAxisTickFormat = "%.0f M"
    )
    result
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-boxplot-6", {
    result <- canvasXpress(
        data = boxplot1.y,
        smpAnnot = boxplot1.x,
        axisTitleFontStyle = "italic",
        colorBy = "Factor1",
        connectBy = "Subject",
        graphOrientation = "vertical",
        graphType = "Boxplot",
        jitter = FALSE,
        showBoxplotOriginalData = TRUE,
        showLegend = FALSE,
        subtitle = "Boxplot Graph",
        title = "Random Data"
    )
    result
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-boxplot-7", {
    result <- canvasXpress(
        data = scents.y,
        smpAnnot = scents.x,
        axisTitleFontStyle = "italic",
        citation = "Hirsch, A. R., and Johnston, L. H. Odors and Learning, Smell & Taste Treatment and Research Foundation, Chicago.",
        graphOrientation = "vertical",
        graphType = "Boxplot",
        jitter = TRUE,
        legendBox = FALSE,
        plotByVariable = TRUE,
        showBoxplotOriginalData = TRUE,
        smpLabelRotate = 90,
        smpTitle = "Smoking Status",
        smpTitleFontStyle = "italic",
        title = "Data on the time subjects required to complete a pencil and paper maze\nwhen they were smelling a floral scent and when they were not."
    )
    result
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-boxplot-8", {
    result <- canvasXpress(
        data = cancersurvival.y,
        smpAnnot = cancersurvival.x,
        varAnnot = cancersurvival.z,
        axisTitleFontStyle = "italic",
        citation = "Cameron, E. and Pauling, L. (1978). Proceedings of the National Academy of Science USA, 75.",
        graphType = "Boxplot",
        showLegend = FALSE,
        showShadow = TRUE,
        showTransition = TRUE,
        title = "Patients with advanced cancers of the stomach,\nbronchus, colon, ovary or breast treated with ascorbate.",
        xAxisTitle = "Survival time (days)"
    )
    result
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})
