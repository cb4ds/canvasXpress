context("canvasXpress Charts - Boxplot")


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
        groupingFactors = list("Species"),
        axisTickFontStyle = "bold",
        axisTitleFontStyle = "italic",
        citation = "R. A. Fisher (1936). The use of multiple measurements in taxonomic problems. Annals of Eugenics 7 (2) => 179-188.",
        citationFontStyle = "italic",
        decorations = list(marker = list(
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
        # showTransition = TRUE,
        smpLabelFontStyle = "italic",
        smpLabelRotate = 90,
        smpTitle = "Species",
        title = "Iris flower data set",
        xAxis2Show = FALSE
    )
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-boxplot-2", {
    result <- canvasXpress(
        data = iris.y,
        smpAnnot = iris.x,
        groupingFactors = list("Species"),
        axisTickFontStyle = "bold",
        axisTitleFontStyle = "italic",
        citation = "R. A. Fisher (1936). The use of multiple measurements in taxonomic problems. Annals of Eugenics 7 (2) => 179-188.",
        citationFontStyle = "italic",
        colorScheme = "White",
        decorations = list(marker = list(
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
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-boxplot-3", {
    result <- canvasXpress(
        data = iris.y,
        smpAnnot = iris.x,
        groupingFactors = list("Species"),
        axisTickFontStyle = "bold",
        axisTitleFontStyle = "italic",
        citation = "R. A. Fisher (1936). The use of multiple measurements in taxonomic problems. Annals of Eugenics 7 (2) => 179-188.",
        citationFontStyle = "italic",
        decorations = list(marker = list(
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
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-boxplot-4", {
    result <- canvasXpress(
        data = iris.y,
        smpAnnot = iris.x,
        groupingFactors = list("Species"),
        axisTickFontStyle = "bold",
        axisTitleFontStyle = "italic",
        citation = "R. A. Fisher (1936). The use of multiple measurements in taxonomic problems. Annals of Eugenics 7 (2) => 179-188.",
        citationFontStyle = "italic",
        colorScheme = "White",
        decorations = list(marker = list(
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
        xAxis = list("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
        xAxis2Show = FALSE
    )
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-boxplot-5", {
    result <- canvasXpress(
        data = boxplot.y,
        smpAnnot = boxplot.x,
        groupingFactors = list("Factor1"),
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
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-boxplot-6", {
    result <- canvasXpress(
        data = boxplot1.y,
        smpAnnot = boxplot1.x,
        groupingFactors = list("Factor1"),
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
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-boxplot-7", {
    result <- canvasXpress(
        data = scents.y,
        smpAnnot = scents.x,
        groupingFactors = list("Smoker"),
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
        xAxis = list("U-Trial 1", "U-Trial 2", "U-Trial 3", 
                     "S-Trial 1", "S-Trial 2", "S-Trial 3"),
        xAxisTitle = "",
        title = "Data on the time subjects required to complete a pencil and paper maze\nwhen they were smelling a floral scent and when they were not."
    )
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-boxplot-8", {
    result <- canvasXpress(
        data = cancersurvival.y,
        smpAnnot = cancersurvival.x,
        varAnnot = cancersurvival.z,
        groupingFactors = list("Organ"),
        axisTitleFontStyle = "italic",
        citation = "Cameron, E. and Pauling, L. (1978). Proceedings of the National Academy of Science USA, 75.",
        graphType = "Boxplot",
        showLegend = FALSE,
        showShadow = TRUE,
        showTransition = TRUE,
        title = "Patients with advanced cancers of the stomach,\nbronchus, colon, ovary or breast treated with ascorbate.",
        xAxisTitle = "Survival time (days)"
    )
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-layout-4", {
    result <- canvasXpress(
        data = iris.y,
        smpAnnot = iris.x,
        axisTickFontStyle = "bold",
        axisTitleFontStyle = "italic",
        citation = "R. A. Fisher (1936). The use of multiple measurements in taxonomic problems. Annals of Eugenics 7 (2) => 179-188.",
        citationFontStyle = "italic",
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
    warning('boxplot - not appearing correctly - not grouped or layed out')
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-layout-8", {
    result <- canvasXpress(
        data = scents.y,
        smpAnnot = scents.x,
        graphOrientation = "vertical",
        graphType = "Boxplot",
        showTransition = TRUE,
        smpLabelRotate = 90,
        smpTitle = "Smoking Status"
    )
    warning('boxplot - not appearing correctly - not grouped or layed out')
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})
