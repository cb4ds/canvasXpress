context("canvasXpress Charts - Scatter2D")


cancersurvivalt.y <- readRDS(system.file("extdata", "cX-cancersurvivalt-dat.RData", package = "canvasXpress"))
cancersurvivalt.x <- readRDS(system.file("extdata", "cX-cancersurvivalt-smp.RData", package = "canvasXpress"))
cancersurvivalt.z <- readRDS(system.file("extdata", "cX-cancersurvivalt-var.RData", package = "canvasXpress"))
alcoholtobaccot.y <- readRDS(system.file("extdata", "cX-alcoholtobaccot-dat.RData", package = "canvasXpress"))
alcoholtobaccot.x <- readRDS(system.file("extdata", "cX-alcoholtobaccot-smp.RData", package = "canvasXpress"))
nonlinearfit.y    <- readRDS(system.file("extdata", "cX-nonlinearfit-dat.RData", package = "canvasXpress"))
irist.y           <- readRDS(system.file("extdata", "cX-irist-dat.RData", package = "canvasXpress"))
irist.z           <- readRDS(system.file("extdata", "cX-irist-var.RData", package = "canvasXpress"))
scentst.y         <- readRDS(system.file("extdata", "cX-scentst-dat.RData", package = "canvasXpress"))
scentst.z         <- readRDS(system.file("extdata", "cX-scentst-var.RData", package = "canvasXpress"))
ageheightt.y      <- readRDS(system.file("extdata", "cX-ageheightt-dat.RData", package = "canvasXpress"))
ageheightt.x      <- readRDS(system.file("extdata", "cX-ageheightt-smp.RData", package = "canvasXpress"))
breastcancert.y   <- readRDS(system.file("extdata", "cX-breastcancert-dat.RData", package = "canvasXpress"))
breastcancert.x   <- readRDS(system.file("extdata", "cX-breastcancert-smp.RData", package = "canvasXpress"))
scatterR.y        <- readRDS(system.file("extdata", "cX-scatterR-dat.RData", package = "canvasXpress"))
scatterR.z        <- readRDS(system.file("extdata", "cX-scatterR-var.RData", package = "canvasXpress"))
scatterR2.y       <- readRDS(system.file("extdata", "cX-scatterR2-dat.RData", package = "canvasXpress"))
scatterR2.z       <- readRDS(system.file("extdata", "cX-scatterR2-var.RData", package = "canvasXpress"))
scatterR3.y       <- readRDS(system.file("extdata", "cX-scatterR3-dat.RData", package = "canvasXpress"))
scatterR3.z       <- readRDS(system.file("extdata", "cX-scatterR3-var.RData", package = "canvasXpress"))
scatterR4.y       <- readRDS(system.file("extdata", "cX-scatterR3-dat.RData", package = "canvasXpress"))
kaplanmeier.y     <- readRDS(system.file("extdata", "cX-kaplanmeier-dat.RData", package = "canvasXpress"))
kaplanmeier.z     <- readRDS(system.file("extdata", "cX-kaplanmeier-var.RData", package = "canvasXpress"))

test_that("cX-histogram-1", {
    result <- canvasXpress(
        data = cancersurvivalt.y,
        smpAnnot = cancersurvivalt.x,
        varAnnot = cancersurvivalt.z,
        axisTitleFontStyle = "italic",
        citation = "Cameron, E. and Pauling, L. (1978). Proceedings of the National Academy of Science USA, 75.",
        graphType = "Scatter2D",
        histogramBins = 50,
        showShadow = TRUE,
        showTransition = TRUE,
        title = "Patients with advanced cancers of the stomach,\nbronchus, colon, ovary or breast treated with ascorbate.",
        xAxisTitle = "Survival (days)",
        yAxisTitle = "Number of Subjects"
    )
    warning('scatter2d - not appearing correctly - histogram grouping')
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-histogram-2", {
    result <- canvasXpress(
        data = cancersurvivalt.y,
        smpAnnot = cancersurvivalt.x,
        varAnnot = cancersurvivalt.z,
        axisTitleFontStyle = "italic",
        citation = "Cameron, E. and Pauling, L. (1978). Proceedings of the National Academy of Science USA, 75.",
        graphType = "Scatter2D",
        histogramBins = 10,
        showShadow = TRUE,
        showTransition = TRUE,
        title = "Patients with advanced cancers of the stomach,\nbronchus, colon, ovary or breast treated with ascorbate.",
        xAxisTitle = "Survival (days)",
        yAxisTitle = "Number of Subjects"
    )
    warning('scatter2d - not appearing correctly - histogram grouping')
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-histogram-3", {
    result <- canvasXpress(
        data = alcoholtobaccot.y,
        smpAnnot = alcoholtobaccot.x,
        citation = "Moore, David S., and George P. McCabe (1989). Introduction to the Practice of Statistics, p. 179.",
        decorations = list(marker = list(
            list(
                sample = c("Alcohol", "Tobacco"),
                text = "Maybe an Outlier?",
                variable = "Northern Ireland",
                x = 0.45,
                y = 0.18
            )
        )),
        graphType = "Scatter2D",
        histogramBins = 20,
        showTransition = TRUE,
        title = "Average weekly household spending, in British pounds, on tobacco products\nand alcoholic beverages for each of the 11 regions of Great Britain.",
        xAxisTitle = "Pounds Spent",
        yAxisTitle = "Frequency"
    )
    warning('scatter2d - not appearing correctly - histogram grouping')
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})


test_that("cX-nonlinear-fit-1", {
    result <- canvasXpress(
        data = nonlinearfit.y,
        graphType = "Scatter2D",
        setMaxY = 350,
        setMinY = 100,
        showDecorations = TRUE,
        xAxisTransform = "log10",
        xAxisTransformTicks = FALSE,
        yAxisExact = TRUE
    )
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-nonlinear-fit-2", {
    result <- canvasXpress(
        data = irist.y,
        varAnnot = irist.z,
        graphType = "Scatter2D",
        showDecorations = TRUE,
        showLegend = FALSE,
        xAxis = c("Sepal.Length", "Sepal.Width"),
        yAxis = c("Petal.Length", "Petal.Width")
    )
    warning('scatter2d - not appearing correctly - nonlinear fit line missing')
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-scatter2d-1", {
    result <- canvasXpress(
        data = alcoholtobaccot.y,
        smpAnnot = alcoholtobaccot.x,
        citation = "Moore, David S., and George P. McCabe (1989). Introduction to the Practice of Statistics, p. 179.",
        decorations = list(marker = list(
            list(
                sample = c("Alcohol", "Tobacco"),
                text = "Maybe an Outlier?",
                variable = "Northern Ireland",
                x = 0.45,
                y = 0.18
            )
        )),
        graphType = "Scatter2D",
        showTransition = TRUE,
        title = "Average weekly household spending, in British pounds, on tobacco products\nand alcoholic beverages for each of the 11 regions of Great Britain.",
        xAxis = c("Alcohol"),
        yAxis = c("Tobacco")
    )
   warning('scatter2d - not appearing correctly - blank')
   if (interactive()) { print(result) }
   expect_s3_class(result, "canvasXpress")
   expect_s3_class(result, "htmlwidget")
})

test_that("cX-scatter2d-2", {
    result <- canvasXpress(
        data = scentst.y,
        varAnnot = scentst.z,
        citation = "Hirsch, A. R., and Johnston, L. H. Odors and Learning, Smell & Taste Treatment and Research Foundation, Chicago.",
        graphType = "Scatter2D",
        setMaxX = 100,
        setMaxY = 150,
        setMinX = 0,
        setMinY = 0,
        shapeBy = "Smoker",
        showTransition = TRUE,
        sizeBy = "Age",
        title = "Data on the time subjects required to complete a pencil and paper maze\nwhen they were smelling a floral scent and when they were not.",
        xAxis = c("U-Trial 1", "U-Trial 2", "U-Trial 3"),
        xAxisExact = TRUE,
        xAxisHistogramShow = TRUE,
        yAxis = c("S-Trial 1", "S-Trial 2", "S-Trial 3"),
        yAxisExact = TRUE,
        yAxisHistogramShow = TRUE
    )
    warning('scatter2d - not appearing correctly - histograms, shapes, colors')
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-scatter2d-3", {
    result <- canvasXpress(
        data = scentst.y,
        varAnnot = scentst.z,
        citation = "Hirsch, A. R., and Johnston, L. H. Odors and Learning, Smell & Taste Treatment and Research Foundation, Chicago.",
        colorScheme = "White",
        graphType = "Scatter2D",
        setMaxX = 100,
        setMaxY = 150,
        setMinX = 0,
        setMinY = 0,
        shapeBy = "Smoker",
        sizeBy = "Age",
        title = "Data on the time subjects required to complete a pencil and paper maze\nwhen they were smelling a floral scent and when they were not.",
        xAxis = c("U-Trial 1", "U-Trial 2", "U-Trial 3"),
        xAxisExact = TRUE,
        xAxisHistogramShow = TRUE,
        yAxis = c("S-Trial 1", "S-Trial 2", "S-Trial 3"),
        yAxisExact = TRUE,
        yAxisHistogramShow = TRUE
    )
    warning('scatter2d - not appearing correctly - histograms, shapes, colors')
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-scatter2d-4", {
    result <- canvasXpress(
        data = ageheightt.y,
        smpAnnot = ageheightt.x,
        citation = "Moore, David S., and George P. McCabe (1989)",
        graphType = "Scatter2D",
        title = "Mean heights of a group of children in Kalama",
        xAxis = c("Age"),
        yAxis = c("Height")
    )
    warning('scatter2d - not appearing correctly - missing points and area')
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-scatter2d-5", {
    result <- canvasXpress(
        data = breastcancert.y,
        smpAnnot = breastcancert.x,
        backgroundType = "window",
        backgroundWindow = "rgb(238,238,238)",
        citation = "Velleman, P. F. and Hoaglin, D. C. (1981).\nApplications, Basics, and Computing of Exploratory Data Analysis. Belmont. CA: Wadsworth, Inc., pp. 127-134.",
        colors = c("rgba(64,64,64,0.5)"),
        decorationsBackgroundColor = "rgb(238,238,238)",
        decorationsBoxColor = "rgb(0,0,0)",
        decorationsPosition = "bottomRight",
        graphType = "Scatter2D",
        legendInside = TRUE,
        plotBox = FALSE,
        showDecorations = TRUE,
        showTransition = TRUE,
        title = "Mean annual temperature (in degrees F) and Mortality Index for neoplasms of the female breast.",
        xAxis = c("Mortality"),
        xAxisTickColor = "rgb(255,255,255)",
        yAxis = c("Temperature"),
        yAxisTickColor = "rgb(255,255,255)"
    )
    warning('scatter2d - not appearing correctly - missing points and area')
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-scatter2d-6", {
    result <- canvasXpress(
        data = nonlinearfit.y,
        decorations = list(nlfit = list(
            list(
                label = "Custom Fit",
                param = c(164, 313, 0.031, -1.5, 1.2e-06, 1.9),
                type = "cst"
            ),
            list(
                label = "Regular Fit",
                param = c(164, 313, 0.031, 1.5, 1.2e-06, 1.9),
                type = "reg"
            )
        )),
        graphType = "Scatter2D",
        setMaxY = 350,
        setMinY = 100,
        showDecorations = TRUE,
        xAxis = c("Concentration"),
        xAxisTransform = "log10",
        xAxisTransformTicks = FALSE,
        yAxis = c("Variable1"),
        yAxisExact = TRUE
    )
    warning('scatter2d - not appearing correctly - loading')
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-scatter2d-7", {
    result <- canvasXpress(
        data = scatterR.y,
        varAnnot = scatterR.z,
        axisAlgorithm = "rPretty",
        backgroundType = "window",
        backgroundWindow = "rgb(238,238,238)",
        colorBy = "Group",
        colors = c(
            "rgba(0,104,139,0.5)",
            "rgba(205,0,0,0.5)",
            "rgba(64,64,64,0.5)"
        ),
        decorations = list(line = list(
            list(
                color = "rgba(205,0,0,0.5)",
                width = 2,
                y = 0.5
            ),
            list(
                color = "rgba(0,104,139,0.5)",
                width = 2,
                y = -0.5
            )
        )),
        graphType = "Scatter2D",
        legendBackgroundColor = "rgb(238,238,238)",
        legendBoxColor = "rgb(0,0,0)",
        legendInside = TRUE,
        legendPosition = "bottomRight",
        plotBox = FALSE,
        showDecorations = TRUE,
        showLoessFit = TRUE,
        showTransition = TRUE,
        sizeBy = "FC",
        sizes = c(4, 14, 16, 18),
        title = "Profile plot",
        xAxis = c("AveExpr"),
        xAxisTickColor = "rgb(255,255,255)",
        yAxis = c("logFC"),
        yAxisTickColor = "rgb(255,255,255)"
    )
    warning('scatter2d - not appearing correctly - missing points')
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-scatter2d-8", {
    result <- canvasXpress(
        data = scatterR2.y,
        varAnnot = scatterR2.z,
        axisAlgorithm = "rPretty",
        backgroundType = "window",
        backgroundWindow = "rgb(238,238,238)",
        colorBy = "Group",
        colors = c(
            "rgba(0,104,139,0.5)",
            "rgba(205,0,0,0.5)",
            "rgba(64,64,64,0.5)"
        ),
        decorations = list(line = list(
            list(
                color = "rgba(205,0,0,0.5)",
                width = 2,
                x = 0.5
            ),
            list(
                color = "rgba(0,104,139,0.5)",
                width = 2,
                x = -0.5
            )
        )),
        graphType = "Scatter2D",
        legendBackgroundColor = "rgb(238,238,238)",
        legendBoxColor = "rgb(0,0,0)",
        plotBox = FALSE,
        showDecorations = TRUE,
        showTransition = TRUE,
        sizeBy = "FC",
        sizes = c(4, 14, 16, 18),
        title = "Volcano plot",
        xAxis = c("logFC"),
        xAxisTickColor = "rgb(255,255,255)",
        yAxis = c("-log-pVal"),
        yAxisTickColor = "rgb(255,255,255)"
    )
    warning('scatter2d - not appearing correctly - missing points')
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-scatter2d-9", {
    result <- canvasXpress(
        data = scatterR3.y,
        varAnnot = scatterR3.z,
        axisAlgorithm = "rPretty",
        backgroundType = "window",
        backgroundWindow = "rgb(238,238,238)",
        colorBy = "Group",
        colors = c(
            "rgba(255,215,0,0.5)",
            "rgba(64,64,64,0.5)",
            "rgba(0,104,139,0.5)",
            "rgba(205,0,0,0.5)"
        ),
        decorations = list(line = list(
            list(
                color = "rgba(64,64,64,0.5)",
                width = 2,
                x = 0
            ),
            list(
                color = "rgba(64,64,64,0.5)",
                width = 2,
                y = 0
            ),
            list(
                color = "rgba(255,215,0,0.5)",
                width = 2,
                x = -5,
                x2 = 5,
                y = -5,
                y2 = 5
            )
        )),
        graphType = "Scatter2D",
        legendBackgroundColor = "rgb(238,238,238)",
        legendBoxColor = "rgb(0,0,0)",
        legendInside = TRUE,
        legendPosition = "bottomRight",
        plotBox = FALSE,
        showDecorations = TRUE,
        showTransition = TRUE,
        sizeBy = "Hit",
        sizeByShowLegend = FALSE,
        sizes = c(4, 14),
        title = "Contrast plot",
        xAxis = c("logFC-X"),
        xAxisTickColor = "rgb(255,255,255)",
        yAxis = c("logFC-Y"),
        yAxisTickColor = "rgb(255,255,255)"
    )
    warning('scatter2d - not appearing correctly - sizes of bubbles seem to be off')
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-scatter2d-10", {
    result <- canvasXpress(
        data = scatterR4.y,
        axisAlgorithm = "rPretty",
        backgroundType = "window",
        backgroundWindow = "rgb(238,238,238)",
        colors = c("rgba(0,104,139,0.5)", "rgba(205,0,0,0.5)"),
        graphType = "Scatter2D",
        legendBackgroundColor = "rgb(238,238,238)",
        legendBoxColor = "rgb(0,0,0)",
        legendInside = TRUE,
        legendPosition = "topRight",
        plotBox = FALSE,
        title = "Waterfall plot",
        xAxis = c("Row"),
        xAxisTickColor = "rgb(255,255,255)",
        yAxis = c("Sample1", "Sample2"),
        yAxisTickColor = "rgb(255,255,255)"
    )
    warning('scatter2d - not appearing correctly - grouping, coloring')
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-kaplan-meier-1", {
    warning('scatter2d - missing test - 1st chart on web for KaplanMeier')
})

test_that("cX-kaplan-meier-2", {
    result <- canvasXpress(
        data = kaplanmeier.y,
        varAnnot = kaplanmeier.z,
        graphType = "Scatter2D",
        showDecorations = TRUE,
        showDecorationsKaplanMeierConfidence = TRUE,
        showLegend = FALSE,
        showTransition = TRUE,
        title = "Kaplan-Meier Plot"
    )
    warning('scatter2d - graph incorrect - kaplanmeier lines/etc')
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-layout-1", {
    result <- canvasXpress(
        data = irist.y,
        varAnnot = irist.z,
        broadcast = TRUE,
        colorBy = "Species",
        graphType = "Scatter2D",
        layoutAdjust = TRUE,
        scatterPlotMatrix = TRUE,
        showTransition = TRUE
    )
    warning('scatter2d - graph incorrect - layout missing areas')
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-layout-2", {
    result <- canvasXpress(
        data = irist.y,
        varAnnot = irist.z,
        broadcast = TRUE,
        colorBy = "Species",
        graphType = "Scatter2D",
        layoutAdjust = FALSE,
        scatterPlotMatrix = TRUE,
        showTransition = TRUE
    )
    warning('scatter2d - graph incorrect - layout colors')
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-layout-3", {
    result <- canvasXpress(
        data = irist.y,
        varAnnot = irist.z,
        broadcast = TRUE,
        colorBy = "Species",
        graphType = "Scatter2D",
        layoutAdjust = FALSE,
        scatterPlotMatrix = TRUE,
        scatterPlotMatrixType = "first"
    )
    warning('scatter2d - graph incorrect - layout colors, missing areas')
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

