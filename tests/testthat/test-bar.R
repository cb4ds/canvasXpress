context("canvasXpress Charts - Bar")


data <- t(iris[, 1:4])
smpAnnot <- as.matrix(iris[, 5])
colnames(data) <- make.names(rep("S", ncol(data)), unique = T)
rownames(smpAnnot) <- colnames(data)
colnames(smpAnnot) <- "Species"
varAnnot <- t(data.frame(color = c("red", "blue", "green", "yellow")))
colnames(varAnnot) <- rownames(data)


test_that("Bar Chart - basic 1", {
    result <- canvasXpress(data,
                           varAnnot = varAnnot,
                           colorBy = "Species",
                           graphType = "Bar")
    print(result)
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})


test_that("Bar Chart - basic 2", {
    result <- canvasXpress(data,
                           smpAnnot = smpAnnot,
                           colorBy = 'Species',
                           graphType = "Bar")
    print(result)
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})


test_that("Bar Chart - grouped", {
    result <- canvasXpress(
        data,
        smpAnnot = smpAnnot,
        groupingFactors = list('Species'),
        graphType = "Bar",
        graphOrientation = "vertical",
        smpLabelRotate = 90,
        legendPosition = "bottom",
        legendColumns = 2
    )
    print(result)
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})


# -- From Isaac, web examples --
stacked1.y <- readRDS(system.file("extdata", "cX-stacked1-dat.RData", package = "canvasXpress"))
stacked1.x <- readRDS(system.file("extdata", "cX-stacked1-smp.RData", package = "canvasXpress"))
basic.y    <- readRDS(system.file("extdata", "cX-basic-dat.RData", package = "canvasXpress"))
basic2.y   <- readRDS(system.file("extdata", "cX-basic2-dat.RData", package = "canvasXpress"))
iris.y     <- readRDS(system.file("extdata", "cX-iris-dat.RData", package = "canvasXpress"))
iris.x     <- readRDS(system.file("extdata", "cX-iris-smp.RData", package = "canvasXpress"))
generic.y  <- readRDS(system.file("extdata", "cX-generic-dat.RData", package = "canvasXpress"))
generic.x  <- readRDS(system.file("extdata", "cX-generic-smp.RData", package = "canvasXpress"))
generic.z  <- readRDS(system.file("extdata", "cX-generic-var.RData", package = "canvasXpress"))
simple.y   <- readRDS(system.file("extdata", "cX-simple-dat.RData", package = "canvasXpress"))
simple.x   <- readRDS(system.file("extdata", "cX-simple-smp.RData", package = "canvasXpress"))
lollipop.y <- readRDS(system.file("extdata", "cX-lollipop-dat.RData", package = "canvasXpress"))

test_that("cX-bar-1", {
    result <- canvasXpress(
        data = stacked1.y,
        smpAnnot = stacked1.x,
        axisAlgorithm = "rPretty",
        colorBy = "GNI",
        decorations = list(marker = c(
            list(
                align = "center",
                baseline = "middle",
                color = "red",
                sample = "Norway",
                text = "Norway is the country\nwith the largest GNI\naccording to 2014 census",
                variable = "population",
                x = 0.65,
                y = 0.7
            ),
            list(
                align = "center",
                baseline = "middle",
                color = "red",
                sample = "China",
                text = "China is the country with\nthe largest population\naccording to 2014 census",
                variable = "population",
                x = 0.15,
                y = 0.1
            )
        )),
        graphOrientation = "vertical",
        graphType = "Stacked",
        legendInside = TRUE,
        legendPosition = "top",
        showTransition = TRUE,
        smpLabelRotate = 45,
        subtitle = "2014 Census",
        title = "Country Population colored by Gross National Income",
        treemapBy = c("ISO3"),
        widthFactor = 4,
        xAxisMinorTicks = FALSE
    )
    warning('bar - not appearing correctly - missing treemaps, etc')
    print(result)
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-bar-2", {
    result <- canvasXpress(
        data = basic.y,
        graphOrientation = "vertical",
        graphType = "Bar",
        title = "Simple Bar graph"
    )
    print(result)
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-bar-3", {
    result <- canvasXpress(
        data = basic2.y,
        decorations = list(marker = c(
            list(
                fontSize = 12,
                sample = "Sample1",
                text = "p < 0.01 ***",
                type = "annotation",
                variable = "Variable1"
            ),
            list(
                fontSize = 12,
                sample = "Sample2",
                text = "p < 0.05 **",
                type = "annotation",
                variable = "Variable1"
            )
        )),
        graphOrientation = "vertical",
        graphType = "Bar",
        title = "Simple Bar graph with annotations"
    )
    warning('bar - not appearing correctly - missing decoration')
    print(result)
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-bar-4", {
    result <- canvasXpress(
        data = iris.y,
        smpAnnot = iris.x,
        axisTitleFontStyle = "italic",
        decorations = list(marker = c(
            list(
                sample = "setosa",
                text = "Species with\nlowest petal\nwidth",
                variable = "Petal.Width",
                x = 0.4,
                y = 0.85
            )
        )),
        graphOrientation = "vertical",
        graphType = "Bar",
        legendBox = FALSE,
        legendColumns = 2,
        legendPosition = "bottom",
        showTransition = TRUE,
        smpLabelRotate = 90,
        smpTitle = "Species",
        title = "Iris flower data set",
        xAxis2Show = FALSE
    )
    warning('bar - not appearing correctly - not grouped, etc')
    print(result)
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-bar-5", {
    result <- canvasXpress(
        data = generic.y,
        smpAnnot = generic.x,
        varAnnot = generic.z,
        fontStyle = "bold italic",
        graphOrientation = "vertical",
        graphType = "Bar",
        legendBox = FALSE,
        legendFontStyle = "italic",
        plotByVariable = TRUE,
        showShadow = TRUE,
        smpLabelFontStyle = "italic",
        smpLabelInterval = 2,
        smpLabelRotate = 45,
        smpTitle = "Sample Title",
        title = "Random data set",
        xAxis2Show = FALSE
    )
    warning('bar - not appearing correctly - missing decoration, colors')
    print(result)
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-bar-6", {
    result <- canvasXpress(
        data = simple.y,
        smpAnnot = simple.x,
        axisTitleFontStyle = "italic",
        colorBy = "Drug Sensitivity",
        colorScheme = "basic",
        decorationScaleFontFactor = 1.3,
        decorations = list(line = c(
            list(
                align = "left",
                color = "rgb(255,0,0)",
                label = "Cutoff",
                value = 50,
                width = 2
            )
        )),
        graphOrientation = "vertical",
        graphType = "Bar",
        showShadow = TRUE,
        smpTitle = "Cell Lines",
        smpTitleFontStyle = "bold",
        title = "Random data set",
        xAxis2Show = FALSE
    )
    warning('bar - not appearing correctly - missing decoration')
    print(result)
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-bar-7", {
    result <- canvasXpress(
        data = simple.y,
        smpAnnot = simple.x,
        axisTitleFontStyle = "italic",
        colorBy = "IC50",
        colorScheme = "basic",
        decorationScaleFontFactor = 1.3,
        decorations = list(line = c(
            list(
                align = "left",
                color = "rgb(255,0,0)",
                label = "Cutoff",
                value = 50,
                width = 2
            )
        )),
        graphOrientation = "vertical",
        graphType = "Bar",
        showShadow = TRUE,
        smpOverlays = c("Drug Sensitivity"),
        smpTitle = "Cell Lines",
        smpTitleFontStyle = "bold",
        title = "Random data set",
        xAxis2Show = FALSE
    )
    warning('bar - not appearing correctly - missing decoration, overlays, colors')
    print(result)
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-bar-8", {
    result <- canvasXpress(
        data = lollipop.y,
        barType = "lollipop",
        graphType = "Bar",
        showTransition = TRUE,
        widthFactor = 0.1
    )
    print(result)
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-bar-9", {
    result <- canvasXpress(
        data = generic.y,
        smpAnnot = generic.x,
        varAnnot = generic.z,
        bar3DInverseWeight = 1.2,
        graphType = "Bar",
        is3DPlot = TRUE,
        scatterType = "bar",
        x3DRatio = 0.5
    )
    print(result)
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})
