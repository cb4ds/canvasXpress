context("canvasXpress Charts - Treemap")


stacked1.y  <- readRDS(system.file("extdata", "cX-stacked1-dat.RData", package = "canvasXpress"))
stacked1.x  <- readRDS(system.file("extdata", "cX-stacked1-smp.RData", package = "canvasXpress"))
generic.y   <- readRDS(system.file("extdata", "cX-generic-dat.RData", package = "canvasXpress"))
generic.x   <- readRDS(system.file("extdata", "cX-generic-smp.RData", package = "canvasXpress"))
generic.z   <- readRDS(system.file("extdata", "cX-generic-var.RData", package = "canvasXpress"))

test_that("cX-treemap-1", {
    result <- canvasXpress(
        data = stacked1.y,
        smpAnnot = stacked1.x,
        groupingFactors = list("continent"),
        colorBy = "GNI",
        decorations = list(marker = list(
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
        graphType = "Treemap",
        showDecorations = FALSE,
        showTransition = TRUE,
        title = "Population colored by Gross National Income 2014"
    )
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-treemap-2", {
    result <- canvasXpress(
        data = generic.y,
        smpAnnot = generic.x,
        varAnnot = generic.z,
        graphType = "Treemap"
    )
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-treemap-3", {
    result <- canvasXpress(
        data = stacked1.y,
        smpAnnot = stacked1.x,
        groupingFactors = list("continent"),
        axisAlgorithm = "rPretty",
        colorBy = "GNI",
        decorations = list(marker = list(
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
        treemapBy = list("ISO3"),
        widthFactor = 4,
        xAxisMinorTicks = FALSE,
        xAxis = list("population"),
        xAxisTitle = ""
    )
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})
