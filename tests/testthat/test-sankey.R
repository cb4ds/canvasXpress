context("canvasXpress Charts - Sankey")


sankey.y <- readRDS(system.file("extdata", "cX-sankey-dat.RData", package = "canvasXpress"))
sankey.x <- readRDS(system.file("extdata", "cX-sankey-smp.RData", package = "canvasXpress"))
sankey2.y <- readRDS(system.file("extdata", "cX-sankey2-dat.RData", package = "canvasXpress"))
sankey2.x <- readRDS(system.file("extdata", "cX-sankey2-smp.RData", package = "canvasXpress"))


test_that("cX-sankey-1", {
    result <- canvasXpress(
        data = sankey.y,
        smpAnnot = sankey.x,
        graphOrientation = "vertical",
        graphType = "Sankey",
        sankeySource = "Source",
        sankeyTarget = "Target",
        showTransition = TRUE,
        title = "Single Level Sankey"
    )
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-sankey-2", {
    result <- canvasXpress(
        data = sankey.y,
        smpAnnot = sankey.x,
        colorBy = "Weight",
        graphOrientation = "vertical",
        graphType = "Sankey",
        sankeySource = "Source",
        sankeyTarget = "Target",
        title = "Single Level Sankey"
    )
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-sankey-3", {
    result <- canvasXpress(
        data = sankey2.y,
        smpAnnot = sankey2.x,
        graphOrientation = "vertical",
        graphType = "Sankey",
        sankeySource = "Source",
        sankeyTarget = "Target",
        title = "Multilevel Sankey"
    )
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-sankey-4", {
    result <- canvasXpress(
        data = sankey2.y,
        smpAnnot = sankey2.x,
        colorBy = "Weight",
        graphOrientation = "vertical",
        graphType = "Sankey",
        sankeySource = "Source",
        sankeyTarget = "Target",
        title = "Multilevel Sankey"
    )
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})
