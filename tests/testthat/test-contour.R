context("canvasXpress Charts - Contour")


volcano.y <- readRDS(system.file("extdata", "cX-volcano-dat.RData", package = "canvasXpress"))

test_that("cX-contour-1", {
    result <- canvasXpress(
        data = volcano.y,
        graphType = "Heatmap",
        heatmapCellBox = FALSE,
        showSampleNames = FALSE,
        showVariableNames = FALSE,
        subtitle = "datasets - volcano",
        title = "Topographic Information on Auckland's Maunga Whau Volcano"
    )
    warning('contour - graph incorrect - lines missing')
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})
    
