context("canvasXpress Charts - Chord")


# -- From Isaac, web examples --
map.y <- readRDS(system.file("extdata", "cX-map-dat.RData", package = "canvasXpress"))
map.z <- readRDS(system.file("extdata", "cX-map-var.RData", package = "canvasXpress"))

test_that("cX-map-1", {
    result <- canvasXpress(
        data = map.y,
        varAnnot = map.z,
        colorBy = "Order",
        graphType = "Map",
        leafletConfig = list(
            attributionControl = FALSE,
            center = c(30, 0),
            zoom = 1.5
        ),
        leafletId = "world",
        topoJSON = list(world50m = "https://canvasxpress.org/json/world-50m.json")
    )
    warning('map - graph incorrect - coloring missing')
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-map-2", {
    result <- canvasXpress(
        data = map.y,
        varAnnot = map.z,
        graphType = "Map",
        leafletConfig = list(
            attributionControl = FALSE,
            center = c(30, 0),
            zoom = 1.5
        ),
        leafletId = "world",
        topoJSON = list(world50m = "https://canvasxpress.org/json/world-50m.json")
    )
    warning('map - graph incorrect - no coloring')
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})
