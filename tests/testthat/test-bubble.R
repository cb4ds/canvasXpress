context("canvasXpress Charts - Bubble")


# -- From Isaac, web examples --
tree.y <- readRDS(system.file("extdata", "cX-tree-dat.RData", package = "canvasXpress"))
tree.x <- readRDS(system.file("extdata", "cX-tree-dat.RData", package = "canvasXpress"))

test_that("cX-bubble-1", {
    result <- canvasXpress(
        data = tree.y,
        smpAnnot = tree.x,
        circularType = "bubble",
        graphType = "Circular",
        showTransition = TRUE,
        title = "Simple Bubble Graph"
    )
    warning('bubble - graph incorrect - cutoff')
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-bubble-2", {
    result <- canvasXpress(
        data = tree.y,
        smpAnnot = tree.x,
        circularRotate = 45,
        circularType = "bubble",
        colorBy = "Level1",
        graphType = "Circular",
        hierarchy = c("Level1", "Level2", "Level3"),
        showTransition = TRUE,
        title = "Hierarchical Colored Bubble Graph"
    )
    warning('bubble - graph incorrect - loading')
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-bubble-3", {
    result <- canvasXpress(
        data = tree.y,
        smpAnnot = tree.x,
        circularRotate = 45,
        circularType = "bubble",
        graphType = "Circular",
        hierarchy = c("Level1", "Level2", "Level3"),
        showTransition = TRUE,
        title = "Hierarchical Bubble Graph"
    )
    warning('bubble - graph incorrect - loading')
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})
