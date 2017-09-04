context("canvasXpress Charts - Tree")


tree.y <- readRDS(system.file("extdata", "cX-tree-dat.RData", package = "canvasXpress"))
tree.x <- readRDS(system.file("extdata", "cX-tree-smp.RData", package = "canvasXpress"))

test_that("cX-tree-1", {
    result <- canvasXpress(
        data = tree.y,
        smpAnnot = tree.x,
        graphType = "Tree",
        hierarchy = c("Level1", "Level2", "Level3"),
        showTransition = TRUE,
        title = "Collapsible Tree"
    )
    warning('tree - loading')
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-tree-2", {
    result <- canvasXpress(
        data = tree.y,
        smpAnnot = tree.x,
        colorBy = "Annot1",
        graphType = "Tree",
        hierarchy = c("Level1", "Level2", "Level3"),
        showTransition = TRUE,
        title = "Collapsible Tree"
    )
    warning('tree - loading')
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-tree-3", {
    result <- canvasXpress(
        data = tree.y,
        smpAnnot = tree.x,
        colorBy = "Annot2",
        graphType = "Tree",
        hierarchy = c("Level1", "Level2", "Level3"),
        showTransition = TRUE,
        title = "Collapsible Tree"
    )
    warning('tree - loading')
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-tree-4 ", {
    result <- canvasXpress(
        data = tree.y,
        smpAnnot = tree.x,
        colorBy = "Annot2",
        graphType = "Tree",
        hierarchy = c("Level1", "Level2", "Level3"),
        showTransition = TRUE,
        title = "Collapsible Tree",
        treeCircular = TRUE
    )
    warning('tree - loading')
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

