context("canvasXpress Charts - Circular")


circular.y  <- readRDS(system.file("extdata", "cX-circular-dat.RData", package = "canvasXpress"))
circular.x  <- readRDS(system.file("extdata", "cX-circular-smp.RData", package = "canvasXpress"))
circular.z  <- readRDS(system.file("extdata", "cX-circular-var.RData", package = "canvasXpress"))
circular2.y <- readRDS(system.file("extdata", "cX-circular2-dat.RData", package = "canvasXpress"))
circular2.x <- readRDS(system.file("extdata", "cX-circular2-smp.RData", package = "canvasXpress"))
circular2.z <- readRDS(system.file("extdata", "cX-circular2-var.RData", package = "canvasXpress"))
sunburst.y  <- readRDS(system.file("extdata", "cX-sunburst-dat.RData", package = "canvasXpress"))
sunburst.x  <- readRDS(system.file("extdata", "cX-sunburst-smp.RData", package = "canvasXpress"))
generic.y   <- readRDS(system.file("extdata", "cX-generic-dat.RData", package = "canvasXpress"))
generic.x   <- readRDS(system.file("extdata", "cX-generic-smp.RData", package = "canvasXpress"))
generic.z   <- readRDS(system.file("extdata", "cX-generic-var.RData", package = "canvasXpress"))
tree.y      <- readRDS(system.file("extdata", "cX-tree-dat.RData", package = "canvasXpress"))
tree.x      <- readRDS(system.file("extdata", "cX-tree-smp.RData", package = "canvasXpress"))
chord.y     <- readRDS(system.file("extdata", "cX-chord-dat.RData", package = "canvasXpress"))

test_that("cX-circular-1", {
    result <- canvasXpress(
        data = circular.y,
        smpAnnot = circular.x,
        varAnnot = circular.z,
        colorScheme = "basic",
        connections = c(
            c("rgb(255,0,0)", "s1", "s15"),
            c("rgb(0,255,0)", "s25", "s120"),
            c("rgb(255,0,0)", "s34", "s2"),
            c("rgb(255,0,0)", "s47", "s69"),
            c("rgb(255,0,0)", "s15", "s74"),
            c("rgb(0,120,0)", "s57", "s87"),
            c("rgb(255,34,0)", "s54", "s118"),
            c("rgb(255,0,100)", "s78", "s18"),
            c("rgb(255,134,0)", "s90", "s48"),
            c("rgb(120,0,0)", "s120", "s68"),
            c("rgb(255,0,0)", "s131", "s92"),
            c("rgb(0,255,0)", "s148", "s119"),
            c("rgb(0,0,255)", "s10", "s14"),
            c("rgb(255,0,0)", "s56", "s6"),
            c("rgb(255,0,0)", "s98", "s90"),
            c("rgb(255,0,0)", "s113", "s20")
        ),
        graphType = "Circular",
        ringsType = c("dot", "heatmap", "bar"),
        ringsWeight = c(50, 25, 25),
        segregateSamplesBy = c("Species"),
        segregateVariablesBy = c("Ring"),
        showTransition = TRUE,
        smpOverlays = c("Species"),
        title = "Iris flower data set (1D Circular Plot)"
    )
    warning('circular - not appearing correctly - loading')
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-circular-2", {
    result <- canvasXpress(
        data = circular2.y,
        smpAnnot = circular2.x,
        varAnnot = circular2.z,
        colorScheme = "basic",
        connections = c(
            c("rgb(0,0,255)", "setosa", 42, "versicolor", 35, 1, 2),
            c("rgb(0,255,0)", "virginica", 26, "versicolor", 15, 4, 8),
            c("rgb(120,0,255)", "setosa", 36, "virginica", 5, 6, 9),
            c("rgb(0,40,255)", "versicolor", 9, "versicolor", 18, 2, 5),
            c("rgb(80,0,55)", "versicolor", 14, "setosa", 9, 3, 4),
            c("rgb(0,55,140)", "setosa", 12, "setosa", 41, 5, 2),
            c("rgb(255,0,0)", "virginica", 25, "setosa", 3, 2, 6)
        ),
        graphType = "Circular",
        rAxis = "Number",
        ringsWeight = c(25, 25, 25, 25),
        segregateSamplesBy = c("Species"),
        segregateVariablesBy = c("Ring"),
        showTransition = TRUE,
        title = "Iris flower data set (2D Circular Plot)"
    )
    warning('circular - not appearing correctly')
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-donnut-1", {
    result <- canvasXpress(
        data = sunburst.y,
        smpAnnot = sunburst.x,
        circularArc = 360,
        circularRotate = 0,
        circularType = "sunburst",
        colorBy = "Mont",
        colorScheme = "Bootstrap",
        graphType = "Circular",
        hierarchy = c("Month"),
        showTransition = TRUE,
        title = "Simple Donnut"
    )
    warning('circular - not appearing correctly - blank')
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-donnut-2", {
    result <- canvasXpress(
        data = sunburst.y,
        smpAnnot = sunburst.x,
        circularArc = 360,
        circularRotate = 0,
        circularType = "sunburst",
        colorBy = "Quarter",
        colorScheme = "RdYlBu",
        graphType = "Circular",
        hierarchy = c("Quarter", "Month"),
        showTransition = TRUE,
        title = "Donnut with two levels"
    )
    warning('circular - not appearing correctly - blank')
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-radar-1", {
    result <- canvasXpress(
        data = generic.y,
        smpAnnot = generic.x,
        varAnnot = generic.z,
        circularArc = 360,
        circularRotate = 0,
        circularType = "radar",
        colorScheme = "Bootstrap",
        graphType = "Circular",
        ringsType = c("line"),
        showTransition = TRUE,
        title = "Radar - Line"
    )
    warning('circular - not appearing correctly')
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-radar-2", {
    result <- canvasXpress(
        data = generic.y,
        smpAnnot = generic.x,
        varAnnot = generic.z,
        circularArc = 360,
        circularRotate = 0,
        circularType = "radar",
        colorScheme = "Bootstrap",
        graphType = "Circular",
        legendPosition = "top",
        ringsType = c("area"),
        showTransition = TRUE,
        title = "Radar - Area"
    )
    warning('circular - not appearing correctly - no lines')
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-radar-3", {
    result <- canvasXpress(
        data = generic.y,
        smpAnnot = generic.x,
        varAnnot = generic.z,
        circularArc = 360,
        circularRotate = 0,
        circularType = "radar",
        colorScheme = "Bootstrap",
        graphType = "Circular",
        ringsType = c("bar"),
        showTransition = TRUE,
        title = "Radar - Bar"
    )
    warning('circular - not appearing correctly - no lines')
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-radar-4", {
    result <- canvasXpress(
        data = generic.y,
        smpAnnot = generic.x,
        varAnnot = generic.z,
        circularArc = 360,
        circularRotate = 0,
        circularType = "radar",
        colorScheme = "Bootstrap",
        graphType = "Circular",
        legendPosition = "top",
        ringsType = c("dot"),
        showTransition = TRUE,
        title = "Radar - Scatter"
    )
    warning('circular - not appearing correctly - no lines')
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-radar-5", {
    result <- canvasXpress(
        data = generic.y,
        smpAnnot = generic.x,
        varAnnot = generic.z,
        circularArc = 360,
        circularRotate = 0,
        circularType = "radar",
        colorScheme = "Bootstrap",
        graphType = "Circular",
        legendPosition = "top",
        ringsType = c("stacked"),
        showTransition = TRUE,
        title = "Radar - Stacked"
    )
    warning('circular - not appearing correctly - no lines')
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-radar-6", {
    result <- canvasXpress(
        data = generic.y,
        smpAnnot = generic.x,
        varAnnot = generic.z,
        circularArc = 180,
        circularRotate = 0,
        circularType = "radar",
        colorScheme = "Bootstrap",
        graphType = "Circular",
        ringsType = c("line"),
        showTransition = TRUE,
        title = "Half Radar"
    )
    warning('circular - not appearing correctly - no lines')
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-radar-7", {
    result <- canvasXpress(
        data = generic.y,
        smpAnnot = generic.x,
        varAnnot = generic.z,
        circularArc = 180,
        circularRotate = -90,
        circularType = "radar",
        colorScheme = "Bootstrap",
        graphType = "Circular",
        legendPosition = "top",
        ringsType = c("line"),
        showTransition = TRUE,
        title = "Rotated Half Radar"
    )
    if (interactive()) { print(result) }
    warning('circular - not appearing correctly - no lines')
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-radar-8", {
    result <- canvasXpress(
        data = generic.y,
        smpAnnot = generic.x,
        varAnnot = generic.z,
        circularArc = 360,
        circularRotate = 0,
        circularType = "radar",
        colorScheme = "Bootstrap",
        graphType = "Circular",
        ringsType = c("line"),
        showTransition = TRUE,
        smpOverlays = c("Factor3", "-", "Factor1", "Factor2"),
        title = "Radar with Overlays"
    )
    warning('circular - not appearing correctly - loading')
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-sunburst-1", {
    result <- canvasXpress(
        data = sunburst.y,
        smpAnnot = sunburst.x,
        circularArc = 360,
        circularRotate = 0,
        circularType = "sunburst",
        colorBy = "Quarter",
        colorScheme = "Bootstrap",
        graphType = "Circular",
        hierarchy = c("Quarter", "Month", "Week"),
        showTransition = TRUE,
        title = "Simple Sunburst"
    )
    warning('circular - not appearing correctly - blank')
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-sunburst-2", {
    result <- canvasXpress(
        data = sunburst.y,
        smpAnnot = sunburst.x,
        circularArc = 360,
        circularRotate = 0,
        circularType = "sunburst",
        colorBy = "Month",
        colorScheme = "RdYlBu",
        graphType = "Circular",
        hierarchy = c("Quarter", "Month", "Week"),
        showTransition = TRUE,
        title = "Simple Sunburst Colored by Category"
    )
    warning('circular - not appearing correctly - blank')
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-sunburst-3", {
    result <- canvasXpress(
        data = sunburst.y,
        smpAnnot = sunburst.x,
        circularArc = 360,
        circularRotate = -90,
        circularType = "sunburst",
        colorScheme = "Bootstrap",
        graphType = "Circular",
        hierarchy = c("Quarter", "Month", "Week"),
        showTransition = TRUE,
        title = "Rotated Sunburst"
    )
    warning('circular - not appearing correctly - blank')
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-sunburst-4", {
    result <- canvasXpress(
        data = sunburst.y,
        smpAnnot = sunburst.x,
        circularArc = 180,
        circularRotate = -90,
        circularType = "sunburst",
        colorScheme = "Bootstrap",
        graphType = "Circular",
        hierarchy = c("Quarter", "Month", "Week"),
        showTransition = TRUE,
        title = "Rotated Half Sunburst"
    )
    warning('circular - not appearing correctly - blank')
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-bubble-1", {
    result <- canvasXpress(
        data = tree.y,
        smpAnnot = tree.x,
        circularType = "bubble",
        graphType = "Circular",
        showTransition = TRUE,
        title = "Simple Bubble Graph"
    )
    warning('circular - graph incorrect - cutoff')
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
    warning('circular - graph incorrect - loading')
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
    warning('circular - graph incorrect - loading')
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-chord-1", {
    result <- canvasXpress(
        data = chord.y,
        circularArc = 360,
        circularRotate = 0,
        circularType = "chord",
        colors = c("#000000", "#FFDD89", "#957244", "#F26223"),
        graphType = "Circular",
        higlightGreyOut = TRUE,
        rAxisTickFormat = c("%sK", "val / 1000"),
        showTransition = TRUE,
        title = "Simple Chord Graph"
    )
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-chord-2", {
    result <- canvasXpress(
        data = chord.y,
        circularArc = 360,
        circularRotate = 180,
        circularType = "chord",
        colors = c("#000000", "#FFDD89", "#957244", "#F26223"),
        graphType = "Circular",
        higlightGreyOut = TRUE,
        rAxisTickFormat = c("%sK", "val / 1000"),
        showTransition = TRUE,
        title = "Rotated Chord Graph"
    )
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-chord-3", {
    result <- canvasXpress(
        data = chord.y,
        circularArc = 180,
        circularRotate = -90,
        circularType = "chord",
        colors = c("#000000", "#FFDD89", "#957244", "#F26223"),
        graphType = "Circular",
        higlightGreyOut = TRUE,
        rAxisTickFormat = c("%sK", "val / 1000"),
        showLegend = FALSE,
        showTransition = TRUE,
        title = "Rotated Half Chord Graph"
    )
    if (interactive()) { print(result) }
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})
