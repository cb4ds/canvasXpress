context("canvasXpress Charts - Scatter3D")


data <- t(iris[, 1:4])
smpAnnot <- as.matrix(iris[, 5])
colnames(data) <- make.names(rep("S", ncol(data)), unique = T)
rownames(smpAnnot) <- colnames(data)
colnames(smpAnnot) <- "Species"
varAnnot <- t(data.frame(color = c("red", "blue", "green", "yellow")))
colnames(varAnnot) <- rownames(data)


test_that("Scatter3D Chart - basic 1", {
    result <- canvasXpress(data, 
                           varAnnot = varAnnot, 
                           colorBy = 'Species',
                           graphType = 'Scatter3D')
    result
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})


# -- From Isaac, web examples --
irist.y     <- readRDS(system.file("extdata", "cX-irist-dat.RData", package = "canvasXpress"))
irist.z     <- readRDS(system.file("extdata", "cX-irist-var.RData", package = "canvasXpress"))
scatter3d.y <- readRDS(system.file("extdata", "cX-scatter3d-dat.RData", package = "canvasXpress"))
generic2.y  <- readRDS(system.file("extdata", "cX-generic2-dat.RData", package = "canvasXpress"))
generic2.x  <- readRDS(system.file("extdata", "cX-generic2-smp.RData", package = "canvasXpress"))
generic2.z  <- readRDS(system.file("extdata", "cX-generic2-var.RData", package = "canvasXpress"))

test_that("cX-scatter3d-1", {
    result <- canvasXpress(
        data = irist.y,
        varAnnot = irist.z,
        axisTickScaleFontFactor = 0.5,
        axisTitleScaleFontFactor = 0.5,
        colorBy = "Species",
        graphType = "Scatter3D",
        title = "Iris Data Set",
        xAxis = c("Sepal.Length"),
        yAxis = c("Sepal.Width"),
        zAxis = c("Petal.Length")
    )
    result
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-scatter3d-2", {
    result <- canvasXpress(
        data = irist.y,
        varAnnot = irist.z,
        axisTickScaleFontFactor = 0.5,
        axisTitleScaleFontFactor = 0.5,
        colorBy = "Species",
        ellipseBy = "Species",
        graphType = "Scatter3D",
        title = "Iris Data Set",
        xAxis = c("Sepal.Length"),
        yAxis = c("Petal.Width"),
        zAxis = c("Petal.Length")
    )
    result
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-scatter3d-", {
    result <- canvasXpress(
        data = scatter3d.y,
        graphType = "Scatter3D",
        xAxis = c("Sample1"),
        yAxis = c("Sample2"),
        zAxis = c("Sample3")
    )
    result
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-scatter3d-4", {
    result <- canvasXpress(
        data = scatter3d.y,
        graphType = "Scatter3D",
        scatterType = "bar",
        xAxis = c("Sample1"),
        yAxis = c("Sample2"),
        zAxis = c("Sample3")
    )
    result
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})

test_that("cX-scatter3d-5", {
    result <- canvasXpress(
        data = generic2.y,
        smpAnnot = generic2.x,
        varAnnot = generic2.z,
        colorBy = "Annt2",
        graphType = "Scatter3D",
        shapeBy = "Annt3",
        sizeBy = "Sample4",
        xAxis = c("Sample1"),
        yAxis = c("Sample2"),
        zAxis = c("Sample3")
    )
    result
    expect_s3_class(result, "canvasXpress")
    expect_s3_class(result, "htmlwidget")
})
