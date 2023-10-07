context("canvasXpress pipe support")
skip_if_offline(host = "www.canvasxpress.org")

test_that("piping - change graphType", {
    data <- as.data.frame(matrix(c(33,44,55),
                                 nrow = 1,
                                 ncol = 3,
                                 byrow = TRUE,
                                 dimnames = list(c("V1"),
                                                 c("S1", "S2", "S3"))))

    obj1 <- canvasXpress(data           = data,
                         graphType      = "Bar")
    check_ui_test(obj1)

    obj2 <- obj1 %>% canvasXpress(
        title      = "Bar to Scatter2D",
        graphType  = "Scatter2D")

    check_ui_test(obj2)

    obj3 <- obj2 %>% canvasXpress(
        title      = "Scatter2D to Boxplot",
        graphType  = "Boxplot")

    check_ui_test(obj3)

    obj4 <- obj3 %>% canvasXpress(
        title      = "Boxplot to Area2D",
        graphType  = "Area2D")

    check_ui_test(obj4)
})


test_that("piping - change events", {

})


test_that("piping - change afterRender", {
    data <- as.data.frame(matrix(c(33,44,55),
                                 nrow = 1,
                                 ncol = 3,
                                 byrow = TRUE,
                                 dimnames = list(c("V1"),
                                                 c("S1", "S2", "S3"))))

    boxplot <- canvasXpress(data       = data,
                             graphType = "Scatter2D")
    check_ui_test(boxplot)

    histogram1 <- boxplot1 %>%
        canvasXpress(afterRender = list(list("createHistogram")))

    check_ui_test(histogram1)

    histogram2 <- histogram1 %>%
        canvasXpress(afterRender = NULL)

    check_ui_test(histogram2)
})


test_that("piping - change width/height", {
    obj1 <- cXstacked1()

    check_ui_test(obj1)

    obj2 <- obj1 %>% canvasXpress(
        subtitle  = "changed height",
        width = NULL
    ) %>%
        canvasXpress(width = "50px")

    check_ui_test(obj2)

})


test_that("piping - change attributes for tojson", {

})


test_that("piping - invalid object", {

})


test_that("piping - attempted data changes", {
    # TODO need to check the error message works
    error_msg <- "Primary object data changes are not supported when modifying a canvasXpress object (ie piping) - ie changes to the data, varAnnot or smpAnnot parameters."
    y         <- read.table("https://www.canvasxpress.org/data/cX-toothgrowth-dat.txt", header=TRUE, sep="\t", quote="", row.names=1, fill=TRUE, check.names=FALSE, stringsAsFactors=FALSE)
    x         <- read.table("https://www.canvasxpress.org/data/cX-toothgrowth-smp.txt", header=TRUE, sep="\t", quote="", row.names=1, fill=TRUE, check.names=FALSE, stringsAsFactors=FALSE)

    histogram <- cXboxplot1() %>%
        canvasXpress(data = test)

    check_ui_test(histogram)

    expect_error(cXdotplot4() %>%
        canvasXpress(smpAnnot = y), regexp = error_msg)

    histogram <- cXbarline3() %>%
        canvasXpress(varAnnot = x)

    check_ui_test(histogram)
})

test_that("piping - area chart", {
    area3 <- cXarea3() %>%
        canvasXpress(title = "smpLabelInterval 40",
                     xAxisTitle = "changed",
                     smpLabelInterval = 40)

    check_ui_test(area3)

    area4 <- cXarea4() %>%
        canvasXpress(title = "colorScheme, legendKey",
                     colorScheme = "canvasXpress",
                     legendKeyBackgroundColor="rgba(255,255,255,0)")

    check_ui_test(area4)

    area8 <- cXarea8() %>%
        canvasXpress(title = "change graph type",
                     graphType = "Bar")

    check_ui_test(area8)
})

test_that("piping - arealine chart", {
    arealine1 <- cXarealine1() %>%
        canvasXpress(title      = "showLegend = F",
                     showLegend = FALSE)

    check_ui_test(arealine1)

    arealine3 <- cXarealine3() %>%
        canvasXpress(title          = "subtitle text, smpLabelRotate = 90",
                     subtitle       = "changed",
                     smpLabelRotate = 90)

    check_ui_test(arealine3)
})

test_that("piping - bar chart", {

    bar3 <- cXbar3() %>% canvasXpress(
        # change a parameter
        graphOrientation = "vertical",
        # add a parameter
        subtitle         = "horizontal to vertical",
        # remove a parameter
        title            = "test",
        smpLabelRotate = 90
    )

    check_ui_test(bar3)

    cXbar8() %>% canvasXpress(
        showLegend = FALSE
    )

    check_ui_test(bar8)

    cXbar13() %>% canvasXpress(
        showLegend = FALSE
    )

    check_ui_test(bar13)

})

test_that("piping - barline chart", {


})

test_that("piping - boxplot chart", {

})

test_that("piping - bubble chart", {

})

test_that("piping - chord chart", {

})

test_that("piping - circular chart", {

})

test_that("piping - contour chart", {

})

test_that("piping - correlation chart", {

})

test_that("piping - dashboard chart", {

})

test_that("piping - density chart", {

})

test_that("piping - donut chart", {

})

test_that("piping - dotline chart", {

})

test_that("piping - dotplot chart", {

})

test_that("piping - facet chart", {

})

test_that("piping - fish chart", {

})

test_that("piping - gantt chart", {

})

test_that("piping - genome chart", {

})

test_that("piping - heatmap chart", {

})

test_that("piping - hexplotbinplot chart", {

})

test_that("piping - histogram chart", {

})

test_that("piping - kaplanmeier chart", {

})

test_that("piping - layout chart", {

})

test_that("piping - line chart", {

})

test_that("piping - linearfit chart", {

})

test_that("piping - lollipop chart", {

})

test_that("piping - map chart", {

})

test_that("piping - meter chart", {

})

test_that("piping - network chart", {

})

test_that("piping - nonlinearfit chart", {

})

test_that("piping - oncoprint chart", {

})

test_that("piping - parallelcoordinates chart", {

})

test_that("piping - pie chart", {

})

test_that("piping - radar chart", {

})

test_that("piping - ridgeline chart", {

})

test_that("piping - sankey chart", {

})

test_that("piping - scatter2D chart", {

})

test_that("piping - scatter3D chart", {

})

test_that("piping - scatterbubbl2D chart", {

})

test_that("piping - splom chart", {

})

test_that("piping - stacked chart", {

})

test_that("piping - stackedline chart", {

})

test_that("piping - stackedpercent chart", {

})

test_that("piping - stackedpercentline chart", {

})

test_that("piping - sunburst chart", {

})

test_that("piping - tagcloud chart", {

})

test_that("piping - tcga chart", {

})

test_that("piping - tree chart", {

})

test_that("piping - treemap chart", {

})

test_that("piping - upset chart", {

})

test_that("piping - venn chart", {

})

test_that("piping - waterfall chart", {

})
