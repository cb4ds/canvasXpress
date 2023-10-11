context("canvasXpress pipe support")
skip_if_offline(host = "www.canvasxpress.org")

test_that("piping - change graphType", {
    # TODO add more interesting data
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
})


test_that("piping - change events", {
    # TODO
})


test_that("piping - change afterRender", {
    # TODO add more interesting data
    data <- as.data.frame(matrix(c(33,44,55),
                                 nrow = 1,
                                 ncol = 3,
                                 byrow = TRUE,
                                 dimnames = list(c("V1"),
                                                 c("S1", "S2", "S3"))))

    boxplot <- canvasXpress(data       = data,
                             graphType = "Scatter2D")
    check_ui_test(boxplot)

    histogram1 <- boxplot %>%
        canvasXpress(title       = "Scatter to histogram",
                     afterRender = list(list("createHistogram")))

    check_ui_test(histogram1)

    histogram2 <- histogram1 %>%
        canvasXpress(afterRender = NULL)

    check_ui_test(histogram2)
})


test_that("piping - change width/height", {
    # TODO - see if we can change this
    obj1 <- cXstacked1()

    check_ui_test(obj1)

    obj2 <- obj1 %>% canvasXpress(
        subtitle  = "changed width and height to 50%",
        width = "50px") %>%
        canvasXpress(height = "50px")

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
    obj1 <- cXarea8()
    check_ui_test(obj1)

    result <- obj1 %>%
        canvasXpress(title           = "decoration lines, xAxisTickSize, shapeby",
                     decorations     = list(line = list(list(color = "rgba(205,0,0,0.5)", width = 2, x = 2000),
                                                        list(color = "rgba(0,104,139,0.5)", width = 2, x = 2005))),
                     xAxisTickSize   = 2,
                     shapeBy         = "country")

    check_ui_test(result)
})

test_that("piping - arealine chart", {
    obj1 <- cXarealine3()
    check_ui_test(obj1)

    result <- obj1 %>%
        canvasXpress(title          = "subtitle text, smpLabelRotate = 90",
                     subtitle       = "changed",
                     smpLabelRotate = 90)

    check_ui_test(result)
})

test_that("piping - bar chart", {
    obj1 <- cXbar13()
    check_ui_test(obj1)

    result <- obj1 %>% canvasXpress(
        title         = "Smp label color, legendOrder",
        smpLabelColor = "red",
        legendOrder   = list("Stage" = list("Stage4", "Stage2", "Stage1", "Stage3"))
    )

    check_ui_test(result)
})

test_that("piping - barline chart", {
    result <- cXbarLine3()
    check_ui_test(obj1)

    result <- obj1 %>% canvasXpress(
        title         = "groupSamples and only plot V2",
        groupSamples  = list("Factor1"),
        xAxis         = list("V2")
    )

    check_ui_test(result)
})

test_that("piping - boxplot chart", {
    obj1 <- cXboxplot5()
    check_ui_test(obj1)

    result <- obj1 %>% canvasXpress(
        title       = "vertical and smpLabelRotate",
        boxplotMean = FALSE,
        smpTitle    = NULL
    )

    check_ui_test(result)
})

test_that("piping - bubble chart", {
    #TODO - change cx arguments starting here
    obj1 <- cXbubble4()
    check_ui_test(obj1)

    result <- obj1 %>% canvasXpress(
        title       = "vertical and smpLabelRotate",
        boxplotMean = FALSE,
        smpTitle    = NULL
    )

    check_ui_test(result)
})

test_that("piping - chord chart", {
    obj1 <- cXchord2()
    check_ui_test(obj1)

    result <- obj1 %>% canvasXpress(
        title       = "vertical and smpLabelRotate",
        boxplotMean = FALSE,
        smpTitle    = NULL
    )

    check_ui_test(result)

})

test_that("piping - circular chart", {
    obj1 <- cXcircular1()
    check_ui_test(obj1)

    result <- obj1 %>%
        canvasXpress(title            = "subtitle NULL, xAxisTitle, smpLabelInterval 40",
                     subtitle         = NULL,
                     xAxisTitle       = "changed",
                     smpLabelInterval = 40)

    check_ui_test(result)
})

test_that("piping - contour chart", {
    obj1 <- cXcontour2()
    check_ui_test(obj1)

    result <- obj1 %>%
        canvasXpress(title                    = "colorScheme, legendKey",
                     colorScheme              = "canvasXpress",
                     legendKeyBackgroundColor = "rgba(255,255,255,0)")

    check_ui_test(result)
})

test_that("piping - correlation chart", {
    obj1 <- cXbubble4()
    check_ui_test(obj1)

    result <- obj1 %>%
        canvasXpress(title           = "showLegend",
                     showLegend      = FALSE)

    check_ui_test(result)
})

test_that("piping - dashboard chart", {
    obj1 <- cXbubble4()
    check_ui_test(obj1)

    result <- obj1 %>% canvasXpress(
        title            = "vertical and smpLabelRotate",
        graphOrientation = "vertical",
        smpLabelRotate   = 90
    )

    check_ui_test(result)
})

test_that("piping - density chart", {
    obj1 <- cXbubble4()
    check_ui_test(obj1)

    result <- obj1 %>% canvasXpress(
        title      = "subtitle and hide legend",
        subtitle   = "horizontal to vertical",
        showLegend = FALSE
    )

    check_ui_test(result)
})

test_that("piping - donut chart", {
    obj1 <- cXbubble4()
    check_ui_test(obj1)

    result <- obj1 %>% canvasXpress(
        title         = "smpTitle and legendColumns 1",
        smpTitle      = "Changed",
        legendColumns = 1
    )

    check_ui_test(result)
})

test_that("piping - dotline chart", {
    obj1 <- cXbubble4()
    check_ui_test(obj1)

    result <- obj1 %>% canvasXpress(
        title       = "vertical and smpLabelRotate",
        boxplotMean = FALSE,
        smpTitle    = NULL
    )

    check_ui_test(result)
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
