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
    obj1 <- cXscatter2d13()
    check_ui_test(obj1)

    events <- JS("{ 'mousemove' : function(o, e, t) {
                                    if (o) {
                                        if (o.objectType == null) {
                                            t.showInfoSpan(e, '<b>' + o.y.vars[0] + '</b><br/>' +
                                            'Some example event here' + '<br/>' +
                                            '<i>Value:</i>' +  o.y.data[0][0]);
                                        }
                                        else {
                                            t.showInfoSpan(e, o.display);
                                        };
                                    };}}")
    result <- obj1 %>%
        canvasXpress(title = "Piped custom events",
                     events = events)

    check_ui_test(result)
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
    obj1 <- cXstacked1()

    check_ui_test(obj1)

    obj2 <- obj1 %>% canvasXpress(
        title  = "changed width and height",
        width  = 100,
        height = 300)

    check_ui_test(obj2)
    warning("you will need to view this in full screen to see the difference")
})


test_that("piping - change attributes for tojson", {
    # Change the attributes that affect json
    # skipping pretty because there won't be a visible change
    obj1 <- cXscatterbubble2d1()
    check_ui_test(obj1)

    result <- obj1 %>%
        canvasXpress(title = "one digit",
                     digits = 1)

    check_ui_test(result)
})


test_that("piping - invalid object", {
    obj1 <- cXboxplot14()
    obj1$x$data$y <- NULL

    result <- obj1 %>%
        canvasXpress(title = "test")

    warning("The plot should be unavailable")
    check_ui_test(result)

})


test_that("piping - attempted data changes", {
    # Piping to a different position
    error_msg <- "data cannot be NULL!"
    obj1      <- cXscatterbubble2d1()
    check_ui_test(obj1)

    expect_error(obj1 %>% canvasXpress(smpAnnot = .), regexp = error_msg)
    expect_error(obj1 %>% canvasXpress(varAnnot = .), regexp = error_msg)

    # Attempt to replace the data variables
    # Get two dataframes to use
    error_msg <- "Primary object data changes are not supported when modifying a canvasXpress object"
    y <- read.table("https://www.canvasxpress.org/data/cX-toothgrowth-dat.txt",
                    header=TRUE, sep="\t", quote="", row.names=1, fill=TRUE, check.names=FALSE, stringsAsFactors=FALSE)
    x <- read.table("https://www.canvasxpress.org/data/cX-toothgrowth-smp.txt",
                    header=TRUE, sep="\t", quote="", row.names=1, fill=TRUE, check.names=FALSE, stringsAsFactors=FALSE)

    expect_error(cXdotplot4() %>% canvasXpress(smpAnnot = y), regexp = error_msg)
    expect_error(cXbarline3() %>% canvasXpress(varAnnot = x), regexp = error_msg)
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
    obj1 <- cXbarline3()
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
        title       = "remove boxplotMean and smpTitle",
        boxplotMean = FALSE,
        smpTitle    = NULL
    )

    check_ui_test(result)
})

test_that("piping - bubble chart", {
    # TODO Question - the modifyLabelCoordinates don't seem to be working.
    #add comment that after render is not included in bubble chart
    obj1 <- cXbubble3()
    check_ui_test(obj1)

    result <- obj1 %>% canvasXpress(
        title       = "bubbleLabelLineType to line and bubbleOutlineColor",
        bubbleLabelLineType = "line",
        bubbleOutlineColor  = "blue"
        # afterRender = list(list("modifyLabelCoordinates", list(list("Central America", -100, 100, TRUE))),
        #                    list("modifyLabelCoordinates", list(list("Oceania", 10, -50, TRUE))),
        #                    list("modifyLabelCoordinates", list(list("Europe", 10, -50))))
    )

    check_ui_test(result)
})

test_that("piping - chord chart", {
    obj1 <- cXchord2()
    check_ui_test(obj1)

    result <- obj1 %>% canvasXpress(
        title       = "legendPosition and theme",
        legendPosition = "bottom",
        theme          = "ggplot",
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
        canvasXpress(title                 = "contourLevel, heatmapIndicator",
                     showContourLevel      = TRUE,
                     showHeatmapIndicator  = FALSE)

    check_ui_test(result)
})

test_that("piping - correlation chart", {
    obj1 <- cXcorrelation3()
    check_ui_test(obj1)

    result <- obj1 %>%
        canvasXpress(title                   = "correlationType to circle and remove correlationAnchorLegend",
                     correlationAnchorLegend = NULL,
                     correlationType         = "circle")

    check_ui_test(result)
})

test_that("piping - dashboard chart", {
    obj1 <- cXdashboard5()
    check_ui_test(obj1)

    result <- obj1 %>% canvasXpress(
        title        = "layout 2x1",
        layoutConfig = list(list(size = "2X1"))
    )

    check_ui_test(result)
})

test_that("piping - density chart", {
    obj1 <- cXdensity10()
    check_ui_test(obj1)
    warning("remove segregateVariablesBy results in different color scheme than if you do it manually on the plot")
    result <- obj1 %>% canvasXpress(
        title      = "remove segregation",
        segregateVariablesBy = list()
    )

    check_ui_test(result)
})

test_that("piping - donut chart", {
    obj1 <- cXdonnut2()
    check_ui_test(obj1)

    result <- obj1 %>% canvasXpress(
        title         = "half circle and legendColumns 3",
        circularArc   = 180,
        legendColumns = 3
    )

    check_ui_test(result)
})

test_that("piping - dotline chart", {
    obj1 <- cXdotline2()
    check_ui_test(obj1)

    result <- obj1 %>% canvasXpress(
        title       = "add annotation to one point",
        decorations             = list(marker = list(list(sample   = "S3",
                                                          variable = "V1",
                                                          text     = "Maybe an Outlier?",
                                                          x        = 0.39,
                                                          y        = 0.71)))
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
# TODO melissa add tests up to here
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
