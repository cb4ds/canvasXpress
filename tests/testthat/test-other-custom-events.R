context("canvasXpress customEvents")


test_that("Barplot customEvents", {

    tryCatch({
        y <- read.table("https://www.canvasxpress.org/data/r/cX-generic-dat.txt", header = TRUE, sep = "\t", quote = "", row.names = 1, fill = TRUE, check.names = FALSE, stringsAsFactors = FALSE)
        x <- read.table("https://www.canvasxpress.org/data/r/cX-generic-smp.txt", header = TRUE, sep = "\t", quote = "", row.names = 1, fill = TRUE, check.names = FALSE, stringsAsFactors = FALSE)
        z <- read.table("https://www.canvasxpress.org/data/r/cX-generic-var.txt", header = TRUE, sep = "\t", quote = "", row.names = 1, fill = TRUE, check.names = FALSE, stringsAsFactors = FALSE)
    },
    error = function(e) {
        skip('Unable to read data files')
    })

    events <- JS("{ 'mousemove' : function(o, e, t) {
                                    if (o) {
                                        if (o.objectType == null) {
                                            t.showInfoSpan(e, '<b>' + o.y.vars[0] + '</b><br/>' +
                                            'Sample:' + o.y.smps[0] +  '<br/>' +
                                            '<i>Value:</i>' +  o.y.data[0][0]);
                                        }
                                        else {
                                            t.showInfoSpan(e, o.display);
                                        };
                                    };}}")
    result <-  canvasXpress(data         = y,
                            smpAnnot     = x,
                            varAnnot     = z,
                            graphType    = "Bar",
                            scatterType  = "bar",
                            is3DPlot     = TRUE,
                            title        = "Barplot - customEvents",
                            events       = events,
                            xAxis        = list("V1", "V2", "V3", "V4"),
                            yAxis        = list("data"),
                            zAxis        = list("S1", "S2", "S3", "S4", "S5", "S6"))

    check_ui_test(result)
})
