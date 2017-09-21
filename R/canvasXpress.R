#' CanvasXpress Visualization Package
#'
#' A package to assist in creating visualizations in CanvasXpress in R. 
#' 
#' CanvasXpress is a standalone JavaScript library for reproducible research
#' with complete tracking of data and end-user modifications stored in a single 
#' PNG image that can be played back for an extensive set of visualizations.
#'
#'
#' @section More Information:
#' \url{http://canvasxpress.org}  
#' 
#' \code{browseVignettes(package = 'canvasXpress')}
#'
#' @docType package
#' @aliases canvasXpress-package
"_PACKAGE"


#' canvasXpress
#'
#' Custom HTML widget creation function based on widget YAML and JavaScript for 
#' use in any html-compatible context
#'  
#' 
#' @param data data.frame-, matrix=, or list- classed object
#' @param smpAnnot additional data that applies to samples (columns)
#' @param varAnnot additional data that applies to variables (rows)
#' @param graphType type of graph to be plotted - default = 'Scatter2D'
#' @param events user-defined events (e.g. mousemove, mouseout, click and dblclick)
#' @param afterRender event triggered after rendering
#' @param pretty print tagged code (json/html) nicely - default = FALSE
#' @param digits display digits - default = 4
#' @param width plot width (valid CSS units) - default = 600px
#' @param height plot height (valid CSS units) - default = 400px
#' @param ... additional parameters passed to canvasXpress
#'
#' @return htmlwidgets object
#'
#' @export
canvasXpress <- function(data = NULL,
                         smpAnnot = NULL,
                         varAnnot = NULL,
                         #config items
                         graphType = 'Scatter2D', 
                         # straight-through
                         events = NULL, 
                         afterRender=NULL,
                         #htmlwidgets options
                         pretty = FALSE,
                         digits = 4,
                         width  = 600, 
                         height = 400,
                         ... ) {
    
    config <- list(graphType = graphType, isR = TRUE, ...)
    assertDataCorrectness(data, graphType, config)
    
    x         <- NULL
    y         <- NULL
    z         <- NULL
    dataframe <- "columns"
    
    if (graphType == "Venn") {
        vdata <- NULL
        if (is.null(data)) {
            if (inherits(config$vennData, "list")) {
                vdata <- config$vennData[[1]]
            }
            else {
                vdata <- config$vennData
            }
        }
        else {
            if (inherits(data, "list")) {
                vdata <- data[[1]]
            }
            else {
                vdata <- data
            }
        }
        legend <- config$vennLegend
       
        # Config - remove venn items
        config <- config[!(names(config) %in% c("vennData", "vennLegend"))]
        
        # CanvasXpress Object
        cx_object <- list(data        = list(venn = list(data = vdata, legend = legend)),
                          config      = config, 
                          events      = events, 
                          afterRender = afterRender)
    }
    else if (graphType == "Map" && 
             (is.null(data) || (inherits(data, "logical") && data == FALSE))) {

        # CanvasXpress Object
        cx_object <- list(data        = FALSE, 
                          config      = config, 
                          events      = events, 
                          afterRender = afterRender)
    }
    else if (graphType == "Network") {
        ndata     <- NULL
        edata     <- NULL
        dataframe <- "rows"

        if (is.null(data)) {
            ndata <- config$nodeData
            edata <- config$edgeData
            config <- config[!(names(config) %in% c("nodeData", "edgeData"))]
        }
        else {
            ndata <- data$nodeData
            edata <- data$edgeData
        }
        
        # CanvasXpress Object
        cx_object <- list(data        = list(nodes = ndata, edges = edata),
                          config      = config, 
                          events      = events, 
                          afterRender = afterRender)
    }
    else if (graphType == "Genome") {
        stop('The Genome graphType is not yet implemented')
    }
    # standard graph
    else {
        y <- setup_y(data)
        x <- setup_x(y$smps, smpAnnot)
        z <- setup_z(y$vars, varAnnot)

        # CanvasXpress Object
        cx_object <- list(data        = list(y = y, x = x, z = z), 
                          config      = config, 
                          events      = events, 
                          afterRender = afterRender)
    } #standard graph

    options(htmlwidgets.TOJSON_ARGS = list(dataframe = dataframe, 
                                           pretty    = pretty, 
                                           digits    = digits))

    htmlwidgets::createWidget("canvasXpress", 
                              cx_object, 
                              width  = width,
                              height = height)
}



#' canvasXpressOutput
#'
#' Output creation function for canvasXpressOutput in Shiny applications and 
#' interactive Rmd documents
#'  
#' @param outputId shiny unique ID
#' @param width width of the element - default = 100\%
#' @param height height of the element - default = 400px
#'
#' @return Output function that enables the use of the widget in applications
#'
#' @seealso \link[canvasXpress]{renderCanvasXpress}
#' @seealso \link[canvasXpress]{cxShinyExample}
#' 
#' @export
canvasXpressOutput <- function(outputId, width = "100%", height = "400px") {
    htmlwidgets::shinyWidgetOutput(outputId, "canvasXpress", 
                                   width, height, 
                                   package = "canvasXpress")
}



#' renderCanvasXpress
#'
#' Render function for canvasXpressOutput in Shiny applications and 
#' interactive Rmd documents
#'  
#' @param expr expression used to render the canvasXpressOutput
#' @param env environment to use - default = parent.frame()
#' @param quoted whether the expression is quoted - default = FALSE
#'
#' @return Render function that enables the use of the widget in applications
#'
#' @seealso \link[canvasXpress]{canvasXpressOutput}
#' @seealso \link[canvasXpress]{cxShinyExample}
#' 
#' @export
renderCanvasXpress <- function(expr, env = parent.frame(), quoted = FALSE) {
    if (!quoted) { 
        expr <- substitute(expr) 
    } # force quoted
    htmlwidgets::shinyRenderWidget(expr, canvasXpressOutput, env, quoted = TRUE)
}
