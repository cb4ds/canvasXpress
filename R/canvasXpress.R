#' CanvasXpress Visualization Package
#'
#' A package to assist in creating visualizations in CanvasXpress in R. 
#' 
#' CanvasXpress is a standalone JavaScript library for reproducible research
#' with complete tracking of data and end-user modifications stored in a single 
#' PNG image that can be played back.
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
#' @param data data.frame or matrix classed object
#' \emph{(rows are referred to as variables; columns are referred to as samples)}
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
canvasXpress <- function(data = NULL,     # y
                         smpAnnot = NULL, # x
                         varAnnot = NULL, # z
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
    
    assertDataCorrectness(data, graphType)
    
    if (graphType == "Venn") {
        stop('not implemented')
        # config <- list(...)
        # 
        # if (!("vennData" %in% names(config)) | !("vennLegend" %in% names(config))) {
        #     stop("Venn diagrams must specify vennData and vennLegend parameters") 
        # }
    }
    else if (graphType == "Map") {
        stop('not implemented')
    }
    else if (graphType == "Network") {
        stop('not implemented')
    }
    else if (graphType == "Genome") {
        stop('not implemented')
    }
    # standard graph
    else {
        if (inherits(data, "list")) {
            y      <- lapply(data, as.matrix, dimnames = list())
            y$smps <- as.list(assignCanvasXpressColnames(data[[1]]))
            y$vars <- as.list(assignCanvasXpressRownames(data[[1]]))
        }
        else {
            y <- list(vars = as.list(assignCanvasXpressRownames(data)), 
                      smps = as.list(assignCanvasXpressColnames(data)), 
                      data = as.matrix(data, dimnames = list()))
        }

        x <- NULL
        z <- NULL
        
        if (!is.null(smpAnnot)) {
            if (identical(as.list(assignCanvasXpressColnames(smpAnnot)), y$smps)) {
                x <- lapply(convertRowsToList(smpAnnot), function(d) if (length(d) > 1) d else list(d))
            }
            else if (!identical(as.list(assignCanvasXpressRownames(smpAnnot)), y$smps)) {
                stop("Rownames in smpAnnot are different from column names in data")
            }
            else {
                x <- lapply(convertRowsToList(t(smpAnnot)), function(d) if (length(d) > 1) d else list(d))
            }
        }
        
        if (!is.null(varAnnot)) {
            if (identical(as.list(assignCanvasXpressRownames(varAnnot)), y$vars)) {
                z <- lapply(convertRowsToList(t(varAnnot)), function(d) if (length(d) > 1) d else list(d))
            }
            else if (!identical(as.list(assignCanvasXpressColnames(varAnnot)), y$vars)) {
                stop("Column names in varAnnot are different from row names in data")
            }
            else {
                z <- lapply(convertRowsToList(varAnnot), function(d) if (length(d) > 1) d else list(d))
            }
        }
        # Data
        data <- list(y = y, x = x, z = z)
        
        # Config
        config <- list(graphType = graphType, isR = TRUE, ...)
        
        # CanvasXpress Object
        cx_object <- list(data        = data, 
                          config      = config, 
                          events      = events, 
                          afterRender = afterRender)
    } #standard graph

    
    options(htmlwidgets.TOJSON_ARGS = list(dataframe = "columns", 
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
