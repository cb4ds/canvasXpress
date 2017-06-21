#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`



#' cxPlot
#' 
#' TBD
#' 
#' @return cxplot object
#' 
#' @export 
cxPlot <- function(data, 
                   smpAnnot = NULL, 
                   varAnnot = NULL,
                   graphType = "Scatter2D", ...) {
    
    plot <- structure(
                list(data = data,
                     smpAnnot = smpAnnot,
                     varAnnot = varAnnot,
                     graphType = graphType,
                     parameters = ...
                     ), 
                class = c("cxplot"))
    
    stop('not implemented yet')
    
    plot
}

#' modPlot
#' 
#' TBD
#' 
#' @return cxplot object
#' 
#' @export 
modPlot <- function(cxplot, ...) {
    stop('not implemented yet')
}



#' Reports whether x is a cxplot object
is.cxplot <- function(x) inherits(x, "cxplot")



#' Explicitly creates htmlwidgets object for output
#'
#' @param x plot to display
#' 
#' @return htmlwidgets object
#' 
#' @export
print.cxplot <- function(x, ...) {
    stop('not implemented yet')
}
