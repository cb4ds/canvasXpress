# @importFrom magrittr %>%
magrittr::`%>%`



# cxPlot
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

# modPlot
modPlot <- function(cxplot, ...) {
    stop('not implemented yet')
}



# is.cxplot
# 
# Reports whether x is a cxplot object
is.cxplot <- function(x) inherits(x, "cxplot")



# print.cxplot
# 
# Explicitly creates htmlwidgets object for output
print.cxplot <- function(x, ...) {
    stop('not implemented yet')
}
