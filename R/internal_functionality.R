assertDataCorrectness <- function(data, graphType) {
    if (is.null(graphType)) stop("graphType cannot be NULL!")
    
    validTypes <- c("Area", "AreaLine",
                    "Bar", "BarLine",
                    "Boxplot",
                    "Circular",
                    "Correlation",
                    "Dotplot", "DotLine",
                    "Heatmap",
                    "Line",
                    "Map",
                    "Pie",
                    "ParallelCoordinates",
                    "Sankey",
                    "Scatter2D", "Scatter3D", "ScatterBubble2D",
                    "Stacked", "StackedPercent", "StackedLine", "StackedPercentLine",
                    "Tree",
                    "Treemap",
                    "TagCloud",
                    "Venn")

    noDataNecessary <- c("Venn")
      
    # To Be Programmed
    # ----------------
    # Network
    # Genome
    # Precalculated Boxplot
    

    if (!(graphType %in% validTypes)) stop("graphType is invalid, must be one of <",
                                           paste(validTypes, collapse = ", "), ">")
    
    if (!(graphType %in% noDataNecessary)) {
        if (is.null(data)) stop("data cannot be NULL!")
        
        if (!class(data) %in% c('data.frame', 'matrix')) {
            stop('data must be a data.frame or matrix classed object')
        }
    }
}


assignCanvasXpressColnames <- function(x) {
    if (is.null(colnames(x))) {
        names <- paste("V", seq(length = ncol(x)), sep = "")
    } else {
        names <- colnames(x)
    }
    return(names)
}


assignCanvasXpressRownames <- function(x) {
    if (is.null(rownames(x))) {
        names <- paste("V", seq(length = nrow(x)), sep = "")
    } else {
        names <- rownames(x)
    }
    return(names)
}


convertRowsToList <- function(x) {
    seq_row <- function(x) {
        # From BBmisc
        seq_len(nrow(x))
    }
    
    # From BBmisc
    res = lapply(seq_row(x), function(i) stats::setNames(x[i,], NULL))
    stats::setNames(res, rownames(x))
}
