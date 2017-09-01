assertDataCorrectness <- function(data, graphType) {
    if (is.null(data))      stop("data cannot be NULL!")
    if (is.null(graphType)) stop("graphType cannot be NULL!")
    
    validTypes <- c("Scatter2D", 
                    "Scatter3D",
                    "Bar", 
                    "Stacked", 
                    "StackedPercent", 
                    "Area", 
                    "Line", 
                    "Dotplot",
                    "BarLine", 
                    "StackedLine", 
                    "AreaLine", 
                    "DotLine", 
                    "StackedPercentLine",
                    "Boxplot",
                    "Heatmap", 
                    "Treemap",
                    "ParallelCoordinates",
                    "ScatterBubble2D",
                    "Pie",
                    "Correlation",
                    "Circular",
                    "TagCloud",
                    "Map",
                    "Sankey")

      
    # To Be Programmed
    # ----------------
    # Candlestick
    # Venn
    # Tree
    # Network
    # Genome
    # Precalculated Boxplot
    

    if (!(graphType %in% validTypes)) stop("graphType is invalid, must be one of <",
                                           paste(validTypes, collapse = ", "), ">")
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
