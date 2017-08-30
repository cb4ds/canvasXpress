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
                    "TagCloud" )

      
    # To Be Programmed
    # ----------------
    # Candlestick
    # Venn
    # Tree
    # Sankey
    # Network
    # Genome
    # Precalculated Boxplot
    

    if (!(graphType %in% validTypes)) stop("graphType is invalid, must be one of <",
                                           paste(validTypes, collapse = ", "), ">")
}


assignCanvasXpressColnames <- function(x) {
    if (is.null(colnames(x))) {
        paste("V", seq(length = ncol(x)), sep = "")
    } else {
        make.names(colnames(x), unique = TRUE)
    }
}


assignCanvasXpressRownames <- function(x) {
    if (is.null(rownames(x))) {
        paste("V", seq(length = nrow(x)), sep = "")
    } else {
        make.names(rownames(x), unique = TRUE)
    }
}


convertRowsToList <- function(x) {
    seq_row <- function(x) {
        # From BBmisc
        seq_len(nrow(x))
    }
    
    # From BBmisc
    res = lapply(seq_row(x), function(i) stats::setNames(x[i,], NULL))
    stats::setNames(res, make.names(rownames(x), unique = TRUE))
}
