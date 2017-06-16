assertDataCorrectness <- function(data, graphType) {
    if (is.null(data))      stop("data cannot be NULL!")
    if (is.null(graphType)) stop("graphType cannot be NULL!")
    
    validTypes <- c("Bar", "Line", "Scatter2D", "Scatter3D", "Stacked")
    
    # validTypes <- c("Bar", "Line", "Area", "AreaLine", "BarLine", "Boxplot", 
                    # "Dotplot", "DotLine", "Heatmap", "Candlestick", "Stacked", 
                    # "StackedLine", "StackedPercent", "StackedPercentLine", 
                    # "Tree", "Treemap", "TagCloud", "ParallelCoordinates", 
                    # "Sankey", "Scatter2D", "ScatterBubble2D", "Scatter3D", 
                    # "Correlation", "Pie", "Venn", "Network", "Genome", 
                    # "Circular")

    if (!(graphType %in% validTypes)) stop("graphType is invalid, must be one of <",
                                           paste(validTypes, collapse =", "), ">")
    
    
    
    #Network
    #Venn
    #Genome
    #Boxplot w/ precalc. flag
    
}


assignCanvasXpressColnames <- function(x) {
    if (is.null(colnames(x))) {
        paste("V", seq(length = ncol(x)), sep = "")
    } else {
        colnames(x)
    }
}


assignCanvasXpressRownames <- function(x) {
    if (is.null(rownames(x))) {
        paste("V", seq(length = nrow(x)), sep = "")
    } else {
        rownames(x)
    }
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
