assertDataCorrectness <- function(data, graphType, config) {
    
    validGraphTypes <- c("Area", "AreaLine", "Bar", "BarLine", "Boxplot",
                         "Circular", "Correlation", "Dotplot", "DotLine", 
                         "Genome", "Heatmap", "Line", "Map", "Network", "Pie", 
                         "ParallelCoordinates", "Sankey", "Scatter2D", 
                         "Scatter3D", "ScatterBubble2D", "Stacked", 
                         "StackedPercent", "StackedLine", "StackedPercentLine", 
                         "Tree", "Treemap", "TagCloud", "Venn")
    noDataNecessary  <- c("Map")
    
    if (is.null(graphType)) stop("graphType cannot be NULL!")

    if (!(graphType %in% validGraphTypes)) {
        stop("graphType is invalid, must be one of <",
             paste(validGraphTypes, collapse = ", "), ">")
    }
    
    if (!(graphType %in% noDataNecessary)) {
        if (is.null(data)) {
            stop("data cannot be NULL!")
        }

        if (!inherits(data, c('data.frame', 'matrix', 'list'))) {
            stop('data must be a data.frame, matrix, or named list')
        }
        
        if (inherits(data, c('list'))) {
            if (length(data) < 1) {
                stop('data specified as a list must contain at least one item')
            }
            
            if (graphType == "Venn" & length(data) > 1) {
                    stop("Venn diagrams do not support multiple datasets")
            }
            
            if (length(data) > 1 ) {
                if (is.null(names(data))) {
                    stop('data specified as a list of multiple items must have named elements') 
                }
                else if (!("y" %in% names(data))) {
                    stop('data specified as a list of multiple items must contain a <y> element') 
                }
            }
            
            fail <- vector(mode = "character", length = 0)
            
            for (name in names(data)) {
                if (!inherits(data[[name]], c('data.frame', 'matrix'))) {
                    fail <- c(fail, name)
                }
            }
            if (length(fail) > 0) {
                stop('data list elements <', paste(fail, collapse = ', '), 
                     '> are not data.frame or matrix elements')
            }
        }
    }
    
    if (graphType == "Venn") {
        if (!("vennLegend" %in% names(config)) | 
            !("vennGroups" %in% names(config))) {
            stop("Venn diagrams must specify both the <vennLegend> and <vennGroups> parameters")
        }
    }
    
} #assertDataCorrectness


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
