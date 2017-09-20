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
    
    # for backwards compatibility we accept both data and vennData
    if (graphType == "Venn") {
        vdata <- ifelse(is.null(data), config$vennData, data)
        
        if (is.null(vdata)) {
            stop("vennData cannot be NULL!")
        }
        
        if (!inherits(vdata, c('data.frame', 'matrix', 'list'))) {
            stop('vennData must be a data.frame, matrix, or named list')
        }
        
        if (inherits(vdata, c('list')) & (length(vdata) > 1)) {
            stop("Venn diagrams do not support multiple datasets")
        }

        if (!("vennLegend" %in% names(config)) | 
            !("vennGroups" %in% names(config))) {
            stop("Venn diagrams must specify both the <vennLegend> and <vennGroups> parameters")
        }
    }
    else if (graphType == "Network") {
        ndata <- NULL
        edata <- NULL
        if (!is.null(data)) {
            if (!("nodeData" %in% names(data)) & !("edgeData" %in% names(data))) {
                stop("Network diagrams must specify both <nodeData> and <edgeData> as parameters or named data list items")
            }
            ndata <- data$nodeData
            edata <- data$edgeData
        }
        else {
            if (!("nodeData" %in% names(config)) | 
                !("edgeData" %in% names(config))) {
                stop("Network diagrams must specify both <nodeData> and <edgeData> as parameters or named data list items")
            }
            ndata <- config$nodeData
            edata <- config$edgeData
        }
        
        if (is.null(ndata)) {
            stop("nodeData cannot be NULL!")
        }
        
        if (is.null(edata)) {
            stop("edgeData cannot be NULL!")
        }
        
        if (!inherits(ndata, c('data.frame', 'matrix'))) {
            stop('nodeData must be a data.frame or matrix')
        }
        
        if (!inherits(ndata, c('data.frame', 'matrix'))) {
            stop('edgeData must be a data.frame or matrix')
        }
    }
    else if (!(graphType %in% noDataNecessary)) {
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


setup_y <- function(data) {
    
    y <- NULL
    
    if (inherits(data, "list")) {
        if (length(data) > 1) {
            y      <- lapply(data, as.matrix, dimnames = list())
            y$smps <- as.list(assignCanvasXpressColnames(data$y))
            y$vars <- as.list(assignCanvasXpressRownames(data$y))
            
            #rename y to data for canvasXpress
            y$data <- y$y  
            y$y    <- NULL
        }
        else {
            y <- list(vars = as.list(assignCanvasXpressRownames(data[[1]])), 
                      smps = as.list(assignCanvasXpressColnames(data[[1]])), 
                      data = as.matrix(data[[1]], dimnames = list()))
        }
    }
    else {
        y <- list(vars = as.list(assignCanvasXpressRownames(data)), 
                  smps = as.list(assignCanvasXpressColnames(data)), 
                  data = as.matrix(data, dimnames = list()))
    }
    
    y
}

setup_x <- function(y_smps, smpAnnot) {
    x <- NULL
    
    if (!is.null(smpAnnot)) {
        if (identical(as.list(assignCanvasXpressColnames(smpAnnot)), y_smps)) {
            x <- lapply(convertRowsToList(smpAnnot), function(d) if (length(d) > 1) d else list(d))
        }
        else if (!identical(as.list(assignCanvasXpressRownames(smpAnnot)), y_smps)) {
            stop("Rownames in smpAnnot are different from column names in data")
        }
        else {
            x <- lapply(convertRowsToList(t(smpAnnot)), function(d) if (length(d) > 1) d else list(d))
        }
    }
    
    x
}

setup_z <- function(y_vars, varAnnot) {
    z <- NULL
    
    if (!is.null(varAnnot)) {
        if (identical(as.list(assignCanvasXpressRownames(varAnnot)), y_vars)) {
            z <- lapply(convertRowsToList(t(varAnnot)), function(d) if (length(d) > 1) d else list(d))
        }
        else if (!identical(as.list(assignCanvasXpressColnames(varAnnot)), y_vars)) {
            stop("Column names in varAnnot are different from row names in data")
        }
        else {
            z <- lapply(convertRowsToList(varAnnot), function(d) if (length(d) > 1) d else list(d))
        }
    }
    
    z
}

