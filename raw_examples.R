library(canvasXpress)
library(dplyr)
library(tibble)
library(tidyr)

# ---
# FROM README

data <- t(iris[,1:4])
varAnnot <- as.matrix(iris[,5])
smpAnnot <- t(as.matrix(iris[,5]))
colnames(varAnnot) <- "Species"
colnames(smpAnnot) <- colnames(data)
rownames(smpAnnot) <- "Species"

canvasXpress(t(data), varAnnot = varAnnot, colorBy = 'Species', 
             graphType = 'Scatter2D')
canvasXpress(t(data), varAnnot = varAnnot, colorBy = 'Species',
             graphType = 'Scatter3D')
canvasXpress(t(data), varAnnot = varAnnot, colorBy = 'Species',
             graphType = "Scatter2D", scatterPlotMatrix = 1)


canvasXpress(data, smpAnnot = smpAnnot, 
             graphType = "Boxplot", groupingFactors = 'Species')

#precalculated boxplot data

vennData <- data.frame(A=57, B=12, C=67, D=72, AB=4, AC=67, AD=25, BC=67, BD=27, CD=38, ABC=69, ABD=28, ACD=52, BCD=46, ABCD=3)
canvasXpress(vennData, graphType='Venn', vennGroups=4, vennLegend=list(A="List1", B="List2", C="List3", D="List4"))

canvasXpress(data, smpAnnot = smpAnnot, 
             graphType = "Heatmap", groupingFactors = 'Species')
canvasXpress(t(data), varAnnot = varAnnot, 
             graphType = "Heatmap")

canvasXpress(data, smpAnnot = smpAnnot, 
             graphType = "Treemap", groupingFactors = "Species")

canvasXpress(t(data), varAnnot = varAnnot, 
             graphType = "ParallelCoordinates")
canvasXpress(data, smpAnnot = smpAnnot, 
             graphType = "ParallelCoordinates", groupingFactors = "Species")

canvasXpress(t(data), varAnnot = varAnnot, colorBy = "Species", 
             graphType = "ScatterBubble2D")

canvasXpress(t(data), varAnnot = varAnnot, 
             graphType = "Pie")
canvasXpress(data, smpAnnot = smpAnnot, 
             graphType = "Pie")

canvasXpress(t(data), varAnnot = varAnnot, 
             graphType = "Correlation")

canvasXpress(t(data), varAnnot = varAnnot, 
             graphType = "Circular")
canvasXpress(data, smpAnnot = smpAnnot, 
             graphType = "Circular")

canvasXpress(t(data), varAnnot = varAnnot, 
             graphType = "TagCloud")
canvasXpress(data, smpAnnot = smpAnnot, 
             graphType = "TagCloud")


# ---
# FROM VIGNETTE
# ---

data <- USArrests %>%
    rownames_to_column(var = "State") %>%
    mutate(Total = (Assault + Rape + Murder),
           Category = cut(Total, 3, 
                          labels = c("low", "med", "high"),
                          ordered_result = T)) 


### Scatter 2D Chart
cxdata          <- data %>% select(Murder, Assault)
cxdata.varAnnot <- data %>% select(UrbanPop, Category) 

rownames(cxdata) <- data[, "State"]
rownames(cxdata.varAnnot) <- data[, "State"]

canvasXpress(data                    = cxdata,
             varAnnot                = cxdata.varAnnot,
             graphType               = "Scatter2D",
             colorBy                 = "UrbanPop",
             shapeBy                 = "Category",
             legendPosition          = "right",
             legendOrder             = list("Category" = list("low", "med", "high")),
             title                   = "Murder vs Assault Rates",
             titleScaleFontFactor    = 0.5)

### Stacked Bar Chart

cxdata           <- t(data %>% select(Assault, Rape, Murder))
colnames(cxdata) <- data$State

canvasXpress(data                  = cxdata,
             graphType             = "Stacked",
             colorScheme           = "Blues",
             graphOrientation      = "vertical",
             legendInside          = TRUE,
             legendPosition        = "topRight",
             smpLabelRotate        = 20,
             title                 = "US Arrests by State and Type",
             titleScaleFontFactor  = 0.5,
             xAxisTitle            = "Total Arrests",
             xAxis2Title           = "Total Arrests")


### Clustered Bar Chart

cxdata           <- t(data %>% select(Assault, Rape, Murder))
colnames(cxdata) <- data$State

canvasXpress(data                    = cxdata,
             graphType               = "Stacked",
             graphOrientation        = "horizontal",
             colorScheme             = "Reds",
             showSampleNames         = FALSE,
             title                   = "Clustered Arrests",
             subtitle                = "(by State and Type)",
             titleScaleFontFactor    = 0.5,
             subtitleScaleFontFactor = 0.25,
             xAxisShow               = FALSE,
             xAxis2Title             = "Total Arrests",
             legendPosition          = "bottom",
             legendColumns           = 3,
             #canvasXpress clustering options  
             samplesClustered        = TRUE,
             linkage                 = "single",
             distance                = "manhattan",
             smpDendrogramPosition   = "left")

cxdata           <- t(data %>% select(Assault, Rape, Murder))
colnames(cxdata) <- data$State

cxdata.tree      <- list("smps" = "((8,32):20.2,(((4,((0,7):14.7,(23,(1,(27,(21,(31,(12,17):6.8):7.8):11.0):14.0):14.7):15.8):16.2):17.9,(39,(30,(2,19):12.4):13.4):18.6):19.9,(((11,(6,(((34,43):5.6,(13,15):6.2):7.5,(26,(37,(16,25):3.8):4.8):7.6):7.6):9.0):9.3,((10,(33,(44,(48,(14,28):2.9):3.9):5.8):8.3):14.1,(22,(40,(18,47):7.1):9.7):17.2):20.1):20.2,((5,(24,(9,(42,(3,41):13.8):14.9):15.0):15.5):21.0,(38,(46,(36,(20,(35,(45,(29,49):5.8):6.0):7.6):7.9):13.0):14.3):23.7):24.2):29.1):33.8):43.2"
)

canvasXpress(data                    = cxdata,
             graphType               = "Stacked",
             graphOrientation        = "horizontal",
             colorScheme             = "Reds",
             
             showSampleNames         = FALSE,
             title                   = "Clustered Arrests",
             subtitle                = "(by State and Type)",
             titleScaleFontFactor    = 0.5,
             subtitleScaleFontFactor = 0.25,
             xAxisShow             = FALSE,
             xAxis2Title           = "Total Arrests",
             legendPosition        = "bottomRight",
             legendInside          = TRUE,
             #user-provided clustering options
             newickData            = cxdata.tree,
             smpDendrogramPosition = "left",
             showSmpDendrogram     = TRUE)


### BoxPlot

reshape <- data %>% gather(key = "Type", value = "Rate", 
                           Assault, Rape, Murder)

cxdata           <- t(reshape %>% select(Rate))
cxdata.smpAnnot  <- t(reshape %>% select(Type))

colnames(cxdata.smpAnnot) <- colnames(cxdata)


canvasXpress(data                  = cxdata,
             smpAnnot              = cxdata.smpAnnot,
             graphType             = "Boxplot",
             colorScheme           = "Pastel",
             graphOrientation      = "vertical",
             groupingFactors       = list("Type"),
             smpLabelFontStyle     = "italic",
             smpLabelRotate        = 90,
             showLegend            = FALSE,
             title                 = "US Arrests by Type",
             titleScaleFontFactor  = 0.5,)


precalc.stats <- apply(data %>% select(Assault, Rape, Murder), 2, boxplot.stats)
precalc.data  <- as.data.frame(lapply(precalc.stats, function(x) { x$stats }))
precalc.out   <- data.frame(lapply(precalc.stats, function(x) { paste(x$out, collapse=',') }), stringsAsFactors = F)

cxdata <- precalc.data %>% mutate_all(as.character)
cxdata <- bind_rows(cxdata, precalc.out)
rownames(cxdata) <- c("iqr1", "qtl1", "median", "qtl3", "iqr3", "outliers")

canvasXpress(data                  = cxdata,
             graphType             = "Boxplot",
             boxplotGroupData      = "Type",
             colorScheme           = "Pastel",
             graphOrientation      = "vertical",
             groupingFactors       = list("Type"),
             smpLabelFontStyle     = "italic",
             smpLabelRotate        = 90,
             showLegend            = FALSE,
             title                 = "US Arrests by Type",
             titleScaleFontFactor  = 0.5,)
