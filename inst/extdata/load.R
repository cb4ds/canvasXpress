
exdata <- c("cX-area3-dat", "cX-area-dat", "cX-area2-dat", "cX-arealine-dat", 
            "cX-stacked1-dat", "cX-stacked1-smp", "cX-basic-dat", 
            "cX-basic2-dat", "cX-iris-dat", "cX-iris-smp", "cX-generic-dat", 
            "cX-generic-smp", "cX-generic-var", "cX-simple-dat", "cX-simple-smp", 
            "cX-lollipop-dat", "cX-boxplot-dat", "cX-boxplot-smp", 
            "cX-boxplot1-dat", "cX-boxplot1-smp", "cX-scents-dat", 
            "cX-scents-smp", "cX-cancersurvival-dat", "cX-cancersurvival-smp",
            "cX-cancersurvival-var", "cX-tree-dat", "cX-tree-smp", 
            "cX-chord-dat", "cX-circular-dat", "cX-circular-smp", 
            "cX-circular-var", "cX-circular2-dat", "cX-circular2-smp", 
            "cX-circular2-var", "cX-volcano-dat", "cX-sunburst-dat", 
            "cX-sunburst-smp", "cX-cars-dat", "cX-cars-smp", "cX-cars-var", 
            "cX-dumbbell-dat", "cX-dumbbell-var", "cX-heatmapR-dat", 
            "cX-heatmapR-smp", "cX-heatmapR-var", 
            "cX-multidimensionalheatmap-dat", "cX-multidimensionalheatmap-dat2", 
            "cX-multidimensionalheatmap-dat3", "cX-multidimensionalheatmap-dat4",
            "cX-multidimensionalheatmap-smp", "cX-multidimensionalheatmap-var", 
            "cX-multidimensionalheatmap2-dat", "cX-cancersurvivalt-dat", 
            "cX-cancersurvivalt-smp", "cX-cancersurvivalt-var",
            "cX-alcoholtobaccot-dat", "cX-alcoholtobaccot-smp",
            "cX-kaplanmeier-dat", "cX-kaplanmeier-var", "cX-irist-dat",
            "cX-irist-var", "cX-irist-smp", "cX-line-dat", "cX-line-smp",
        # "cX-map-dat", "cX-map-var",
            "cX-nonlinearfit-dat", 
        # "cX-oncoprint-dat",
            "cX-oncoprint-dat4", "cX-oncoprint-smp", "cX-oncoprint-var", 
            "cX-sankey-dat", "cX-sankey-smp", "cX-sankey2-dat", "cX-sankey2-smp",
            "cX-scentst-dat", "cX-scentst-var", "cX-ageheightt-dat", 
            "cX-ageheightt-smp", "cX-breastcancert-dat", "cX-breastcancert-smp", 
            "cX-scatterR-dat", "cX-scatterR-var", "cX-scatterR2-dat", 
            "cX-scatterR2-var", "cX-scatterR3-dat", "cX-scatterR3-var", 
            "cX-scatterR4-dat", "cX-scatter3d-dat", "cX-generic2-dat", 
            "cX-generic2-smp", "cX-generic2-var", "cX-bubble-dat", "cX-bubble-var", 
            "cX-stacked2-dat", "cX-stacked2-smp", "cX-diverging-dat"
            )


for (file in exdata) {
    message(file)
    data <- read.delim(paste0("https://canvasxpress.org/data/", file, ".txt"),
                       row.names = 1)
    saveRDS(data, 
            file = paste0("inst/extdata/", file, ".RData"),
            ascii = TRUE)
}
   
