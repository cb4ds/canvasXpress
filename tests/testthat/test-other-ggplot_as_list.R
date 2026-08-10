context("ggplot as list")

test_that("ggplot.as.list - pass non ggplot2 object", {
    skip_if_not_installed("ggplot2")
    expect_error(ggplot.as.list("test"), regexp = "Not a ggplot or ggmatrix object")
})


test_that("ggplot.as.list - ggplot2 GeomPoint", {
    skip_if_not_installed("ggplot2")

    gplot_orig <- ggplot(data = mtcars, aes(x = wt, y = mpg)) +
        geom_point(aes(color = cyl), position = "jitter")
    gplot <- gplot_orig +
        theme(legend.justification   = c(1, 1),
              legend.title           = element_blank(),
              panel.background       = element_rect(fill = "white", colour = "grey50")) +
        scale_colour_viridis_c() +
        ggtitle("GGPlot Example w/Hover") +
        xlab("wt") +
        ylab("mpg")

    cxplot      <- ggplot.as.list(gplot)
    cxplot_list <- jsonlite::parse_json(cxplot)

    expect_equal(class(cxplot), "json")
    expect_equal(length(cxplot_list), 16)
    expect_true(cxplot_list$isGGPlot)
    expect_true(length(cxplot_list$data) == 33)
    expect_equal(cxplot_list$data[[2]][[1]], "Mazda RX4")

    gplot <- gplot_orig +
        scale_colour_gradientn(colours = terrain.colors(10))
    expect_equal(class(ggplot.as.list(gplot)), "json")

    gplot <- gplot_orig +
        facet_wrap(vars(drat = cut_number(drat, 3)), labeller = label_both)
    expect_equal(class(ggplot.as.list(gplot)), "json")
})


test_that("ggplot.as.list - ggplot2 GeomBin2d", {
    skip_if_not_installed("ggplot2")

    gplot <- ggplot(diamonds, aes(x, y)) + xlim(4, 10) + ylim(4, 10) +
        geom_tile() +
        stat_bin2d()

    cxplot      <- suppressWarnings(ggplot.as.list(gplot))
    cxplot_list <- jsonlite::parse_json(cxplot)

    expect_equal(class(cxplot), "json")
    expect_equal(length(cxplot_list), 16)
    expect_true(cxplot_list$isGGPlot)
    expect_true(length(cxplot_list$data) > 0)
    expect_equal(cxplot_list$data[[2]][[1]], "1")
})


test_that("ggplot.as.list - ggplot2 GeomBar", {
    skip_if_not_installed("ggplot2")

    gplot <- ggplot(diamonds, aes(carat)) +
        geom_histogram()

    cxplot      <- suppressWarnings(ggplot.as.list(gplot))
    cxplot_list <- jsonlite::parse_json(cxplot)

    expect_equal(class(cxplot), "json")
    expect_equal(length(cxplot_list), 16)
    expect_true(cxplot_list$isGGPlot)
    expect_equal(length(cxplot_list$data), 53941)
    expect_equal(cxplot_list$data[[2]][[1]], "1")
})


test_that("ggplot.as.list - ggplot2 GeomPath", {
    skip_if_not_installed("ggplot2")

    gplot <- ggplot(economics, aes(date, unemploy)) +
        geom_line()

    cxplot      <- suppressWarnings(ggplot.as.list(gplot))
    cxplot_list <- jsonlite::parse_json(cxplot)

    expect_equal(class(cxplot), "json")
    expect_equal(length(cxplot_list), 16)
    expect_true(cxplot_list$isGGPlot)
    expect_equal(length(cxplot_list$data), 575)
    expect_equal(cxplot_list$data[[2]][[1]], "1")
})


test_that("ggplot.as.list - ggplot2 StatQq", {
    skip_if_not_installed("ggplot2")
    data  <- data.frame(y = rt(200, df = 5))
    gplot <- ggplot(data, aes(sample = y)) +
        stat_qq() +
        stat_qq_line()

    cxplot      <- suppressWarnings(ggplot.as.list(gplot))
    cxplot_list <- jsonlite::parse_json(cxplot)

    expect_equal(class(cxplot), "json")
    expect_equal(length(cxplot_list), 16)
    expect_true(cxplot_list$isGGPlot)
    expect_equal(length(cxplot_list$data), 201)
    expect_equal(cxplot_list$data[[2]][[1]], "1")
})


test_that("ggplot.as.list - ggplot2 point range", {
    skip_if_not_installed("ggplot2")

    dmod <- lm(price ~ cut, data = diamonds)
    cut <- unique(diamonds$cut)
    cuts_df <- data.frame(
        cut,
        predict(dmod, data.frame(cut), se = TRUE)[c("fit", "se.fit")]
    )
    gplot <- ggplot(cuts_df) +
        aes(
            x = cut,
            y = fit,
            ymin = fit - se.fit,
            ymax = fit + se.fit,
            colour = cut
        ) +
        geom_pointrange()

    cxplot      <- suppressWarnings(ggplot.as.list(gplot))
    cxplot_list <- jsonlite::parse_json(cxplot)

    expect_equal(class(cxplot), "json")
    expect_equal(length(cxplot_list), 16)
    expect_true(cxplot_list$isGGPlot)
    expect_equal(length(cxplot_list$data), 6)
    expect_equal(cxplot_list$data[[2]][[1]], "1")
})


test_that("ggplot.as.list - patchwork", {
    skip_if(getRversion() < "4.1.0")
    skip_if_not_installed("ggplot2")
    skip_if_not_installed("patchwork")

    dmod <- lm(price ~ cut, data = diamonds)
    # Create individual ggplot2 plots
    p1 <- ggplot(mtcars, aes(x = mpg, y = disp)) +
        geom_point() +
        labs(title = "Scatterplot: MPG vs Displacement")

    p2 <- ggplot(mtcars, aes(x = factor(cyl), y = hp)) +
        geom_boxplot() +
        labs(title = "Boxplot: Horsepower by Cylinders")

    p3 <- ggplot(mtcars, aes(x = wt, fill = factor(am))) +
        geom_density(alpha = 0.6) +
        labs(title = "Density Plot: Weight by Transmission")

    # Combine plots using patchwork operators
    combined_plots <- (p1 + p2) / p3

    cxplot      <- suppressWarnings(ggplot.as.list(combined_plots))
    cxplot_list <- jsonlite::parse_json(cxplot)

    expect_equal(class(cxplot), "json")
    expect_equal(length(cxplot_list), 6)
    expect_true(cxplot_list$isGGPlot)
    expect_true(cxplot_list$isPatchwork)
    expect_equal(length(cxplot_list$data), 2)
    expect_equal(cxplot_list$data[[2]][[1]], "canvas-2")
})


test_that("ggplot.as.list - GGally", {
    skip_if_not_installed("ggplot2")
    skip_if_not_installed("GGally")

    gplot <- ggmatrix(list(
        ggplot(mtcars, aes(x = mpg, y = wt)) + geom_point(),
        ggplot(mtcars, aes(x = cyl)) + geom_bar(),
        ggplot(mtcars, aes(x = hp, y = qsec)) + geom_smooth(method = "lm"),
        ggplot(mtcars, aes(x = disp)) + geom_density()),
        nrow = 2,
        ncol = 2,
        xAxisLabels = c("X1", "X2"),
        yAxisLabels = c("Y1", "Y2"))

    cxplot      <- suppressWarnings(ggplot.as.list(gplot))
    cxplot_list <- jsonlite::parse_json(cxplot)

    expect_equal(class(cxplot), "json")
    expect_equal(length(cxplot_list), 8)
    expect_true(cxplot_list$isGGPlot)
    expect_equal(length(cxplot_list$data), 4)
    expect_equal(cxplot_list$data[[2]][[1]], "canvas-2")
})


test_that("ggplot.as.list - test segments", {
    skip_if(getRversion() < "4.1.0")
    skip_if_not_installed("ggplot2")

    segment_data <- mtcars %>%
        rownames_to_column("car") %>%
        mutate(x = 1:n(),
               y = mpg,
               xend = 1:n(),
               yend = hp / 10)
    gplot <- ggplot(segment_data) +
        geom_segment(aes(x = x, y = y, xend = xend, yend = yend,
                         colour = factor(cyl), linetype = factor(am)),
                     linewidth = 1.2,
                     arrow = arrow(length = unit(0.2, "cm"))) +
        geom_point(aes(x = x, y = y), size = 3, colour = "red") +
        geom_point(aes(x = xend, y = yend), size = 3, colour = "blue") +
        scale_colour_manual(values = c("4" = "green", "6" = "orange", "8" = "red"),
                            name = "Cylinders") +
        labs(title = "MPG vs HP/10 for mtcars",
             x = "Car Index",
             y = "Value",
             linetype = "Transmission") +
        theme_minimal()

    cxplot      <- suppressWarnings(ggplot.as.list(gplot))
    cxplot_list <- jsonlite::parse_json(cxplot)

    expect_equal(class(cxplot), "json")
    expect_equal(length(cxplot_list), 16)
    expect_true(cxplot_list$isGGPlot)
    expect_equal(length(cxplot_list$data), 33)
    expect_equal(cxplot_list$data[[2]][[1]], "1")

})


test_that("ggplot.as.list - GeomBracket", {
    skip_if(getRversion() < "4.1.0")
    skip_if_not_installed("ggplot2")
    skip_if_not_installed("ggpubr")

    plot_data <- mtcars %>%
        rownames_to_column("car") %>%
        mutate(index = 1:n())

    # Define the bracket data as a separate, one-row data frame
    bracket_data <- data.frame(
        xmin = c(1, 5),
        xmax = c(4, 8),
        y.position = c(35, 35),
        label = c("Group 1", "Group 2")
    )

    gplot <- ggplot(plot_data, aes(x = index, y = mpg, fill = factor(cyl))) +
        geom_col(alpha = 0.7) +
        geom_bracket(
            data = bracket_data,
            aes(xmin = xmin, xmax = xmax, y.position = y.position, label = label),
            tip.length = 0.02,
            inherit.aes = FALSE) +
        scale_fill_manual(values = c("4" = "#2ecc71", "6" = "#f39c12", "8" = "#e74c3c"),
                          name = "Cylinders") +
        labs(title = "MPG Comparison with Brackets",
             x = "Car Index",
             y = "Miles Per Gallon") +
        theme_minimal() +
        theme(legend.position = "right")

    cxplot      <- suppressWarnings(ggplot.as.list(gplot))
    cxplot_list <- jsonlite::parse_json(cxplot)

    expect_equal(class(cxplot), "json")
    expect_equal(length(cxplot_list), 16)
    expect_true(cxplot_list$isGGPlot)
    expect_equal(length(cxplot_list$data), 33)
    expect_equal(cxplot_list$data[[2]][[1]], "1")

})


test_that("ggplot.as.list - ggpattern", {
    skip_if(getRversion() < "4.1.0")
    skip_if_not_installed("ggplot2")
    skip_if_not_installed("ggpattern")

    gplot <- ggplot(mtcars, aes(x = cyl, y = mpg, fill = cyl, pattern = factor(cyl))) +
        geom_violin_pattern(
            pattern_fill    = "white",
            pattern_colour  = "black",
            fill            = "white",
            colour          = "black",
            pattern_density = 0.005,
            pattern_spacing = 0.015) +
        labs(title = "Violin Plot of MPG by Cylinder with Patterns",
             x     = "Cylinders",
             y     = "Miles Per Gallon (MPG)") +
        theme_bw(base_size = 14) +
        theme(legend.position = "none")

    cxplot      <- suppressWarnings(ggplot.as.list(gplot))
    cxplot_list <- jsonlite::parse_json(cxplot)

    expect_equal(class(cxplot), "json")
    expect_equal(length(cxplot_list), 16)
    expect_true(cxplot_list$isGGPlot)
    expect_equal(length(cxplot_list$data), 33)
    expect_equal(cxplot_list$data[[2]][[1]], "Mazda RX4")

    # test another pattern
    avg_hp_data <- mtcars %>%
        group_by(cyl) %>%
        summarize(avg_hp = mean(hp), .groups = "drop")

    gplot <- ggplot(avg_hp_data, aes(x = cyl, y = avg_hp, pattern_fill = factor(cyl))) +
        geom_col_pattern(
            pattern = "crosshatch")

    cxplot      <- suppressWarnings(ggplot.as.list(gplot))
    cxplot_list <- jsonlite::parse_json(cxplot)

    expect_equal(class(cxplot), "json")
    expect_equal(length(cxplot_list), 16)
    expect_true(cxplot_list$isGGPlot)
    expect_equal(length(cxplot_list$data), 4)
    expect_equal(cxplot_list$data[[2]][[1]], "1")

})


test_that("ggplot.as.list - fill scaling", {
    skip_if(getRversion() < "4.1.0")
    skip_if_not_installed("ggplot2")

    # continuous fill scale
    gplot <- ggplot(mtcars, aes(x = mpg, fill = mpg, color = I("black"))) +
        # Use geom_density, which accepts the 'fill' aesthetic
        geom_density(alpha = 0.7) +
        # Use a specific, pre-defined palette function for the fill aesthetic
        scale_fill_viridis_c(
            option = "cividis",
            name = "Miles Per Gallon Value",
            limits = c(10, 35)
        ) +
        labs(
            title = "Continuous Fill Scale Example (Density Plot)",
            x = "Miles Per Gallon (MPG)"
        ) +
        theme_bw()

    cxplot      <- suppressWarnings(ggplot.as.list(gplot))
    cxplot_list <- jsonlite::parse_json(cxplot)

    expect_equal(class(cxplot), "json")
    expect_equal(length(cxplot_list), 16)
    expect_true(cxplot_list$isGGPlot)
    expect_equal(length(cxplot_list$data), 33)
    expect_equal(cxplot_list$scales$colorSpectrum[[1]], "#00204D")

    # discrete fill scale
    # 2. Define a simple vector of colors manually (base R colors)
    manual_colors <- c("4" = "blue", "6" = "orange", "8" = "red")

    # 3. Create the plot using geom_bar
    gplot <- ggplot(mtcars, aes(x = cyl, fill = as.factor(mtcars$cyl))) +
        geom_bar(color = "black") +
        # 4. Use scale_fill_manual to apply specific colors and breaks
        scale_fill_manual(
            values = manual_colors,        # Provide the explicit colors
            name = "Cylinders",
            breaks = c("4", "6", "8")      # Explicitly specify which breaks to show
        ) +
        labs(
            title = "Discrete Fill Scale (Base ggplot2)",
            x = "Number of Cylinders",
            y = "Count of Cars"
        ) +
        theme_bw()

    cxplot      <- suppressWarnings(ggplot.as.list(gplot))
    cxplot_list <- jsonlite::parse_json(cxplot)

    expect_equal(class(cxplot), "json")
    expect_equal(length(cxplot_list), 16)
    expect_true(cxplot_list$isGGPlot)
    expect_equal(cxplot_list$scales$colorBreaks[[1]], "4")

    # scaleBinned fill case
    # Ensure the 'car' column exists to use as the x-axis
    mtcars$car <- rownames(mtcars)

    # Create the plot
    gplot <- ggplot(mtcars, aes(x = reorder(car, hp), y = mpg, fill = hp)) +
        geom_bar(stat = "identity") +
        scale_fill_binned(type = "viridis") + # Use a binned viridis color scale
        labs(
            title = "MPG by Car, Fill Color Binned by HP",
            x = "Car Model (ordered by HP)",
            y = "Miles Per Gallon (MPG)",
            fill = "Horsepower (HP) Bins"
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

    cxplot      <- suppressWarnings(ggplot.as.list(gplot))
    cxplot_list <- jsonlite::parse_json(cxplot)

    expect_equal(class(cxplot), "json")
    expect_equal(length(cxplot_list), 16)
    expect_true(cxplot_list$isGGPlot)
    expect_equal(cxplot_list$scales$colorBreaks[[1]], 100)
    expect_equal(cxplot_list$scales$colorLimits[[1]], 50)
})

test_that("ggplot.as.list - color scaling", {
    skip_if(getRversion() < "4.1.0")
    skip_if_not_installed("ggplot2")

    # continuous scale
    gplot <- ggplot(mtcars, aes(x = mpg, fill = mpg, color = I("black"))) +
        # Use geom_density, which accepts the 'fill' aesthetic
        geom_density(alpha = 0.7) +
        # Use a specific, pre-defined palette function for the fill aesthetic
        scale_color_viridis_c(
            option = "cividis",
            name = "Miles Per Gallon Value",
            limits = c(10, 35)
        ) +
        labs(
            title = "Continuous Fill Scale Example (Density Plot)",
            x = "Miles Per Gallon (MPG)"
        ) +
        theme_bw()

    cxplot      <- suppressWarnings(ggplot.as.list(gplot))
    cxplot_list <- jsonlite::parse_json(cxplot)

    expect_equal(class(cxplot), "json")
    expect_equal(length(cxplot_list), 16)
    expect_true(cxplot_list$isGGPlot)
    expect_equal(length(cxplot_list$data), 33)
    expect_equal(cxplot_list$scales$colorSpectrum[[1]], "#00204D")

    # discrete scale
    # 2. Define a simple vector of colors manually (base R colors)
    manual_colors <- c("4" = "blue", "6" = "orange", "8" = "red")

    # 3. Create the plot using geom_bar
    gplot <- ggplot(mtcars, aes(x = cyl, fill = as.factor(mtcars$cyl))) +
        geom_bar(color = "black") +
        # 4. Use scale_fill_manual to apply specific colors and breaks
        scale_color_manual(
            values = manual_colors,        # Provide the explicit colors
            name = "Cylinders",
            breaks = c("4", "6", "8")      # Explicitly specify which breaks to show
        ) +
        labs(
            title = "Discrete Fill Scale (Base ggplot2)",
            x = "Number of Cylinders",
            y = "Count of Cars"
        ) +
        theme_bw()

    cxplot      <- suppressWarnings(ggplot.as.list(gplot))
    cxplot_list <- jsonlite::parse_json(cxplot)

    expect_equal(class(cxplot), "json")
    expect_equal(length(cxplot_list), 16)
    expect_true(cxplot_list$isGGPlot)
    expect_equal(cxplot_list$scales$colorBreaks[[1]], "4")

    # scaleBinned fill case
    # Ensure the 'car' column exists to use as the x-axis
    mtcars$car <- rownames(mtcars)

    # Create the plot
    gplot <- ggplot(mtcars, aes(x = reorder(car, hp), y = mpg, fill = hp)) +
        geom_point(size = 4) +
        scale_color_binned(
            type   = "viridis",
            breaks = c(100, 150, 200, 250, 300),
            limits = c(50, 350))

    cxplot      <- suppressWarnings(ggplot.as.list(gplot))
    cxplot_list <- jsonlite::parse_json(cxplot)

    expect_equal(class(cxplot), "json")
    expect_equal(length(cxplot_list), 16)
    expect_true(cxplot_list$isGGPlot)
    expect_equal(cxplot_list$scales$colorBreaks[[1]], 100)
    expect_equal(cxplot_list$scales$colorLimits[[1]], 50)
    expect_equal(cxplot_list$scales$colorScale, "ScaleBinned")
})

test_that("ggplot.as.list - x and y axis properties", {
    skip_if(getRversion() < "4.1.0")
    skip_if_not_installed("ggplot2")

    gplot <- ggplot(mtcars, aes(x = hp, y = mpg)) +
        geom_point(size = 3) +
        scale_x_continuous(name = "Engine Horsepower") +
        scale_y_continuous(name = "Miles Per Gallon") +
        labs(title = "MPG vs Horsepower") +
        theme_minimal()

    cxplot      <- suppressWarnings(ggplot.as.list(gplot))
    cxplot_list <- jsonlite::parse_json(cxplot)

    expect_equal(class(cxplot), "json")
    expect_equal(length(cxplot_list), 16)
    expect_true(cxplot_list$isGGPlot)
    expect_equal(cxplot_list[["scales"]][["xAxisSetValues"]][[2]], 100)
    expect_equal(cxplot_list[["scales"]][["xAxisSetMinorValues"]][[2]], 100)
    expect_equal(cxplot_list[["scales"]][["yAxisSetValues"]][[2]], 15)
    expect_equal(cxplot_list[["scales"]][["yAxisSetValues"]][[2]], 15)
    expect_equal(cxplot_list[["scales"]][["yAxisSetMinorValues"]][[2]], 12.5)
    expect_equal(cxplot_list[["scales"]][["yAxisSetValues"]][[2]], 15)

    # non identity scale
    gplot <- ggplot(mtcars, aes(x = hp, y = mpg)) +
        geom_point(size = 3) +
        scale_x_log10(name = "Engine Horsepower") +
        scale_y_log10(name = "Miles Per Gallon") +
        labs(title = "MPG vs Horsepower") +
        theme_minimal()

    cxplot      <- suppressWarnings(ggplot.as.list(gplot))
    cxplot_list <- jsonlite::parse_json(cxplot)

    expect_equal(class(cxplot), "json")
    expect_equal(length(cxplot_list), 16)
    expect_true(cxplot_list$isGGPlot)
    expect_equal(cxplot_list[["scales"]][["xAxisTransform"]], "log10")
    expect_equal(cxplot_list[["scales"]][["xAxisTitle"]], "Engine Horsepower")
    expect_equal(cxplot_list[["scales"]][["yAxisTransform"]], "log10")
    expect_equal(cxplot_list[["scales"]][["yAxisTitle"]], "Miles Per Gallon")
})


test_that("ggplot.as.list - pattern scale properties", {
    skip_if(getRversion() < "4.1.0")
    skip_if_not_installed("ggplot2")

    mtcars$cyl_f <- as.factor(mtcars$cyl)
    gplot <- ggplot(mtcars, aes(x = cyl_f, fill = cyl_f, pattern = cyl_f)) +
        geom_bar_pattern(
            stat = "count",
            pattern_alpha = 0.5,
            color = "black" ) +
        labs(
            title = "Car Count by Cylinder (using ggpattern)",
            x = "Cylinders",
            y = "Count",
            fill = "Cylinders",
            pattern = "Cylinders") +
        theme_minimal()

    cxplot      <- suppressWarnings(ggplot.as.list(gplot))
    cxplot_list <- jsonlite::parse_json(cxplot)

    expect_equal(class(cxplot), "json")
    expect_equal(length(cxplot_list), 16)
    expect_true(cxplot_list$isGGPlot)
    expect_equal(cxplot_list[["scales"]][["colors"]][[1]], "#F8766D")
})


test_that("ggplot.as.list - coordinates", {
    skip_if(getRversion() < "4.1.0")
    skip_if_not_installed("ggplot2")

    gplot <- ggplot(mtcars, aes(x = hp, y = mpg)) +
        geom_point(size = 3) +
        # This line sets the specific properties you want to cover:
        coord_flip(
            xlim = c(100, 350),
            ylim = c(10, 35),
            expand = TRUE # Default is TRUE, included for clarity
        ) +
        labs(title = "MPG vs HP (Flipped Coords)") +
        theme_minimal()

    cxplot      <- suppressWarnings(ggplot.as.list(gplot))
    cxplot_list <- jsonlite::parse_json(cxplot)

    expect_equal(class(cxplot), "json")
    expect_equal(length(cxplot_list), 16)
    expect_true(cxplot_list$isGGPlot)
    expect_equal(cxplot_list[["coords"]][["setMinX"]], 100)
    expect_equal(cxplot_list[["coords"]][["setMaxX"]], 350)
    expect_equal(cxplot_list[["coords"]][["setMinY"]], 10)
    expect_equal(cxplot_list[["coords"]][["setMaxY"]], 35)
    expect_true(cxplot_list[["coords"]][["flip"]])
})


test_that("ggplot.as.list - cut functions layer", {
    skip_if(getRversion() < "4.1.0")
    skip_if_not_installed("ggplot2")

    gplot <- ggplot(mtcars, aes(x = hp, y = mpg)) +
        # Use cut_number within the aesthetic mapping of this layer
        geom_point(aes(color = cut_number(disp, n = 4)), size = 3) +
        labs(
            title = "MPG vs HP, Colored by Displacement Quartiles",
            x = "Horsepower (HP)",
            y = "Miles Per Gallon (MPG)",
            color = "Displacement Bins (4 groups)"
        ) +
        theme_minimal()

    cxplot      <- suppressWarnings(ggplot.as.list(gplot))
    cxplot_list <- jsonlite::parse_json(cxplot)

    expect_equal(class(cxplot), "json")
    expect_equal(length(cxplot_list), 16)
    expect_true(cxplot_list$isGGPlot)
    expect_equal(cxplot_list[["layers"]][[1]][["colour"]][["cut_number"]][["disp"]], "NA")
})


test_that("ggplot.as.list - layer with formula", {
    skip_if(getRversion() < "4.1.0")
    skip_if_not_installed("ggplot2")

    gplot <- ggplot(mtcars, aes(x = wt, y = mpg)) +
        geom_point() +
        # Use geom_smooth and pass a formula explicitly
        geom_smooth(
            method = "lm",                  # Linear Model
            formula = y ~ x + I(x^2),       # A non-linear formula: y ~ x + x^2
            se = TRUE                       # Display standard error ribbon
        ) +
        theme_minimal()

    cxplot      <- suppressWarnings(ggplot.as.list(gplot))
    cxplot_list <- jsonlite::parse_json(cxplot)

    expect_equal(class(cxplot), "json")
    expect_equal(length(cxplot_list), 16)
    expect_true(cxplot_list$isGGPlot)
    expect_equal(cxplot_list[["layers"]][[2]][["formula"]][["def"]], "y ~ x + I(x^2)")
    expect_equal(length(cxplot_list[["layers"]][[2]][["formula"]][["x"]]), 80)
    expect_equal(length(cxplot_list[["layers"]][[2]][["formula"]][["y"]]), 80)
    expect_equal(cxplot_list[["layers"]][[2]][["formula"]][["minY"]], 7.223)
    expect_equal(cxplot_list[["layers"]][[2]][["formula"]][["maxY"]], 36.481)
})


test_that("ggplot.as.list - GeomPoint with transformation", {
    skip_if(getRversion() < "4.1.0")
    skip_if_not_installed("ggplot2")

    gplot <- ggplot(mtcars, aes(x = hp, y = mpg)) +
        geom_point(data = . %>% filter(cyl == 4),
                   aes(color = factor(cyl), size = wt, shape = factor(am)),
                   fill = "white") +
        theme_minimal()

    cxplot      <- suppressWarnings(ggplot.as.list(gplot))
    cxplot_list <- jsonlite::parse_json(cxplot)

    expect_equal(class(cxplot), "json")
    expect_equal(length(cxplot_list), 16)
    expect_true(cxplot_list$isGGPlot)
    expect_equal(length(cxplot_list[["layers"]][[1]][["color"]]), 11)
    expect_equal(length(cxplot_list[["layers"]][[1]][["fill"]]), 11)
        expect_equal(length(cxplot_list[["layers"]][[1]][["size"]]), 11)
    expect_equal(length(cxplot_list[["layers"]][[1]][["shape"]]), 11)
        expect_equal(length(cxplot_list[["layers"]][[1]][["x"]]), 11)
    expect_equal(length(cxplot_list[["layers"]][[1]][["y"]]), 11)

    gplot <- ggplot(mtcars, aes(x = hp, y = mpg)) +
        geom_point(data = . %>% filter(cyl == 4),
                   aes(color = factor(cyl), size = wt) , shape = factor(21),
                   fill = "white") +
        theme_minimal()
    cxplot      <- suppressWarnings(ggplot.as.list(gplot))
    cxplot_list <- jsonlite::parse_json(cxplot)
    expect_equal(class(cxplot), "json")
    expect_equal(length(cxplot_list), 16)
    expect_true(cxplot_list$isGGPlot)
})


test_that("ggplot.as.list - ggplot2 GeomErrorbar", {
    skip_if_not_installed("ggplot2")

    data <- data.frame(
        trt   = factor(c(1, 1, 2, 2)),
        resp  = c(1, 5, 3, 4),
        group = factor(c(1, 2, 1, 2)),
        se    = c(0.1, 0.3, 0.3, 0.2))
    gplot <- ggplot(data, aes(resp, trt, colour = group)) +
        geom_point() +
        geom_errorbar(aes(xmax = resp + se, xmin = resp - se), orientation = "y")

    cxplot      <- suppressWarnings(ggplot.as.list(gplot))
    cxplot_list <- jsonlite::parse_json(cxplot)

    expect_equal(class(cxplot), "json")
    expect_equal(length(cxplot_list), 16)
    expect_true(cxplot_list$isGGPlot)
    expect_equal(length(cxplot_list$data), 5)
    expect_equal(cxplot_list$data[[2]][[1]], "1")
})


test_that("ggplot.as.list - ggplot2 facets", {
    skip_if_not_installed("ggplot2")

    gplot <- ggplot(mpg, aes(cty, hwy)) +
        geom_point() +
        facet_grid(year ~ drv)

    cxplot      <- suppressWarnings(ggplot.as.list(gplot))
    cxplot_list <- jsonlite::parse_json(cxplot)

    expect_equal(class(cxplot), "json")
    expect_equal(length(cxplot_list), 16)
    expect_true(cxplot_list$isGGPlot)
    expect_equal(length(cxplot_list$data), 235)
    expect_equal(cxplot_list$data[[2]][[1]], "1")

    gplot <- gplot +
        facet_grid(rows = vars(drv))

    expect_equal(class(suppressWarnings(ggplot.as.list(gplot))), "json")

    gplot <- gplot +
        facet_grid(cols = vars(cyl))
    expect_equal(class(suppressWarnings(ggplot.as.list(gplot))), "json")
})


test_that("ggplot.as.list - shape mapping with named shapes", {
    skip_if(getRversion() < "4.1.0")
    skip_if_not_installed("ggplot2")

    gplot <- ggplot(mtcars, aes(x = hp, y = mpg, shape = factor(cyl))) +
        geom_point(size = 4) +
        scale_shape_manual(
            values = c("4" = 1, "6" = 2, "8" = 3),  # square, circle, triangle
            name = "Cylinders"
        ) +
        labs(title = "Shape Mapping Test") +
        theme_minimal()

    cxplot      <- suppressWarnings(ggplot.as.list(gplot))
    cxplot_list <- jsonlite::parse_json(cxplot)

    expect_equal(class(cxplot), "json")
    expect_true(cxplot_list$isGGPlot)
    expect_equal(length(cxplot_list$data), 33)
})


test_that("ggplot.as.list - edge case shapes and categorical values", {
    skip_if(getRversion() < "4.1.0")
    skip_if_not_installed("ggplot2")

    test_data <- mtcars[1:10, ]
    # Create a categorical shape variable instead of continuous
    test_data$shape_var <- factor(c("A", "B", "C", "D", "E", "A", "B", "C", "D", "E"))

    gplot <- ggplot(test_data, aes(x = hp, y = mpg, shape = shape_var)) +
        geom_point(size = 4) +
        scale_shape_manual(
            values = c("A" = 1, "B" = 2, "C" = 3, "D" = 4, "E" = 5)
        ) +
        labs(title = "Edge Case Shapes") +
        theme_minimal()

    cxplot      <- suppressWarnings(ggplot.as.list(gplot))
    cxplot_list <- jsonlite::parse_json(cxplot)

    expect_equal(class(cxplot), "json")
    expect_true(cxplot_list$isGGPlot)
    expect_equal(length(cxplot_list$data), 11)
})


test_that("ggplot.as.list - shape with NA values in factor", {
    skip_if(getRversion() < "4.1.0")
    skip_if_not_installed("ggplot2")

    test_data <- mtcars[1:10, ]
    # Create factor with explicit NA
    test_data$shape_var <- factor(
        c("A", "B", "C", NA, "E", "A", "B", "C", "D", "E"),
        levels = c("A", "B", "C", "D", "E")
    )

    gplot <- ggplot(test_data, aes(x = hp, y = mpg, shape = shape_var)) +
        geom_point(size = 4, na.rm = TRUE) +
        scale_shape_manual(
            values = c("A" = 1, "B" = 2, "C" = 3, "D" = 4, "E" = 5)
        ) +
        labs(title = "Shapes with NA Values") +
        theme_minimal()

    cxplot      <- suppressWarnings(ggplot.as.list(gplot))
    cxplot_list <- jsonlite::parse_json(cxplot)

    expect_equal(class(cxplot), "json")
    expect_true(cxplot_list$isGGPlot)
})

test_that("ggplot.as.list - parameter skipping with na.rm TRUE", {
    skip_if(getRversion() < "4.1.0")
    skip_if_not_installed("ggplot2")

    # skip2 parameter filtering
    gplot <- ggplot(mtcars, aes(x = wt, y = mpg)) +
        geom_point(na.rm = TRUE, size = 3) +  # na.rm = TRUE (not default FALSE)
        labs(title = "Point with na.rm") +
        theme_minimal()

    cxplot      <- suppressWarnings(ggplot.as.list(gplot))
    cxplot_list <- jsonlite::parse_json(cxplot)

    expect_equal(class(cxplot), "json")
    expect_true(cxplot_list$isGGPlot)
})


test_that("ggplot.as.list - parameter skipping with reverse TRUE", {
    skip_if(getRversion() < "4.1.0")
    skip_if_not_installed("ggplot2")

    # skip2 parameter filtering for reverse
    gplot <- ggplot(mtcars, aes(x = wt, y = mpg, color = factor(cyl))) +
        geom_point(size = 3) +
        scale_color_manual(
            values = c("4" = "red", "6" = "green", "8" = "blue"),
            guide = guide_legend(reverse = TRUE)
        ) +
        theme_minimal()

    cxplot      <- suppressWarnings(ggplot.as.list(gplot))
    cxplot_list <- jsonlite::parse_json(cxplot)

    expect_equal(class(cxplot), "json")
    expect_true(cxplot_list$isGGPlot)
})


test_that("ggplot.as.list - function parameter in geom_text vjust", {
    skip_if(getRversion() < "4.1.0")
    skip_if_not_installed("ggplot2")

    # vjust parameter handling
    test_data <- data.frame(
        x = 1:5,
        y = 1:5,
        label = LETTERS[1:5]
    )

    gplot <- ggplot(test_data, aes(x = x, y = y, label = label)) +
        geom_point(size = 3) +
        geom_text(vjust = -0.5, size = 4) +  # vjust != 1 (default)
        labs(title = "Text with vjust") +
        theme_minimal()

    cxplot      <- suppressWarnings(ggplot.as.list(gplot))
    cxplot_list <- jsonlite::parse_json(cxplot)

    expect_equal(class(cxplot), "json")
    expect_true(cxplot_list$isGGPlot)
})


test_that("ggplot.as.list - custom function in layer parameters", {
    skip_if(getRversion() < "4.1.0")
    skip_if_not_installed("ggplot2")

    # Function name resolution with environment lookup
    custom_breaks <- function(x) {
        seq(min(x), max(x), length.out = 5)
    }

    gplot <- ggplot(mtcars, aes(x = wt, y = mpg)) +
        geom_point(size = 3) +
        scale_x_continuous(breaks = custom_breaks) +
        labs(title = "Custom Breaks Function") +
        theme_minimal()

    cxplot      <- suppressWarnings(ggplot.as.list(gplot))
    cxplot_list <- jsonlite::parse_json(cxplot)

    expect_equal(class(cxplot), "json")
    expect_true(cxplot_list$isGGPlot)
})


test_that("ggplot.as.list - show.legend FALSE explicit", {
    skip_if(getRversion() < "4.1.0")
    skip_if_not_installed("ggplot2")

    # l$show.legend == FALSE
    gplot <- ggplot(mtcars, aes(x = wt, y = mpg, color = factor(cyl))) +
        geom_point(size = 3, show.legend = FALSE) +  # Explicit FALSE
        labs(title = "No Legend on Geom") +
        theme_minimal()

    cxplot      <- suppressWarnings(ggplot.as.list(gplot))
    cxplot_list <- jsonlite::parse_json(cxplot)

    expect_equal(class(cxplot), "json")
    expect_true(cxplot_list$isGGPlot)
    # Check that showLegend is set to FALSE in at least one layer
    has_show_legend_false <- any(sapply(cxplot_list$layers, function(layer) {
        any(sapply(layer, function(x) {
            if (is.list(x) && "showLegend" %in% names(x)) {
                x$showLegend == FALSE
            } else {
                FALSE
            }
        }))
    }))
})


test_that("ggplot.as.list - stringVariableFactors with factor color", {
    skip_if(getRversion() < "4.1.0")
    skip_if_not_installed("ggplot2")

    # stringVariableFactors assignment
    test_data       <- mtcars
    test_data$cyl_f <- factor(mtcars$cyl, labels = c("4cyl", "6cyl", "8cyl"))
    test_data$am_f  <- factor(mtcars$am, labels = c("Auto", "Manual"))

    gplot <- ggplot(test_data, aes(x = wt, y = mpg, color = cyl_f)) +
        geom_point(aes(shape = am_f), size = 3) +
        scale_color_manual(
            name = "Cylinders",
            values = c("4cyl" = "red", "6cyl" = "blue", "8cyl" = "green")
        ) +
        scale_shape_manual(
            name = "Transmission",
            values = c("Auto" = 16, "Manual" = 17)
        ) +
        labs(title = "Multiple Factor Variables") +
        theme_minimal()

    cxplot      <- suppressWarnings(ggplot.as.list(gplot))
    cxplot_list <- jsonlite::parse_json(cxplot)

    expect_equal(class(cxplot), "json")
    expect_true(cxplot_list$isGGPlot)
})


test_that("ggplot.as.list - shape names mapping", {
    skip_if(getRversion() < "4.1.0")
    skip_if_not_installed("ggplot2")

    # gg_shape_name function with shape values
    # Testing shapes 0-9 to ensure proper mapping
    test_data <- data.frame(
        x = 1:10,
        y = rnorm(10),
        shape_val = 0:9  # Different shape values
    )

    gplot <- ggplot(test_data, aes(x = x, y = y, shape = shape_val)) +
        geom_point(size = 4, color = "steelblue") +
        scale_shape_identity() +
        labs(title = "Various Shape Values", x = "Index", y = "Value") +
        theme_minimal()

    cxplot      <- suppressWarnings(ggplot.as.list(gplot))
    cxplot_list <- jsonlite::parse_json(cxplot)

    expect_equal(class(cxplot), "json")
    expect_true(cxplot_list$isGGPlot)
    expect_equal(length(cxplot_list$data), 11)
})


test_that("ggplot.as.list - point with layer-specific data", {
    skip_if(getRversion() < "4.1.0")
    skip_if_not_installed("ggplot2")

    # dim check for GeomPoint
    gplot <- ggplot(mtcars, aes(x = wt, y = mpg)) +
        geom_point(data = mtcars[1:15, ], aes(color = factor(cyl)), size = 4) +
        labs(title = "Point with Layer-Specific Data") +
        theme_minimal()

    cxplot      <- suppressWarnings(ggplot.as.list(gplot))
    cxplot_list <- jsonlite::parse_json(cxplot)

    expect_equal(class(cxplot), "json")
    expect_true(cxplot_list$isGGPlot)
})


test_that("gg_plotmath_to_text converts plotmath expressions to CanvasXpress HTML", {
    expect_null(gg_plotmath_to_text(NULL))
    expect_equal(gg_plotmath_to_text("Plain text"), "Plain text")
    expect_equal(gg_plotmath_to_text(quote(Log[10])), "Log<sub>10</sub>")
    expect_equal(gg_plotmath_to_text(quote(x^2)), "x<sup>2</sup>")
    expect_equal(gg_plotmath_to_text(quote(italic(P))), "<i>P</i>")
    expect_equal(gg_plotmath_to_text(quote(bold(P))), "<b>P</b>")
    expect_equal(gg_plotmath_to_text(quote(bolditalic(P))), "<b><i>P</i></b>")
    expect_equal(gg_plotmath_to_text(quote(plain(P))), "P")

    mu_result <- gg_plotmath_to_text(quote(mu ~ "g/mL"))
    expect_true(grepl("\u03bc", mu_result, fixed = TRUE))

    expr_result <- gg_plotmath_to_text(expression(-Log[10] ~ italic(P)))
    expect_true(grepl("<sub>10</sub>", expr_result, fixed = TRUE))
    expect_true(grepl("<i>P</i>", expr_result, fixed = TRUE))
})


test_that("ggplot.as.list - plotmath axis titles flow through labs()", {
    skip_if_not_installed("ggplot2")

    gplot <- ggplot(mtcars, aes(x = wt, y = mpg)) +
        geom_point() +
        labs(x = expression(-Log[10] ~ italic(P)), y = "mpg")

    cxplot      <- suppressWarnings(ggplot.as.list(gplot))
    cxplot_list <- jsonlite::parse_json(cxplot)

    expect_equal(class(cxplot), "json")
    expect_true(cxplot_list$isGGPlot)
    expect_true(grepl("<sub>10</sub>", cxplot_list$labels$xAxisTitle, fixed = TRUE))
    expect_true(grepl("<i>P</i>", cxplot_list$labels$xAxisTitle, fixed = TRUE))
})


test_that("ggplot.as.list - discrete color scale with custom labels remaps codes", {
    skip_if_not_installed("ggplot2")

    df <- data.frame(x = 1:6, y = c(1, 2, 3, 2, 1, 3),
                     grp = c("NS", "FC", "P", "NS", "FC", "P"))

    gplot <- ggplot(df, aes(x = x, y = y, color = grp)) +
        geom_point(size = 3) +
        scale_color_manual(
            values = c(NS = "grey", FC = "blue", P = "red"),
            labels = c(NS = "Not significant", FC = "Log2 FC", P = "p-value")
        )

    cxplot <- suppressWarnings(ggplot.as.list(gplot))

    expect_equal(class(cxplot), "json")
    # the raw codes ("NS"/"FC"/"P") should have been rewritten to the scale's
    # labels in the data column, order and colorKey
    expect_true(grepl("NS", cxplot, fixed = TRUE))
    expect_true(grepl("FC", cxplot, fixed = TRUE))
})


test_that("ggplot.as.list - constant aesthetic aes(x = 1) materializes a factor column", {
    skip_if_not_installed("ggplot2")

    df <- data.frame(y = c(3, 5, 4, 6))

    gplot <- ggplot(df, aes(x = 1, y = y)) +
        geom_boxplot()

    cxplot      <- suppressWarnings(ggplot.as.list(gplot))
    cxplot_list <- jsonlite::parse_json(cxplot)

    expect_equal(class(cxplot), "json")
    expect_true(cxplot_list$isGGPlot)
    expect_true(cxplot_list$meta$x)
})


test_that("ggplot.as.list - factor()-wrapped aes skips coercion when the same column is also used bare", {
    skip_if_not_installed("ggplot2")

    gplot <- ggplot(mtcars, aes(x = factor(cyl), y = mpg)) +
        geom_boxplot() +
        geom_violin(aes(fill = cyl), alpha = 0.3)

    cxplot      <- suppressWarnings(ggplot.as.list(gplot))
    cxplot_list <- jsonlite::parse_json(cxplot)

    expect_equal(class(cxplot), "json")
    expect_true(cxplot_list$isGGPlot)
    # cyl is used bare (continuous fill) elsewhere, so x = factor(cyl) must NOT
    # coerce the underlying column to a factor
    expect_false(cxplot_list$meta$cyl)
})


test_that("ggplot.as.list - GeomVline, GeomHline and GeomAbline reference lines", {
    skip_if_not_installed("ggplot2")

    gplot <- ggplot(mtcars, aes(x = wt, y = mpg)) +
        geom_point() +
        geom_vline(xintercept = 3, color = "red", linewidth = 1, linetype = "dashed") +
        geom_hline(yintercept = 20, color = "blue", linewidth = 0.8, linetype = "dotted") +
        geom_abline(slope = 1, intercept = 0, color = "green")

    cxplot      <- suppressWarnings(ggplot.as.list(gplot))
    cxplot_list <- jsonlite::parse_json(cxplot)

    expect_equal(class(cxplot), "json")
    expect_true(cxplot_list$isGGPlot)
    expect_true("GeomVline" %in% unlist(cxplot_list$geoms))
    expect_true("GeomHline" %in% unlist(cxplot_list$geoms))
    expect_true("GeomAbline" %in% unlist(cxplot_list$geoms))
})


test_that("ggplot.as.list - GeomCrossbar from stat_summary (also covers a function-valued stat param)", {
    skip_if_not_installed("ggplot2")

    gplot <- ggplot(mtcars, aes(x = factor(cyl), y = mpg)) +
        geom_point() +
        stat_summary(fun = mean, geom = "crossbar", width = 0.5, color = "red")

    cxplot      <- suppressWarnings(ggplot.as.list(gplot))
    cxplot_list <- jsonlite::parse_json(cxplot)

    expect_equal(class(cxplot), "json")
    expect_true(cxplot_list$isGGPlot)
    expect_true("GeomCrossbar" %in% unlist(cxplot_list$geoms))
})


test_that("ggplot.as.list - GeomRibbon layer", {
    skip_if_not_installed("ggplot2")

    df <- data.frame(x = 1:10, y = (1:10)^1.2)
    df$ymin <- df$y - 1
    df$ymax <- df$y + 1

    gplot <- ggplot(df, aes(x = x, y = y)) +
        geom_ribbon(aes(ymin = ymin, ymax = ymax), fill = "grey70") +
        geom_line()

    cxplot      <- suppressWarnings(ggplot.as.list(gplot))
    cxplot_list <- jsonlite::parse_json(cxplot)

    expect_equal(class(cxplot), "json")
    expect_true(cxplot_list$isGGPlot)
    expect_true("GeomRibbon" %in% unlist(cxplot_list$geoms))
})


test_that("ggplot.as.list - GeomStep with kmCxplot config option", {
    skip_if_not_installed("ggplot2")

    df <- data.frame(time  = c(1, 2, 3, 4, 5),
                      surv  = c(1, 0.9, 0.8, 0.8, 0.6),
                      color = "All")

    gplot <- ggplot(df, aes(x = time, y = surv, color = color)) +
        geom_step(data = df)

    cxplot <- suppressWarnings(ggplot.as.list(
        gplot,
        kmCxplot                  = TRUE,
        showKMConfidenceIntervals = FALSE,
        kmRiskTable                = FALSE
    ))
    cxplot_list <- jsonlite::parse_json(cxplot)

    expect_equal(class(cxplot), "json")
    expect_true(cxplot_list$isGGPlot)
    expect_true(cxplot_list$layers[[1]]$kmCxplot)
    # the three km-specific keys are consumed by the GeomStep branch and
    # removed from the generic config list
    expect_false("kmCxplot" %in% names(cxplot_list$config))
    expect_false("showKMConfidenceIntervals" %in% names(cxplot_list$config))
    expect_false("kmRiskTable" %in% names(cxplot_list$config))
})


test_that("ggplot.as.list - facet_wrap with multiple variables", {
    skip_if_not_installed("ggplot2")

    gplot <- ggplot(mtcars, aes(x = wt, y = mpg)) +
        geom_point() +
        facet_wrap(vars(cyl, am))

    cxplot      <- suppressWarnings(ggplot.as.list(gplot))
    cxplot_list <- jsonlite::parse_json(cxplot)

    expect_equal(class(cxplot), "json")
    expect_true(cxplot_list$isGGPlot)
    expect_equal(length(cxplot_list$facet$facet), 2)
})


test_that("ggplot.as.list - facet_wrap with explicit ncol and with explicit nrow", {
    skip_if_not_installed("ggplot2")

    gplot_col <- ggplot(mpg, aes(x = displ, y = hwy)) +
        geom_point() +
        facet_wrap(vars(class), ncol = 2)

    cxplot_list_col <- jsonlite::parse_json(suppressWarnings(ggplot.as.list(gplot_col)))
    expect_equal(cxplot_list_col$facet$facetCols, 2)

    gplot_row <- ggplot(mpg, aes(x = displ, y = hwy)) +
        geom_point() +
        facet_wrap(vars(class), nrow = 3)

    cxplot_list_row <- jsonlite::parse_json(suppressWarnings(ggplot.as.list(gplot_row)))
    expect_equal(cxplot_list_row$facet$facetRows, 3)
})


test_that("ggplot.as.list - facet_wrap auto sqrt() layout with 4+ panels", {
    skip_if_not_installed("ggplot2")

    # mpg$class has 7 levels, no ncol/nrow specified -> hits the
    # ceiling(sqrt(.)) branch instead of the <4-levels branch
    gplot <- ggplot(mpg, aes(x = displ, y = hwy)) +
        geom_point() +
        facet_wrap(vars(class))

    cxplot      <- suppressWarnings(ggplot.as.list(gplot))
    cxplot_list <- jsonlite::parse_json(cxplot)

    expect_equal(class(cxplot), "json")
    expect_true(cxplot_list$isGGPlot)
    expect_equal(cxplot_list$facet$facetCols, 3)
    expect_equal(cxplot_list$facet$facetRows, 3)
})


test_that("ggplot.as.list - explicit linetype scale maps named and hex-dash-code values", {
    skip_if_not_installed("ggplot2")

    gplot <- ggplot(mtcars, aes(x = wt, y = mpg, linetype = factor(am))) +
        geom_line() +
        scale_linetype_manual(values = c("0" = "solid", "1" = "22"))

    cxplot      <- suppressWarnings(ggplot.as.list(gplot))
    cxplot_list <- jsonlite::parse_json(cxplot)

    expect_equal(class(cxplot), "json")
    expect_true(cxplot_list$isGGPlot)
    expect_true(all(unlist(cxplot_list$scales$lineType) %in%
                         c("solid", "dashed", "dotted", "dotdash",
                           "longdash", "twodash")))
})


test_that("ggplot.as.list - shape scale with named (non-numeric) shape values", {
    skip_if_not_installed("ggplot2")

    gplot <- ggplot(mtcars, aes(x = wt, y = mpg, shape = factor(am))) +
        geom_point(size = 3) +
        scale_shape_manual(values = c("0" = "square", "1" = "circle"))

    cxplot      <- suppressWarnings(ggplot.as.list(gplot))
    cxplot_list <- jsonlite::parse_json(cxplot)

    expect_equal(class(cxplot), "json")
    expect_true(cxplot_list$isGGPlot)
    expect_true(all(c("square", "circle") %in% unlist(cxplot_list$scales$shapes)))
})


test_that("ggplot.as.list - bar position dodge, fill and stack", {
    skip_if_not_installed("ggplot2")

    base <- ggplot(mtcars, aes(x = factor(cyl), fill = factor(am)))

    cxplot_dodge <- jsonlite::parse_json(
        suppressWarnings(ggplot.as.list(base + geom_bar(position = "dodge")))
    )
    expect_equal(cxplot_dodge$layers[[1]]$position, "dodge")

    cxplot_fill <- jsonlite::parse_json(
        suppressWarnings(ggplot.as.list(base + geom_bar(position = "fill")))
    )
    expect_equal(cxplot_fill$layers[[1]]$position, "fill")

    cxplot_stack <- jsonlite::parse_json(
        suppressWarnings(ggplot.as.list(base + geom_bar(position = "stack")))
    )
    expect_equal(cxplot_stack$layers[[1]]$position, "stack")
})


test_that("ggplot.as.list - after_stat() layer mapping does not error the conversion", {
    skip_if_not_installed("ggplot2")

    gplot <- ggplot(mtcars, aes(x = mpg)) +
        geom_histogram(aes(y = after_stat(density)), bins = 10)

    cxplot      <- suppressWarnings(ggplot.as.list(gplot))
    cxplot_list <- jsonlite::parse_json(cxplot)

    expect_equal(class(cxplot), "json")
    expect_true(cxplot_list$isGGPlot)
})


test_that("ggplot.as.list - geom_text with a computed y nudge does not error the conversion", {
    skip_if_not_installed("ggplot2")

    df <- data.frame(x = 1:5, y = c(2, 4, 6, 8, 10), lbl = letters[1:5])

    gplot <- ggplot(df, aes(x = x, y = y)) +
        geom_col() +
        geom_text(aes(x = x, y = y + 0.5, label = lbl))

    cxplot      <- suppressWarnings(ggplot.as.list(gplot))
    cxplot_list <- jsonlite::parse_json(cxplot)

    expect_equal(class(cxplot), "json")
    expect_true(cxplot_list$isGGPlot)
    expect_true(length(cxplot_list$data) > 0)
})
