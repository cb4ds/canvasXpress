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
    expect_equal(length(cxplot_list), 15)
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
    expect_equal(length(cxplot_list), 15)
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
    expect_equal(length(cxplot_list), 15)
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
    expect_equal(length(cxplot_list), 15)
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
    expect_equal(length(cxplot_list), 15)
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
    expect_equal(length(cxplot_list), 15)
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
    expect_equal(length(cxplot_list), 15)
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
    expect_equal(length(cxplot_list), 15)
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
    expect_equal(length(cxplot_list), 15)
    expect_true(cxplot_list$isGGPlot)
    expect_equal(length(cxplot_list$data), 33)
    expect_equal(cxplot_list$data[[2]][[1]], "Mazda RX4")

    # test another pattern
    avg_hp_data <- mtcars %>%
        group_by(cyl) %>%
        summarize(avg_hp = mean(hp), .groups = "drop")

    gplot <- ggplot(avg_hp_data, aes(x = cyl, y = avg_hp, pattern_fill = factor(cyl))) +
        geom_col_pattern(
            pattern = "crosshatch")#,
            # fill = "white",
            # color = "black",
            # pattern_spacing = 0.02,
            # key_glyph = draw_key_polygon_pattern) #+
        # labs(
        #     title = "Average Horsepower by Cylinder Count (Column Plot)",
        #     x = "Number of Cylinders",
        #     y = "Average Horsepower (hp)",
        #     pattern_fill = "Cylinders") +
        # theme_bw(18)

    cxplot      <- suppressWarnings(ggplot.as.list(gplot))
    cxplot_list <- jsonlite::parse_json(cxplot)

    expect_equal(class(cxplot), "json")
    expect_equal(length(cxplot_list), 15)
    expect_true(cxplot_list$isGGPlot)
    expect_equal(length(cxplot_list$data), 4)
    expect_equal(cxplot_list$data[[2]][[1]], "1")

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
    expect_equal(length(cxplot_list), 15)
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
    expect_equal(length(cxplot_list), 15)
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
