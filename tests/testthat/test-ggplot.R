context("canvasXpress ggplot conversions")


# TESTING NOTES:
# * ggplots will show up in RStudio's Plots pane, CX will show in the Viewer pane (this is by design)
# --- It is suggested that you layout your RStudio panels so that you can see both the plots and viewer
# --- when testing so that the visual comparison can be made between ggplots and cx plots. If you turn
# --- off all the other panels temporarily you can view both on the screen at once without flipping tabs.
#
# * since this functionality is experimental, use "warning()" instead of "fail()" and describe the issues

# KNOWN DIFFERENCES (not reportable, expected)
# --- NA values will sort differently in 1D ggplot charts (end/right)


test_that("simple scatterplot", {
    gg <- ggplot(data = mpg,
                 aes(x      = displ,
                     y      = hwy)) +
        geom_point(aes(color = factor(cyl)))

    compare_ggplot_to_cx_conversion(gg)
})


# from https://r4stats.com/examples/graphics-ggplot2
ggplot_exData1 <- read.csv('tests/testthat/ggplot_exData1.csv')

test_that("r4stats ex1", {
    gg <- ggplot(ggplot_exData1,
                 aes(x = factor(""), fill = workshop) ) +
        geom_bar()

    compare_ggplot_to_cx_conversion(gg)
    warning('ggplot has a single stacked bar, cx has many bars')
})

test_that("r4stats ex2", {
    gg <- ggplot(ggplot_exData1,
                 aes(x = factor(""), fill = workshop) ) +
        geom_bar() +
        coord_polar(theta = "y") +
        scale_x_discrete("")

    compare_ggplot_to_cx_conversion(gg)
    warning('ggplot has a pie chart, cx has many bars')
})

test_that("r4stats ex3-1", {
    gg <- ggplot(ggplot_exData1) +
        geom_bar( aes(workshop) )

    compare_ggplot_to_cx_conversion(gg)
})

test_that("r4stats ex3-2", {
    gg <- ggplot(ggplot_exData1,
                 aes(workshop, fill = workshop ) ) +
        geom_bar()

    compare_ggplot_to_cx_conversion(gg)
    warning('ggplot is colored by workshop, cx is not')
})

test_that("r4stats ex3-3", {

    gg <- ggplot(ggplot_exData1,
                 aes(gender, fill = workshop) ) +
        geom_bar(position = "stack")

    compare_ggplot_to_cx_conversion(gg)
    warning('ggplot is stacked and colored by workshop, cx is only basic bars')
})

test_that("r4stats ex3-4", {
    gg <- ggplot(ggplot_exData1,
                 aes(gender, fill=workshop) ) +
        geom_bar(position="fill")

    compare_ggplot_to_cx_conversion(gg)
    warning('ggplot is stacked and colored by workshop and is 100% based, cx is only basic bars')
})

test_that("r4stats ex3-5", {
    gg <- ggplot(ggplot_exData1,
                 aes(gender, fill=workshop ) ) +
        geom_bar(position="dodge")

    compare_ggplot_to_cx_conversion(gg)
    warning('ggplot is grouped bars colored by workshop, cx is only basic bars')
})

test_that("r4stats ex3-6", {
    gg <- ggplot(ggplot_exData1,
                 aes(gender, fill=workshop ) ) +
        geom_bar(position="dodge")  +
        scale_fill_grey(start = 0, end = 1)

    compare_ggplot_to_cx_conversion(gg)
    warning('ggplot is grouped bars colored by workshop, cx is only basic bars')
})

test_that("r4stats ex3-7", {
    gg <- ggplot(ggplot_exData1,
                 aes(workshop) ) +
        geom_bar() + facet_grid(gender ~ .)

    compare_ggplot_to_cx_conversion(gg)
    warning('ggplot is faceted by gender, cx is unfaceted')
})

