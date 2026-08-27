context("ggplot as list - ggh4x")

# 1. requireNamespace checks for installation WITHOUT loading ggh4x into memory
if (!requireNamespace("ggh4x", quietly = TRUE)) {
  testthat::skip("ggh4x is not installed")
}

# 2. Completely bypass this file on older R environments on CircleCI
# where ggh4x causes irreversible namespace pollution
if (isTRUE(as.logical(Sys.getenv("CI"))) && getRversion() <= "4.4.3") {
  testthat::skip("Skipping ggh4x tests on legacy CircleCI environments to protect namespace")
}


test_that("ggplot.as.list - ggh4x per-strip fill colors (gg_strip_colors)", {
    skip_if_not_installed("ggplot2")

    strip_cols <- ggh4x::strip_themed(
        background_x = ggh4x::elem_list_rect(fill = c("skyblue", "salmon", "lightgreen"))
    )

    gplot <- ggplot(mtcars, aes(x = wt, y = mpg)) +
        geom_point() +
        ggh4x::facet_wrap2(vars(factor(cyl)), strip = strip_cols)

    cxplot      <- suppressWarnings(ggplot.as.list(gplot))
    cxplot_list <- jsonlite::parse_json(cxplot)

    expect_equal(class(cxplot), "json")
    expect_true(cxplot_list$isGGPlot)
    expect_false(is.null(cxplot_list$theme$strip.background.fill))
})
