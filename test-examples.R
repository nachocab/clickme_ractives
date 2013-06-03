test_that("the HTML example file for the vega ractive is generated", {
    ractive <- "vega"

    # we do this to ensure that the HTML file doesn't exist before we create it
    spec <- "area"
    opts <- get_opts(ractive, params = list(spec = spec))
    unlink(opts$path$html_file)
    data <- read.csv(file.path(opts$path$data, "area_bar_scatter.csv"))

    opts <- clickme_vega(data, spec, browse = FALSE)
    expect_true(file.exists(opts$path$html_file))

    spec <- "bar"
    opts <- get_opts(ractive, params = list(spec = spec))
    unlink(opts$path$html_file)
    data <- read.csv(file.path(opts$path$data, "area_bar_scatter.csv"))

    opts <- clickme_vega(data, spec, browse = FALSE)
    expect_true(file.exists(opts$path$html_file))

    spec <- "scatter"
    opts <- get_opts(ractive, params = list(spec = spec))
    unlink(opts$path$html_file)
    data <- read.csv(file.path(opts$path$data, "area_bar_scatter.csv"))

    opts <- clickme_vega(data, spec, browse = FALSE)
    expect_true(file.exists(opts$path$html_file))

    spec <- "stocks"
    opts <- get_opts(ractive, params = list(spec = spec))
    unlink(opts$path$html_file)
    stocks <- read.csv(file.path(opts$path$data, "stocks.csv"))

    opts <- clickme_vega(stocks, spec, browse = FALSE)
    expect_true(file.exists(opts$path$html_file))

    spec <- "lifelines"
    opts <- get_opts(ractive, params = list(spec = spec))
    unlink(opts$path$html_file)
    people <- read.csv(file.path(opts$path$data, "lifelines_people.csv"))
    events <- read.csv(file.path(opts$path$data, "lifelines_events.csv"))

    opts <- clickme_vega(people, spec, params = list(event_data = events, height = 200), browse = FALSE)
    expect_true(file.exists(opts$path$html_file))
})

test_that("clickme_vega", {
    ractive <- "vega"

    # we do this to ensure that the HTML file doesn't exist before we create it
    spec <- "area"
    opts <- get_opts(ractive, params = list(spec = spec))
    unlink(opts$path$html_file)
    data <- read.csv(file.path(opts$path$data, "area_bar_scatter.csv"))

    opts <- clickme_vega(data, "area", browse = FALSE)

    expect_equal(opts$name$html_file, "data_area-vega.html")
    expect_true(file.exists(opts$path$html_file))

    opts <- clickme_vega(data, "area", data_prefix = "my_data", params = list(width = 401), browse = FALSE)

    expect_equal(opts$params$spec, "area")
    expect_equal(opts$params$width, 401)
    unlink(opts$path$html_file)

    opts <- clickme_vega(data, "area", data_prefix = "my_data", browse = FALSE)

    expect_equal(opts$data_prefix, "my_data")
    expect_equal(opts$name$html_file, "my_data-vega.html")
    unlink(opts$path$html_file)
})


test_that("the HTML example file for the one_zoom ractive is generated", {
    ractive <- "one_zoom"

    # we do this to ensure that the HTML file doesn't exist before we create it
    opts <- get_opts(ractive)
    unlink(opts$path$html_file)
    data <- file.path(opts$path$data, "mammals.tree")

    clickme(data, ractive, browse = FALSE)

    expect_true(file.exists(opts$path$html_file))
})

test_that("the HTML example file for the line_with_focus ractive is generated", {
    ractive <- "line_with_focus"

    # we do this to ensure that the HTML file doesn't exist before we create it
    opts <- get_opts(ractive)
    unlink(opts$path$html_file)
    data <- read.csv(file.path(opts$path$data, "original_data.csv"))
    clickme(data, ractive, browse = FALSE)

    expect_true(file.exists(opts$path$html_file))
})

test_that("the HTML example file for the longitudinal_heatmap ractive is generated", {
    ractive <- "longitudinal_heatmap"

    # we do this to ensure that the HTML file doesn't exist before we create it
    opts <- get_opts(ractive)
    unlink(opts$path$html_file)
    data <- read.csv(file.path(opts$path$data, "original_data.csv"))

    clickme(data, ractive, browse = FALSE)
    expect_true(file.exists(opts$path$html_file))
})

