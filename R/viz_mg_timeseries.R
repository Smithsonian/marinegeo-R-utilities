#' Plot annual time-series data with grouped lines
#'
#' @description
#' Creates a `ggplot2` line-and-point chart for visualising yearly MarineGEO
#' monitoring data. Each level of `y_grouping_var` is drawn as a separate
#' coloured line. Optionally, the chart can be faceted by a categorical
#' variable and y-axis limits can be fixed.
#'
#' To render the result as an interactive plotly widget, pass the returned
#' object to [viz_mg_ggplotly()].
#'
#' @param df A data frame containing the columns named by `x_var`, `y_var`,
#'   `y_grouping_var`, and (if supplied) `facet_var`.
#' @param y_var Character scalar. Name of the column in `df` to map to the
#'   y-axis (the response variable).
#' @param x_var Character scalar. Name of the column in `df` to map to the
#'   x-axis. Defaults to `"year"`. The column is treated as a discrete axis
#'   via [ggplot2::scale_x_discrete()].
#' @param y_label Character scalar. Label to display on the y-axis.
#' @param y_limits Numeric vector of length 2, `c(min, max)`, passed to
#'   [ggplot2::ylim()]. If `NULL` (default), axis limits are determined
#'   automatically. **Note:** data points outside the specified range are
#'   silently dropped by ggplot2.
#' @param y_grouping_var Character scalar. Name of the column in `df` whose
#'   unique values define the line groups (colour and line grouping).
#' @param facet_var Character scalar or `NULL` (default). Name of the column
#'   in `df` to use for [ggplot2::facet_wrap()] panels. When `NULL`, no
#'   faceting is applied.
#' @param facet_num_cols Positive integer or `NULL` (default). Number of
#'   columns passed to [ggplot2::facet_wrap()] as `ncol`. Ignored when
#'   `facet_var` is `NULL`.
#' @param plot_theme A complete ggplot2 theme object (e.g.,
#'   `ggplot2::theme_bw()`) or `NULL` (default). When non-`NULL`, the theme
#'   is added to the plot with `+`.
#'
#' @return A `ggplot` object. The caller is responsible for printing or saving
#'   the plot (e.g., with [ggplot2::ggsave()]) or converting it to an
#'   interactive widget with [viz_mg_ggplotly()].
#'
#' @details
#' The x-axis is rendered as a discrete scale so that years with no
#' observations are not interpolated. If `x_var` holds a numeric year column,
#' convert it to a factor or character before passing to this function.
#'
#' `y_limits` is applied via [ggplot2::ylim()], which drops data points
#' outside the specified range. Use [ggplot2::coord_cartesian()] directly on
#' the returned object if you need to zoom without dropping data.
#'
#' @export
#'
#' @examples
#' df <- data.frame(
#'   year    = factor(rep(2020:2022, each = 2)),
#'   cover   = c(10, 20, 15, 25, 12, 18),
#'   species = rep(c("Halodule wrightii", "Thalassia testudinum"), 3),
#'   site    = rep(c("Site A", "Site B", "Site A"), each = 2)
#' )
#'
#' # Minimal call
#' viz_mg_timeseries_annual(
#'   df             = df,
#'   y_var          = "cover",
#'   y_label        = "Percent cover",
#'   y_grouping_var = "species"
#' )
#'
#' # Faceted by site with fixed y limits and a theme
#' viz_mg_timeseries_annual(
#'   df             = df,
#'   y_var          = "cover",
#'   y_label        = "Percent cover",
#'   y_grouping_var = "species",
#'   facet_var      = "site",
#'   facet_num_cols = 2L,
#'   y_limits       = c(0, 100),
#'   plot_theme     = ggplot2::theme_bw()
#' )
viz_mg_timeseries_annual <- function(
  df,
  y_var,
  x_var = "year",
  y_label,
  y_limits = NULL,
  y_grouping_var,
  facet_var = NULL,
  facet_num_cols = NULL,
  plot_theme = NULL
) {
  # --- Input validation -------------------------------------------------------
  if (!is.data.frame(df)) {
    stop("`df` must be a data frame.")
  }

  .assert_chr_scalar <- function(x, name) {
    if (!is.character(x) || length(x) != 1L || is.na(x)) {
      stop(paste0("`", name, "` must be a single non-NA character string."))
    }
  }
  .assert_chr_scalar(y_var, "y_var")
  .assert_chr_scalar(x_var, "x_var")
  .assert_chr_scalar(y_grouping_var, "y_grouping_var")
  .assert_chr_scalar(y_label, "y_label")

  .assert_col_exists <- function(col, name) {
    if (!col %in% colnames(df)) {
      stop(paste0("`", name, "` column '", col, "' not found in `df`."))
    }
  }
  .assert_col_exists(y_var, "y_var")
  .assert_col_exists(x_var, "x_var")
  .assert_col_exists(y_grouping_var, "y_grouping_var")

  if (!is.null(facet_var)) {
    .assert_chr_scalar(facet_var, "facet_var")
    .assert_col_exists(facet_var, "facet_var")
  }

  if (!is.null(y_limits)) {
    if (!is.numeric(y_limits) || length(y_limits) != 2L || anyNA(y_limits)) {
      stop("`y_limits` must be a numeric vector of length 2 with no NA values.")
    }
  }

  if (!is.null(facet_num_cols)) {
    if (
      !is.numeric(facet_num_cols) ||
        length(facet_num_cols) != 1L ||
        is.na(facet_num_cols) ||
        facet_num_cols != floor(facet_num_cols) ||
        facet_num_cols < 1L
    ) {
      stop("`facet_num_cols` must be a single positive whole number.")
    }
  }

  if (!is.null(plot_theme) && !inherits(plot_theme, "theme")) {
    stop(
      "`plot_theme` must be a ggplot2 theme object (e.g., `ggplot2::theme_bw()`)."
    )
  }

  # --- Build plot -------------------------------------------------------------
  plot <- ggplot2::ggplot(
    df,
    ggplot2::aes(
      x = .data[[x_var]],
      y = .data[[y_var]],
      color = .data[[y_grouping_var]],
      group = .data[[y_grouping_var]]
    )
  ) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_point() +
    ggplot2::scale_x_discrete(drop = FALSE) +
    ggplot2::ylab(y_label) +
    ggplot2::xlab(x_var)

  if (!is.null(facet_var)) {
    if (!is.null(facet_num_cols)) {
      plot <- plot +
        ggplot2::facet_wrap(
          ggplot2::vars(.data[[facet_var]]),
          ncol = facet_num_cols
        )
    } else {
      plot <- plot +
        ggplot2::facet_wrap(ggplot2::vars(.data[[facet_var]]))
    }
  }

  if (!is.null(y_limits)) {
    plot <- plot + ggplot2::ylim(y_limits[1], y_limits[2])
  }

  if (!is.null(plot_theme)) {
    plot <- plot + plot_theme
  }

  return(plot)
}
