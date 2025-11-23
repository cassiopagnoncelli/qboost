#' Plot distribution with histogram and density overlay
#'
#' @param data Numeric vector or data frame with numeric column
#' @param bins Number of histogram bins (default: NULL, uses Sturges' rule)
#' @param vline X-coordinate for vertical reference line (default: 0)
#' @param title Plot title (default: "Distribution")
#' @return ggplot2 object showing histogram with density curve and statistical markers
#' @examples
#' plot_distribution(rnorm(1000))
#' plot_distribution(data.frame(x = rnorm(500)), vline = 1, title = "My Distribution")
plot_distribution <- function(data, bins = NULL, vline = 0, title = "Distribution") {
  # Handle different input types
  if (is.vector(data) || is.atomic(data)) {
    # If data is a vector, convert to tibble
    data <- tibble::tibble(value = data)
  } else if (is.data.frame(data)) {
    # If data is a tibble/data.frame, find first numeric column
    numeric_cols <- sapply(data, is.numeric)
    if (!any(numeric_cols)) {
      stop("No numeric columns found in the data")
    }
    first_numeric <- names(data)[which(numeric_cols)[1]]
    data <- tibble::tibble(value = data[[first_numeric]])
  }

  # Calculate number of bins using Sturges' rule if not specified
  if (is.null(bins)) {
    n <- nrow(data)
    bins <- ceiling(3.3 * log10(n) + 1)
  }

  # Calculate statistics for labels
  mean_val <- mean(data$value, na.rm = TRUE)
  sd_val <- sd(data$value, na.rm = TRUE)
  
  # Create histogram with density scale and smoothing curve
  ggplot2::ggplot(data, ggplot2::aes(x = value)) +
    ggplot2::geom_histogram(
      aes(y = ggplot2::after_stat(density)),
      bins = bins,
      fill = "#851d91",
      # color = "#676cce",
      alpha = 0.7
    ) +
    ggplot2::geom_density(
      aes(y = ggplot2::after_stat(density)),
      fill = "#1d9146",
      # color = "#2a965a",
      alpha = .55,
      linewidth = .2
    ) +
    ggplot2::geom_vline(
      xintercept = vline,
      color = "#ff0000",
      linetype = "solid",
      linewidth = .4,
      alpha = .85
    ) +
    ggplot2::geom_vline(
      xintercept = mean_val,
      color = "#000000",
      linetype = "dashed",
      linewidth = .8
    ) +
    ggplot2::geom_vline(
      xintercept = mean_val + sd_val,
      color = "#4e4e4e",
      linetype = "dashed",
      linewidth = .5
    ) +
    ggplot2::geom_vline(
      xintercept = mean_val - sd_val,
      color = "#4e4e4e",
      linetype = "dashed",
      linewidth = .5
    ) +
    ggplot2::geom_vline(
      xintercept = mean_val + 2 * sd_val,
      color = "#a3a3a3",
      linetype = "dashed",
      linewidth = .35
    ) +
    ggplot2::geom_vline(
      xintercept = mean_val - 2 * sd_val,
      color = "#a3a3a3",
      linetype = "dashed",
      linewidth = .35
    ) +
    ggplot2::annotate(
      "text", x = mean_val, y = Inf,
      label = sprintf("μ=%.2f", mean_val),
      vjust = 1.5, hjust = 0.5, size = 5, color = "#000000"
    ) +
    ggplot2::annotate(
      "text", x = mean_val + sd_val, y = Inf,
      label = sprintf("+1σ=%.2f", mean_val + sd_val),
      vjust = 1.5, hjust = 0.5, size = 4.5, color = "#4e4e4e"
    ) +
    ggplot2::annotate(
      "text", x = mean_val - sd_val, y = Inf,
      label = sprintf("-1σ=%.2f", mean_val - sd_val),
      vjust = 1.5, hjust = 0.5, size = 4.5, color = "#4e4e4e"
    ) +
    ggplot2::annotate(
      "text", x = mean_val + 2 * sd_val, y = Inf,
      label = sprintf("+2σ=%.2f", mean_val + 2 * sd_val),
      vjust = 1.5, hjust = 0.5, size = 4.5, color = "#a3a3a3"
    ) +
    ggplot2::annotate(
      "text", x = mean_val - 2 * sd_val, y = Inf,
      label = sprintf("-2σ=%.2f", mean_val - 2 * sd_val),
      vjust = 1.5, hjust = 0.5, size = 4.5, color = "#a3a3a3"
    ) +
    ggplot2::labs(title = title, x = "Value", y = "Density") +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid = ggplot2::element_blank())
}

#' Analyse distribution statistics by groups
#'
#' @param data Numeric vector or data frame with numeric column
#' @param groups Numeric vector of threshold values to split data into groups (default: c(0))
#' @param extreme_threshold Threshold for identifying extreme values (default: 1e15)
#' @return List with overall_results (summary statistics and special value counts) and group_results (statistics per group)
#' @examples
#' analyse_distribution(rnorm(1000), groups = c(-1, 0, 1))
analyse_distribution <- function(data, groups = c(0), extreme_threshold = 1e15) {
  # Handle different input types
  if (is.vector(data) || is.atomic(data)) {
    # If data is a vector, convert to tibble
    data <- tibble::tibble(value = data)
  } else if (is.data.frame(data)) {
    # If data is a tibble/data.frame, find first numeric column
    numeric_cols <- sapply(data, is.numeric)
    if (!any(numeric_cols)) {
      stop("No numeric columns found in the data")
    }
    first_numeric <- names(data)[which(numeric_cols)[1]]
    data <- tibble::tibble(value = data[[first_numeric]])
  }

  # Count special values before processing
  na_count <- sum(is.na(data$value))
  inf_count <- sum(is.infinite(data$value), na.rm = TRUE)
  nan_count <- sum(is.nan(data$value))
  extreme_count <- sum(abs(data$value) > extreme_threshold, na.rm = TRUE)

  # Sort groups to ensure proper ordering
  groups <- sort(groups)

  # Create group labels and split data
  n_groups <- length(groups) + 1
  group_labels <- paste0("g", 1:n_groups)

  # Assign each value to a group
  data <- data |>
    dplyr::mutate(
      group = dplyr::case_when(
        value < groups[1] ~ group_labels[1],
        TRUE ~ group_labels[n_groups]
      )
    )

  # Handle intermediate groups if there are multiple thresholds
  if (length(groups) > 1) {
    for (i in 2:length(groups)) {
      data <- data |>
        dplyr::mutate(
          group = dplyr::if_else(
            value >= groups[i - 1] & value < groups[i],
            group_labels[i],
            group
          )
        )
    }
  }

  total_n <- nrow(data)
  group_results <- data |>
    dplyr::group_by(group) |>
    dplyr::summarise(
      count = dplyr::n(),
      prob = dplyr::n() / total_n,
      mean = mean(value, na.rm = TRUE),
      expected = prob * mean,
      sd = sd(value, na.rm = TRUE),
      min = quantile(value, probs = 0, na.rm = TRUE),
      q_0.05 = quantile(value, probs = 0.05, na.rm = TRUE),
      q_0.32 = quantile(value, probs = 0.32, na.rm = TRUE),
      median = quantile(value, probs = 0.5, na.rm = TRUE),
      q_0.68 = quantile(value, probs = 0.68, na.rm = TRUE),
      q_0.95 = quantile(value, probs = 0.95, na.rm = TRUE),
      max = quantile(value, probs = 1, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(group = factor(group, levels = group_labels)) |>
    dplyr::arrange(group)

  # Calculate dataset metrics
  overall_results <- data |>
    dplyr::summarise(
      expected_value = group_results$expected |> sum(na.rm = TRUE),
      mean = mean(value, na.rm = TRUE),
      median = quantile(value, probs = 0.5, na.rm = TRUE),
      sd = sd(value, na.rm = TRUE),
      n = nrow(data),
      NA_count = na_count,
      Inf_count = inf_count,
      NaN_count = nan_count,
      extreme_count = extreme_count
    )

  return(
    list(
      overall_results = overall_results,
      group_results = group_results
    )
  )
}

#' Cap distribution values at specified quantiles
#'
#' @param x Numeric vector to cap
#' @param quantiles Numeric vector of length 2 with lower and upper quantile thresholds (default: c(0.001, 0.999))
#' @return Numeric vector with values capped at the specified quantiles
#' @examples
#' cap_distribution(c(1:100, 1000), quantiles = c(0.05, 0.95))
cap_distribution <- function(x, quantiles = c(0.001, 0.999)) {
  if (length(quantiles) != 2) {
    stop("quantiles must be a vector of length 2")
  }
  if (any(quantiles < 0) || any(quantiles > 1)) {
    stop("quantiles must be between 0 and 1")
  }
  if (quantiles[1] >= quantiles[2]) {
    stop("the first quantile must be less than the second quantile")
  }
  qs <- quantile(x, probs = quantiles, na.rm = TRUE)
  x[x < qs[1]] <- qs[1]
  x[x > qs[2]] <- qs[2]
  return(x)
}
