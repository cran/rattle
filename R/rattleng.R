# A function to provide the dataset summary as JSON which can then be
# parsed by Rattle as the dataset summary from which Rattle gets all
# of it's meta data.

meta_data <- function(df) {
  summary_list <- lapply(names(df), function(var_name) {
    x <- df[[var_name]]
    if (is.numeric(x)) {
      list(
        datatype = "numeric",
        min = min(x, na.rm = TRUE),
        max = max(x, na.rm = TRUE),
        mean = mean(x, na.rm = TRUE),
        median = median(x, na.rm = TRUE),
        variance = var(x, na.rm = TRUE),
        unique = length(unique(x)),
        missing = sum(is.na(x))
      )
    } else if (is.factor(x)) {
      list(
        datatype = "factor",
        unique = length(unique(x)),
        missing = sum(is.na(x))
      )
    } else if (is.character(x)) {
      list(
        datatype = "character",
        min = min(x, na.rm = TRUE),
        max = max(x, na.rm = TRUE),
        unique = length(unique(x)),
        missing = sum(is.na(x))
      )
    } else if (lubridate::is.Date(x) || lubridate::is.POSIXct(x)) {
      list(
        datatype = "date",
        min = min(x, na.rm = TRUE),
        max = max(x, na.rm = TRUE),
        unique = length(unique(x)),
        missing = sum(is.na(x))
      )
    } else {
      list(
        datatype = "other",
        unique = length(unique(x)),
        missing = sum(is.na(x))
      )
    }
  })

  # Name the list elements by the variable names

  names(summary_list) <- names(df)

  # Convert the list to a JSON string.

  json_output <- jsonlite::toJSON(summary_list, pretty = TRUE)
  return(json_output)
}

# Identify columns (except real numbers) with unique values to treat
# as identifiers. (20250311 gjw)
## 20260118 gjw MOVED TO R RATTLE PACKAGE

unique_columns <- function(tbl) {
  tbl %>%
    dplyr::select(tidyselect::where(~ !is.numeric(.x) || all(.x == as.integer(.x), na.rm=TRUE))) ->
  non_real_cols

  non_real_cols %>%
    dplyr::select(tidyselect::where(~ dplyr::n_distinct(.x, na.rm=TRUE) == nrow(tbl))) %>%
    colnames() ->
  unique_non_real_cols

  return(unique_non_real_cols)
}

## 20260118 gjw MOVED TO R RATTLE PACKAGE
find_fewest_levels <- function(df) {
  # Select only the categoric (factor) columns from the data frame
  cat_vars <- df[, sapply(df, is.factor), drop = FALSE]

  # Check if there are any categoric variables
  if (ncol(cat_vars) > 0) {
    # Find the variable with the fewest levels
    fewest_levels_var <- names(cat_vars)[which.min(sapply(cat_vars, nlevels))]

    # Find all variables that have the fewest levels
    min_levels <- min(sapply(cat_vars, nlevels))
    fewest_levels_vars <- names(cat_vars)[sapply(cat_vars, nlevels) == min_levels]

    # Select the last variable in case of ties
    fewest_levels_var <- fewest_levels_vars[length(fewest_levels_vars)]

    # Return the variable with the fewest levels
    return(fewest_levels_var)
  } else {
    # If no categoric variables are found, return a message
    return("")
  }
}

####################################
# A Rattle Theme for Graphics
####################################

# A palette for rattle!
## 20260118 gjw MOVED TO R RATTLE PACKAGE

rattlePalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442",
                   "#0072B2", "#D55E00", "#CC79A7", "#000000")

# A ggplot2 theme for rattle.
## 20260118 gjw MOVED TO R RATTLE PACKAGE

theme_rattle <- function(base_size = 11, base_family = "") {
  ggplot2::theme_grey(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      # Customize text elements
      plot.title = ggplot2::element_text(color = "darkblue",
                                face = "bold",
                                size = base_size * 1.2),
      axis.title = ggplot2::element_text(color = "darkblue"),
      axis.text = ggplot2::element_text(color = "darkblue"),
      legend.title = ggplot2::element_text(color = "darkblue"),
      legend.text = ggplot2::element_text(color = "darkblue"),
      # Customize panel background
      panel.background = ggplot2::element_rect(fill = "white"),
      # Customize grid lines
      panel.grid.major = ggplot2::element_line(color = "lightgrey"),
      panel.grid.minor = ggplot2::element_line(color = "lightgrey", linetype = "dotted")
    )
}
