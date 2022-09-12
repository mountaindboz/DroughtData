#' @title Calculate seasonal and/or regional averages
#' @description Calculates the seasonal, regional, or seasonal-regional averages
#'   of raw data used for the Drought Synthesis
#'
#' @param df A data frame or tibble containing the raw data to average. Must
#'   contain variables for `Month`, `Season`, `Region`, and `YearAdj`.
#' @param data_var The variable name in `df` containing the values to be
#'   averaged. Supports tidy or non-standard evaluation as default. Place the
#'   variable name in quotes if the `.quote` argument is `TRUE`. Also, the
#'   default behavior is for `NA` values in `data_var` to be removed before
#'   averaging. Set `.remove_na = FALSE` to keep `NA` values when averaging.
#' @param avg_type Use `"season"` for seasonal averages, `"region"` for regional
#'   averages, or `"both"` for seasonal-regional averages
#' @param month_na Either `"strict"` to make sure all months are represented in
#'   each season, or `"relaxed"` to allow for one or more missing months in a
#'   season
#' @param .quote An optional argument that determines whether the function uses
#'   tidy or non-standard evaluation. The default (`FALSE`) requires the variable
#'   name in `data_var` to be unquoted. If this argument is set to `TRUE`,
#'   `data_var` must be quoted. Allowing for quoted variable names can be useful
#'   to run more than one variable through the function using a character vector
#'   of variable names.
#' @param .remove_na An optional argument that determines whether `NA` values in
#'   `data_var` are removed before averaging. The default (`TRUE`) removes `NA`
#'   values in `data_var` before averaging, while `FALSE` keeps `NA` values.
#'   WARNING: keeping `NA` values in `data_var` may result in unintended `NA`
#'   values in the calculated averages.
#'
#' @return A tibble containing the averaged data
#'
#' @examples
#' # Calculate seasonal averages of the water temperature data
#' drt_avg_data(raw_wq_1975_2021, Temperature, avg_type = "season", month_na = "strict")
#'
#' # Set the month_na argument to "relaxed" to allow for one or more missing
#'   # months in a season when calculating the averages
#' drt_avg_data(raw_wq_1975_2021, Temperature, avg_type = "season", month_na = "relaxed")
#'
#' # Change the avg_type argument to get different types of averages
#' # Regional averages:
#' drt_avg_data(raw_wq_1975_2021, Temperature, avg_type = "region", month_na = "relaxed")
#' # Seasonal-regional averages
#' drt_avg_data(raw_wq_1975_2021, Temperature, avg_type = "both", month_na = "relaxed")
#'
#' # Use the optional .quote argument to allow for a quoted data variable name
#' drt_avg_data(raw_wq_1975_2021, "Temperature", avg_type = "both", month_na = "relaxed", .quote = TRUE)
#'
#' # Use the optional .remove_na argument to keep NA values when averaging
#'   # Note that there are more NA values in the resulting tibble
#' drt_avg_data(raw_wq_1975_2021, Temperature, avg_type = "both", month_na = "relaxed", .remove_na = FALSE)
#'
#' @export
drt_avg_data <- function(df,
                         data_var,
                         avg_type = c("season", "region", "both"),
                         month_na = c("strict", "relaxed"),
                         .quote = FALSE,
                         .remove_na = TRUE) {
  # Argument checking
  avg_type <- match.arg(avg_type, c("season", "region", "both"))
  month_na <- match.arg(month_na, c("strict", "relaxed"))

  # Set local variables to NULL to avoid no visible binding for global variable
  . <- NULL
  Month <- NULL
  Season <- NULL

  # Remove rows with NAs in data_var if .remove_na is TRUE
  if (.remove_na == TRUE) {
    # Method depends on .quote argument
    if (.quote == TRUE) {
      df <- dplyr::filter(df, !is.na(.data[[data_var]]))
    } else {
      df <- dplyr::filter(df, !is.na({{ data_var }}))
    }
  }

  # Calculate monthly mean for each region - method depends on .quote argument
  df_avg_month <- df %>%
    dplyr::group_by(.data$Month, .data$Season, .data$Region, .data$YearAdj) %>%
    {if (.quote == TRUE) {
      dplyr::summarize(., var_month_mean = mean(.data[[data_var]]), .groups = "drop")
    } else {
      dplyr::summarize(., var_month_mean = mean({{ data_var }}), .groups = "drop")
    }}

  # Calculate seasonal-regional averages for each year
  df_avg_seas_reg <- df_avg_month %>%
    {if (month_na == "strict") {
      # Fill in NAs for data_var for any missing Month, Region, YearAdj
        # combinations to make sure all months are represented in each season
      tidyr::complete(., tidyr::nesting(Month, Season), .data$Region, .data$YearAdj)
    } else {
      # Fill in NAs for data_var for any missing Season, Region, YearAdj
        # combinations to make sure all seasons and regions are represented when
        # averaging
      tidyr::complete(., .data$Season, .data$Region, .data$YearAdj)
    }} %>%
    # Calculate mean
    dplyr::group_by(.data$Season, .data$Region, .data$YearAdj) %>%
    dplyr::summarize(var_mean = mean(.data$var_month_mean), .groups = "drop")

  # Calculate either the overall seasonal or regional averages, or keep as
    # seasonal-regional averages as determined by the avg_type argument
  if (avg_type == "both") {
    df_avg_f <- df_avg_seas_reg %>%
      dplyr::rename({{ data_var }} := .data$var_mean)
  } else if (avg_type == "season") {
    df_avg_f <- df_avg_seas_reg %>%
      # Group by season in order to calculate seasonal averages for entire Delta
      dplyr::group_by(.data$Season, .data$YearAdj) %>%
      dplyr::summarize(
        {{ data_var }} := mean(.data$var_mean),
        .groups = "drop"
      )
  } else {
    df_avg_f <- df_avg_seas_reg %>%
      # Group by region in order to calculate regional averages for each year
      dplyr::group_by(.data$Region, .data$YearAdj) %>%
      dplyr::summarize(
        {{ data_var }} := mean(.data$var_mean),
        .groups = "drop"
      )
  }

  return(df_avg_f)
}

#' @title Add year assignment information
#' @description Adds year assignment information to a data frame or tibble. The
#'   information added includes the following variables for each year in the
#'   data frame. This information is used in the analyses for the Drought
#'   Synthesis.
#' * `SVIndex` - Sacramento Valley Water Year Index
#' * `YearType` - Water year type based on Sacramento Valley Water Year Index
#'   (Critical, Dry, Below Normal, Above Normal, or Wet)
#' * `Drought` - Drought/Wet/Neutral classification. Multiple Dry, Critical, and
#'   Below Normal years in a row are a drought (D), multiple Wet or Above Normal
#'   years in a row are a wet period (W), and years that are not on a streak are
#'   neutral (N).
#'
#' @param df A data frame or tibble containing the data to add the year
#'   assignment information to. Must contain a variable for `YearAdj`, which is
#'   the adjusted calendar year (December-November, with December of the
#'   previous calendar year included with the following year).
#'
#' @return A tibble containing the original data with the addition of the year
#'   assignment information
#'
#' @examples
#' # Add year assignment information to raw water quality data
#' df_wq <- drt_add_yr_assign(raw_wq_1975_2021)
#' dplyr::glimpse(df_wq)
#'
#' @export
drt_add_yr_assign <- function(df) {
  df %>% dplyr::left_join(df_yr_type, by = "YearAdj")
}

