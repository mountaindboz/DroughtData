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
#' drt_avg_data(
#'   raw_wq_1975_2021,
#'   Temperature,
#'   avg_type = "season",
#'   month_na = "strict"
#' )
#'
#' # Set the month_na argument to "relaxed" to allow for one or more missing
#'   # months in a season when calculating the averages
#' drt_avg_data(
#'   raw_wq_1975_2021,
#'   Temperature,
#'   avg_type = "season",
#'   month_na = "relaxed"
#' )
#'
#' # Change the avg_type argument to get different types of averages
#' # Regional averages:
#' drt_avg_data(
#'   raw_wq_1975_2021,
#'   Temperature,
#'   avg_type = "region",
#'   month_na = "relaxed"
#' )
#' # Seasonal-regional averages
#' drt_avg_data(
#'   raw_wq_1975_2021,
#'   Temperature,
#'   avg_type = "both",
#'   month_na = "relaxed"
#' )
#'
#' # Use the optional .quote argument to allow for a quoted data variable name
#' drt_avg_data(
#'   raw_wq_1975_2021,
#'   "Temperature",
#'   avg_type = "both",
#'   month_na = "relaxed",
#'   .quote = TRUE
#' )
#'
#' # Use the optional .remove_na argument to keep NA values when averaging
#'   # Note that there are more NA values in the resulting tibble
#' drt_avg_data(
#'   raw_wq_1975_2021,
#'   Temperature,
#'   avg_type = "both",
#'   month_na = "relaxed",
#'   .remove_na = FALSE
#' )
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

#' @title Replace values below the reporting limit
#' @description Replaces values measured below the analytical reporting limit
#'   with a random number of uniform distribution between 0.00001 (default) and
#'   the reporting limit. This function only replaces the values below the
#'   reporting limit. It does not change any other values in the data.
#'
#' @param df A data frame or tibble containing the data to run the replacement
#'   procedure on.
#' @param data_var The variable name in `df` containing the values to run the
#'   replacement procedure on. Must be numeric with the records measured below
#'   the reporting limit substituted with the analytical reporting limit. Can
#'   contain a mixture of values, some measured below the reporting limit and
#'   others that aren't.
#' @param sign_var The variable name in `df` containing the qualifier codes for
#'   the values. The codes used to indicate whether a value was measured below
#'   the reporting limit must start with "<".
#' @param min_val The minimum value allowed for the random number generation for
#'   the values below the reporting limit. Default is 0.00001, but the user can
#'   specify their preferred value.
#' @param seed The seed specification for the random number generation. Default
#'   is 1, but the user can specify their preferred value. Allows for the random
#'   number generation procedure to be reproducible. See the documentation for
#'   [base::set.seed()] for more information.
#'
#' @return A tibble containing the original data with random numbers of uniform
#'   distribution substituted for the values measured below the reporting limit.
#'   All other values remain the same.
#' @seealso [stats::runif()]
#' @examples
#' # Replace Dissolved Ammonia values below the reporting limit with a random
#'   # number
#' drt_replace_rl(raw_nutr_1975_2021, DissAmmonia, DissAmmonia_Sign)
#'
#' # Use the min_val argument to set a different minimum value for the range of
#'   # random values generated
#' drt_replace_rl(raw_nutr_1975_2021, DissAmmonia, DissAmmonia_Sign, min_val = 0.0000001)
#'
#' # Use the seed argument to set a different random seed
#' drt_replace_rl(raw_nutr_1975_2021, DissAmmonia, DissAmmonia_Sign, seed = 500)
#'
#' @export
drt_replace_rl <- function(df, data_var, sign_var, min_val = 0.00001, seed = 1) {
  # Pull out values that are below the RL
  df_blw_rl <- df %>% dplyr::filter(stringr::str_detect({{ sign_var }}, "^<"))

  # Replace below RL values with simulated ones
  withr::with_seed(
    # Set seed for reproducibility
    seed = seed,
    df_blw_rl_sim <- df_blw_rl %>%
      dplyr::mutate(
        {{ data_var }} := round(stats::runif(nrow(df_blw_rl), min = min_val, max = {{ data_var }}), 5)
      )
  )

  # Add simulated values back to main data frame
  df %>%
    dplyr::filter(!stringr::str_detect({{ sign_var }}, "^<") | is.na({{ sign_var }})) %>%
    dplyr::bind_rows(df_blw_rl_sim)
}

