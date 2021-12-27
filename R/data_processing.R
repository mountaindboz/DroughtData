#' @title Calculate seasonal or regional averages
#' @description Calculates either the seasonal or regional averages of raw data
#'   used for the Drought Synthesis
#'
#' @param df A data frame or tibble containing the raw data to average. Must
#'   contain variables for `Month`, `Season`, `Region`, and `YearAdj`.
#' @param data_var The variable name in `df` containing the values to be
#'   averaged. Supports tidy or non-standard evaluation as default. Place the
#'   variable name in quotes if the `.quote` argument is `TRUE`.
#' @param avg_type Either `"season"` for seasonal averages, or `"region"` for
#'   regional averages
#' @param month.na Either `"strict"` to make sure all months are represented in
#'   each season, or `"relaxed"` to allow for one or more missing months in a
#'   season
#' @param .quote An optional argument that determines whether the function uses
#'   tidy or non-standard evaluation. The default (`TRUE`) requires the variable
#'   name in `data_var` to be unquoted. If this argument is set to `FALSE`,
#'   `data_var` must be quoted. Allowing for quoted variable names can be useful
#'   to run more than one variable through the function using a character vector
#'   of variable names.
#'
#' @return A tibble containing the averaged data
#' @export
drt_avg_data <- function(df,
                         data_var,
                         avg_type = c("season", "region"),
                         month.na = c("strict", "relaxed"),
                         .quote = FALSE) {
  # Argument checking
  avg_type <- match.arg(avg_type, c("season", "region"))
  month.na <- match.arg(month.na, c("strict", "relaxed"))

  # Set local variable to NULL to avoid no visible binding for global variable
  . <- NULL

  # Calculate seasonal averages for each region
  df_avg_seas <- df %>%
    # Remove any rows with NAs in data_var to summarize - method depends on .quote argument
    {if (.quote == TRUE) {
      dplyr::filter(., !is.na(.data[[data_var]]))
    } else {
      dplyr::filter(., !is.na({{ data_var }}))
    }} %>%
    # Calculate monthly mean for each region - method depends on .quote argument
    dplyr::group_by(.data$Month, .data$Season, .data$Region, .data$YearAdj) %>%
    {if (.quote == TRUE) {
      dplyr::summarize(., var_month_mean = mean(.data[[data_var]]))
    } else {
      dplyr::summarize(., var_month_mean = mean({{ data_var }}))
    }} %>%
    dplyr::ungroup() %>%
    {if (month.na == "strict") {
      # Fill in NAs for data_var for any missing Month, Region, YearAdj
        # combinations to make sure all months are represented in each season
      tidyr::complete(., tidyr::nesting(.data$Month, .data$Season), .data$Region, .data$YearAdj)
    } else {
      # Fill in NAs for data_var for any missing Season, Region, YearAdj
        # combinations to make sure all seasons and regions are represented when
        # averaging
      tidyr::complete(., .data$Season, .data$Region, .data$YearAdj)
    }} %>%
    # Calculate seasonal mean for each region
    dplyr::group_by(.data$Season, .data$Region, .data$YearAdj) %>%
    dplyr::summarize(var_mean = mean(.data$var_month_mean)) %>%
    dplyr::ungroup()

  # Calculate either the overall seasonal or regional averages
  df_avg_f <- df_avg_seas %>%
    {if (avg_type == "season") {
      # Group by season in order to calculate seasonal averages for entire Delta
      dplyr::group_by(., .data$Season, .data$YearAdj)
    } else {
      # Group by region in order to calculate regional averages for each year
      dplyr::group_by(., .data$Region, .data$YearAdj)
    }} %>%
    dplyr::summarize(
      {{ data_var }} := mean(.data$var_mean),
      .groups = "drop"
    )

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
#' @export
drt_add_yr_assign <- function(df) {
  load("R/sysdata.rda")
  df %>% dplyr::left_join(df_yr_type, by = "YearAdj")
}

