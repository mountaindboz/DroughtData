#' @title Color palettes for Drought Synthesis report
#' @description Apply custom color palettes to the figures in the Drought
#'   Synthesis report. The palettes are based on the
#'   [viridis](https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html)
#'    palette. Functions can only be used with [ggplot2::ggplot()] objects.
#'   There are two palette options:
#' * `drt_color_pal_drought()` is intended for the three Drought categories (D,
#'   N, W)
#' * `drt_color_pal_yrtype()` is intended for the five Year Type categories
#'   (Critical, Dry, Below Normal, Above Normal, Wet)
#'
#' @param aes_type A string that identifies the aesthetic type to apply the
#'   palette to. Must be either `"fill"` or `"color"`.
#' @param scale_title (optional) A string that provides a title for the scale
#'   of the color palette. Defaults are:
#' * "Drought" for `drt_color_pal_drought()`
#' * "Year Type" for `drt_color_pal_yrtype()`
#'
#' @return A discrete color scale for a [ggplot2::ggplot()] object
#' @seealso [ggplot2::scale_fill_manual()], [ggplot2::scale_color_manual()]
#' @examples
#' # A simple boxplot of seasonal average outflow for the Drought categories -
#'   # using default ggplot2 colors for the fill aesthetic
#' lt_outflow <- dplyr::filter(lt_avg_hydro, !is.na(Outflow))
#'
#' library(ggplot2)
#' p <- ggplot(lt_outflow, aes(x = Season, y = Outflow, fill = Drought)) +
#'   geom_boxplot()
#' p
#'
#' # Apply the custom color palette for the Drought categories
#' p + drt_color_pal_drought(aes_type = "fill")
#'
#' # Change the default scale title
#' p + drt_color_pal_drought(aes_type = "fill", scale_title = "Something Else")
#'
#' # Use the custom color palette for the color aesthetic instead
#' ggplot(lt_outflow, aes(x = Season, y = Outflow, color = Drought)) +
#'   geom_boxplot() +
#'   drt_color_pal_drought(aes_type = "color")
#'
#' @name drt_color_pal
NULL

#' @rdname drt_color_pal
#' @export
drt_color_pal_drought <- function(aes_type = c("fill", "color"), scale_title = "Drought") {
  # Evaluate choices for aes_type
  aes_type <- match.arg(aes_type, c("fill", "color"))

  # Define color palette for the Drought categories
  pal_drought <- c(
    "D" = "#FDE333",
    "N" = "#53CC67",
    "W" = "#00588B"
  )

  # Apply palette based on aes_type
  if (aes_type == "fill") {
    ggplot2::scale_fill_manual(name = scale_title, values = pal_drought)
  } else {
    ggplot2::scale_color_manual(name = scale_title, values = pal_drought)
  }
}

#' @rdname drt_color_pal
#' @export
drt_color_pal_yrtype <- function(aes_type = c("fill", "color"), scale_title = "Year Type") {
  # Evaluate choices for aes_type
  aes_type <- match.arg(aes_type, c("fill", "color"))

  # Define color palette for the Year Type categories
  pal_yrtype <- c(
    "Critical" = "#FDE333",
    "Dry" = "#53CC67",
    "Below Normal" = "#009B95",
    "Above Normal" = "#00588B",
    "Wet" = "#4B0055"
  )

  # Apply palette based on aes_type
  if (aes_type == "fill") {
    ggplot2::scale_fill_manual(name = scale_title, values = pal_yrtype)
  } else {
    ggplot2::scale_color_manual(name = scale_title, values = pal_yrtype)
  }
}

