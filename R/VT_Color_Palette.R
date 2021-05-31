vt_colors <- c(
  `VT_Yellow` = "#F0E614",
  `VT_White` = "#FFFFFF",
  `VT_Red` = "#D7004B",
  `VT_Grey` = "#4B4B46",
  `VT_Secondary_1` = "#B4C832",
  `VT_Secondary_2` = "#64AF5A",
  `VT_Secondary_3` = "#3CB4A0",
  `VT_Secondary_4` = "#008269",
  `VT_Secondary_5` = "#005546",
  `VT_tertiary_1` = "#00A5E6",
  `VT_tertiary_2` = "#006EAA",
  `VT_tertiary_3` = "#005082",
  `VT_tertiary_4` = "#DC5F96",
  `VT_tertiary_5` = "#7D0F5F",
  `traffic_red` = "#DC230F",
  `traffic_yellow` = "#FFC300",
  `traffic_green` = "#64AF5A")

#' Function to extract VT colors as hex codes
#'
#' @param ... Character names of vt_colors 
#'
vt_cols <- function(...) {
  cols <- c(...)
  
  if (is.null(cols))
    return (drsimonj_colors)
  
  vt_colors[cols]
}

vt_palettes <- list(
  `primary` = vt_cols("VT_Yellow", "VT_Red", "VT_Grey"), 
  `secondary` = vt_cols("VT_Secondary_1", "VT_Secondary_2", "VT_Secondary_3", "VT_Secondary_4", "VT_Secondary_5"), 
  `tertiary` = vt_cols("VT_tertiary_1", "VT_tertiary_2", "VT_tertiary_3", "VT_tertiary_4", "VT_tertiary_5"), 
  `traffic` = vt_cols("traffic_red", "traffic_yellow", "traffic_green")
)

#' Return function to interpolate a VT color palette
#'
#' @param palette Character name of palette in vt_palettes
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments to pass to colorRampPalette()
#'
vt_pal <- function(palette = "primary", reverse = FALSE, ...) {
  pal <- vt_palettes[[palette]]
  
  if (reverse) pal <- rev(pal)
  
  colorRampPalette(pal, ...)
}



#' Color scale constructor for vt colors
#'
#' @param palette Character name of palette in vt_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_color_gradient(), used respectively when discrete is TRUE or FALSE
#'
#' @export
scale_color_vt <- function(palette = "primary", discrete = TRUE, reverse = FALSE, ...) {
  pal <- vt_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("colour", paste0("vt_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

#' Fill scale constructor for VT colors
#'
#' @param palette Character name of palette in vt_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_fill_gradient(), used respectively when discrete is TRUE or FALSE
#'
#' @export
scale_fill_vt <- function(palette = "primary", discrete = TRUE, reverse = FALSE, ...) {
  pal <- vt_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("fill", paste0("vt_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}


#library(tidyverse)
#ggplot(mpg, aes(manufacturer, fill = manufacturer)) +
#  geom_bar() +
#  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#  scale_fill_vt(palette = "primary", guide = "none")

# Color by numeric variable with cool palette
#ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Sepal.Length)) +
#  geom_point(size = 4, alpha = .6) +
#  scale_color_vt(discrete = FALSE, palette = "primary") + 
#  theme_minimal()