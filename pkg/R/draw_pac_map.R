


#' Draw map of Pacific Island countries and territories
#' 
#' @details draws a map of Padcific Island countries and territories EEZs, with
#'   possibility of drawing it as a choropleth map.
#' @param fill_df (Optional) A data frame with data to be used for colouring in
#'   the EEZs
#' @param join_col Name of a column in fill_df to join to pac_map_sf. Should be
#'   either geo_pict, iso3, or name2, and needs to match exactly the contents of
#'   those columns in pac_map_sf
#' @param fill_col Column in fill_df to map to colour
#' @param fill_col_label Label for the fill colour legend
#' @param base_size base size of fonts eg for title, passed on to theme_minimal()
#' @param xlim longitude to limit the map to
#' @param ylim latitude to limt the map to
#' @param country_labels whether or not to print the country names as labels (Palau, Guam, etc)
#' @param country_label_size Size of country labels. Set to zero to make them invisible.
#' @param country_label_col  Colour of country labels.
#' @param idl_col Colour to use for the International date line
#' @param idl_label_size Size of the International date line label,
#' @param ocean_col Colour to draw the ocean
#' @param family Font family
#' @param leg_pos Legend position (in grid units i.e. 0,0 is bottom left, 1,1 is top right)
#' @importFrom ggplot2 ggplot geom_polygon annotate geom_text theme_minimal theme geom_sf coord_sf
#' @export
#' @returns A ggplot2 object
#' @examples 
#' 
#' draw_pac_map(country_label_size = 5)
#' 
#' country_data <- tribble(~name2, ~var,
#' "Cook Islands", 5,
#'   "Fiji", 10,
#'   "Guam", 4,
#'   "Palau", 5,
#'   "Niue", 20,
#'   "Tonga", 3,
#'   "Tuvalu", 17,
#'   "Papua New Guinea", 12)
#' 
#' draw_pac_map(fill_df = country_data, join_col = "name2", fill_col = "var") +
#'   scale_fill_viridis_c() +
#'   labs(title = "Some random variables",
#'   fill = "")
draw_pac_map <- function(fill_df = NULL, join_col = "geo_pict", fill_col = NULL, 
                         fill_col_label = fill_col,
                         base_size = 11,
                         xlim = c(120, 240), ylim = c(-50, 50),
                         country_labels = TRUE,
                         country_label_size = 3, country_label_col = "black",
                         idl_col = "steelblue", idl_label_size = country_label_size, 
                         leg_pos = c(0.8, 0.7), ocean_col = "lightsteelblue",
                         family = "sans"){
  
  if(is.null(fill_df)){
    m0 <- pac_map_sf |>
      ggplot2::ggplot() +
      ggplot2::geom_sf(colour = "grey70", alpha = 0.9) 
    
  } else {
    d <- pac_map_sf |>
      dplyr::left_join(fill_df, by = join_col)
    
    if(nrow(d) != nrow(pac_map_sf)){
      warning("Some extra rows in pac_map_sf introduced when trying to join to fill_df")
    }
    
    d$fill_col <- pull(d, fill_col)
    
    m0 <- d |>
      ggplot2::ggplot() +
      ggplot2::geom_sf(aes(fill = fill_col), colour = "grey70", alpha = 0.9) 
  }
  
  m1 <- m0 +
    ggplot2::geom_polygon(data = country_borders_tb,
                 aes(x = long, y = lat, group = group),
                 fill = "white",
                 alpha = 0.8) +
    ggplot2::geom_sf(data = international_date_line_sf, colour = idl_col, linetype = 1, alpha = 0.5) +
    annotate("text", x = 182, y = 38, label = "International date line", 
             colour = idl_col, hjust = 0, family = family, size = idl_label_size) 
  
  if(country_labels){
    m1 <- m1 + 
      ggplot2::geom_text(aes(label = name2, x = X, y = Y),
                         colour = country_label_col, family = family, size = country_label_size, angle = 15) 
      
  }
  
  m1 <- m1 +
    ggplot2::theme_minimal(base_family = family, base_size = base_size) +
    ggplot2::theme(legend.position = leg_pos,
          panel.background = element_rect(fill = ocean_col, colour = NA),
          panel.grid = element_blank(),
          plot.caption = element_text(colour = "grey50")) +
    ggplot2::coord_sf(xlim = xlim,  ylim = ylim) +
    ggplot2::labs(x = "",
         y = "",
         fill = fill_col_label)
  
  return(m1)  
}
