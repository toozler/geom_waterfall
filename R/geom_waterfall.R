library(ggplot2)
library(dplyr)
library(magrittr)
library(tibble)





#' Parse input data
#' 
#' @return Original data frame, or error if invalid structure
parse_input_data <- function(...) {
  
  #verify input data table
  #must start with total, end with total
  #intermediate totals must be value NA
  #steps must have values, can't be NA
  #verify names and column names
  #cannot have column name 'fill'
  
}


#' Compute the waterfall intermediate values
#' 
#' @return Original data frame with additional columns for plotting
compute_waterfall <- function(waterfall_data) {
  
  waterfall_data %>% 
    mutate(rownum = rownames(.) %>% as.numeric,
           cs = value %>% coalesce(0) %>% cumsum,
           ymin = ifelse(type == "total", 0, cs - value),
           ymax = cs,
           ymean = (ymax+ymin) / 2) %>% 
    return
  
}


#' Waterfall plot 
#' 
#' @param waterfall_data Data frame with data to generate plot. See details for structure.
#' @param label Adds geom_label to waterfall bars. Default "all", but can be "totals", "steps" or "none"
#' @param arrows Adds geom_segment arrows to up and down bars. Default FALSE
#' @param connectors Adds horizontal connectors between bars. Default TRUE
#' @param fill_colors Color scheme to fill bars and label. Default theme is gray for totals, forestgreen and tomato for up and down. 
#'  A list can be passed with custom colors, i.e. list(total = "gray", up = "forestgreen", down = "tomato")
#' @param bar_width Numeric value for width of the bars. Values from 1 to 10, default is 10 for widest bar. 
#' 
#' @return ggplot2 object
waterfall_plot <- function(waterfall_data, 
                           label = "all", 
                           arrows = FALSE, 
                           connectors = TRUE,
                           fill_colors = NULL,
                           bar_width = 1) {
  
  # add intermediary values for plotting
  computed_waterfall <- waterfall_data %>% 
    compute_waterfall %>%  
    add_colors(fill_colors)
  
  # create a basic plot object
  plot_object <- computed_waterfall %>% 
    ggplot() +
    geom_rect(aes(xmin = rownum - 0.1, xmax = rownum + 0.6, ymin = ymin, ymax = ymax, fill = fill), 
              fill = computed_waterfall$fill, 
              alpha = 0.9) +
    scale_x_continuous(label = computed_waterfall$category,
                       breaks = computed_waterfall$rownum + 0.25) +
    labs(x = NULL, y = NULL)
  
  
  # add plot options
  plot_object + 
    add_arrows(computed_waterfall, arrows) + 
    add_connectors(computed_waterfall, connectors) +
    add_label(computed_waterfall, label)
  
  
} 





#' Given a bar width between 1 and 10, computes the adders to align plot objects
#' 
#' @param bar_width Numeric
#' 
#' @return Named list with adders for different plot elements
compute_bar_width_adders <- function(bar_width) {
  
  
}

