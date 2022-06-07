#' Define colors for subtotals, up and down bars
#' 
#' @return Data frame with fill column
add_colors <- function(computed_waterfall, fill_colors) {
  
  # check if options passed are valid
  if (!is.null(fill_colors) &
      (!is.list(fill_colors) || 
       length(fill_colors) != 3 || 
       !all(c("total", "up", "down") %in% names(fill_colors)) ||
       !(class(as.vector(unlist(fill_colors))) %in% colors()))) {
    
    warning("Invalid fill_colors settings. If not NULL (default theme), fill colors must be passed as a named list with color names for total, up and down. Defaulting to base theme")
    
    fill_colors <- NULL
    
  }
  
  # return default theme if not
  if (is.null(fill_colors)) {
    fill_colors <- list(total = "gray40", up = "forestgreen", down = "tomato")
  } 
  
  # add fill column to waterfall
  computed_waterfall %>% 
    mutate(fill = ifelse(type == "total", fill_colors$total, 
                         ifelse(ymax <= ymin, 
                                fill_colors$down, 
                                fill_colors$up))) %>% 
    return
  
}



#' Add up and down arrors
#'
#' @return ggplot2 object with arrows
add_arrows <- function(computed_waterfall, arrows) {
  
  if (arrows == TRUE) {
    geom_segment(data = computed_waterfall %>% filter(type !=  'total'),
                 aes(x = rownum + 0.25, xend= rownum + 0.25, y = ymin, yend = ymax), 
                 arrow = arrow(type =  "closed", length = unit(0.1, "in")),
                 color = "white", alpha = 0.9) 
  } else { 
    return(NULL)
  }
  
}


#' Add horizontal connectors between bars
#' 
#' @return ggplot2 object with horizontal connectors
add_connectors <- function(computed_waterfall, connectors) {
  
  if (connectors == TRUE) {
    geom_segment(data = computed_waterfall %>% head(-1),
                 aes(x = rownum + 0.6, xend = rownum + 0.9, y = ymax, yend = ymax),
                 size = 0.5, alpha = 0.5,
                 linetype = 'dashed', color = 'gray')
  } else {
    return(NULL)
  }
}

#' Add label to totals, up and down bars, or no label at all
#' 
#' @param computed_waterfall
#' @param label 
#' 
#' @return ggplot2 objecty with label
add_label <- function(computed_waterfall, label) {
  
  if (!(label %in% c("all", "totals", "steps", "none"))) {
    
    warning("Label options are 'all, 'none', 'totals' or 'steps'. Defaulting to 'all'")
    label <- "all"
    
  }
  
  if (label == 'none') {
    return(NULL)
  }
  
  label_list <- list(geom_label(data = computed_waterfall %>% filter(type == 'total'),
                                aes(x = rownum + 0.25, y = ymax, label = cs),
                                color = 'gray', size = 3),
                     geom_label(data = computed_waterfall %>% filter(type != 'total' & ymin >= ymax),
                                aes(x = rownum + 0.25, y = ymean, label = paste0("-", abs(ymax-ymin))),
                                color = "tomato", size  = 3),
                     geom_label(data = computed_waterfall %>% filter(type != 'total' & ymin < ymax),
                                aes(x = rownum + 0.25, y = ymean, label = paste0("+", abs(ymax-ymin))),
                                color = "forestgreen", size  = 3)) 
  
  if (label == "all") {
    return(label_list)
  }
  
  if (label == "totals") {
    return(label_list[1])
  }
  
  if (label == "steps") {
    return(label_list[2:3])
  }
  
}