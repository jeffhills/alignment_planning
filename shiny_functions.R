
spinal_segments_labels_vector <- c('L5-S1', 'L4-L5', 'L3-L4', 'L2-L3', 'L1-L2', 
                                   'T12-L1', 'T11-T12', 'T10-T11', 'T9-T10', 'T8-T9', 'T7-T8', 'T6-T7', 'T5-T6', 'T4-T5', 'T3-T4', 'T2-T3', 'T1-T2',
                                   'C7-T1', 'C6-C7', 'C5-C6', 'C4-C5', 'C3-C4', 'C2-C3', 'C1-C2')

jh_spine_levels_factors_df <- tibble(level = c("c1", "c2", "c3", "c4", "c5", "c6", "c7", "t1", "t2", "t3", "t4", "t5", "t6", "t7", "t8", "t9", "t10", "t11", "t12", "l1", "l2", "l3", "l4", "l5")) %>%
  mutate(level_label = str_to_upper(level)) %>% 
  mutate(level_tilt = paste0(level, "_tilt")) %>%
  mutate(level_pelvic_angle = paste0(level, "_pelvic_angle")) %>%
  mutate(level_cobb_angle = paste0(level, "_s1")) %>%
  mutate(level_tilt_label = paste0(level_label, " Tilt")) %>%
  mutate(level_pelvic_angle_label = paste0(level_label, " Pelvic Angle")) %>%
  mutate(level_cobb_label = paste0(level_label, "-S1")) %>%
  select(level, level_tilt, level_pelvic_angle, level_cobb_angle, level_label, level_tilt_label, level_pelvic_angle_label, level_cobb_label) %>%
  mutate(across(everything(), fct_inorder)) %>%
  mutate(across(everything(), fct_rev)) %>%
  mutate(interspace = paste0(level_label, "-", lead(level_label))) %>%
  mutate(interspace = if_else(interspace == "L5-NA", "L5-S1", interspace)) %>%
  mutate(interspace = fct_inorder(interspace))

# Define the labels for both modes
get_spine_labels <- function(all_centroids = FALSE) {
  if (all_centroids) {
    return(c("fem_head_center", 
             "s1_anterior_superior", "s1_posterior_superior", 
             "l5_centroid", "l4_centroid", "l3_centroid", "l2_centroid", "l1_centroid", 
             "t12_centroid", "t11_centroid", "t10_centroid", "t9_centroid", 
             "t8_centroid", "t7_centroid", "t6_centroid", "t5_centroid", 
             "t4_centroid", "t3_centroid", "t2_centroid", "t1_centroid", 
             "c7_centroid", "c6_centroid", "c5_centroid", "c4_centroid", "c3_centroid", 
             "c2_centroid"))
  } else {
    return(c("fem_head_center", 
             "s1_anterior_superior", "s1_posterior_superior", 
             "l4_centroid", "l1_centroid", "t9_centroid", "t4_centroid", 
             "t1_centroid", "c2_centroid", 'calibration_1', 'calibration_2'))
  }
}

### THEME FUNCTION
jh_theme_function <- function(theme_template = theme_minimal(), 
                              axis_text_size = 8,
                              axis_title_size = 10,
                              axis_ticks_length = 1,
                              plot_title_size = 12,
                              axis_line_size = 0.5, 
                              top_margin = 5,
                              bottom_margin = 5, 
                              left_margin = 5, 
                              right_margin = 5){
  my_theme <- theme_template +
    theme(strip.text = element_text(face = "bold", size = 10, hjust = 0.5), 
          strip.placement = "outside",
          axis.text = element_text(size = axis_text_size),
          axis.title = element_text(face = "bold", size = axis_title_size, hjust = 0.5),
          axis.line = element_line(colour = "black", size = axis_line_size),
          axis.ticks = element_line(rel(axis_ticks_length)),
          plot.subtitle = element_text(size = axis_title_size, hjust = 0.5),
          plot.caption = element_text(size = 8, hjust = 1),
          plot.title = element_text(face = "bold", size = plot_title_size, hjust = 0.5),
          plot.margin = margin(top_margin,right_margin,bottom_margin,left_margin)
    )

  
  return(my_theme)
}

## SYMBOLS LIST
# https://cran.r-project.org/web/packages/utf8/vignettes/utf8.html
jh_symbols_list <- list(superscript_2 = "²",
                        degree_symbol = "º",
                        rho_label = sprintf('\u03c1'),
                        multiplication_symbol = "×", 
                        beta_symbol = "β", 
                        plus_minus_sign = "±")

# jh_symbols_list

# 0 1 2 3 4 5 6 7 8 9 a b c d e f
# 8                                
# 9                                
# a   ¡ ¢ £ ¤ ¥ ¦ § ¨ © ª « ¬   ® ¯
# b ° ± ² ³ ´ µ ¶ · ¸ ¹ º » ¼ ½ ¾ ¿
# c À Á Â Ã Ä Å Æ Ç È É Ê Ë Ì Í Î Ï
# d Ð Ñ Ò Ó Ô Õ Ö × Ø Ù Ú Û Ü Ý Þ ß
# e à á â ã ä å æ ç è é ê ë ì í î ï
# f ð ñ ò ó ô õ ö ÷ ø ù ú û ü ý þ ÿ

## COLOR PALETTES 

colorblind_palette <<- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
colorblind_palette_dark <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

color_palette_orange_red_9 <- RColorBrewer::brewer.pal(n = 9, name = "OrRd")
color_palette_paired_8 <- RColorBrewer::brewer.pal(12, name = "Paired")
color_palette_reds_9 <<- RColorBrewer::brewer.pal(n = 9, name = "Reds")
color_palette_blues_9 <<- RColorBrewer::brewer.pal(n = 9, name = "Blues")
color_palette_greens_9 <- RColorBrewer::brewer.pal(n = 9, name = "PuBuGn")
color_palette_dark_8 <- RColorBrewer::brewer.pal(n =8,name = "Dark2")

jh_colors_list <- list(
  # view_all_colors = colors_plot,
  colors_reds_4 = color_palette_reds_9[9:6],
  colors_reds_5 = color_palette_reds_9[9:5],
  colors_reds_6 = color_palette_reds_9[9:4],
  colors_blues_4 = color_palette_blues_9[9:6],
  colors_blues_5 = color_palette_blues_9[9:5],
  colors_blues_6 = color_palette_blues_9[9:4],
  colors_greens_4 = color_palette_greens_9[9:6],
  colors_greens_5 = color_palette_greens_9[9:5],
  colors_greens_6 = color_palette_greens_9[9:4], 
  colorblind_palette_light = colorblind_palette,
  colorblind_palette_dark = colorblind_palette_dark
)

jh_colors_list$view_all_colors_plot <- tibble(jh_colors_list) %>%
  mutate(group = names(jh_colors_list)) %>%
  unnest() %>%
  group_by(group) %>%
  mutate(count = row_number()) %>%
  ungroup() %>%
  ggplot(aes(x = 1, y = count, color = jh_colors_list)) +
  geom_point(size = 3) +
  geom_text(aes(label = jh_colors_list, x = 0.45, y = count)) +
  xlim(0, 1.1) +
  facet_wrap(facets = "group", scales = "free") +
  jh_theme_function() +
  scale_color_identity()



jh_view_color_palette_function <- function(color_palette){
  tibble(y = c(1:length(color_palette))) %>%
    mutate(color = color_palette) %>%
    ggplot() + 
    geom_hline(aes(yintercept = y, color = color), size = 3) + 
    scale_color_identity()
}


spine_levels_indexed_df <- tibble(level = c('occiput', 'o_c1', 'c1', 'c1_c2', 'c2', 'c2_c3', 'c3', 'c3_c4', 'c4', 'c4_c5', 'c5', 'c5_c6', 'c6', 'c6_c7', 'c7', 'c7_t1', 't1', 't1_t2', 't2', 't2_t3', 't3', 't3_t4', 't4', 't4_t5', 't5', 't5_t6', 't6', 't6_t7', 't7', 't7_t8', 't8', 't8_t9', 't9', 't9_t10', 't10', 't10_t11', 't11', 't11_t12', 't12', 't12_l1', 'l1', 'l1_l2', 'l2', 'l2_l3', 'l3', 'l3_l4', 'l4', 'l4_l5', 'l5', 'l5_s1', 's1', 'pelvis'), 
                                  level_index = c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6, 6.5, 7, 7.5, 8, 8.5, 9, 9.5, 10, 10.5, 11, 11.5, 12, 12.5, 13, 13.5, 14, 14.5, 15, 15.5, 16, 16.5, 17, 17.5, 18, 18.5, 19, 19.5, 20, 20.5, 21, 21.5, 22, 22.5, 23, 23.5, 24, 24.5, 25, 26))

jh_spine_level_numeric_function <- function(spine_level = "x", spine_index = 100){
  if(spine_level !='x'){
    spine_level <- str_to_lower(str_replace(spine_level, "-", "_"))
    
    return(spine_levels_indexed_df %>%
             filter(level == spine_level) %>%
             pull(level_index))
  }else if(spine_index < 28){
    return(spine_levels_indexed_df %>%
             filter(level_index == spine_index) %>%
             pull(level))
  }
}





############################ SHINY INPUT & MODAL FUNCTIONS #####################
create_spine_rigid_level_input_function <- function(segment_input_label){
  segment_id <- paste0("preop_", str_to_lower(str_replace_all(segment_input_label, pattern = "-", "_")), "_segment")
  rigid_segment_id <- str_replace_all(segment_id, "_segment", "_rigid_xray")
  segment_label <- segment_input_label
  div(
    class = "segment-input",
    prettyCheckbox(
      inputId = rigid_segment_id,
      label = segment_label,
      value = FALSE,
      bigger = TRUE,
      status = "danger",
      shape = "curve"
    )
  )
  # )
}


generate_spine_level_controls <- function(spine_level, 
                                          spine_level_font_size = 11, 
                                          return_as_full_table = TRUE) {
  
  label_style <- glue("font-size:{paste(spine_level_font_size)}px; color:darkblue; font-weight:bold; text-align:center; margin-top:0; margin-bottom:0")
  
  spine_level_id <- str_to_lower(str_replace_all(spine_level, "-", "_"))
  
  label_percent_width <- 30
  button_percent_width <- (100-label_percent_width)/4
  
  row <- tags$tr(width = "100%",
                 tags$td(width = paste0(button_percent_width, "%"),
                         actionBttn(
                           inputId = paste0(spine_level_id, "_lordosis_down_5"),
                           label = "-5",
                           style = "material-circle",
                           size = "xs"
                         )
                 ),
                 tags$td(width = paste0(button_percent_width, "%"),
                         actionBttn(
                           inputId = paste0(spine_level_id, "_lordosis_down_1"),
                           label = "-1",
                           style = "material-circle",
                           size = "xs"
                         )
                 ),
                 tags$td(width = paste0(label_percent_width, "%"), 
                         tags$div(style = label_style, paste(spine_level))),
                 tags$td(width = paste0(button_percent_width, "%"),
                         actionBttn(
                           inputId = paste0(spine_level_id, "_lordosis_up_1"),
                           label = "+1",
                           style = "material-circle",
                           size = "xs"
                         )
                 ),
                 tags$td(width = paste0(button_percent_width, "%"),
                         actionBttn(
                           inputId = paste0(spine_level_id, "_lordosis_up_5"),
                           label = "+5",
                           style = "material-circle",
                           size = "xs"
                         )
                 ),
  )
  
  if(return_as_full_table == TRUE){
    return(tags$table(width = "100%",
                      row))
  }else{
    return(row)
  }
  
}



update_spine_segmental_planning_df_function <- function(spine_segmental_planning_df, spine_interspace_input, change) {
  spine_segmental_planning_df$df <- spine_segmental_planning_df$df %>%
    mutate(adjustment = if_else(spine_interspace == spine_interspace_input, adjustment + change, adjustment))
}


update_spine_segmental_planning_table_observe_button_function <- function(spine_segmental_planning_df, 
                                                                          spine_interspace, session) {
  spine_interspace_id <- gsub("-", "_", tolower(spine_interspace))  # Ensure ID consistency
  
  observeEvent(session$input[[paste0(spine_interspace_id, "_lordosis_down_5")]], {
    spine_segmental_planning_df$df <- update_spine_segmental_planning_df_function(spine_segmental_planning_df = spine_segmental_planning_df,
                                                                                  spine_interspace, -5)
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  observeEvent(session$input[[paste0(spine_interspace_id, "_lordosis_down_1")]], {
    spine_segmental_planning_df$df <- update_spine_segmental_planning_df_function(spine_segmental_planning_df = spine_segmental_planning_df,
                                                                                  spine_interspace, -1)
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  observeEvent(session$input[[paste0(spine_interspace_id, "_lordosis_up_1")]], {
    spine_segmental_planning_df$df <- update_spine_segmental_planning_df_function(spine_segmental_planning_df = spine_segmental_planning_df,
                                                                                  spine_interspace, 1)
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  observeEvent(session$input[[paste0(spine_interspace_id, "_lordosis_up_5")]], {
    spine_segmental_planning_df$df <-  update_spine_segmental_planning_df_function(spine_segmental_planning_df = spine_segmental_planning_df,
                                                                                   spine_interspace, 5)
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
}



jh_construct_rod_coordinates_function <- function(planned_spine_coord_df, uiv = "T4", number_of_knots = 10){
  
  spine_coord_for_rod_list <- jh_convert_spine_coord_df_to_lists_function(planned_spine_coord_df)
  
  inferior_rod_point <- jh_get_point_along_line_function(coord_a = spine_coord_for_rod_list$l5$ip, 
                                                         coord_b = spine_coord_for_rod_list$sacrum$s1_posterior_superior, 
                                                         percent_a_to_b = 3)
  
  superior_rod_point <-  spine_coord_for_rod_list[[which(names(spine_coord_for_rod_list) == str_to_lower(uiv)) + 1]]$ip
  
  s1_length <- jh_calculate_distance_between_2_points_function(point_1 = spine_coord_for_rod_list$sacrum$s1_anterior_superior, point_2 = spine_coord_for_rod_list$sacrum$s1_posterior_superior)
  
  rod_coord_df <- planned_spine_coord_df %>%
    filter(vert_point %in% c("s1_posterior_superior", "sp")) %>%
    select(spine_level, x, y) %>%
    filter(y < superior_rod_point[[2]])%>%
    filter(y > inferior_rod_point[[2]]) %>% 
    add_row(spine_level = "superior_rod", x = superior_rod_point[[1]], y = superior_rod_point[[2]]) %>%
    add_row(spine_level = "inferior_rod", x = inferior_rod_point[[1]], y = inferior_rod_point[[2]]) %>%
    mutate(x = x + s1_length)%>%
    arrange(y)
  
  
  dist_vals <- sqrt(diff(rod_coord_df$x)^2 + diff(rod_coord_df$y)^2)
  t_vals <- c(0, cumsum(dist_vals))
  spx <- smooth.spline(t_vals, rod_coord_df$x)
  spy <- smooth.spline(t_vals, rod_coord_df$y)
  t_seq <- seq(from = min(t_vals), to = max(t_vals), length.out = number_of_knots)
  
  smoothed_rod_df <-tibble(x = predict(spx, t_seq)$y,
                           y = predict(spy, t_seq)$y
  )
  
  smoothed_rod_df <- smoothed_rod_df %>%
    mutate(index = seq(from = 0, to = 90, length = length(smoothed_rod_df$x))) %>%
    full_join(tibble(index = c(0:90))) %>%
    arrange(index) %>%
    mutate(x = zoo::na.spline(x)) %>%
    mutate(y = zoo::na.spline(y)) 
  
  return(smoothed_rod_df)
  
  
  # jh_smooth_rod_coords <- function(rod_coord_df, n_points = 500) {
  #   dist_vals <- sqrt(diff(rod_coord_df$x)^2 + diff(rod_coord_df$y)^2)
  #   t_vals <- c(0, cumsum(dist_vals))
  #   spx <- smooth.spline(t_vals, rod_coord_df$x)
  #   spy <- smooth.spline(t_vals, rod_coord_df$y)
  #   t_seq <- seq(from = min(t_vals), to = max(t_vals), length.out = n_points)
  #   tibble(
  #     x = predict(spx, t_seq)$y,
  #     y = predict(spy, t_seq)$y
  #   )
  # }
  
  # smoothed_rod_df <- jh_smooth_rod_coords(rod_coord_df = rod_coord_df, n_points = 500)
  
}


jh_format_text_to_print_tibble_in_shiny_function <- function(df){
  # Start building the code string
  code_str <- "spine_plan_df <- tibble("
  
  # Loop through each column of the tibble
  for(i in seq_along(df)) {
    col_name <- names(df)[i]
    col_values <- df[[i]]
    
    # If the column is character or factor, wrap values in quotes
    if(is.character(col_values) || is.factor(col_values)) {
      col_text <- paste0("'", col_values, "'", collapse = ", ")
    } else {
      col_text <- paste0(col_values, collapse = ", ")
    }
    
    # Add this column’s code to the overall string
    code_str <- paste0(
      code_str, "\n  ", col_name, " = c(", col_text, ")",
      if (i < ncol(df)) "," else ""
    )
  }
  
  # Close the tibble call
  code_str <- paste0(code_str, "\n)")
  
  # Return the full tibble definition as text
  code_str 
}

jh_reformat_vert_tibble <- function(tibble_input) {
  # Convert the tibble to a named list with each point as a vector
  named_list <- list(
    sp = c(tibble_input$x[tibble_input$vert_point == "sp"], tibble_input$y[tibble_input$vert_point == "sp"]),
    sa = c(tibble_input$x[tibble_input$vert_point == "sa"], tibble_input$y[tibble_input$vert_point == "sa"]),
    ia = c(tibble_input$x[tibble_input$vert_point == "ia"], tibble_input$y[tibble_input$vert_point == "ia"]),
    ip = c(tibble_input$x[tibble_input$vert_point == "ip"], tibble_input$y[tibble_input$vert_point == "ip"]),
    centroid = c(tibble_input$x[tibble_input$vert_point == "centroid"], tibble_input$y[tibble_input$vert_point == "centroid"])
  )
  return(named_list)
}



jh_convert_spine_coord_df_to_lists_function <- function(spine_coord_df = tibble(spine_level = character(), vert_point = character(), x = numeric(), y = numeric())){
  spine_coord_df_with_list_df <- spine_coord_df %>%
    mutate(coord_list = map2(.x = x, .y = y, .f = ~ c(.x, .y))) %>%
    select(spine_level, vert_point, coord_list) %>%
    group_by(spine_level) 
  
  names(spine_coord_df_with_list_df$coord_list) <- spine_coord_df_with_list_df$vert_point
  
  spine_coord_list <- map(.x = unique(spine_coord_df_with_list_df$spine_level),
                          .f = ~ spine_coord_df_with_list_df %>%
                            filter(spine_level == .x)%>%
                            pull(coord_list))
  
  names(spine_coord_list) <- unique(spine_coord_df_with_list_df$spine_level)
  return(spine_coord_list)
}   
