
spinal_segments_labels_vector <- c('L5-S1', 'L4-L5', 'L3-L4', 'L2-L3', 'L1-L2', 
                                   'T12-L1', 'T11-T12', 'T10-T11', 'T9-T10', 'T8-T9', 'T7-T8', 'T6-T7', 'T5-T6', 'T4-T5', 'T3-T4', 'T2-T3', 'T1-T2',
                                   'C7-T1', 'C6-C7', 'C5-C6', 'C4-C5', 'C3-C4', 'C2-C3', 'C1-C2')

spine_level_interspace_df <- tibble(level = c('L5-S1', 'L5', 'L4-L5', 'L4', 'L3-L4', 'L3', 'L2-L3', 'L2', 'L1-L2', 'L1', 'T12-L1', 'T12', 'T11-T12', 'T11', 'T10-T11', 'T10', 'T9-T10', 'T9', 'T8-T9', 'T8', 'T7-T8', 'T7', 'T6-T7', 'T6', 'T5-T6', 'T5', 'T4-T5', 'T4', 'T3-T4', 'T3', 'T2-T3', 'T2', 'T1-T2', 'T1', 'C7-T1', 'C7', 'C6-C7', 'C6', 'C5-C6', 'C5', 'C4-C5', 'C4', 'C3-C4', 'C3', 'C2-C3', 'C2', 'C1-C2')) %>%
  mutate(level_count = seq(from = 0.5, by = 0.5, length = nrow(.)))

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
get_spine_labels <- function(all_centroids = FALSE, centroid_labels_for_spline = FALSE) {
  if (all_centroids) {
    return(c("fem_head_center", 
             "s1_anterior_superior", "s1_posterior_superior", 
             "l5_centroid", "l4_centroid", "l3_centroid", "l2_centroid", "l1_centroid", 
             "t12_centroid", "t11_centroid", "t10_centroid", "t9_centroid", 
             "t8_centroid", "t7_centroid", "t6_centroid", "t5_centroid", 
             "t4_centroid", "t3_centroid", "t2_centroid", "t1_centroid", 
             "c7_centroid", "c6_centroid", "c5_centroid", "c4_centroid", "c3_centroid", 
             "c2_centroid"))
  } else if(centroid_labels_for_spline){
    return(c("l4_centroid", "l1_centroid", "t9_centroid", "t4_centroid", 
             "t1_centroid", "c2_centroid"))
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


# generate_spine_level_controls <- function(spine_level, 
#                                           spine_level_font_size = 11, 
#                                           return_as_full_table = TRUE) {
#   
#   label_style <- glue("font-size:{paste(spine_level_font_size)}px; color:darkblue; font-weight:bold; text-align:center; margin-top:0; margin-bottom:0")
#   
#   spine_level_id <- str_to_lower(str_replace_all(spine_level, "-", "_"))
#   
#   label_percent_width <- 30
#   button_percent_width <- (100-label_percent_width)/4
#   
#   row <- tags$tr(width = "100%",
#                  tags$td(width = paste0(button_percent_width, "%"),
#                          actionBttn(
#                            inputId = paste0(spine_level_id, "_lordosis_down_5"),
#                            label = "-5",
#                            style = "material-circle",
#                            size = "xs"
#                          )
#                  ),
#                  tags$td(width = paste0(button_percent_width, "%"),
#                          actionBttn(
#                            inputId = paste0(spine_level_id, "_lordosis_down_1"),
#                            label = "-1",
#                            style = "material-circle",
#                            size = "xs"
#                          )
#                  ),
#                  tags$td(width = paste0(label_percent_width, "%"), 
#                          tags$div(style = label_style, paste(spine_level))),
#                  tags$td(width = paste0(button_percent_width, "%"),
#                          actionBttn(
#                            inputId = paste0(spine_level_id, "_lordosis_up_1"),
#                            label = "+1",
#                            style = "material-circle",
#                            size = "xs"
#                          )
#                  ),
#                  tags$td(width = paste0(button_percent_width, "%"),
#                          actionBttn(
#                            inputId = paste0(spine_level_id, "_lordosis_up_5"),
#                            label = "+5",
#                            style = "material-circle",
#                            size = "xs"
#                          )
#                  ),
#   )
#   
#   if(return_as_full_table == TRUE){
#     return(tags$table(width = "100%",
#                       row))
#   }else{
#     return(row)
#   }
#   
# }

# generate_spine_level_controls <- function(spine_level) {
#   spine_level_id <- str_to_lower(str_replace_all(spine_level, "-", "_"))
#   
#   tags$tr( 
#     tags$div(
#       class = "btn-group",
#       role  = "group",
#       `aria-label` = "Basic example",
#       # -5 button
#       tags$button(
#         type  = "button",
#         class = "btn btn-secondary",
#         id    = paste0(spine_level_id, "_lordosis_down_5"),
#         "-5º"
#       ),
#       
#       # -1 button
#       tags$button(
#         type  = "button",
#         class = "btn btn-secondary",
#         id    = paste0(spine_level_id, "_lordosis_down_1"),
#         "-1º"
#       )
#     ),
#     
#     # Spine level label
#     tags$span(
#       spine_level,
#       style = "font-weight: bold; padding: 0 10px; color: darkblue;"
#     ),
#     tags$div(
#       class = "btn-group",
#       role  = "group",
#       `aria-label` = "Basic example",
#       # -5 button
#       # +1 button
#       tags$button(
#         type  = "button",
#         class = "btn btn-secondary",
#         id    = paste0(spine_level_id, "_lordosis_up_1"),
#         "+1º"
#       ),
#       
#       # +5 button
#       tags$button(
#         type  = "button",
#         class = "btn btn-secondary",
#         id    = paste0(spine_level_id, "_lordosis_up_5"),
#         "+5º"
#       )
#     ))
# }

generate_spine_level_controls <- function(spine_level) {
  spine_level_id <- str_to_lower(str_replace_all(spine_level, "-", "_"))
  
  left_button_group_style <- "font-weight: bold;
        padding: 0px; 
        float: right;
        border-radius: 10px;
        border: groove;
        margin: 1px;
        color: darkblue"  
  
  right_button_group_style <- "font-weight: bold;
        padding: 0px; 
        float: left;
        border-radius: 10px;
        border: groove;
        margin: 1px;
        color: darkblue"  
  
  left_button_style <- "border-right: 2px solid darkgrey;
  padding: 0px 10px;
  line-height: 1;
  font-size: 14px"  
  
  right_button_style <- "border-left: 2px solid darkgrey;
  padding: 0px 10px;
  line-height: 1;
  font-size: 14px" 
  
  spine_level_style <- "font-weight: bold;
        font-size: 12px;
        padding: 1px;
        line-height: 1;
        text-align: -webkit-center;
        color: darkblue"
  
  tags$tr(
    tags$td(
            class = "btn-group",
            role  = "group",
            `aria-label` = "Basic example",
            style = left_button_group_style,
            tags$button(
              type  = "button",
              class = "btn btn-secondary",
              style = left_button_style,
              id    = paste0(spine_level_id, "_lordosis_down_5"),
              "-5º"
            ),
            tags$button(
              type  = "button",
              class = "btn btn-secondary",
              style = right_button_style,
              id    = paste0(spine_level_id, "_lordosis_down_1"),
              "-1º"
            )
    ),
      tags$td(
        style = spine_level_style,
        spine_level
      ),
    tags$td(
      class = "btn-group",
      role  = "group",
      `aria-label` = "Basic example",
      style = right_button_group_style,
      tags$button(
        type  = "button",
        class = "btn btn-secondary",
        style = left_button_style,
        id    = paste0(spine_level_id, "_lordosis_up_1"),
        "+1º"
      ),
      tags$button(
        type  = "button",
        class = "btn btn-secondary",
        style = right_button_style,
        id    = paste0(spine_level_id, "_lordosis_up_5"),
        "+5º"
      )
    ),
  )
  
}

update_spine_segmental_planning_df_function <- function(spine_segmental_planning_df,
                                                        spine_interspace_input, 
                                                        change) {
  spine_segmental_planning_df$df <- spine_segmental_planning_df$df %>%
    mutate(adjustment = if_else(spine_interspace == spine_interspace_input, adjustment + change, adjustment))
    # mutate(adjustment_performed = if_else(spine_interspace == spine_interspace_input, "no", adjustment_performed))
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


########################### CONSTRUCTION ROD FUNCTION ##############################


jh_construct_rod_coordinates_function <- function(planned_spine_coord_df,
                                                  uiv = "T4",
                                                  liv = "Pelvis",
                                                  spine_orientation = "left",
                                                  contouring_percent = 100) {
  
  # Validate input
  contouring_percent <- max(0, min(100, as.numeric(contouring_percent)))
  if(is.na(contouring_percent)) contouring_percent <- 100
  
  spine_levels_ordered <- c("pelvis", "s1", "l5", "l4", "l3", "l2", "l1",
                            "t12", "t11", "t10", "t9", "t8", "t7", "t6", "t5",
                            "t4", "t3", "t2", "t1",
                            "c7", "c6", "c5", "c4", "c3", "c2")
  
  uiv_lower <- str_to_lower(uiv)
  liv_lower <- str_to_lower(liv)
  liv_lower <- if(liv_lower == "sacrum") "s1" else liv_lower
  
  uiv_idx <- which(spine_levels_ordered == uiv_lower)
  liv_idx <- which(spine_levels_ordered == liv_lower)
  
  if(length(uiv_idx) == 0 || length(liv_idx) == 0) stop("Invalid UIV or LIV specified")
  
  instrumented_levels <- spine_levels_ordered[liv_idx:uiv_idx]
  
  # ── Get s1 posterior points for iliac extension ───────────────────────────
  s1_post_df <- planned_spine_coord_df %>%
    filter(spine_level == "s1", vert_point %in% c("sp", "ip"))
  
  s1_sp <- c(filter(s1_post_df, vert_point == "sp")$x,
             filter(s1_post_df, vert_point == "sp")$y)
  s1_ip <- c(filter(s1_post_df, vert_point == "ip")$x,
             filter(s1_post_df, vert_point == "ip")$y)
  
  # ── Compute rod contact point for each instrumented vertebra ──────────────
  rod_points_df <- planned_spine_coord_df %>%
    filter(spine_level %in% instrumented_levels,
           vert_point %in% c("sa", "sp")) %>%
    pivot_wider(names_from = vert_point, values_from = c(x, y)) %>%
    filter(!is.na(x_sp), !is.na(x_sa)) %>%
    mutate(
      rod_x = 2 * x_sp - x_sa,
      rod_y = 2 * y_sp - y_sa
    ) %>%
    mutate(spine_level = factor(spine_level,
                                levels = spine_levels_ordered,
                                ordered = TRUE)) %>%
    arrange(spine_level) %>%
    select(spine_level, x = rod_x, y = rod_y)
  
  # ── Add distal extension depending on LIV ────────────────────────────────
  if(liv_lower == "pelvis"){
    # Extend to iliac screw position
    s1_rod_point <- c(filter(rod_points_df, spine_level == "s1")$x,
                      filter(rod_points_df, spine_level == "s1")$y)
    
    pelvis_rod_point <- s1_ip + s1_rod_point - s1_sp
    pelvis_rod_point <- jh_get_point_along_line_function(
      coord_a        = s1_rod_point,
      coord_b        = pelvis_rod_point,
      percent_a_to_b = 0.4
    )
    
    rod_points_df <- tibble(
      spine_level = factor("pelvis", levels = spine_levels_ordered, ordered = TRUE),
      x           = pelvis_rod_point[1],
      y           = pelvis_rod_point[2]
    ) %>%
      bind_rows(rod_points_df) %>%
      arrange(spine_level)
    
  } else {
    # For any other LIV, extend distally past the LIV vertebra's ip
    liv_post_df <- planned_spine_coord_df %>%
      filter(spine_level == liv_lower, vert_point %in% c("sp", "ip"))
    
    if(nrow(liv_post_df) >= 2){
      liv_sp <- c(filter(liv_post_df, vert_point == "sp")$x,
                  filter(liv_post_df, vert_point == "sp")$y)
      liv_ip <- c(filter(liv_post_df, vert_point == "ip")$x,
                  filter(liv_post_df, vert_point == "ip")$y)
      
      liv_rod_point <- c(filter(rod_points_df, spine_level == liv_lower)$x,
                         filter(rod_points_df, spine_level == liv_lower)$y)
      
      # Reflect liv_ip through liv_sp to get the distal extension direction
      liv_distal_point <- liv_ip + liv_rod_point - liv_sp
      
      extension_percent <- if(liv_lower == "s1") 0.3 else 0.75

      liv_extension <- jh_get_point_along_line_function(
        coord_a        = liv_rod_point,
        coord_b        = liv_distal_point,
        percent_a_to_b = extension_percent
      )
      
      # liv_extension <- liv_distal_point
      
      # Add as plain tibble row, arrange by y to maintain order
      rod_points_df <- rod_points_df %>%
        bind_rows(
          tibble(
            spine_level = factor(liv_lower,
                                 levels = spine_levels_ordered,
                                 ordered = TRUE),
            x           = liv_extension[1],
            y           = liv_extension[2]
          )
        ) %>%
        arrange(y)
    }
  }
  
  # ── Build the rod curve ───────────────────────────────────────────────────
  y_seq <- seq(min(rod_points_df$y), max(rod_points_df$y), length.out = 200)
  
  if(contouring_percent == 0 || nrow(rod_points_df) < 3){
    rod_coord_df <- tibble(
      y = y_seq,
      x = approx(
        x    = c(min(rod_points_df$y), max(rod_points_df$y)),
        y    = c(rod_points_df$x[which.min(rod_points_df$y)],
                 rod_points_df$x[which.max(rod_points_df$y)]),
        xout = y_seq
      )$y
    )
  } else {
    straight_x <- approx(
      x    = c(min(rod_points_df$y), max(rod_points_df$y)),
      y    = c(rod_points_df$x[which.min(rod_points_df$y)],
               rod_points_df$x[which.max(rod_points_df$y)]),
      xout = y_seq
    )$y
    
    n_unique   <- length(unique(rod_points_df$y))
    df_target  <- max(2, min(n_unique - 1, nrow(rod_points_df)))
    
    spline_x <- tryCatch({
      fit <- smooth.spline(x = rod_points_df$y, y = rod_points_df$x, df = df_target)
      predict(fit, y_seq)$y
    }, warning = function(w){
      fit <- smooth.spline(x = rod_points_df$y, y = rod_points_df$x, df = 2)
      predict(fit, y_seq)$y
    }, error = function(e){
      approx(x = rod_points_df$y, y = rod_points_df$x, xout = y_seq)$y
    })
    
    blend <- contouring_percent / 100
    rod_coord_df <- tibble(
      y = y_seq,
      x = (1 - blend) * straight_x + blend * spline_x
    )
  }
  
  rod_coord_df
}



# ── Helper: generate rod template plot and save to file ──────────────────────
generate_rod_template_pdf_function <- function(planned_vert_coord_df, rod_coord_df, rod_uiv, rod_liv, file) {
  
  spine_levels <- c("pelvis", "s1", "l5", "l4", "l3", "l2", "l1",
                    "t12", "t11", "t10", "t9", "t8", "t7", "t6", "t5", 
                    "t4", "t3", "t2", "t1",
                    "c7", "c6", "c5", "c4", "c3", "c2", "c1")
  
  origin_x <- rod_coord_df$x[[1]]
  origin_y <- rod_coord_df$y[[1]]
  
  instrumented_vert_centered_df <- planned_vert_coord_df %>%
    mutate(spine_level = factor(spine_level, levels = spine_levels, ordered = TRUE)) %>%
    group_by(spine_level) %>%
    filter(spine_level >= str_to_lower(rod_liv) & spine_level <= str_to_lower(rod_uiv)) %>%
    mutate(x = x - origin_x, y = y - origin_y)
  
  rod_coord_centered_df <- rod_coord_df %>%
    mutate(x = x - origin_x, y = y - origin_y)
  
  # ── Shift so bottom-left is (0, 0) ───────────────────────────────────────
  all_x <- c(rod_coord_centered_df$x, instrumented_vert_centered_df$x)
  all_y <- c(rod_coord_centered_df$y, instrumented_vert_centered_df$y)
  
  x_shift <- floor(min(all_x, na.rm = TRUE) / 20) * 20
  y_shift <- floor(min(all_y, na.rm = TRUE) / 50) * 50
  
  instrumented_vert_centered_df <- instrumented_vert_centered_df %>%
    mutate(x = x - x_shift, y = y - y_shift)
  
  rod_coord_centered_df <- rod_coord_centered_df %>%
    mutate(x = x - x_shift, y = y - y_shift)
  
  # ── Grid dimensions (now starting from 0) ────────────────────────────────
  x_range <- range(c(rod_coord_centered_df$x, instrumented_vert_centered_df$x), na.rm = TRUE)
  y_range <- range(c(rod_coord_centered_df$y, instrumented_vert_centered_df$y), na.rm = TRUE)
  
  x_grid_min <- 0
  y_grid_min <- 0
  x_grid_max <- ceiling(x_range[[2]] / 20) * 20
  y_grid_max <- ceiling(y_range[[2]] / 50) * 50
  
  grid_corners_df <- expand_grid(x = c(x_grid_min, x_grid_max), y = c(y_grid_min, y_grid_max))
  
  y_gridline_df   <- tibble(x_min = x_grid_min, x_max = x_grid_max,
                            y = seq(y_grid_min, y_grid_max, by = 50))
  x_gridline_df   <- tibble(x = seq(x_grid_min, x_grid_max, by = 50),
                            y_min = y_grid_min, y_max = y_grid_min + 15)
  
  spine_levels_labels_df <- instrumented_vert_centered_df %>%
    group_by(spine_level) %>%
    summarise(center_x = mean(x), center_y = mean(y), .groups = "drop") %>%
    mutate(spine_level = str_to_upper(spine_level))
  
  rod_plot <- ggplot() +
    geom_polygon(data = instrumented_vert_centered_df,
                 aes(x = x, y = y, group = spine_level),
                 color = "grey50", fill = NA, alpha = 0.3) +
    geom_segment(data = y_gridline_df,
                 aes(x = x_min, xend = x_max, y = y, yend = y),
                 color = "grey75", linetype = "dashed") +
    draw_text(text = paste0(y_gridline_df$y, "mm"),
              x = x_grid_max - 5,
              y = y_gridline_df$y + 3,
              size = 9, hjust = 1) +
    geom_segment(data = x_gridline_df,
                 aes(x = x, xend = x, y = y_min, yend = y_max),
                 color = "grey75", linetype = "dashed") +
    draw_text(text = paste0(x_gridline_df$x, "mm"),
              x = x_gridline_df$x, y = x_gridline_df$y_max + 5, size = 7) +
    draw_text(text = spine_levels_labels_df$spine_level,
              x    = spine_levels_labels_df$center_x,
              y    = spine_levels_labels_df$center_y, size = 8) +
    geom_path(data = rod_coord_centered_df,
              aes(x = x, y = y),
              color = "blue", linewidth = 2, lineend = "round", linejoin = "round") +
    geom_point(data = grid_corners_df, aes(x = x, y = y), color = "red") +
    coord_fixed(expand = FALSE,
                xlim = c(x_grid_min, x_grid_max),
                ylim = c(y_grid_min, y_grid_max)) +
    theme_void()
  
  plot_width_in  <- diff(c(x_grid_min, x_grid_max)) / 25.4
  plot_height_in <- diff(c(y_grid_min, y_grid_max)) / 25.4
  

  ggsave(file,
         plot      = rod_plot,
         device    = cairo_pdf,
         width     = plot_width_in,
         height    = plot_height_in,
         units     = "in",
         dpi       = 300,
         limitsize = FALSE)
}

# generate_rod_template_pdf <- function(planned_vert_coord_df, rod_coord_df, rod_uiv, rod_liv, file) {
#   
#   spine_levels <- c("pelvis", "s1", "l5", "l4", "l3", "l2", "l1",
#                     "t12", "t11", "t10", "t9", "t8", "t7", "t6", "t5", 
#                     "t4", "t3", "t2", "t1",
#                     "c7", "c6", "c5", "c4", "c3", "c2", "c1")
#   
#   origin_x <- rod_coord_df$x[[1]]
#   origin_y <- rod_coord_df$y[[1]]
#   
#   instrumented_vert_centered_df <- planned_vert_coord_df %>%
#     mutate(spine_level = factor(spine_level, levels = spine_levels, ordered = TRUE)) %>%
#     group_by(spine_level) %>%
#     filter(spine_level >= str_to_lower(rod_liv) & spine_level <= str_to_lower(rod_uiv)) %>%
#     mutate(x = x - origin_x, y = y - origin_y)
#   
#   rod_coord_centered_df <- rod_coord_df %>%
#     mutate(x = x - origin_x, y = y - origin_y)
#   
#   x_range <- range(c(rod_coord_centered_df$x, instrumented_vert_centered_df$x), na.rm = TRUE)
#   y_range <- range(c(rod_coord_centered_df$y, instrumented_vert_centered_df$y), na.rm = TRUE)
#   
#   y_grid_breaks <- 50
#   y_grid_max <- ceiling(y_range[[2]] / y_grid_breaks) * y_grid_breaks
#   y_grid_min <- floor(y_range[[1]]   / y_grid_breaks) * y_grid_breaks
#   x_grid_max <- ceiling(x_range[[2]] / 20) * 20
#   x_grid_min <- floor(x_range[[1]]   / 20) * 20
#   
#   grid_corners_df    <- expand_grid(x = c(x_grid_min, x_grid_max), y = c(y_grid_min, y_grid_max))
#   y_gridline_df      <- tibble(x_min = x_grid_min, x_max = x_grid_max,
#                                y = seq(y_grid_min, y_grid_max, by = 50))
#   x_gridline_df      <- tibble(x = seq(x_grid_min, x_grid_max, by = 40),
#                                y_min = y_grid_min, y_max = y_grid_min + 25) %>% slice(-1)
#   
#   spine_levels_labels_df <- instrumented_vert_centered_df %>%
#     group_by(spine_level) %>%
#     summarise(center_x = mean(x), center_y = mean(y), .groups = "drop") %>%
#     mutate(spine_level = str_to_upper(spine_level))
#   
#   rod_plot <- ggplot() +
#     geom_polygon(data = instrumented_vert_centered_df,
#                  aes(x = x, y = y, group = spine_level),
#                  color = "grey50", fill = NA, alpha = 0.3) +
#     geom_segment(data = y_gridline_df,
#                  aes(x = x_min, xend = x_max, y = y, yend = y),
#                  color = "grey75", linetype = "dashed") +
#     draw_text(text = paste0(y_gridline_df$y, "mm"),
#               x = y_gridline_df$x_min + 20, y = y_gridline_df$y, size = 7) +
#     geom_segment(data = x_gridline_df,
#                  aes(x = x, xend = x, y = y_min, yend = y_max),
#                  color = "grey75", linetype = "dashed") +
#     draw_text(text = paste0(x_gridline_df$x, "mm"),
#               x = x_gridline_df$x, y = x_gridline_df$y_max, size = 7) +
#     draw_text(text = spine_levels_labels_df$spine_level,
#               x = spine_levels_labels_df$center_x,
#               y = spine_levels_labels_df$center_y, size = 8) +
#     geom_path(data = rod_coord_centered_df,
#               aes(x = x, y = y),
#               color = "blue", linewidth = 2, lineend = "round", linejoin = "round") +
#     geom_point(data = grid_corners_df, aes(x = x, y = y), color = "red") +
#     coord_fixed(expand = FALSE,
#                 xlim = c(x_grid_min, x_grid_max),
#                 ylim = c(y_grid_min, y_grid_max)) +
#     theme_void()
#   
#   plot_width_in  <- diff(c(x_grid_min, x_grid_max)) / 25.4
#   plot_height_in <- diff(c(y_grid_min, y_grid_max)) / 25.4
#   
#   ggsave(file,
#          plot      = rod_plot,
#          device    = cairo_pdf,
#          width     = plot_width_in,
#          height    = plot_height_in,
#          units     = "in",
#          dpi       = 300,
#          limitsize = FALSE)
# }


# jh_construct_rod_coordinates_function <- function(planned_spine_coord_df,
#                                                   uiv = "T4", 
#                                                   liv = "Pelvis",
#                                                   spine_orientation = "left",
#                                                   number_of_knots = 5){
#   
#   if(number_of_knots < 4){
#     number_of_knots <- 4
#   }
#   if(number_of_knots > 10){
#     number_of_knots <- 10
#   }
#   
#   spine_level_numeric_df <-  planned_spine_coord_df %>%
#     select(spine_level) %>%
#     distinct()%>%
#     mutate(spine_level = fct_inorder(spine_level)) %>%
#     mutate(spine_level_numeric = as.numeric(spine_level)) 
#   
#   ### UIV first ####
#   uiv_numeric <- filter(spine_level_numeric_df, spine_level == str_to_lower(uiv))$spine_level_numeric
#   
#   
#   ### LIV NOW ###
#   if(str_to_lower(liv) == "pelvis" | str_to_lower(liv) == "s1" | str_to_lower(liv) == "sacrum"){
#     
#     liv_numeric <- 0
#     
#     s1_post_points_df <- planned_spine_coord_df %>%
#       filter(spine_level == "s1") %>%
#       filter(vert_point %in% c("sp", "ip"))
#     
#     s1_sp <- c(filter(s1_post_points_df, vert_point == "sp")$x, filter(s1_post_points_df, vert_point == "sp")$y)
#     s1_ip <- c(filter(s1_post_points_df, vert_point == "ip")$x, filter(s1_post_points_df, vert_point == "ip")$y)
#     
#   }else{
#     liv_numeric <- filter(spine_level_numeric_df, spine_level == str_to_lower(liv))$spine_level_numeric
#   }
#   
#   # levels_spanned <- uiv_numeric - liv_numeric
#   # 
#   # if(number_of_knots > levels_spanned){
#   #   print("yes")
#   #   # number_of_knots <- levels_spanned
#   # }
#   
#   ### NOW GET THE DF ###
#   vert_sup_endplates_for_rod_df <- planned_spine_coord_df %>%
#     mutate(spine_level = fct_inorder(spine_level)) %>%
#     mutate(spine_level_numeric = as.numeric(spine_level)) %>%
#     filter(between(spine_level_numeric, liv_numeric, uiv_numeric)) %>%
#     filter(vert_point %in% c("sa", "sp"))
#   
#   new_rod_coord_df <- vert_sup_endplates_for_rod_df %>%
#     pivot_wider(names_from = vert_point, values_from = c(x, y)) %>%
#     filter(!is.na(x_sp), !is.na(x_sa)) %>%
#     mutate(
#       x = 2 * x_sp - x_sa,
#       y = 2 * y_sp - y_sa
#     ) %>%
#     select(spine_level, x, y) 
#   
#   
#   if(str_to_lower(liv) == "pelvis"){
#     rod_sup <- c(filter(new_rod_coord_df, spine_level == "s1")$x, filter(new_rod_coord_df, spine_level == "s1")$y)
#     rod_inf <- s1_ip + rod_sup - s1_sp
#     
#     rod_inf_coord <- jh_get_point_along_line_function(coord_a = rod_sup, coord_b = rod_inf, percent_a_to_b = 0.4)
#     
#     new_rod_coord_df <- tibble(spine_level = "pelvis", x = rod_inf_coord[1], y = rod_inf_coord[2]) %>%
#       bind_rows(new_rod_coord_df)
#     
#   }else if(str_to_lower(liv) == "s1" | str_to_lower(liv) == "sacrum"){
#     rod_sup <- c(filter(new_rod_coord_df, spine_level == "s1")$x, filter(new_rod_coord_df, spine_level == "s1")$y)
#     rod_inf <- s1_ip + rod_sup - s1_sp
#     
#     rod_inf_coord <- jh_get_point_along_line_function(coord_a = rod_sup, coord_b = rod_inf, percent_a_to_b = 0.2)
#     
#     new_rod_coord_df <- tibble(spine_level = "pelvis", x = rod_inf_coord[1], y = rod_inf_coord[2]) %>%
#       bind_rows(new_rod_coord_df)
#   }
#   
#   # # Clamp knots to valid range
#   # n_unique <- length(unique(new_rod_coord_df$y))
#   # number_of_knots <- min(number_of_knots, n_unique - 1)
#   # number_of_knots <- max(number_of_knots, 2)
#   # 
#   # # Fit a spline through the rod points
#   # spline_fit <- smooth.spline(x = new_rod_coord_df$y, 
#   #                             y = new_rod_coord_df$x, 
#   #                             nknots = number_of_knots)
#   # 
#   # y_seq <- seq(min(new_rod_coord_df$y), max(new_rod_coord_df$y), length.out = 200)
#   # 
#   # rod_coord_df <- tibble(
#   #   y = y_seq,
#   #   x = predict(spline_fit, y_seq)$y
#   # )
#   # Clamp to valid range
#   n_points <- nrow(new_rod_coord_df)
#   n_unique <- length(unique(new_rod_coord_df$y))
#   
#   # df must be between 2 and n_unique
#   df_target <- min(number_of_knots, n_unique - 1)
#   df_target <- max(df_target, 2)
#   
#   # Fit spline with fallback
#   spline_fit <- tryCatch({
#     smooth.spline(
#       x  = new_rod_coord_df$y,
#       y  = new_rod_coord_df$x,
#       df = df_target
#     )
#   }, warning = function(w){
#     # On warning, fall back to simpler fit
#     smooth.spline(
#       x  = new_rod_coord_df$y,
#       y  = new_rod_coord_df$x,
#       df = 2
#     )
#   }, error = function(e){
#     # On error, fit a straight line via lm and fake a spline-like object
#     # by using approx interpolation instead
#     NULL
#   })
#   
#   # If spline failed entirely, fall back to linear interpolation
#   if(is.null(spline_fit)){
#     y_seq <- seq(min(new_rod_coord_df$y), max(new_rod_coord_df$y), length.out = 200)
#     rod_coord_df <- tibble(
#       y = y_seq,
#       x = approx(x = new_rod_coord_df$y, y = new_rod_coord_df$x, xout = y_seq)$y
#     )
#   } else {
#     y_seq <- seq(min(new_rod_coord_df$y), max(new_rod_coord_df$y), length.out = 200)
#     rod_coord_df <- tibble(
#       y = y_seq,
#       x = predict(spline_fit, y_seq)$y
#     )
#   }
#   
#   # return_list <- list()
#   # 
#   # return_list$new_rod_coord_df <- new_rod_coord_df
#   # return_list$vert_sup_endplates_for_rod_df <- vert_sup_endplates_for_rod_df
#   # return_list$rod_coord_df <- rod_coord_df
#   
#   # return_list$uiv_numeric <- uiv_numeric
#   # return_list$liv_numeric <- liv_numeric
#   
#   return(rod_coord_df)
#   
# }

# 
# jh_construct_rod_coordinates_function <- function(planned_spine_coord_df,
#                                                   uiv = "T4", 
#                                                   liv = "Pelvis",
#                                                   spine_orientation = "left",
#                                                   number_of_knots = 10){
#   
#   spine_coord_for_rod_list <- jh_convert_spine_coord_df_to_lists_function(planned_spine_coord_df)
#   
#   if(str_to_lower(liv) == "pelvis"){
#     inferior_rod_point <- jh_get_point_along_line_function(coord_a = spine_coord_for_rod_list$l5$ip, 
#                                                            coord_b = spine_coord_for_rod_list$s1$sp, 
#                                                            percent_a_to_b = 10)
#     
#     # print("got to pelvis in rod function")
#   }else if(str_to_lower(liv) == "s1" | str_to_lower(liv) == "sacrum"){
#     
#     inferior_rod_point <- jh_get_point_along_line_function(coord_a = spine_coord_for_rod_list$l5$ip, 
#                                                            coord_b = spine_coord_for_rod_list$s1$sp, 
#                                                            percent_a_to_b = 5)
#     
#     # print("got to inf rod point in non-pelvis liv function")
#   }else{
#     inferior_rod_point <-  spine_coord_for_rod_list[[which(names(spine_coord_for_rod_list) == str_to_lower(liv))]]$ip
#   }
#   
#   
#   
#   superior_rod_point <-  spine_coord_for_rod_list[[which(names(spine_coord_for_rod_list) == str_to_lower(uiv)) + 1]]$ip
#   
#   s1_length <- jh_calculate_distance_between_2_points_function(point_1 = spine_coord_for_rod_list$s1$sa, 
#                                                                point_2 = spine_coord_for_rod_list$s1$sp)
#   
#   x_modifier <- if_else(spine_orientation == "left", s1_length, s1_length*-1)
#   
#   rod_coord_df <- planned_spine_coord_df %>%
#     filter(vert_point %in% c("s1_posterior_superior", "sp")) %>%
#     select(spine_level, x, y) %>%
#     filter(y < superior_rod_point[[2]])%>%
#     filter(y > inferior_rod_point[[2]]) %>% 
#     add_row(spine_level = "superior_rod", x = superior_rod_point[[1]], y = superior_rod_point[[2]]) %>%
#     add_row(spine_level = "inferior_rod", x = inferior_rod_point[[1]], y = inferior_rod_point[[2]]) %>%
#     mutate(x = x + x_modifier)%>%
#     arrange(y)
#   
#   
#   dist_vals <- sqrt(diff(rod_coord_df$x)^2 + diff(rod_coord_df$y)^2)
#   t_vals <- c(0, cumsum(dist_vals))
#   spx <- smooth.spline(t_vals, rod_coord_df$x)
#   spy <- smooth.spline(t_vals, rod_coord_df$y)
#   t_seq <- seq(from = min(t_vals), to = max(t_vals), length.out = number_of_knots)
#   
#   smoothed_rod_df <-tibble(x = predict(spx, t_seq)$y,
#                            y = predict(spy, t_seq)$y
#   )
#   
#   smoothed_rod_df <- smoothed_rod_df %>%
#     mutate(index = seq(from = 0, to = 90, length = length(smoothed_rod_df$x))) %>%
#     full_join(tibble(index = c(0:90))) %>%
#     arrange(index) %>%
#     mutate(x = zoo::na.spline(x)) %>%
#     mutate(y = zoo::na.spline(y)) 
#   
#   return(smoothed_rod_df)
#   
# }


jh_format_text_to_print_tibble_in_shiny_function <- function(df, tibble_name = "spine_df"){
  # Start building the code string
  code_str <- as.character(glue("{tibble_name} <- tibble("))
  
  # code_str <- "tibble_name <- tibble("
  
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



jh_make_rod_plot_for_pdf_function <- function(uiv, liv, planned_spine_vert_coord_df, rod_coord_df){
  
  return_list <- list()
  
  spine_levels <- c("pelvis", "sacrum", "l5", "l4", "l3", "l2", "l1",
                    "t12", "t11", "t10", "t9", "t8", "t7", "t6", "t5", "t4", "t3", "t2", "t1",
                    "c7", "c6", "c5", "c4", "c3", "c2", "c1")
  
  # Ensure spine_level is a factor with the correct order
  instrumented_vert_centered_df <- planned_spine_vert_coord_df %>%
    mutate(spine_level = factor(spine_level, levels = spine_levels, ordered = TRUE)) %>%
    group_by(spine_level) %>%
    filter(spine_level >= str_to_lower(liv) & spine_level <= str_to_lower(uiv)) %>%
    mutate(x = x - rod_coord_df$x[[1]])%>%
    mutate(y = y - rod_coord_df$y[[1]])
  
  rod_coord_centered_df <- rod_coord_df %>%
    mutate(x = x - rod_coord_df$x[[1]])%>%
    mutate(y = y - rod_coord_df$y[[1]])
  
  # # Calculate the actual plot range after centering
  x_range_centered <- range(c(rod_coord_centered_df$x, instrumented_vert_centered_df$x), na.rm = TRUE)
  y_range_centered <- range(c(rod_coord_centered_df$y, instrumented_vert_centered_df$y), na.rm = TRUE)
  
  y_grid_breaks <- 50
  
  y_grid_max <- ceiling(y_range_centered[[2]]/ y_grid_breaks) * y_grid_breaks
  y_grid_min <- floor(y_range_centered[[1]]/ y_grid_breaks) * y_grid_breaks
  x_grid_max <- ceiling(x_range_centered[[2]]/ 20) * 20
  x_grid_min <- floor(x_range_centered[[1]]/ 20) * 20
  
  grid_corners_df <- expand_grid(x = c(x_grid_min, x_grid_max), y = c(y_grid_min, y_grid_max))
  
  y_gridline_tibble_df <- tibble(x_min = x_grid_min, x_max = x_grid_max, y = seq(from = y_grid_min, to = y_grid_max, by = 50))
  
  x_gridline_tibble_df <- tibble(x = seq(from = x_grid_min, to = x_grid_max, by = 40), y_min = y_grid_min, y_max = y_grid_min + 25)%>%
    slice(-1)
  
  return_list$grid_corners_df <- grid_corners_df
  
  return_list$rod_plot_template <- ggplot() +
    geom_polygon(data = instrumented_vert_centered_df, aes(x = x, y = y, group = spine_level), color = "grey50", fill = NA, alpha = 0.3) +
    geom_segment(data = y_gridline_tibble_df, aes(x = x_min, xend = x_max, y = y, yend = y), color = "grey75", linetype = "dashed")+ 
    draw_text(text = paste0(as.character(y_gridline_tibble_df$y), "mm"), x = y_gridline_tibble_df$x_min + 20, y = y_gridline_tibble_df$y, size = 7) +
    geom_segment(data = x_gridline_tibble_df, aes(x = x, xend = x, y = y_min, yend = y_max), color = "grey75", linetype = "dashed")+ 
    draw_text(text = paste0(as.character(x_gridline_tibble_df$x), "mm"),
              x = x_gridline_tibble_df$x, y = x_gridline_tibble_df$y_max, size = 7) +
    geom_path(data = rod_coord_centered_df,
              aes(x = x, y = y),
              color = "blue",
              size = 2,
              lineend = "round",
              linejoin = "round") +
    geom_point(data = grid_corners_df, aes(x =x, y = y), color = "red") +
    coord_fixed(expand = FALSE, xlim = c(x_grid_min, x_grid_max), ylim = c(y_grid_min, y_grid_max)) +
    theme_void()
  
  return_list$plot_width_mm <- diff(c(x_grid_min, x_grid_max))  # Width in mm
  return_list$plot_height_mm <- diff(c(y_grid_min, y_grid_max)) # Height in mm
  
  # Convert mm to inches (1 inch = 25.4 mm)
  return_list$plot_width_in <- return_list$plot_width_mm / 25.4
  return_list$plot_height_in <- return_list$plot_height_mm / 25.4
  
  return(return_list)
  
}
