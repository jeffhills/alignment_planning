jh_calculate_slopes_perpendicular_to_spline_function <- function(df) {
  # df should have columns x and y in ascending vertebra order
  # Use spline interpolation to pass exactly through each centroid
  t <- seq_len(nrow(df))
  fx <- splinefun(t, df$x, method = "fmm")
  fy <- splinefun(t, df$y, method = "fmm")
  
  # Compute derivatives at each point
  dx <- fx(t, deriv = 1)
  dy <- fy(t, deriv = 1)
  
  # Slope of tangent line
  slope_tangent <- dy / dx
  
  # Slope of perpendicular line
  slope_perp <- -1 / slope_tangent
  
  slope <- slope_perp*(180/pi)
  
  # Return slopes (and optionally the original coordinates)
  # data.frame(x = df$x, y = df$y, slope_perp = slope_perp)
  return(df %>% mutate(slope = slope))
}

compute_square_coordinates <- function(sp, ip, endplate_width) {
  # Compute direction vector from sp to ip
  dir_vector <- ip - sp
  dir_vector <- dir_vector / sqrt(sum(dir_vector^2)) # Normalize
  
  # Compute perpendicular direction vector (90-degree rotation)
  perp_vector <- c(-dir_vector[2], dir_vector[1]) * endplate_width
  
  # Compute missing coordinates
  sa <- sp - perp_vector
  ia <- ip - perp_vector
  
  # Return as a list
  list(sp = sp, ip = ip, ia = ia, sa = sa)
}

jh_construct_odontoid_c2_coordinates_function <- function(c2_vert_coord_list = list()){
  if (length(c2_vert_coord_list) == 4) {
    new_c2_vert_list <- list()
    
    mid_post_wall_point <- jh_get_point_along_line_function(coord_a = c2_vert_coord_list$sp, coord_b = c2_vert_coord_list$ip,percent_a_to_b = 0.5)
    mid_sup_point <- jh_get_point_along_line_function(coord_a = c2_vert_coord_list$sp, coord_b = c2_vert_coord_list$sa,percent_a_to_b = 0.5)
    mid_inf_point <- jh_get_point_along_line_function(coord_a = c2_vert_coord_list$ip, coord_b = c2_vert_coord_list$ia,percent_a_to_b = 0.5)
    mid_c2 <- jh_get_point_along_line_function(coord_a = mid_sup_point, coord_b = mid_inf_point, percent_a_to_b = 0.5)
    
    new_c2_vert_list$sp <-  mid_sup_point
    new_c2_vert_list$mid <-  mid_c2
    new_c2_vert_list$mid_post <-  mid_post_wall_point
    new_c2_vert_list$ip <-  c2_vert_coord_list$ip
    new_c2_vert_list$ia <-  c2_vert_coord_list$ia
    new_c2_vert_list$sa <-  c2_vert_coord_list$sa
    
    return(new_c2_vert_list)
  }
}

jh_calculate_coordinate_from_slope_hypotenuse_function <- function(x, 
                                                                   y, 
                                                                   slope_deg, 
                                                                   hypotenuse, 
                                                                   spine_facing = "left", 
                                                                   posterior_or_anterior_coordinate = "posterior"){
  slope_rad <- slope_deg*(pi/180)
  
  y_adj <- sin(slope_rad)*hypotenuse
  
  x_adj <- cos(slope_rad)*hypotenuse
  
  if(spine_facing == "left"){
    if(posterior_or_anterior_coordinate == "posterior"){
      new_coord <- c(x + x_adj, y + y_adj)
    }else{
      new_coord <- c(x - x_adj, y - y_adj)
    }
    
  }else{
    if(posterior_or_anterior_coordinate == "posterior"){
      new_coord <- c(x - x_adj, y - y_adj)
    }else{
      new_coord <- c(x + x_adj, y + y_adj)
    }
  }
  
  return(new_coord)
}

jh_construct_spine_geom_sf_function <- function(vert_coord_list, baseline_spine = TRUE, fade_baseline_spine = FALSE) {
  polygon_list <- lapply(vert_coord_list, function(coords) {
    
    # Convert each coordinate into a matrix of (x, y) values
    coord_matrix <- do.call(rbind, lapply(coords, function(pt) {
      if (is.list(pt) && all(c("x", "y") %in% names(pt))) {
        # Extract x and y when they are named
        c(pt$x, pt$y)
      } else if (is.numeric(pt) && length(pt) == 2) {
        # Handle atomic vector case (already c(x, y))
        pt
      } else {
        stop("Invalid coordinate format detected")
      }
    }))
    
    # Ensure the polygon is closed (first point = last point)
    if (!identical(coord_matrix[1, ], coord_matrix[nrow(coord_matrix), ])) {
      coord_matrix <- rbind(coord_matrix, coord_matrix[1, ])
    }
    
    # Convert to st_polygon
    st_polygon(list(coord_matrix))
  })
  
  # Preserve names
  names(polygon_list) <- names(vert_coord_list)
  
  buffered_vert_polygon_list <- map(.x = polygon_list, .f = ~ st_sfc(st_buffer(st_buffer(.x, dist = -3, endCapStyle = "ROUND", joinStyle = "ROUND"), 
                                                                                      dist = 3, endCapStyle = "ROUND", joinStyle = "ROUND")))
  
  return_list <- list()
  # return_list$buffered_vert_polygon_list <- buffered_vert_polygon_list
  
  return_list$vert_polygon_df <- tibble(
    spine_level = names(buffered_vert_polygon_list),
    geometry = buffered_vert_polygon_list
  )  %>%
    unnest()
  
  if(baseline_spine){
    if(fade_baseline_spine){
      return_list$spine_geom_sf <- geom_sf(data = return_list$vert_polygon_df, aes(geometry = geometry),
                                           color = "lightgrey",
                                           fill = "lightgrey", 
                                           alpha = 0.25) 
      
    }else{
      return_list$spine_geom_sf <- geom_sf(data = return_list$vert_polygon_df, aes(geometry = geometry),
                                           color = "black",
                                           fill = "grey90", 
                                           alpha = 0.75)  
    }
  }else{
    return_list$spine_geom_sf <- geom_sf(data = return_list$vert_polygon_df, aes(geometry = geometry),
                                         color = "darkblue",
                                         fill = "grey90", 
                                         alpha = 0.8)
  }
  
  return(return_list)
}




jh_calculate_posterior_interspace_coord_from_coord_df_function <- function(vert_coord_df){
  vert_coord_df %>%
    filter(vert_point %in% c("sp", "ip")) %>%
    mutate(vert_coord = map2(.x = x, .y = y, .f = ~ c(.x, .y))) %>%
    select(spine_level, vert_point, vert_coord) %>%
    pivot_wider(names_from = vert_point, values_from = vert_coord) %>%
    mutate(interspace = paste0(spine_level, "_", lag(spine_level))) %>%
    mutate(posterior_interspace_coord = map2(.x = ip, .y = lag(sp), .f = ~ jh_calculate_point_along_line_function(coord_a = .x,
                                                                                                                  coord_b = .y, 
                                                                                                                  percent_a_to_b = 0.5)))  %>%
    select(interspace, posterior_interspace_coord) %>%
    filter(interspace != "sacrum_NA") %>%
    mutate(interspace = str_replace_all(interspace, "sacrum", "s1")) %>%
    unnest_wider(col = posterior_interspace_coord, names_sep = "_") %>%
    select(interspace, x = posterior_interspace_coord_1, y = posterior_interspace_coord_2)
  
}





jh_construct_spine_coord_df_from_centroids_function <- function(s1_posterior_superior = NULL , 
                                                                s1_anterior_superior = NULL, 
                                                                centroid_df = tibble(),
                                                                femoral_head_center = c(0,0),
                                                                spine_orientation = "left"){
  
  ## adjust all coordinates so that fem heads are at 0,0
  s1_posterior_superior <- s1_posterior_superior - femoral_head_center 
  s1_anterior_superior <- s1_anterior_superior - femoral_head_center 
  centroid_df <- centroid_df %>%
    mutate(x = x - femoral_head_center[[1]], 
           y = y - femoral_head_center[[2]])
  
  femoral_head_center <- c(0,0)
  
  return_list <- list()
  
  s1_width <- jh_calculate_distance_between_2_points_function(s1_posterior_superior, s1_anterior_superior)
  
  s1_inf_point <- jh_find_sacrum_inf_point_function(s1_posterior_sup = s1_posterior_superior, s1_anterior_sup = s1_anterior_superior, inf_length_multiplier = 1)
  
  ### First, fit a spline to locate all interspaces
  centroid_interspaces_df <- centroid_df %>%
    add_row(spine_point = "head_center", x = tail(centroid_df, 1)$x, y = tail(centroid_df, 1)$y + 100)%>%
    add_row(spine_point = "s1_inf", x = s1_inf_point[[1]], y = s1_inf_point[[2]]) %>%
    arrange(y) %>%
    separate(spine_point, into = c("spine_level", "spine_point"), sep = "_") %>%
    mutate(spine_numeric = row_number() -1 )%>%
    bind_rows(tibble(spine_level = str_to_lower(str_replace_all(rev(jh_spine_levels_factors_df$interspace), "-", "_"))) %>%
                mutate(spine_point = "interspace") %>%
                mutate(spine_numeric = row_number() + 0.5)) %>%
    arrange(spine_numeric) %>%
    mutate(y = zoo::na.spline(y)) %>%
    mutate(x = zoo::na.spline(x)) 
  
  # spine_orientation <- "left"
  
  
  ### With known endplate width, calculate the posterior wall or posterior disc space coordinate that is perpendicular 
  coord_w_post_wall_df <- jh_calculate_slopes_perpendicular_to_spline_function(df = centroid_interspaces_df) %>%
    mutate(endplate_width = seq(from = s1_width, to = s1_width*0.5, length = nrow(.))) %>%
    mutate(posterior_wall_point = pmap(.l = list(..1 = x,
                                                 ..2 = y, 
                                                 ..3 = slope, 
                                                 ..4 = endplate_width*0.5), 
                                       .f = ~ jh_calculate_coordinate_from_slope_hypotenuse_function(x = ..1, 
                                                                                                     y = ..2,
                                                                                                     slope_deg = ..3, 
                                                                                                     hypotenuse = ..4,
                                                                                                     spine_facing = spine_orientation,
                                                                                                     posterior_or_anterior_coordinate = "posterior")))  %>%
    mutate(posterior_wall_x = map(.x = posterior_wall_point, .f = ~ .x[[1]]))%>%
    mutate(posterior_wall_y = map(.x = posterior_wall_point, .f = ~ .x[[2]])) %>%
    select(-posterior_wall_point) %>%
    unnest() %>%
    mutate(posterior_wall_x = if_else(spine_point == "s1_center", s1_posterior_superior[[1]], posterior_wall_x),
           posterior_wall_y = if_else(spine_point == "s1_center", s1_posterior_superior[[2]], posterior_wall_y))
  
  
  ## construct anterior interspace coordinates for thoracic wedging correction later:
  ## construct anterior interspace coordinates for thoracic wedging correction later:
  anterior_wall_coord_for_wedged_vert_df  <- coord_w_post_wall_df %>%
    mutate(anterior_wall_point = pmap(.l = list(..1 = x,
                                                ..2 = y, 
                                                ..3 = slope, 
                                                ..4 = endplate_width*0.5), 
                                      .f = ~ jh_calculate_coordinate_from_slope_hypotenuse_function(x = ..1, 
                                                                                                    y = ..2,
                                                                                                    slope_deg = ..3, 
                                                                                                    hypotenuse = ..4,
                                                                                                    spine_facing = spine_orientation,                                                                                                posterior_or_anterior_coordinate = "anterior"))) %>%
    mutate(anterior_wall_x = map(.x = anterior_wall_point, .f = ~ .x[[1]]))%>%
    mutate(anterior_wall_y = map(.x = anterior_wall_point, .f = ~ .x[[2]])) %>%
    select(-anterior_wall_point) %>%
    unnest()  %>%
    filter(str_detect(spine_point, "center|interspace")) %>%
    filter(spine_level != "l5_s1") %>%
    select(spine_level, anterior_wall_x, anterior_wall_y) %>%
    mutate(spine_level = str_replace_all(spine_level, "s1", "l5_s1")) %>%
    mutate(disc_level = spine_level)%>%
    separate(spine_level, into = c("spine_level", "caudal"))%>%
    select(-caudal) %>%
    mutate(anterior_wall_x = if_else(spine_level == "l5", s1_anterior_superior[[1]], anterior_wall_x),
           anterior_wall_y = if_else(spine_level == "l5", s1_anterior_superior[[2]], anterior_wall_y)) %>%
    mutate(anterior_wall_coord = map2(.x = anterior_wall_x, .y = anterior_wall_y, 
                                      .f = ~ c(.x, .y))) %>%
    mutate(ia_by_disc = map2(.x = anterior_wall_coord, .y = lead(anterior_wall_coord),
                             .f = ~ jh_get_point_along_line_function(coord_a = .x, 
                                                                     coord_b = .y,
                                                                     percent_a_to_b = 0.1)))%>%
    mutate(sa_by_disc = map2(.x = anterior_wall_coord, .y = lead(anterior_wall_coord),
                             .f = ~ jh_get_point_along_line_function(coord_a = .x, 
                                                                     coord_b = .y,
                                                                     percent_a_to_b = 0.9))) %>%
    select(spine_level, ia_by_disc, sa_by_disc) %>%
    filter(spine_level != "head") %>%
    mutate(anterior_wall_height_by_disc = map2(.x = ia_by_disc, .y = sa_by_disc, .f = ~ jh_calculate_distance_between_2_points_function(point_1 = .x, point_2 = .y))) %>%
    unnest(anterior_wall_height_by_disc)
  
  # return_list$coord_w_post_wall_df <- coord_w_post_wall_df
  
  ## computer posterior corner coordinates
  posterior_corners_nested_df <- coord_w_post_wall_df %>%
    filter(str_detect(spine_point, "center|interspace")) %>%
    filter(spine_level != "l5_s1") %>%
    select(spine_level, posterior_wall_x, posterior_wall_y) %>%
    mutate(spine_level = str_replace_all(spine_level, "s1", "l5_s1")) %>%
    mutate(disc_level = spine_level) %>%
    separate(spine_level, into = c("spine_level", "caudal"))%>%
    select(-caudal) %>%
    mutate(posterior_wall_x = if_else(spine_level == "l5", s1_posterior_superior[[1]], posterior_wall_x),
           posterior_wall_y = if_else(spine_level == "l5", s1_posterior_superior[[2]], posterior_wall_y)) %>%
    mutate(posterior_wall_coord = map2(.x = posterior_wall_x, .y = posterior_wall_y, 
                                       .f = ~ c(.x, .y))) %>%
    mutate(ip = map2(.x = posterior_wall_coord, .y = lead(posterior_wall_coord),
                     .f = ~ jh_get_point_along_line_function(coord_a = .x, 
                                                             coord_b = .y,
                                                             percent_a_to_b = 0.1)))%>%
    mutate(sp = map2(.x = posterior_wall_coord, .y = lead(posterior_wall_coord),
                     .f = ~ jh_get_point_along_line_function(coord_a = .x, 
                                                             coord_b = .y,
                                                             percent_a_to_b = 0.9))) %>%
    filter(spine_level != "head") 
  

  vert_coord_nested_df <-  posterior_corners_nested_df %>%
    select(spine_level, ip, sp) %>%
    left_join(coord_w_post_wall_df %>%
                filter(spine_point == "centroid") %>%
                select(spine_level, endplate_width)) %>%
    mutate(posterior_wall_height = map2(.x = ip, .y = sp, .f = ~ jh_calculate_distance_between_2_points_function(point_1 = .x, point_2 = .y))) %>%
    unnest(posterior_wall_height) %>%
    left_join(anterior_wall_coord_for_wedged_vert_df) %>%
    mutate(vert_shape = if_else(anterior_wall_height_by_disc > posterior_wall_height, "square", "wedged")) %>%
    select(spine_level, endplate_width, vert_shape, ip, sp, ia_by_disc, sa_by_disc) %>%
    mutate(vert_coord_list_wedged = pmap(.l = list(..1 = sp, ..2 = ip, ..3 = ia_by_disc, ..4 = sa_by_disc), 
                                         .f = ~ list(sp = ..1, ip = ..2, ia = ..3, sa = ..4))) %>%
    mutate(vert_coord_list = pmap(.l = list(..1 = sp,
                                            ..2 = ip, 
                                            ..3 =endplate_width,
                                            ..4 = vert_shape), 
                                  .f = ~ compute_square_coordinates(sp = ..1, ip = ..2, endplate_width = ..3))) %>%
    mutate(vert_coord_list = if_else(vert_shape == "wedged", vert_coord_list_wedged, vert_coord_list))%>%
    select(spine_level, vert_shape, vert_coord_list)
  
  
  vert_coord_list <- vert_coord_nested_df$vert_coord_list
  
  names(vert_coord_list) <- vert_coord_nested_df$spine_level
  
  
  
  # fix C1
  vert_coord_list$c1$sp <- vert_coord_list$c2$sp
  vert_coord_list$c1$ip <- jh_get_point_along_line_function(coord_a = vert_coord_list$c2$sp, coord_b = vert_coord_list$c2$ip,percent_a_to_b = 0.3)
  vert_coord_list$c1$ia <- jh_get_point_along_line_function(coord_a = vert_coord_list$c2$sa, coord_b = vert_coord_list$c2$ia,percent_a_to_b = 0.3)
  vert_coord_list$c1$sa <- vert_coord_list$c2$sa
  
  #fix c2
  vert_coord_list$c2 <- jh_construct_odontoid_c2_coordinates_function(c2_vert_coord_list = vert_coord_list$c2)
  
  
  #add sacrum
  sac_inf_far <- jh_find_sacrum_inf_point_function(s1_posterior_sup = s1_posterior_superior, 
                                                   s1_anterior_sup = s1_anterior_superior, 
                                                   spine_facing = spine_orientation,
                                                   inf_length_multiplier = 2.5)
  
  
  sacrum <- list()
  sacrum$sa <- s1_anterior_superior
  sacrum$sp <- s1_posterior_superior
  sacrum$inf <- c(sac_inf_far[[1]], sac_inf_far[[2]])
  
  # Prepend the sacrum list to the beginning of vert_coord_list
  return_list$vert_coord_list <- c(list(sacrum = sacrum), vert_coord_list)
  

  ## construct interspace coord df
  # l5_s1_posterior_interspace_coord <- jh_calculate_point_along_line_function(coord_a = s1_posterior_superior,
  #                                                                            coord_b = vert_coord_list$l5$ip, 
  #                                                                            percent_a_to_b = 0.5)
  # 
  # return_list$interspace_coord_df <- posterior_corners_nested_df %>%
  #   select(interspace = disc_level, posterior_wall_coord) %>%
  #   unnest_wider(col = posterior_wall_coord, names_sep = "_")  %>%
  #   select(interspace, x = posterior_wall_coord_1, y = posterior_wall_coord_2) %>%
  #   mutate(x = if_else(interspace == "l5_s1", l5_s1_posterior_interspace_coord[[1]], x), 
  #          y = if_else(interspace == "l5_s1", l5_s1_posterior_interspace_coord[[2]], y))
  
  # names(return_list$interspace_list) <- return_list$interspace_coord_df$interspace
  
  ## assemble coordinates into df
  return_list$vert_coord_df <- enframe(return_list$vert_coord_list) %>%
    unnest() %>%
    mutate(vert_point = names(value)) %>%
    unnest_wider(value, names_sep = "_") %>%
    select(spine_level = name, vert_point, x = value_1, y = value_2) 
  
  ## construct interspace coord df
  return_list$interspace_coord_df <- jh_calculate_posterior_interspace_coord_from_coord_df_function(vert_coord_df = return_list$vert_coord_df)
  
  return_list$interspace_list <- (return_list$interspace_coord_df %>%
                                    mutate(coord = map2(.x = x, .y = y, .f = ~ c(.x, .y))))$coord
  
  names(return_list$interspace_list) <- return_list$interspace_coord_df$interspace
  
  # return_list$interspace_list
  
  ## add the new centroids to the return list 
  centroid_coord_list <- (centroid_df %>%
                            mutate(centroid_coord = map2(.x = x, .y = y, .f = ~ c(.x, .y))))$centroid_coord
  
  names(centroid_coord_list) <- str_remove_all(centroid_df$spine_point, pattern = "_center|_centroid")
  
  return_list$centroid_coord_list <- c(list(femoral_head = c(0,0)), centroid_coord_list)
  
  return(return_list)
  
} 




jh_spine_coord_list_to_sf_geoms_function <- function(vert_coord_list = list()){
  return_list <- list()
  vert_polygon_list <-  jh_construct_st_polygons_from_vert_coord_list_function(vert_coord_list = return_list$vert_coord_list)
  
  return_list$buffered_vert_polygon_list <- map(.x = vert_polygon_list, .f = ~ st_buffer(st_buffer(.x, dist = -3, endCapStyle = "ROUND", joinStyle = "ROUND"), 
                                                                                         dist = 3, endCapStyle = "ROUND", joinStyle = "ROUND"))
  
  return_list$vert_polygon_df <- tibble(
    spine_level = names(return_list$vert_polygon_list),
    geometry = st_sfc(return_list$vert_polygon_list)
  ) %>%
    st_as_sf()
  
  return(return_list)
}



jh_convert_spine_coord_df_to_named_list_function <- function(spine_df) {
  # Preserve the original order of spine_level
  spine_levels_order <- unique(spine_df$spine_level)
  
  spine_df %>%
    group_by(spine_level) %>%
    summarise(vert_list = list(setNames(map2(x, y, c), vert_point)), .groups = "drop") %>%
    arrange(factor(spine_level, levels = spine_levels_order)) %>%  # Ensure order is preserved
    deframe()
}



jh_calculate_vertebra_centroid_function <- function(vertebra_coords, vertebral_level) {
  
  if(str_detect(vertebral_level, "sacrum|s1")){
    centroid <- jh_calculate_point_along_line_function(coord_a = vertebra_coords$sp, coord_b = vertebra_coords$sa, percent_a_to_b = 0.5)
  }else if(vertebral_level == "c2"){
    centroid <- jh_calculate_point_along_line_function(coord_a = vertebra_coords$sa, coord_b = vertebra_coords$mid, percent_a_to_b = 0.5)
  }else{
    # Extract x and y coordinates
    xy_matrix <- do.call(rbind, vertebra_coords)
    
    # Compute centroid
    centroid_x <- mean(xy_matrix[, 1])
    centroid_y <- mean(xy_matrix[, 2])
    
    centroid <- c(centroid_x,centroid_y) 
  }
  return(centroid)
}

jh_construct_adjusted_spine_df_function<- function(sa_adjustment_df = tibble(),
                                                   spine_list = list(), 
                                                   spine_orientation = "left",
                                                   adjust_for_pt_change = TRUE){
  
  return_list <- list()
  
  nonzero_sa_df <- sa_adjustment_df %>%
    filter(adjustment != 0) %>%
    mutate(spine_interspace = str_to_lower(str_replace_all(spine_interspace, "-", "_"))) %>%
    separate(remove = FALSE, spine_interspace, into = c("cranial", "caudal"))%>%
    mutate(caudal = if_else(caudal == "s1", "sacrum", caudal))
  
  if(nrow(nonzero_sa_df)>0){

    adjusted_full_spine_coord_df <- spine_list$vert_coord_df
    
    for (i in c(1:length(nonzero_sa_df$spine_interspace))) {
      
      spine_to_adjust_df <- adjusted_full_spine_coord_df %>%
        filter(row_number() > max(which(adjusted_full_spine_coord_df$spine_level == paste0(nonzero_sa_df$caudal[[i]]))))
      
      interspace_rotation_point_df <- jh_calculate_posterior_interspace_coord_from_coord_df_function(vert_coord_df = adjusted_full_spine_coord_df) %>%
        filter(interspace == nonzero_sa_df$spine_interspace[[i]])
      
      proximal_adjusted_spine_df <- jh_rotate_proximal_spine_at_segment_function(spine_df = spine_to_adjust_df, 
                                                                        angle_degrees = nonzero_sa_df$adjustment[[i]], 
                                                                        point_of_rotation = c(interspace_rotation_point_df$x[[1]], interspace_rotation_point_df$y[[1]]), 
                                                                        spine_orientation = spine_orientation)
      
      
      adjusted_full_spine_coord_df <- adjusted_full_spine_coord_df %>%
        filter(spine_level %in% proximal_adjusted_spine_df$spine_level == FALSE) %>%
        union_all(proximal_adjusted_spine_df)
      
    }
    adjusted_full_spine_df <- adjusted_full_spine_coord_df
  
    return(adjusted_full_spine_df)
  }
}


jh_construct_adjusted_spine_list_function <- function(sa_adjustment_df = tibble(),
                                                      spine_list = list(), 
                                                      spine_orientation = "left",
                                                      adjust_for_pt_change = TRUE){
  
  return_list <- list()
  
  # sa_adjustments_nonzero_df <- sa_adjustment_df %>%
  #   mutate(spine_interspace = str_to_lower(spine_interspace)) %>%
  #   mutate(interspace_count = row_number())%>%
  #   mutate(spine_interspace = str_replace_all(spine_interspace, "-", "_")) %>%
  #   filter(adjustment != 0) %>%
  #   filter(adjustment_performed == "no")
  
  nonzero_sa_df <- sa_adjustment_df %>%
    filter(adjustment != 0) %>%
    mutate(spine_interspace = str_to_lower(str_replace_all(spine_interspace, "-", "_"))) %>%
    separate(remove = FALSE, spine_interspace, into = c("cranial", "caudal"))%>%
    mutate(caudal = if_else(caudal == "s1", "sacrum", caudal))
  
  if(nrow(nonzero_sa_df)>0){
    
    adjusted_full_spine_coord_df <- spine_list$vert_coord_df
    
    for (i in c(1:length(nonzero_sa_df$spine_interspace))) {
      
      spine_to_adjust_df <- adjusted_full_spine_coord_df %>%
        filter(row_number() > max(which(adjusted_full_spine_coord_df$spine_level == paste0(nonzero_sa_df$caudal[[i]]))))
      
      interspace_rotation_point_df <- jh_calculate_posterior_interspace_coord_from_coord_df_function(vert_coord_df = adjusted_full_spine_coord_df) %>%
        filter(interspace == nonzero_sa_df$spine_interspace[[i]])
      
      proximal_adjusted_spine_df <- jh_rotate_proximal_spine_at_segment_function(spine_df = spine_to_adjust_df, 
                                                                                 angle_degrees = nonzero_sa_df$adjustment[[i]], 
                                                                                 point_of_rotation = c(interspace_rotation_point_df$x[[1]], interspace_rotation_point_df$y[[1]]), 
                                                                                 spine_orientation = spine_orientation)
      
      
      adjusted_full_spine_coord_df <- adjusted_full_spine_coord_df %>%
        filter(spine_level %in% proximal_adjusted_spine_df$spine_level == FALSE) %>%
        union_all(proximal_adjusted_spine_df)
      
    }
    
    adjusted_full_spine_df <- adjusted_full_spine_coord_df
  
  if(adjust_for_pt_change == TRUE){
    
    preop_tilt_vpa_list <- jh_calculate_vertebral_tilt_and_vpas_from_coordinates_function(full_centroid_coord_list = spine_list$centroid_coord_list,
                                                                                          spine_orientation = spine_orientation)
    
    
    preop_c2_tilt <- preop_tilt_vpa_list$vert_tilt_list$c2
    preop_pt <- preop_tilt_vpa_list$vert_tilt_list$pt
    
    c2_coord_df <- adjusted_full_spine_df %>%
      filter(spine_level == "c2") %>%
      mutate(vert_coord = map2(.x = x, .y = y, .f = ~ c(.x, .y))) 
    
    c2centroid <- jh_calculate_point_along_line_function(coord_a = (filter(.data = c2_coord_df, vert_point == "sa"))$vert_coord[[1]], 
                                                         coord_b = (filter(.data = c2_coord_df, vert_point == "mid"))$vert_coord[[1]], 
                                                         percent_a_to_b = 0.5)
    
    postop_c2_tilt_pre_adjust <- jh_calculate_vertebral_tilt_function(vertebral_centroid = c2centroid, 
                                                                      femoral_head_center = c(0,0), 
                                                                      spine_orientation = spine_orientation, 
                                                                      pelvic_tilt_modifier = FALSE)
    
    postop_c2pa <- preop_pt + postop_c2_tilt_pre_adjust
    
    predicted_pt <- predict_postop_pt_function(preop_c2_tilt = preop_c2_tilt, 
                                               preop_pt = preop_pt, 
                                               postop_c2pa = postop_c2pa)
    
    pt_change <- preop_pt - predicted_pt
    
    adjusted_full_spine_df <- jh_rotate_spine_for_pt_adjustment_function_function(spine_df = adjusted_full_spine_df,
                                                                                  angle_degrees = pt_change,
                                                                                  point_of_rotation = c(0,0))
    
  }
  
  return_list$vert_coord_df <- adjusted_full_spine_df
  
  return_list$vert_coord_list <- jh_convert_spine_coord_df_to_named_list_function(spine_df = return_list$vert_coord_df)
  
  return_list$interspace_coord_df <- jh_calculate_posterior_interspace_coord_from_coord_df_function(vert_coord_df = return_list$vert_coord_df)
  
  return_list$interspace_list <- (return_list$interspace_coord_df %>%
                                    mutate(coord = map2(.x = x, .y = y, .f = ~ c(.x, .y))))$coord
  
  names(return_list$interspace_list) <- return_list$interspace_coord_df$interspace
  
  return_list$centroid_coord_list <- map2(.x = return_list$vert_coord_list, .y = names(return_list$vert_coord_list),
                                          .f = ~ jh_calculate_vertebra_centroid_function(vertebra_coords = .x, vertebral_level = .y))
  
  names(return_list$centroid_coord_list) <- str_replace_all(names(return_list$vert_coord_list), "sacrum", "s1")
  
  return_list$centroid_coord_list <- c(list(femoral_head = c(0,0)), return_list$centroid_coord_list)
  
  return_list$centroid_coord_list$c1 <- NULL
  
  # return_list$
  return_list$sa_adjustment_current_df <- sa_adjustment_df 
    # mutate(adjustment_performed = if_else(str_to_lower(str_replace_all(spine_interspace, "-", "_")) == paste0(sa_adjustments_nonzero_df$spine_interspace[[1]]), "yes", adjustment_performed))
  
  }else{
    
    return_list$vert_coord_df <- spine_list$vert_coord_df
      return_list$vert_coord_list <- spine_list$vert_coord_list
      return_list$interspace_coord_df <-spine_list$interspace_coord_df
      return_list$interspace_list <- spine_list$interspace_list
      
    return_list$centroid_coord_list <- spine_list$centroid_coord_list
    return_list$sa_adjustment_current_df <- sa_adjustment_df
    
  }
  
  return(return_list)
  
}


jh_construct_vpa_line_geoms_from_vert_coord_function <- function(vertebral_level, 
                                                                 centroid_coord_list, 
                                                                 line_size = 2, 
                                                                 line_color = "blue"){
  
  fem_head_center <- centroid_coord_list$femoral_head
  s1_center <- centroid_coord_list$s1
  
  level_centroid <- centroid_coord_list[[paste0(str_to_lower(vertebral_level))]]
  
  vpa_df <- tibble(spine_level = c("s1", "femoral_heads", paste("l1")), 
                   x = c(s1_center[[1]], fem_head_center[[1]], level_centroid[[1]]), 
                   y = c(s1_center[[2]], fem_head_center[[2]], level_centroid[[2]])
  )
  geom_path(data = vpa_df, 
            aes(x = x, y= y), 
            size = line_size, 
            color = line_color, 
            lineend = "round", 
            linejoin = "round") 
}
