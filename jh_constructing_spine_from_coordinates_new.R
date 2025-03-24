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

jh_construct_spine_geom_sf_function <- function(vert_coord_list, 
                                                baseline_spine = TRUE, 
                                                fade_baseline_spine = FALSE, return_vert_geoms_list = FALSE) {
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
  
  if(return_vert_geoms_list){
    return_list$polygon_list <- polygon_list
    return_list$buffered_vert_polygon_list <- buffered_vert_polygon_list
  }
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
                                                                calibration_modifier = 1,
                                                                spine_orientation = "left"){
  
  if(calibration_modifier != 1){
    femoral_head_center <- femoral_head_center*calibration_modifier
    s1_anterior_superior <- s1_anterior_superior*calibration_modifier
    s1_posterior_superior <- s1_posterior_superior*calibration_modifier
    centroid_df <- centroid_df %>%
      mutate(x = x*calibration_modifier, 
             y = y*calibration_modifier)
  }
  
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

jh_convert_spine_coord_list_to_df_function <- function(spine_coord_list, single_vertebra = FALSE) {
  if(single_vertebra){
    tibble(
      vert_point = names(spine_coord_list),
      x = sapply(spine_coord_list, `[[`, 1),
      y = sapply(spine_coord_list, `[[`, 2)
    )
  }else{
    enframe(map(.x = spine_coord_list,
                .f = ~ tibble(
                  vert_point = names(.x),
                  x = sapply(.x, `[[`, 1),
                  y = sapply(.x, `[[`, 2)
                ))) %>%
      unnest(value) %>%
      select(spine_level = name, everything()) 
  }
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

# jh_rotate_translate_vert_list_coord_function <- function(vert_coord_list,
#                                                          point_of_rotation,
#                                                          segment_angle_adjustment, 
#                                                          initial_translation_correction = c(0,0), 
#                                                          spine_orientation = "right") {
#   
#   spine_orientation_modifier = if_else(spine_orientation == "right", 1, -1)
#   
#   original_sp <- vert_coord_list$sp
#   
#   # Convert the angle from degrees to radians
#   angle_rad <- segment_angle_adjustment * pi / 180
#   
#   ## translate to new location
#   translated_vert_coord_list <- map(.x = vert_coord_list, .f = ~ .x + initial_translation_correction*spine_orientation_modifier)
#   
#   ## coords list to matrix
#   coords_matrix <- do.call(rbind, translated_vert_coord_list)
#   
#   # Translate the coordinates so the point of rotation is at the origin
#   translated_coords <- sweep(coords_matrix, 2, point_of_rotation, FUN = "-")
#   
#   # Define the rotation matrix
#   rotation_matrix <- matrix(c(cos(angle_rad), -sin(angle_rad),
#                               sin(angle_rad), cos(angle_rad)), ncol = 2)
#   
#   # Apply the rotation matrix to the translated coordinates
#   rotated_coords <- t(rotation_matrix %*% t(translated_coords))
#   
#   # Translate the coordinates back to the new rotated position (not the original position)
#   final_coords_matrix <- sweep(rotated_coords, 2, point_of_rotation, FUN = "+")
#   
#   row_names <- rownames(final_coords_matrix)
#   
#   # Convert matrix to list and apply row names
#   vert_rot_coord_list <- setNames(split(final_coords_matrix, row(final_coords_matrix)), rownames(final_coords_matrix))
#   
#   sp_translation <- original_sp - vert_rot_coord_list$sp
#   
#   return(list(vert_coord = vert_rot_coord_list, 
#               sp_translation = sp_translation))
#   
# }

# jh_calculate_pso_coordinates_function <- function(vertebral_coord_list, pso_angle) {
#   # Extract corners
#   sp <- vertebral_coord_list$sp
#   ip <- vertebral_coord_list$ip
#   sa <- vertebral_coord_list$sa
#   
#   # 2D rotation function (angle in degrees)
#   rotate_2d <- function(vec, angle_deg) {
#     angle_rad <- angle_deg * pi / 180
#     rot_mat <- matrix(c(cos(angle_rad), -sin(angle_rad),
#                         sin(angle_rad),  cos(angle_rad)),
#                       nrow = 2)
#     as.numeric(rot_mat %*% vec)
#   }
#   
#   # Vector from sa to sp
#   v1 <- sp - sa
#   # Rotate that vector by pso_angle
#   v2 <- rotate_2d(v1, pso_angle)
#   
#   # We want the intersection of:
#   #  1) The line from sa in the direction v2: sa + alpha*v2
#   #  2) The line from sp to ip: sp + t*(ip - sp)
#   #
#   # Solve alpha*v2 - t*(ip - sp) = (sp - sa)
#   
#   A <- ip - sp
#   B <- v2
#   C <- sp - sa
#   
#   # 2x2 system: [ Bx  -Ax ] [ alpha ] = [ Cx ]
#   #             [ By  -Ay ] [   t   ]   [ Cy ]
#   M <- matrix(c(B[1], -A[1],
#                 B[2], -A[2]), 
#               nrow = 2, byrow = TRUE)
#   alpha_t <- solve(M, C)
#   alpha <- alpha_t[1]
#   
#   new_sp <- sa + alpha * v2
#   
#   vertebral_coord_list$sp <- new_sp
#   vertebral_coord_list
# }

jh_rotate_translate_vert_list_coord_function <- function(vert_coord_list,
                                                         point_of_rotation,
                                                         segment_angle_adjustment, 
                                                         inferior_vertebra_original_list = list(),
                                                         inferior_vertebra_new_list = list(),
                                                         # initial_translation_correction = c(0,0), 
                                                         spine_orientation = "right") {
  
  initial_translation_correction <- inferior_vertebra_original_list$sp - inferior_vertebra_new_list$sp
  # initial_translation_correction <- c(0,0)
  
  spine_orientation_modifier = if_else(spine_orientation == "right", 1, -1)
  
  original_sp <- vert_coord_list$sp
  
  # Convert the angle from degrees to radians
  angle_rad <- segment_angle_adjustment * pi / 180
  
  ## translate to new location
  translated_vert_coord_list <- map(.x = vert_coord_list, .f = ~ .x + initial_translation_correction*spine_orientation_modifier)
  
  ## coords list to matrix
  coords_matrix <- do.call(rbind, translated_vert_coord_list)
  
  # Translate the coordinates so the point of rotation is at the origin
  translated_coords <- sweep(coords_matrix, 2, point_of_rotation, FUN = "-")
  
  # Define the rotation matrix
  rotation_matrix <- matrix(c(cos(angle_rad), -sin(angle_rad),
                              sin(angle_rad), cos(angle_rad)), ncol = 2)
  
  # Apply the rotation matrix to the translated coordinates
  rotated_coords <- t(rotation_matrix %*% t(translated_coords))
  
  # Translate the coordinates back to the new rotated position (not the original position)
  final_coords_matrix <- sweep(rotated_coords, 2, point_of_rotation, FUN = "+")
  
  row_names <- rownames(final_coords_matrix)
  
  # Convert matrix to list and apply row names
  vert_rot_coord_list <- setNames(split(final_coords_matrix, row(final_coords_matrix)), rownames(final_coords_matrix))
  
  # sp_translation <- original_sp - vert_rot_coord_list$sp
  
  return(vert_rot_coord_list)
  
}

jh_calculate_pso_coordinates_function <- function(vertebral_coord_list, pso_angle) {
  # Extract coordinates
  sp <- vertebral_coord_list$sp
  ip <- vertebral_coord_list$ip
  ia <- vertebral_coord_list$ia
  sa <- vertebral_coord_list$sa
  
  # Convert angle to radians
  theta <- pso_angle * pi / 180
  
  # Compute the new sp coordinate
  # Define the vector from sa to sp
  sa_sp_vec <- c(sp[[1]] - sa[[1]], sp[[2]] - sa[[2]])
  
  # Compute the rotation matrix
  rot_matrix <- matrix(c(cos(theta), -sin(theta), sin(theta), cos(theta)), nrow = 2)
  
  # Apply the rotation to sa_sp_vec
  new_sp_vec <- rot_matrix %*% sa_sp_vec
  
  # Compute the new sp coordinate
  new_sp <- c(sa[[1]] + new_sp_vec[1], sa[[2]] + new_sp_vec[2])
  # new_sp <- list(x = sa[[1]] + new_sp_vec[1], y = sa$y + new_sp_vec[2])
  
  # Return updated coordinate list
  updated_vertebral_coords <- list(sp = new_sp, ip = ip, ia = ia, sa = sa)
  
  return(updated_vertebral_coords)
}

jh_construct_adjusted_spine_list_function <- function(segment_angle_adjustment_df = tibble(), 
                                                      pso_list = list(),
                                                 spine_list = list(),
                                                 spine_orientation = "left", 
                                                 adjust_for_pt_change = TRUE){
  
  
  spine_level_adjustment_df <- segment_angle_adjustment_df %>%
    mutate(spine_interspace = str_to_lower(str_replace_all(spine_interspace, "-", "_"))) %>%
    separate(remove = FALSE, spine_interspace, into = c("cranial", "caudal"))%>%
    mutate(caudal = if_else(caudal == "s1", "sacrum", caudal)) %>%
    select(spine_level = cranial, adjustment)
  
  sa_adjustment_list <- as.list(spine_level_adjustment_df$adjustment)
  names(sa_adjustment_list) <- spine_level_adjustment_df$spine_level
  
  rotated_translated_spine_list <- list()
  
  ## sacrum
  rotated_translated_spine_list$sacrum <- spine_list$vert_coord_list$sacrum
  ##### next vert:
  
  spine_vert_list <- spine_list$vert_coord_list

  if(any(names(pso_list)== "l5")){
    spine_vert_list$l5 <- jh_calculate_pso_coordinates_function(vertebral_coord_list = spine_vert_list$l5, pso_angle = pso_list$l5)
    sa_adjustment_list$l4 <- sa_adjustment_list$l4 + pso_list$l5
  }
  
  rotated_translated_spine_list$l5 <- jh_rotate_translate_vert_list_coord_function(vert_coord_list = spine_vert_list$l5, 
                                                                    point_of_rotation = spine_list$vert_coord_list$sacrum$sp,
                                                                    segment_angle_adjustment = sa_adjustment_list$l5,
                                                                    inferior_vertebra_original_list = spine_list$vert_coord_list$sacrum,
                                                                    inferior_vertebra_new_list = spine_list$vert_coord_list$sacrum,
                                                                    # initial_translation_correction = c(0,0), 
                                                                    spine_orientation = spine_orientation)
  

  ##### next vert:
  
  if(any(names(pso_list)== "l4")){
    spine_vert_list$l4 <- jh_calculate_pso_coordinates_function(vertebral_coord_list = spine_vert_list$l4, pso_angle = pso_list$l4)
    sa_adjustment_list$l3 <- sa_adjustment_list$l3 + pso_list$l4
  }
  
  rotated_translated_spine_list$l4 <- jh_rotate_translate_vert_list_coord_function(vert_coord_list = spine_vert_list$l4, 
                                                                    point_of_rotation = rotated_translated_spine_list$l5$sp,
                                                                    segment_angle_adjustment = sum(unlist(sa_adjustment_list)[1:2]),
                                                                    inferior_vertebra_original_list = spine_list$vert_coord_list$l5,
                                                                    inferior_vertebra_new_list = rotated_translated_spine_list$l5,
                                                                    # initial_translation_correction = l5_rot_trans_list$sp_translation,
                                                                    spine_orientation = spine_orientation)
  
  # rotated_translated_spine_list$l4 <- l4_rot_trans_list$vert_coord
  
  ##### next vert:
  if(any(names(pso_list)== "l3")){
    
    spine_vert_list$l3 <- jh_calculate_pso_coordinates_function(vertebral_coord_list = spine_vert_list$l3, pso_angle = pso_list$l3)
    sa_adjustment_list$l2 <- sa_adjustment_list$l2 + pso_list$l3
  }
  
  rotated_translated_spine_list$l3 <- jh_rotate_translate_vert_list_coord_function(vert_coord_list = spine_vert_list$l3, 
                                                                    point_of_rotation = rotated_translated_spine_list$l4$sp,
                                                                    segment_angle_adjustment = sum(unlist(sa_adjustment_list)[1:3]),
                                                                    inferior_vertebra_original_list = spine_list$vert_coord_list$l4,
                                                                    inferior_vertebra_new_list = rotated_translated_spine_list$l4,
                                                                    # initial_translation_correction = l4_rot_trans_list$sp_translation,
                                                                    spine_orientation = spine_orientation)
  
  # rotated_translated_spine_list$l3 <- l3_rot_trans_list$vert_coord
  
  ##### next vert:
  
  if(any(names(pso_list)== "l2")){
    spine_vert_list$l2 <- jh_calculate_pso_coordinates_function(vertebral_coord_list = spine_vert_list$l2, pso_angle = pso_list$l2)
    sa_adjustment_list$l1 <- sa_adjustment_list$l1 + pso_list$l2
  }
  
  rotated_translated_spine_list$l2 <- jh_rotate_translate_vert_list_coord_function(vert_coord_list = spine_vert_list$l2, 
                                                                    point_of_rotation = rotated_translated_spine_list$l3$sp,
                                                                    segment_angle_adjustment = sum(unlist(sa_adjustment_list)[1:4]),
                                                                    inferior_vertebra_original_list = spine_list$vert_coord_list$l3,
                                                                    inferior_vertebra_new_list = rotated_translated_spine_list$l3,
                                                                    # initial_translation_correction = l3_rot_trans_list$sp_translation,
                                                                    spine_orientation = spine_orientation)
  
  # rotated_translated_spine_list$l2 <- l2_rot_trans_list$vert_coord
  
  ##### next vert:
  if(any(names(pso_list)== "l1")){
    spine_vert_list$l1 <- jh_calculate_pso_coordinates_function(vertebral_coord_list = spine_vert_list$l1, pso_angle = pso_list$l1)
    sa_adjustment_list$t12 <- sa_adjustment_list$t12 + pso_list$l1
  }
  
  rotated_translated_spine_list$l1 <- jh_rotate_translate_vert_list_coord_function(vert_coord_list = spine_vert_list$l1, 
                                                                    point_of_rotation = rotated_translated_spine_list$l2$sp,
                                                                    segment_angle_adjustment = sum(unlist(sa_adjustment_list)[1:5]),
                                                                    inferior_vertebra_original_list = spine_list$vert_coord_list$l2,
                                                                    inferior_vertebra_new_list = rotated_translated_spine_list$l2,
                                                                    # initial_translation_correction = l2_rot_trans_list$sp_translation,
                                                                    spine_orientation = spine_orientation)
  
  # rotated_translated_spine_list$l1 <- l1_rot_trans_list$vert_coord
  
  ##### next vert:
  ##### next vert:
  if(any(names(pso_list)== "t12")){
    spine_vert_list$t12 <- jh_calculate_pso_coordinates_function(vertebral_coord_list = spine_vert_list$t12, pso_angle = pso_list$t12)
    sa_adjustment_list$t11 <- sa_adjustment_list$t11 + pso_list$t12
  }
  
  
  rotated_translated_spine_list$t12 <- jh_rotate_translate_vert_list_coord_function(vert_coord_list = spine_vert_list$t12, 
                                                                     point_of_rotation = rotated_translated_spine_list$l1$sp,
                                                                     segment_angle_adjustment = sum(unlist(sa_adjustment_list)[1:6]),
                                                                     inferior_vertebra_original_list = spine_list$vert_coord_list$l1,
                                                                     inferior_vertebra_new_list = rotated_translated_spine_list$l1,
                                                                     # initial_translation_correction = l1_rot_trans_list$sp_translation,
                                                                     spine_orientation = spine_orientation)
  
  # rotated_translated_spine_list$t12 <- t12_rot_trans_list$vert_coord
  
  ##### next vert:
  if(any(names(pso_list)== "t11")){
    spine_vert_list$l5 <- jh_calculate_pso_coordinates_function(vertebral_coord_list = spine_vert_list$l5, pso_angle = pso_list$t11)
    sa_adjustment_list$t10 <- sa_adjustment_list$t10 + pso_list$t11
  }
  
  rotated_translated_spine_list$t11 <- jh_rotate_translate_vert_list_coord_function(vert_coord_list = spine_vert_list$t11, 
                                                                     point_of_rotation = rotated_translated_spine_list$t12$sp,
                                                                     segment_angle_adjustment = sum(unlist(sa_adjustment_list)[1:7]),
                                                                     inferior_vertebra_original_list = spine_list$vert_coord_list$t12,
                                                                     inferior_vertebra_new_list = rotated_translated_spine_list$t12,
                                                                     # initial_translation_correction = t12_rot_trans_list$sp_translation,
                                                                     spine_orientation = spine_orientation)
  
  # rotated_translated_spine_list$t11 <- t11_rot_trans_list$vert_coord
  
  ##### next vert:
  if(any(names(pso_list)== "t10")){
    spine_vert_list$l5 <- jh_calculate_pso_coordinates_function(vertebral_coord_list = spine_vert_list$l5, pso_angle = pso_list$t10)
    sa_adjustment_list$t9 <- sa_adjustment_list$t9 + pso_list$t10
  }
  
  rotated_translated_spine_list$t10 <- jh_rotate_translate_vert_list_coord_function(vert_coord_list = spine_vert_list$t10, 
                                                                     point_of_rotation = rotated_translated_spine_list$t11$sp,
                                                                     segment_angle_adjustment = sum(unlist(sa_adjustment_list)[1:8]),
                                                                     inferior_vertebra_original_list = spine_list$vert_coord_list$t11,
                                                                     inferior_vertebra_new_list = rotated_translated_spine_list$t11,
                                                                     # initial_translation_correction = t11_rot_trans_list$sp_translation,
                                                                     spine_orientation = spine_orientation)
  
  # rotated_translated_spine_list$t10 <- t10_rot_trans_list$vert_coord
  
  ##### next vert:
  if(any(names(pso_list)== "t9")){
    spine_vert_list$l5 <- jh_calculate_pso_coordinates_function(vertebral_coord_list = spine_vert_list$l5, pso_angle = pso_list$t9)
    sa_adjustment_list$t8 <- sa_adjustment_list$t8 + pso_list$t9
  }
  
  rotated_translated_spine_list$t9 <- jh_rotate_translate_vert_list_coord_function(vert_coord_list = spine_vert_list$t9, 
                                                                    point_of_rotation = rotated_translated_spine_list$t10$sp,
                                                                    segment_angle_adjustment = sum(unlist(sa_adjustment_list)[1:9]),
                                                                    inferior_vertebra_original_list = spine_list$vert_coord_list$t10,
                                                                    inferior_vertebra_new_list = rotated_translated_spine_list$t10,
                                                                    # initial_translation_correction = t10_rot_trans_list$sp_translation,
                                                                    spine_orientation = spine_orientation)
  
  # rotated_translated_spine_list$t9 <- t9_rot_trans_list$vert_coord
  
  ##### next vert:
  if(any(names(pso_list)== "t8")){
    spine_vert_list$l5 <- jh_calculate_pso_coordinates_function(vertebral_coord_list = spine_vert_list$l5, pso_angle = pso_list$t8)
    sa_adjustment_list$t7 <- sa_adjustment_list$t7 + pso_list$t8
  }
  
  rotated_translated_spine_list$t8 <- jh_rotate_translate_vert_list_coord_function(vert_coord_list = spine_vert_list$t8, 
                                                                    point_of_rotation = rotated_translated_spine_list$t9$sp,
                                                                    segment_angle_adjustment = sum(unlist(sa_adjustment_list)[1:10]),
                                                                    inferior_vertebra_original_list = spine_list$vert_coord_list$t9,
                                                                    inferior_vertebra_new_list = rotated_translated_spine_list$t9,
                                                                    # initial_translation_correction = t9_rot_trans_list$sp_translation,
                                                                    spine_orientation = spine_orientation)
  
  # rotated_translated_spine_list$t8 <- t8_rot_trans_list$vert_coord
  
  
  ##### next vert:
  if(any(names(pso_list)== "t7")){
    spine_vert_list$l5 <- jh_calculate_pso_coordinates_function(vertebral_coord_list = spine_vert_list$l5, pso_angle = pso_list$t7)
    sa_adjustment_list$t6 <- sa_adjustment_list$t6 + pso_list$t7
  }
  
  rotated_translated_spine_list$t7 <- jh_rotate_translate_vert_list_coord_function(vert_coord_list = spine_vert_list$t7, 
                                                                    point_of_rotation = rotated_translated_spine_list$t8$sp,
                                                                    segment_angle_adjustment = sum(unlist(sa_adjustment_list)[1:11]),
                                                                    inferior_vertebra_original_list = spine_list$vert_coord_list$t8,
                                                                    inferior_vertebra_new_list = rotated_translated_spine_list$t8,
                                                                    # initial_translation_correction = t8_rot_trans_list$sp_translation,
                                                                    spine_orientation = spine_orientation)
  
  # rotated_translated_spine_list$t7 <- t7_rot_trans_list$vert_coord
  
  ##### next vert:
  if(any(names(pso_list)== "t6")){
    spine_vert_list$l5 <- jh_calculate_pso_coordinates_function(vertebral_coord_list = spine_vert_list$l5, pso_angle = pso_list$t6)
    sa_adjustment_list$t5 <- sa_adjustment_list$t5 + pso_list$t6
  }
  
  rotated_translated_spine_list$t6 <- jh_rotate_translate_vert_list_coord_function(vert_coord_list = spine_vert_list$t6, 
                                                                    point_of_rotation = rotated_translated_spine_list$t7$sp,
                                                                    segment_angle_adjustment = sum(unlist(sa_adjustment_list)[1:12]),
                                                                    inferior_vertebra_original_list = spine_list$vert_coord_list$t7,
                                                                    inferior_vertebra_new_list = rotated_translated_spine_list$t7,
                                                                    # initial_translation_correction = t7_rot_trans_list$sp_translation,
                                                                    spine_orientation = spine_orientation)
  
  # rotated_translated_spine_list$t6 <- t6_rot_trans_list$vert_coord
  
  ##### next vert:
  if(any(names(pso_list)== "t5")){
    spine_vert_list$l5 <- jh_calculate_pso_coordinates_function(vertebral_coord_list = spine_vert_list$l5, pso_angle = pso_list$t5)
    sa_adjustment_list$t4 <- sa_adjustment_list$t4 + pso_list$t5
  }
  
  rotated_translated_spine_list$t5 <- jh_rotate_translate_vert_list_coord_function(vert_coord_list = spine_vert_list$t5, 
                                                                    point_of_rotation = rotated_translated_spine_list$t6$sp,
                                                                    segment_angle_adjustment = sum(unlist(sa_adjustment_list)[1:13]),
                                                                    inferior_vertebra_original_list = spine_list$vert_coord_list$t6,
                                                                    inferior_vertebra_new_list = rotated_translated_spine_list$t6,
                                                                    # initial_translation_correction = t6_rot_trans_list$sp_translation,
                                                                    spine_orientation = spine_orientation)
  
  # rotated_translated_spine_list$t5 <- t5_rot_trans_list$vert_coord
  
  ##### next vert:
  if(any(names(pso_list)== "t4")){
    spine_vert_list$l5 <- jh_calculate_pso_coordinates_function(vertebral_coord_list = spine_vert_list$l5, pso_angle = pso_list$t4)
    sa_adjustment_list$t3 <- sa_adjustment_list$t3 + pso_list$t4
  }
  
  rotated_translated_spine_list$t4 <- jh_rotate_translate_vert_list_coord_function(vert_coord_list = spine_vert_list$t4, 
                                                                    point_of_rotation = rotated_translated_spine_list$t5$sp,
                                                                    segment_angle_adjustment = sum(unlist(sa_adjustment_list)[1:14]),
                                                                    inferior_vertebra_original_list = spine_list$vert_coord_list$t5,
                                                                    inferior_vertebra_new_list = rotated_translated_spine_list$t5,
                                                                    # initial_translation_correction = t5_rot_trans_list$sp_translation,
                                                                    spine_orientation = spine_orientation)
  
  # rotated_translated_spine_list$t4 <- t4_rot_trans_list$vert_coord
  
  ##### next vert:
  if(any(names(pso_list)== "t3")){
    spine_vert_list$l5 <- jh_calculate_pso_coordinates_function(vertebral_coord_list = spine_vert_list$l5, pso_angle = pso_list$t3)
    sa_adjustment_list$t2 <- sa_adjustment_list$t2 + pso_list$t3
  }
  
  rotated_translated_spine_list$t3 <- jh_rotate_translate_vert_list_coord_function(vert_coord_list = spine_vert_list$t3, 
                                                                    point_of_rotation = rotated_translated_spine_list$t4$sp,
                                                                    segment_angle_adjustment = sum(unlist(sa_adjustment_list)[1:15]),
                                                                    inferior_vertebra_original_list = spine_list$vert_coord_list$t4,
                                                                    inferior_vertebra_new_list = rotated_translated_spine_list$t4,
                                                                    # initial_translation_correction = t4_rot_trans_list$sp_translation,
                                                                    spine_orientation = spine_orientation)
  
  # rotated_translated_spine_list$t3 <- t3_rot_trans_list$vert_coord
  
  ##### next vert:
  if(any(names(pso_list)== "t2")){
    spine_vert_list$l5 <- jh_calculate_pso_coordinates_function(vertebral_coord_list = spine_vert_list$l5, pso_angle = pso_list$t2)
    sa_adjustment_list$t1 <- sa_adjustment_list$t1 + pso_list$t2
  }
  
  rotated_translated_spine_list$t2 <- jh_rotate_translate_vert_list_coord_function(vert_coord_list = spine_vert_list$t2, 
                                                                    point_of_rotation = rotated_translated_spine_list$t3$sp,
                                                                    segment_angle_adjustment = sum(unlist(sa_adjustment_list)[1:16]),
                                                                    inferior_vertebra_original_list = spine_list$vert_coord_list$t3,
                                                                    inferior_vertebra_new_list = rotated_translated_spine_list$t3,
                                                                    # initial_translation_correction = t3_rot_trans_list$sp_translation,
                                                                    spine_orientation = spine_orientation)
  
  # rotated_translated_spine_list$t2 <- t2_rot_trans_list$vert_coord
  
  ##### next vert:
  if(any(names(pso_list)== "t1")){
    spine_vert_list$l5 <- jh_calculate_pso_coordinates_function(vertebral_coord_list = spine_vert_list$l5, pso_angle = pso_list$t1)
    sa_adjustment_list$c7 <- sa_adjustment_list$c7 + pso_list$t1
  }
  
  rotated_translated_spine_list$t1 <- jh_rotate_translate_vert_list_coord_function(vert_coord_list = spine_vert_list$t1, 
                                                                    point_of_rotation = rotated_translated_spine_list$t2$sp,
                                                                    segment_angle_adjustment = sum(unlist(sa_adjustment_list)[1:17]),
                                                                    inferior_vertebra_original_list = spine_list$vert_coord_list$t2,
                                                                    inferior_vertebra_new_list = rotated_translated_spine_list$t2,
                                                                    # initial_translation_correction = t2_rot_trans_list$sp_translation,
                                                                    spine_orientation = spine_orientation)
  
  # rotated_translated_spine_list$t1 <- t1_rot_trans_list$vert_coord
  
  ##### next vert:
  if(any(names(pso_list)== "c7")){
    spine_vert_list$l5 <- jh_calculate_pso_coordinates_function(vertebral_coord_list = spine_vert_list$l5, pso_angle = pso_list$c7)
    sa_adjustment_list$c6 <- sa_adjustment_list$c6 + pso_list$c7
  }
  
  rotated_translated_spine_list$c7 <- jh_rotate_translate_vert_list_coord_function(vert_coord_list = spine_vert_list$c7, 
                                                                    point_of_rotation = rotated_translated_spine_list$t1$sp,
                                                                    segment_angle_adjustment = sum(unlist(sa_adjustment_list)[1:18]),
                                                                    inferior_vertebra_original_list = spine_list$vert_coord_list$t1,
                                                                    inferior_vertebra_new_list = rotated_translated_spine_list$t1,
                                                                    # initial_translation_correction = t1_rot_trans_list$sp_translation,
                                                                    spine_orientation = spine_orientation)
  
  # rotated_translated_spine_list$c7 <- c7_rot_trans_list$vert_coord
  
  ##### next vert:
  
  rotated_translated_spine_list$c6 <- jh_rotate_translate_vert_list_coord_function(vert_coord_list = spine_vert_list$c6, 
                                                                    point_of_rotation = rotated_translated_spine_list$c7$sp,
                                                                    segment_angle_adjustment = sum(unlist(sa_adjustment_list)[1:19]),
                                                                    inferior_vertebra_original_list = spine_list$vert_coord_list$c7,
                                                                    inferior_vertebra_new_list = rotated_translated_spine_list$c7,
                                                                    # initial_translation_correction = c7_rot_trans_list$sp_translation,
                                                                    spine_orientation = spine_orientation)
  
  # rotated_translated_spine_list$c6 <- c6_rot_trans_list$vert_coord
  
  ##### next vert:
  
  rotated_translated_spine_list$c5 <- jh_rotate_translate_vert_list_coord_function(vert_coord_list = spine_vert_list$c5, 
                                                                    point_of_rotation = rotated_translated_spine_list$c6$sp,
                                                                    segment_angle_adjustment = sum(unlist(sa_adjustment_list)[1:20]),
                                                                    inferior_vertebra_original_list = spine_list$vert_coord_list$c6,
                                                                    inferior_vertebra_new_list = rotated_translated_spine_list$c6,
                                                                    # initial_translation_correction = c6_rot_trans_list$sp_translation,
                                                                    spine_orientation = spine_orientation)
  
  # rotated_translated_spine_list$c5 <- c5_rot_trans_list$vert_coord
  
  ##### next vert:
  
  rotated_translated_spine_list$c4 <- jh_rotate_translate_vert_list_coord_function(vert_coord_list = spine_vert_list$c4, 
                                                                    point_of_rotation = rotated_translated_spine_list$c5$sp,
                                                                    segment_angle_adjustment = sum(unlist(sa_adjustment_list)[1:21]),
                                                                    inferior_vertebra_original_list = spine_list$vert_coord_list$c5,
                                                                    inferior_vertebra_new_list = rotated_translated_spine_list$c5,
                                                                    # initial_translation_correction = c5_rot_trans_list$sp_translation,
                                                                    spine_orientation = spine_orientation)
  
  # rotated_translated_spine_list$c4 <- c4_rot_trans_list$vert_coord
  
  ##### next vert:
  
  rotated_translated_spine_list$c3 <- jh_rotate_translate_vert_list_coord_function(vert_coord_list = spine_vert_list$c3, 
                                                                    point_of_rotation = rotated_translated_spine_list$c4$sp,
                                                                    segment_angle_adjustment = sum(unlist(sa_adjustment_list)[1:22]),
                                                                    inferior_vertebra_original_list = spine_list$vert_coord_list$c4,
                                                                    inferior_vertebra_new_list = rotated_translated_spine_list$c4,
                                                                    # initial_translation_correction = c4_rot_trans_list$sp_translation,
                                                                    spine_orientation = spine_orientation)
  
  # rotated_translated_spine_list$c3 <- c3_rot_trans_list$vert_coord
  
  ##### next vert:
  
  rotated_translated_spine_list$c2 <- jh_rotate_translate_vert_list_coord_function(vert_coord_list = spine_vert_list$c2, 
                                                                    point_of_rotation = rotated_translated_spine_list$c3$sp,
                                                                    segment_angle_adjustment = sum(unlist(sa_adjustment_list)[1:23]),
                                                                    inferior_vertebra_original_list = spine_list$vert_coord_list$c3,
                                                                    inferior_vertebra_new_list = rotated_translated_spine_list$c3,
                                                                    # initial_translation_correction = c3_rot_trans_list$sp_translation,
                                                                    spine_orientation = spine_orientation)
  
  # rotated_translated_spine_list$c2_rot_trans_list <- c2_rot_trans_list
  
  # rotated_translated_spine_list$c2 <- c2_rot_trans_list$vert_coord
  
  
  # rotated_translated_spine_list$c2_rot_trans_list <- c2_rot_trans_list
  ##### next vert:
  
  c1_rot_trans_list <- list()
  
  c1_rot_trans_list$sp <- jh_get_point_along_line_function(coord_a = rotated_translated_spine_list$c2$sa, coord_b = rotated_translated_spine_list$c2$sp, percent_a_to_b = 2)
  c1_rot_trans_list$ip <- jh_get_point_along_line_function(coord_a = rotated_translated_spine_list$c2$ip, coord_b = rotated_translated_spine_list$c2$mid_post, percent_a_to_b = 1.5)
  c1_rot_trans_list$ia <- jh_get_point_along_line_function(coord_a = rotated_translated_spine_list$c2$ia, coord_b = rotated_translated_spine_list$c2$sa, percent_a_to_b = 0.75)
  c1_rot_trans_list$sa <- rotated_translated_spine_list$c2$sa
  
  # 
  # # c1_rot_trans_list$sp <- c2_rot_trans_list$vert_coord$sp
  # c1_rot_trans_list$sp <- h_get_point_along_line_function(coord_a = c2_rot_trans_list$vert_coord$sa, coord_b = c2_rot_trans_list$vert_coord$sp, percent_a_to_b = 2)
  # c1_rot_trans_list$ip <- jh_get_point_along_line_function(coord_a = c2_rot_trans_list$vert_coord$sp, coord_b = c2_rot_trans_list$vert_coord$ip, percent_a_to_b = 0.3)
  # c1_rot_trans_list$ia <- jh_get_point_along_line_function(coord_a = c2_rot_trans_list$vert_coord$sa, coord_b = c2_rot_trans_list$vert_coord$ia, percent_a_to_b = 0.3)
  # c1_rot_trans_list$sa <- c2_rot_trans_list$vert_coord$sa
  
  rotated_translated_spine_list$c1 <- c1_rot_trans_list
  
  return_list <- list()
  if(adjust_for_pt_change == TRUE){
    
    preop_pt <- jh_calculate_vertebral_tilt_function(vertebral_centroid = spine_list$centroid_coord_list$s1, 
                                                     femoral_head_center = spine_list$centroid_coord_list$femoral_head, 
                                                     pelvic_tilt_modifier = TRUE,
                                                     spine_orientation = spine_orientation)
    
    preop_c2_tilt <- jh_calculate_vertebral_tilt_function(vertebral_centroid = spine_list$centroid_coord_list$c2, 
                                                          femoral_head_center = spine_list$centroid_coord_list$femoral_head, 
                                                          pelvic_tilt_modifier = FALSE,
                                                          spine_orientation = spine_orientation)
    
    c2centroid <- jh_calculate_point_along_line_function(coord_a = rotated_translated_spine_list$c2$sa, coord_b = rotated_translated_spine_list$c2$mid, percent_a_to_b = 0.5)
    
    
    postop_c2_tilt_pre_adjust <- jh_calculate_vertebral_tilt_function(vertebral_centroid = c2centroid, 
                                                                      femoral_head_center = c(0,0), 
                                                                      spine_orientation = spine_orientation, 
                                                                      pelvic_tilt_modifier = FALSE)
    
    postop_c2pa <- preop_pt + postop_c2_tilt_pre_adjust
    
    predicted_pt <- predict_postop_pt_function(preop_c2_tilt = preop_c2_tilt, 
                                               preop_pt = preop_pt, 
                                               postop_c2pa = postop_c2pa)
    
    pt_change <- preop_pt - predicted_pt
    
    jh_convert_spine_coord_list_to_df_function(rotated_translated_spine_list)
    
    return_list$vert_coord_df <- jh_rotate_spine_for_pt_adjustment_function_function(spine_df = jh_convert_spine_coord_list_to_df_function(rotated_translated_spine_list),
                                                                                     angle_degrees = pt_change,
                                                                                     point_of_rotation = c(0,0))
    
    return_list$vert_coord_list <- jh_convert_spine_coord_df_to_named_list_function(spine_df = return_list$vert_coord_df)
    
    return_list$interspace_coord_df <- jh_calculate_posterior_interspace_coord_from_coord_df_function(vert_coord_df = return_list$vert_coord_df)
    
    return_list$interspace_list <- (return_list$interspace_coord_df %>%
                                      mutate(coord = map2(.x = x, .y = y, .f = ~ c(.x, .y))))$coord
    
    return_list$centroid_coord_list <- map2(.x = return_list$vert_coord_list, .y = names(return_list$vert_coord_list),
                                            .f = ~ jh_calculate_vertebra_centroid_function(vertebra_coords = .x, vertebral_level = .y))
    
    names(return_list$centroid_coord_list) <- str_replace_all(names(return_list$vert_coord_list), "sacrum", "s1")
    
    return_list$centroid_coord_list <- c(list(femoral_head = c(0,0)), return_list$centroid_coord_list)
    
    return_list$centroid_coord_list$c1 <- NULL
    
    
    
  }else{
    return_list$vert_coord_df <- jh_convert_spine_coord_list_to_df_function(rotated_translated_spine_list)
    
    return_list$vert_coord_list <- rotated_translated_spine_list
    
    return_list$interspace_coord_df <- jh_calculate_posterior_interspace_coord_from_coord_df_function(vert_coord_df = return_list$vert_coord_df)
    
    return_list$interspace_list <- (return_list$interspace_coord_df %>%
                                      mutate(coord = map2(.x = x, .y = y, .f = ~ c(.x, .y))))$coord
    
    return_list$centroid_coord_list <- map2(.x = return_list$vert_coord_list, .y = names(return_list$vert_coord_list),
                                            .f = ~ jh_calculate_vertebra_centroid_function(vertebra_coords = .x, vertebral_level = .y))
    
    names(return_list$centroid_coord_list) <- str_replace_all(names(return_list$vert_coord_list), "sacrum", "s1")
    
    return_list$centroid_coord_list <- c(list(femoral_head = c(0,0)), return_list$centroid_coord_list)
    
    return_list$centroid_coord_list$c1 <- NULL
    
    
  }
  
  
  return(return_list)
}


# jh_construct_adjusted_spine_list_function <- function(sa_adjustment_df = tibble(),
#                                                       spine_list = list(), 
#                                                       spine_orientation = "left",
#                                                       adjust_for_pt_change = TRUE){
#   
#   return_list <- list()
# 
#   nonzero_sa_df <- sa_adjustment_df %>%
#     filter(adjustment != 0) %>%
#     mutate(spine_interspace = str_to_lower(str_replace_all(spine_interspace, "-", "_"))) %>%
#     separate(remove = FALSE, spine_interspace, into = c("cranial", "caudal"))%>%
#     mutate(caudal = if_else(caudal == "s1", "sacrum", caudal))
#   
#   ## for PSO... just identify the correct 'point_of_rotation'
#   
#   if(nrow(nonzero_sa_df)>0){
#     
#     adjusted_full_spine_coord_df <- spine_list$vert_coord_df
#     
#     for (i in c(1:length(nonzero_sa_df$spine_interspace))) {
#       
#       spine_to_adjust_df <- adjusted_full_spine_coord_df %>%
#         filter(row_number() > max(which(adjusted_full_spine_coord_df$spine_level == paste0(nonzero_sa_df$caudal[[i]]))))
#       
#       interspace_rotation_point_df <- jh_calculate_posterior_interspace_coord_from_coord_df_function(vert_coord_df = adjusted_full_spine_coord_df) %>%
#         filter(interspace == nonzero_sa_df$spine_interspace[[i]])
#       
#       proximal_adjusted_spine_df <- jh_rotate_proximal_spine_at_segment_function(spine_df = spine_to_adjust_df, 
#                                                                                  angle_degrees = nonzero_sa_df$adjustment[[i]], 
#                                                                                  point_of_rotation = c(interspace_rotation_point_df$x[[1]], interspace_rotation_point_df$y[[1]]), 
#                                                                                  spine_orientation = spine_orientation)
#       
#       
#       adjusted_full_spine_coord_df <- adjusted_full_spine_coord_df %>%
#         filter(spine_level %in% proximal_adjusted_spine_df$spine_level == FALSE) %>%
#         union_all(proximal_adjusted_spine_df)
#       
#     }
#     
#     adjusted_full_spine_df <- adjusted_full_spine_coord_df
#   
#   if(adjust_for_pt_change == TRUE){
#     
#     preop_tilt_vpa_list <- jh_calculate_vertebral_tilt_and_vpas_from_coordinates_function(full_centroid_coord_list = spine_list$centroid_coord_list,
#                                                                                           spine_orientation = spine_orientation)
#     
#     
#     preop_c2_tilt <- preop_tilt_vpa_list$vert_tilt_list$c2
#     preop_pt <- preop_tilt_vpa_list$vert_tilt_list$pt
#     
#     c2_coord_df <- adjusted_full_spine_df %>%
#       filter(spine_level == "c2") %>%
#       mutate(vert_coord = map2(.x = x, .y = y, .f = ~ c(.x, .y))) 
#     
#     c2centroid <- jh_calculate_point_along_line_function(coord_a = (filter(.data = c2_coord_df, vert_point == "sa"))$vert_coord[[1]], 
#                                                          coord_b = (filter(.data = c2_coord_df, vert_point == "mid"))$vert_coord[[1]], 
#                                                          percent_a_to_b = 0.5)
#     
#     postop_c2_tilt_pre_adjust <- jh_calculate_vertebral_tilt_function(vertebral_centroid = c2centroid, 
#                                                                       femoral_head_center = c(0,0), 
#                                                                       spine_orientation = spine_orientation, 
#                                                                       pelvic_tilt_modifier = FALSE)
#     
#     postop_c2pa <- preop_pt + postop_c2_tilt_pre_adjust
#     
#     predicted_pt <- predict_postop_pt_function(preop_c2_tilt = preop_c2_tilt, 
#                                                preop_pt = preop_pt, 
#                                                postop_c2pa = postop_c2pa)
#     
#     pt_change <- preop_pt - predicted_pt
#     
#     adjusted_full_spine_df <- jh_rotate_spine_for_pt_adjustment_function_function(spine_df = adjusted_full_spine_df,
#                                                                                   angle_degrees = pt_change,
#                                                                                   point_of_rotation = c(0,0))
#     
#   }
#   
#   return_list$vert_coord_df <- adjusted_full_spine_df
#   
#   return_list$vert_coord_list <- jh_convert_spine_coord_df_to_named_list_function(spine_df = return_list$vert_coord_df)
#   
#   return_list$interspace_coord_df <- jh_calculate_posterior_interspace_coord_from_coord_df_function(vert_coord_df = return_list$vert_coord_df)
#   
#   return_list$interspace_list <- (return_list$interspace_coord_df %>%
#                                     mutate(coord = map2(.x = x, .y = y, .f = ~ c(.x, .y))))$coord
#   
#   names(return_list$interspace_list) <- return_list$interspace_coord_df$interspace
#   
#   return_list$centroid_coord_list <- map2(.x = return_list$vert_coord_list, .y = names(return_list$vert_coord_list),
#                                           .f = ~ jh_calculate_vertebra_centroid_function(vertebra_coords = .x, vertebral_level = .y))
#   
#   names(return_list$centroid_coord_list) <- str_replace_all(names(return_list$vert_coord_list), "sacrum", "s1")
#   
#   return_list$centroid_coord_list <- c(list(femoral_head = c(0,0)), return_list$centroid_coord_list)
#   
#   return_list$centroid_coord_list$c1 <- NULL
#   
#   # return_list$
#   return_list$sa_adjustment_current_df <- sa_adjustment_df 
#     # mutate(adjustment_performed = if_else(str_to_lower(str_replace_all(spine_interspace, "-", "_")) == paste0(sa_adjustments_nonzero_df$spine_interspace[[1]]), "yes", adjustment_performed))
#   
#   }else{
#     
#     return_list$vert_coord_df <- spine_list$vert_coord_df
#       return_list$vert_coord_list <- spine_list$vert_coord_list
#       return_list$interspace_coord_df <-spine_list$interspace_coord_df
#       return_list$interspace_list <- spine_list$interspace_list
#       
#     return_list$centroid_coord_list <- spine_list$centroid_coord_list
#     return_list$sa_adjustment_current_df <- sa_adjustment_df
#     
#   }
#   
#   return(return_list)
#   
# }





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
