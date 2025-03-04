
# jh_compute_vpa_from_xray_data_function <- function(fem_head_center = c(0,0), 
#                                                    vertebral_centroid = c(0.15, 0.3), 
#                                                    spine_facing = "left",
#                                                    pelvic_tilt = 15){
#   
#   
#   fem_head_to_centroid_length <- jh_calculate_distance_between_2_points_function(point_1 = vertebral_centroid, 
#                                                                                  point_2 = fem_head_center)
#   
#   fem_head_to_centroid_x_length <- jh_calculate_distance_between_2_points_function(point_1 = vertebral_centroid, 
#                                                                                    point_2 = c(fem_head_center[1],
#                                                                                                vertebral_centroid[2]))
#   
#   tilt_orientation_modifier <- case_when(
#     spine_facing == "left" & fem_head_center[[1]] > vertebral_centroid[[1]] ~ 1,
#     spine_facing == "left" & fem_head_center[[1]] < vertebral_centroid[[1]] ~ -1,
#     spine_facing == "right" & fem_head_center[[1]] > vertebral_centroid[[1]] ~ -1,
#     spine_facing == "right" & fem_head_center[[1]] < vertebral_centroid[[1]] ~ 1
#   )
#   
#   vertebral_tilt <- asin(fem_head_to_centroid_x_length/fem_head_to_centroid_length)*180/pi*tilt_orientation_modifier
#   
#   vpa <- pelvic_tilt + vertebral_tilt 
#   
#   return(vpa)
#   
# }


# Function to create vertebra as an sf polygon
jh_create_vertebra_from_coordinates_function <- function(centroid_x = 0, centroid_y = 0, width = 5, height = 4, spine_orientation = "right") {
  half_height <- height / 2
  half_width <- width / 2
  
  if(spine_orientation == "right"){
    # Define the four corners of the vertebra before rotation
    sp <- c(centroid_x - half_width, centroid_y + half_height)
    sa <- c(centroid_x + half_width, centroid_y + half_height)
    ia <- c(centroid_x + half_width, centroid_y - half_height)
    ip <- c(centroid_x - half_width, centroid_y - half_height)  
  }else{
    # Define the four corners of the vertebra before rotation
    sa <- c(centroid_x - half_width, centroid_y + half_height)
    sp <- c(centroid_x + half_width, centroid_y + half_height)
    ip <- c(centroid_x + half_width, centroid_y - half_height)
    ia <- c(centroid_x - half_width, centroid_y - half_height)
  }
  
  
  vert_body <- rbind(sp, sa, ia, ip, sp) ## binds the corners to make a square
  
  return(st_polygon(list(vert_body)))
}

jh_find_sacrum_inf_point_function <- function(s1_posterior_sup = c(0,0), 
                                              s1_anterior_sup = c(1, 1), 
                                              spine_facing = "right") {
  
  inf_s1_line_length <- jh_calculate_distance_between_2_points_function(s1_posterior_sup, s1_anterior_sup)*2.5
  
  s1_center <- jh_get_point_along_line_function(coord_a = s1_posterior_sup,
                                                coord_b = s1_anterior_sup, percent_a_to_b = 0.5)
  
  
  # Extract coordinates for points A and B
  x1 <- s1_posterior_sup[1]
  y1 <- s1_posterior_sup[2]
  x2 <- s1_anterior_sup[1]
  y2 <- s1_anterior_sup[2]
  
  # Calculate the slope of sacrum
  dx <- x2 - x1
  dy <- y2 - y1
  
  # Check for vertical line case (when dx = 0)
  if (dx == 0) {
    # Line is vertical, so perpendicular line is horizontal
    # Determine the direction based on spine_facing
    if (spine_facing == "right") {
      inferior_sacrum <- c(s1_center[1] + inf_s1_line_length, s1_center[2])
    } else {
      inferior_sacrum <- c(s1_center[1] - inf_s1_line_length, s1_center[2])
    }
  } else {
    # Calculate the perpendicular slope
    perp_slope <- -dx / dy
    
    # Calculate the angle of the perpendicular line
    angle_perp <- atan(perp_slope)
    
    # Determine direction based on spine_facing
    if (spine_facing == "right") {
      inferior_sacrum_x <- s1_center[1] - inf_s1_line_length * cos(angle_perp)
      inferior_sacrum_y <- s1_center[2] - abs(inf_s1_line_length * sin(angle_perp))
    } else {
      inferior_sacrum_x <- s1_center[1] + inf_s1_line_length * cos(angle_perp)
      inferior_sacrum_y <- s1_center[2] - abs(inf_s1_line_length * sin(angle_perp))
    }
    
    # Set the coordinates for inferior_sacrum
    inferior_sacrum <- c(x = inferior_sacrum_x, y = inferior_sacrum_y)
  }
  
  # Return the coordinates for inferior_sacrum
  return(inferior_sacrum)
  
  
}


jh_find_fem_head_center_given_pi_and_thickness_function <- function(inf_sacrum, s1_center, length_s1_fem, angle_degrees) {
  # Convert the angle to radians
  angle_radians <- angle_degrees * (pi / 180)
  
  # Calculate the difference in x and y between inf_sacrum and s1_center
  dx <- s1_center[1] - inf_sacrum[1]
  dy <- s1_center[2] - inf_sacrum[2]
  
  # Calculate the angle of the line from inf_sacrum to s1_center
  angle_sacrum_to_s1 <- atan2(dy, dx)
  
  # Calculate the angle for the fem_head_center relative to the horizontal
  angle_fem <- angle_sacrum_to_s1 + angle_radians
  
  slope_s1_to_hips <- 180 - angle_fem*180/pi
  
  slope_s1_to_hips_radians <- slope_s1_to_hips * (pi / 180)
  
  # Calculate the x and y coordinates of fem_head_center
  fem_head_center_x <- s1_center[1] + cos(slope_s1_to_hips_radians)*length_s1_fem
  fem_head_center_y <- s1_center[2] - sin(slope_s1_to_hips_radians)*length_s1_fem
  
  # Return the coordinates of fem_head_center as a named vector
  fem_head_center <- c(x = fem_head_center_x, y = fem_head_center_y)
  return(fem_head_center)
  # return(angle_fem*180/pi)
}


jh_build_c1_from_c2_geometry_function <- function(c2_geom){
  
  c2_sa_coord <- st_coordinates(c2_geom)[,1:2]["sa",]
  c2_ia_coord <- st_coordinates(c2_geom)[,1:2]["ia",]
  c2_sp_coord <- st_coordinates(c2_geom)[,1:2]["sp",]
  c2_ip_coord <- st_coordinates(c2_geom)[,1:2]["ip",]
  
  ia <- jh_get_point_along_line_function(coord_a = c2_ia_coord, c2_sa_coord, percent_a_to_b = 1.4)
  ip  <- jh_get_point_along_line_function(coord_a = c2_ip_coord, c2_sp_coord, percent_a_to_b = 1.4)  
  sa <- jh_get_point_along_line_function(coord_a = c2_ia_coord, c2_sa_coord, percent_a_to_b = 1.75)
  sp <- jh_get_point_along_line_function(coord_a = c2_ip_coord, c2_sp_coord, percent_a_to_b = 1.75)

  
  c1_coord_list <- list()
  c1_coord_list$sp <- sp
  c1_coord_list$sa <- sa
  c1_coord_list$ia <- ia
  c1_coord_list$ip <- ip
  
  c1_geom <- st_polygon(list(rbind(sp, sa, ia, ip, sp)))
  
  vert_coord_df <-  clean_names(bind_rows(c1_coord_list) ) %>%
    mutate(vert_point = names(c1_coord_list))%>%
    select(vert_point, x, y)
  
  return(list(
    c1_geom = c1_geom,
    vert_coord_df = vert_coord_df,
    c1_coord_list = c1_coord_list
  ))
  
}



jh_compute_inferior_vertebral_corner_function <- function(x, y, inferior_x, inferior_y, return_x_or_y = "x") {
  # Calculate the new x and y coordinates for the point 20% along the line
  new_x <- inferior_x + 0.2 * (x - inferior_x)
  new_y <- inferior_y + 0.2 * (y - inferior_y)
  
  # Return the new coordinates as a vector
  # return(c(new_x, new_y))
  if(return_x_or_y == "x"){
    return(new_x)
  }else{
    return(new_y)
  }
}



jh_get_inferior_superior_disc_rotation_coordinates_function <- function(full_vert_coord_list, 
                                                                        index){
  inferior_index <- index - 1
  vert_of_interest_index <- index
  superior_vert_index <- index + 1
  
  vert_coord_list <- full_vert_coord_list[[index]]
  
  if(inferior_index >0){
    inferior_coord_list <- full_vert_coord_list[[inferior_index]]
    inferior_posterior_disc <- jh_get_point_along_line_function(coord_a = vert_coord_list$ip, 
                                                                coord_b = inferior_coord_list$sp, 
                                                                percent_a_to_b = 0.5)
    
    vert_coord_list$inferior_posterior_disc <- inferior_posterior_disc 
  }
  if(superior_vert_index <= length(full_vert_coord_list)){
    superior_coord_list <- full_vert_coord_list[[superior_vert_index]]
    superior_posterior_disc <- jh_get_point_along_line_function(coord_a = vert_coord_list$sp, 
                                                                coord_b = superior_coord_list$ip, 
                                                                percent_a_to_b = 0.5)
    
    vert_coord_list$superior_posterior_disc <- superior_posterior_disc 
  }
  
  return(vert_coord_list)
}


jh_safely_buffer_vert_function <- function(vert_geom, buffer_amount = 0.1){
  neg_buffer <- st_buffer(x = vert_geom, dist = -buffer_amount, endCapStyle = "ROUND")
  
  while (st_is_empty(neg_buffer)) {
    buffer_amount <- buffer_amount*0.8
    neg_buffer <- st_buffer(x = vert_geom, dist = -buffer_amount*0.5, endCapStyle = "ROUND")
  }
  
  buffered_geom <- st_buffer(x = neg_buffer, dist = buffer_amount, endCapStyle = "ROUND")
  
  buffered_geom
  
}


jh_construct_vert_polygon_from_coordinates_list_function <- function(vert_list, buffer_amount = 0){
  vert_points <- names(vert_list)
  
  if(any(vert_points %in% c("sp", "sa", "ia", "ip"))){
    square_geom <- st_polygon(list(rbind(vert_list$sp, vert_list$sa, vert_list$ia, vert_list$ip, vert_list$sp)))
    
    if(buffer_amount > 0){
      geom <- jh_safely_buffer_vert_function(vert_geom = square_geom, buffer_amount = buffer_amount)
      return(geom)
    }else{
      return(square_geom)
    }
  }
}

jh_extract_coord_from_vert_coord_list_function <- function(vert_coord_level_list){
  # vert_level_coord <- tibble(vert_point = names(vert_coord_level_list)) %>%
  #   mutate(index_position = row_number()) %>%
  #   mutate(x = map(.x = index_position, .f = ~ vert_coord_level_list[[.x]][1])) %>%
  #   unnest()%>%
  #   mutate(y = map(.x = index_position, .f = ~ vert_coord_level_list[[.x]][2])) %>%
  #   unnest() %>%
  #   select(vert_point, x, y)
  
  # vert_level_coord
  tibble(spine_level = names(vert_coord_level_list)) %>%
    mutate(index_position = row_number()) %>%
    mutate(xy_coord = map(.x = index_position, .f = ~ vert_coord_level_list[[.x]])) %>%
    mutate(vert_point = map(.x = index_position, .f = ~ names(vert_coord_level_list[[.x]]))) %>%
    unnest()  %>%
    mutate(x = map(.x = xy_coord, .f = ~ .x[[1]])) %>%
    mutate(y = map(.x = xy_coord, .f = ~ .x[[2]]))  %>%
    unnest() %>%
    select(spine_level, vert_point, x, y) %>%
    distinct()
  
}

jh_construct_vpa_lines_from_spine_coord_df_function <- function(spine_coord_df,
                                                                fem_head_center_coord, 
                                                                s1_mid = 0){
  
  if(length(s1_mid) != 2){
    s1_mid <- c((spine_coord_df %>%
                   filter(vert_point == "center", spine_level == "sacrum"))$x, (spine_coord_df %>%
                                                                                  filter(vert_point == "center", spine_level == "sacrum"))$y) 
  }else{
    s1_mid <- s1_mid
  }
  
  vpa_lines_df <- spine_coord_df %>%
    filter(vert_point == "centroid") %>%
    mutate(vpa_line_sf = map2(.x = x, .y = y, .f = ~ st_linestring(rbind(c(.x, .y), 
                                                                         fem_head_center_coord, 
                                                                         s1_mid)))) %>%
    rowwise() %>%
    mutate(vpa_line_sf = st_sfc(vpa_line_sf)) 
  
  vpa_lines_list <- vpa_lines_df$vpa_line_sf
  
  names(vpa_lines_list) <- paste0(vpa_lines_df$spine_level, "pa_line")
  
  return(vpa_lines_list)
}

############################# BUILD SPINE FROM COORDINATES ###############################
############################# BUILD SPINE FROM COORDINATES ###############################
############################# BUILD SPINE FROM COORDINATES ###############################
############################# BUILD SPINE FROM COORDINATES ###############################
############################# BUILD SPINE FROM COORDINATES ###############################


jh_build_spine_from_coordinates_function <- function(femoral_head_center = c(0,0),
                                                     s1_anterior_superior = c(1, 1),
                                                     s1_posterior_superior = c(1, 2),
                                                     centroid_df = tibble(),
                                                     spine_facing = "left", 
                                                     calibration_modifier = 1, 
                                                     center_femoral_heads = FALSE){
  
  if(calibration_modifier != 1){
    femoral_head_center <- femoral_head_center*calibration_modifier
    s1_anterior_superior <- s1_anterior_superior*calibration_modifier
    s1_posterior_superior <- s1_posterior_superior*calibration_modifier
    centroid_df <- centroid_df %>%
      mutate(x = x*calibration_modifier, 
             y = y*calibration_modifier)
  }
  
  
  return_list <- list()
  
  s1_endplate_width <- jh_calculate_distance_between_2_points_function(point_1 = s1_anterior_superior, point_2 = s1_posterior_superior)
  
  ##### PRELIM STEP: ADJUST FEMORAL HEADS TO 0,0
  if(center_femoral_heads){
    centering_x_adjustment <- 0 - femoral_head_center[[1]]
    centering_y_adjustment <- 0 - femoral_head_center[[2]]
    
    femoral_head_center <- c(0,0)
    
    s1_anterior_superior <- c(s1_anterior_superior[[1]] + centering_x_adjustment, s1_anterior_superior[[2]] + centering_y_adjustment) 
    s1_posterior_superior <- c(s1_posterior_superior[[1]] + centering_x_adjustment, s1_posterior_superior[[2]] + centering_y_adjustment) 
    
    centroid_df <- centroid_df %>%
      mutate(x = x + centering_x_adjustment, y = y + centering_y_adjustment)
  }
  
  
  ##### FIRST STEP IS TO CONSTRUCT THE VERTEBRAE AND COMPUTE THE ESTIMATED SLOPES AT EACH LEVEL
  
  vertebral_heights_df <- centroid_df %>%
    filter(str_detect(spine_point, "centroid|center")) %>%
    mutate(distance_to_cranial_vertebra = pmap(.l = list(..1 = x, ..2 = y, ..3 = lead(x), ..4 = lead(y)), 
                                               .f = ~ jh_calculate_distance_between_2_points_function(point_1 = c(..1, ..2),
                                                                                                      point_2 = c(..3, ..4)))) %>%
    unnest() %>%
    mutate(distance_to_caudal_vertebra = pmap(.l = list(..1 = x, ..2 = y, ..3 = lag(x), ..4 = lag(y)), 
                                              .f = ~ jh_calculate_distance_between_2_points_function(point_1 = c(..1, ..2),
                                                                                                     point_2 = c(..3, ..4)))) %>%
    unnest() %>%
    mutate(vertebral_height = 0.4*distance_to_cranial_vertebra + if_else(spine_point == "l5_centroid", 
                                                                         0.7*distance_to_caudal_vertebra, 0.4*distance_to_caudal_vertebra)) %>%
    mutate(vertebral_height = if_else(is.na(vertebral_height), lag(vertebral_height), vertebral_height)) %>%
    select(spine_point, x, y, vertebral_height)%>%
    mutate(vertebral_height = if_else(spine_point == "c2_centroid", vertebral_height*0.5, vertebral_height))
  
  # return_list$vertebral_heights_df <- vertebral_heights_df
  
  ### compute the superior endplate slope 
  vert_angles_df <- vertebral_heights_df %>%
    mutate(vert_angle = pmap(.l = list(..1 = lead(x),
                                       ..2 = lead(y),
                                       ..3 = x,
                                       ..4 = y
    ), 
    .f = ~ jh_calculate_perpendicular_angle_function(coord1_x = ..1,
                                          coord1_y = ..2, 
                                          coord2_x = ..3, 
                                          coord2_y = ..4))) %>%
    unnest(vert_angle) %>%
    filter(str_detect(spine_point, "centroid")) %>%
    mutate(vert_angle = if_else(spine_point == "c2_centroid", lag(vert_angle), vert_angle)) %>%
    mutate(vert_angle = vert_angle*-1)
  
  buffer_amount <- median(vertebral_heights_df$vertebral_height, na.rm = TRUE)*0.2
  
  return_list$buffer_amount <- buffer_amount
  
  ### this creates the rotated square vertebrae without endplates aligned. 
  vert_coord_for_sup_endplates_df <- vertebral_heights_df %>%
    filter(str_detect(spine_point, "centroid")) %>%
    mutate(vertebral_width = seq(from = s1_endplate_width, to = 0.4*s1_endplate_width, length = nrow(.))) %>%
    mutate(vert_sf = pmap(.l = list(..1 = x, ..2 = y, ..3 = vertebral_width, ..4 = vertebral_height, ..5 = spine_facing),
                          .f = ~ jh_create_vertebra_from_coordinates_function(centroid_x = ..1,
                                                                              centroid_y = ..2,
                                                                              width = ..3,
                                                                              height = ..4, 
                                                                              spine_orientation = ..5))) %>%
    left_join(vert_angles_df)%>%
    mutate(vert_sf = map(.x = vert_sf, .f = ~ st_geometry(.x))) %>%
    mutate(vert_sf_rot = map2(.x = vert_sf, .y = vert_angle,
                              .f = ~ jh_rotate_polygon_around_centroid(polygon = .x, 
                                                                       angle_degrees = .y))) %>%
    rowwise() %>%
    mutate(geometry = st_sfc(vert_sf_rot)) %>%
    ungroup()
  
  
  c1_build_list <- jh_build_c1_from_c2_geometry_function(c2_geom = (vert_coord_for_sup_endplates_df %>% filter(spine_point == "c2_centroid"))$geometry)
  
  superior_endplate_coord_df <- tibble(spine_level = "s1_superior_endplate",
                                       sp_x = s1_posterior_superior[1],
                                       sp_y = s1_posterior_superior[2],
                                       sa_x = s1_anterior_superior[1],
                                       sa_y = s1_anterior_superior[2]) %>%
    union_all(
      vert_coord_for_sup_endplates_df %>%
        mutate(spine_level = str_replace_all(spine_point, "centroid", "superior_endplate")) %>%
        select(spine_level, geometry) %>%
        mutate(sp_x = map(.x = geometry, 
                          .f = ~ st_coordinates(.x)[,1:2]["sp","X"])) %>%
        unnest(sp_x) %>%
        mutate(sp_y = map(.x = geometry, 
                          .f = ~ st_coordinates(.x)[,1:2]["sp","Y"])) %>%
        unnest(sp_y) %>%
        mutate(sa_x = map(.x = geometry, 
                          .f = ~ st_coordinates(.x)[,1:2]["sa","X"])) %>%
        unnest(sa_x) %>%
        mutate(sa_y = map(.x = geometry, 
                          .f = ~ st_coordinates(.x)[,1:2]["sa","Y"])) %>%
        unnest(sa_y) %>%
        select(-geometry)
    ) %>%
    add_row(spine_level = "c1_superior_endplate",
            sp_x = c1_build_list$c1_coord_list$sp[1],
            sp_y = c1_build_list$c1_coord_list$sp[2],
            sa_x = c1_build_list$c1_coord_list$sa[1],
            sa_y = c1_build_list$c1_coord_list$sa[2])
  
  ### Align the inferior vertebral corners to the superior vertebral corners: 
  
  aligned_vert_geometry_df <- superior_endplate_coord_df  %>%
    mutate(caudal_level = lag(spine_level)) %>%
    mutate(inferior_sp_x = lag(sp_x),
           inferior_sp_y = lag(sp_y),
           inferior_sa_x = lag(sa_x),
           inferior_sa_y = lag(sa_y)
    ) %>%
    filter(!is.na(caudal_level)) %>%
    mutate(ia_x = pmap(.l = list(
      ..1 = sa_x,
      ..2 = sa_y,
      ..3 = inferior_sa_x,
      ..4 = inferior_sa_y
    ), .f = ~ jh_compute_inferior_vertebral_corner_function(x = ..1, y = ..2, inferior_x = ..3, inferior_y = ..4, return_x_or_y = "x"))) %>%
    unnest(ia_x) %>%
    mutate(ia_y = pmap(.l = list(
      ..1 = sa_x,
      ..2 = sa_y,
      ..3 = inferior_sa_x,
      ..4 = inferior_sa_y
    ), .f = ~ jh_compute_inferior_vertebral_corner_function(x = ..1, y = ..2, inferior_x = ..3, inferior_y = ..4, return_x_or_y = "y"))) %>%
    unnest(ia_y)%>%
    mutate(ip_x = pmap(.l = list(
      ..1 = sp_x,
      ..2 = sp_y,
      ..3 = inferior_sp_x,
      ..4 = inferior_sp_y
    ), .f = ~ jh_compute_inferior_vertebral_corner_function(x = ..1, y = ..2, inferior_x = ..3, inferior_y = ..4, return_x_or_y = "x"))) %>%
    unnest(ip_x) %>%
    mutate(ip_y = pmap(.l = list(
      ..1 = sp_x,
      ..2 = sp_y,
      ..3 = inferior_sp_x,
      ..4 = inferior_sp_y
    ), .f = ~ jh_compute_inferior_vertebral_corner_function(x = ..1, y = ..2, inferior_x = ..3, inferior_y = ..4, return_x_or_y = "y"))) %>%
    unnest(ip_y) %>%
    select(spine_level, sp_x, sp_y, sa_x, sa_y, ia_x, ia_y, ip_x, ip_y) %>%
    pivot_longer(cols = c(sp_x, sa_x, ia_x, ip_x, sp_y, sa_y, ia_y, ip_y), names_to = "vert_point", values_to = "value") %>%
    mutate(x_or_y = if_else(str_detect(vert_point, "_x"), "x", "y")) %>%
    mutate(vert_point = str_remove_all(vert_point, "_x|_y")) %>%
    pivot_wider(names_from = x_or_y, values_from = value) %>%
    group_by(spine_level) %>%
    nest() %>%
    ungroup() %>%
    mutate(vert_coord_df = map(.x = data, .f = ~ .x %>% union_all(head(.x, n = 1)))) %>%
    select(spine_level, vert_coord_df) %>%
    mutate(geometry = map(.x = vert_coord_df, .f = ~ st_polygon(list(as.matrix(.x %>% select(x, y)))))) %>%
    rowwise() %>%
    mutate(geometry = st_sfc(geometry)) %>%
    ungroup()%>%
    mutate(spine_level = str_remove_all(spine_level, "_superior_endplate")) %>%
    filter(spine_level != "c1") %>%
    add_row(spine_level = "c1", vert_coord_df = list(c1_build_list$vert_coord_df), geometry = st_sfc(c1_build_list$c1_geom))%>%
    mutate(centroid = map(.x = geometry, .f = ~ clean_names(as_tibble(st_coordinates(st_centroid(.x)))) %>% mutate(vert_point = "centroid") %>% select(vert_point, x, y))) %>%
    mutate(vert_coord_df = map2(.x = vert_coord_df, .y = centroid, .f = ~ .x %>% union_all(.y))) %>%
    select(-centroid)
  
  
  # aligned_vert_geometry_df
  
  # return_list$superior_endplate_coord_df <- superior_endplate_coord_df
  
  # return_list$aligned_vert_geometry_df <- aligned_vert_geometry_df
  
  ### we now have the aligned vertebral bodies. 
  
  vert_geoms_square_list <- aligned_vert_geometry_df$geometry
  
  names(vert_geoms_square_list) <- aligned_vert_geometry_df$spine_level
  
  vert_coord_list <- map(.x = aligned_vert_geometry_df$vert_coord_df, .f = ~ .x %>% distinct())
  
  names(vert_coord_list) <- aligned_vert_geometry_df$spine_level
  
  vert_coord_list <- map(.x = vert_coord_list, .f = ~ jh_reformat_vert_tibble(.x))
  
  
  return_list$vert_geoms_square_list <- vert_geoms_square_list
  return_list$vert_coord_list <- vert_coord_list
  
  return_list$fem_head_center <- femoral_head_center
  
  
  #### NOW COMPUTE THE SEGMENT ANGLES FROM EACH SUPERIOR ENDPLATE TO SUPERIOR ENDPLATE
  ### compute segment angles
  # segment_angles_for_planning_df <- 
  segment_angles_df <- superior_endplate_coord_df %>%
    mutate(distal_sp_x = lag(sp_x),
           distal_sp_y = lag(sp_y),
           distal_sa_x = lag(sa_x),
           distal_sa_y = lag(sa_y)
    ) %>%
    filter(spine_level != "s1_superior_endplate") %>%
    mutate(segment_angle = pmap(.l = list(..1 = sp_x, 
                                          ..2 = sp_y,
                                          ..3 = sa_x, 
                                          ..4 = sa_y,
                                          ..5 = distal_sp_x, 
                                          ..6 = distal_sp_y,
                                          ..7 = distal_sa_x, 
                                          ..8 = distal_sa_y
    ),
    .f =~ jh_calculate_segment_angle_between_vertebrae_from_coordinates_function(sp_upper = c(..1, ..2),
                                                                                 sa_upper = c(..3, ..4),
                                                                                 sp_lower = c(..5, ..6),
                                                                                 sa_lower = c(..7, ..8)))) %>%
    unnest(segment_angle) %>%
    mutate(segment_angle = round(segment_angle, 4)) %>%
    mutate(spine_level = str_replace_all(spine_level, "_superior_endplate", "_segment_angle")) %>%
    select(spine_level, segment_angle) %>%
    filter(spine_level != "c1_segment_angle") %>%
    add_row(spine_level = "c1_segment_angle", 
            segment_angle = 0)
  
  segment_angles_list <- as.list(segment_angles_df$segment_angle)
  names(segment_angles_list) <- segment_angles_df$spine_level
  
  return_list$segment_angles_list <- segment_angles_list
  
  ############## now fix C2 ###############
  
  dens_sa <- jh_get_point_along_line_function(coord_a = return_list$vert_coord_list$c2$ia, 
                                              coord_b = return_list$vert_coord_list$c2$sa, 
                                              percent_a_to_b = 1.8)
  
  c2_posterior_superior_extended_point <- jh_get_point_along_line_function(coord_a = return_list$vert_coord_list$c2$ip, 
                                                                           coord_b = return_list$vert_coord_list$c2$sp, 
                                                                           percent_a_to_b = 1.8)
  
  dens_sp <- jh_get_point_along_line_function(coord_a = c2_posterior_superior_extended_point, 
                                              coord_b = dens_sa, 
                                              percent_a_to_b = 0.65)
  
  dens <- st_polygon(list(rbind(dens_sp,
                                return_list$vert_coord_list$c2$ip,
                                return_list$vert_coord_list$c2$ia,
                                dens_sa, dens_sp)))
  
  # return_list$c2_list <- c2_list
  # return_list$united_c2 <- st_union(dens, return_list$vert_geoms_square_list$c2)
  
  return_list$vert_geoms_square_list$c2 <- st_union(dens, return_list$vert_geoms_square_list$c2)
  
  # return_list$vert_geoms_square_list$c2 <- dens
  
  
  ### create femoral heads and Sacrum #######
  return_list$fem_head_sf <- st_buffer(x = st_point(femoral_head_center),
                                       dist = buffer_amount * 4,
                                       endCapStyle = "ROUND")
  
  
  s1_mid <- c((centroid_df %>% filter(spine_point == "s1_center"))$x, (centroid_df %>% filter(spine_point == "s1_center"))$y)
  
  # sac_inf_1 <- jh_find_sacrum_inf_point_function(s1_posterior_sup = s1_posterior_superior, 
  #                                             s1_midpoint = s1_mid, 
  #                                             femoral_heads_center = femoral_head_center, 
  #                                             spine_facing = spine_facing)
  
  sac_inf_1 <- jh_find_sacrum_inf_point_function(s1_posterior_sup = s1_posterior_superior, 
                                                 s1_anterior_sup = s1_anterior_superior, 
                                                 spine_facing = spine_facing)
  
  sacrum_sf <- st_polygon(list(rbind(s1_anterior_superior, 
                                     sac_inf_1,
                                     s1_posterior_superior, 
                                     s1_anterior_superior)))
  
  
  # return_list$sacrum_sf <-  st_buffer(x = st_buffer(x = sacrum_sf, dist = -buffer_amount, endCapStyle = "ROUND"), dist = buffer_amount, endCapStyle = "ROUND")
  
  
  
  s1_list <- list()
  s1_list$sp <- s1_posterior_superior
  s1_list$sa <- s1_anterior_superior
  s1_list$ia <- sac_inf_1
  s1_list$ip <- sac_inf_1
  s1_list$center <- s1_mid
  
  return_list$vert_coord_list <- prepend(vert_coord_list, list('sacrum' = s1_list))
  
  return_list$vert_geoms_square_list <- prepend(return_list$vert_geoms_square_list, list('sacrum' = sacrum_sf))
  
  return_list$vert_geom_list  <- map(.x = return_list$vert_geoms_square_list, 
                                     .f = ~ jh_safely_buffer_vert_function(vert_geom = .x, buffer_amount = buffer_amount)
  )
  
  vert_coord_index_vector <- c(1:length(return_list$vert_coord_list))
  
  return_list$vert_coord_list <- map(.x = vert_coord_index_vector, 
                                     .f = ~ jh_get_inferior_superior_disc_rotation_coordinates_function(full_vert_coord_list = return_list$vert_coord_list, index = .x))
  
  names(return_list$vert_coord_list) <- names(return_list$vert_geom_list)
  
  return_list$spine_coord_df <- jh_extract_coord_from_vert_coord_list_function(vert_coord_level_list = return_list$vert_coord_list)
  
  ########################### VPA DF ##########################
  
  return_list$vpa_df <- enframe(return_list$vert_coord_list) %>%
    mutate(centroid_x = map(.x = value, .f = ~ .x$centroid[1])) %>%
    unnest(centroid_x) %>%
    mutate(centroid_y = map(.x = value, .f = ~ .x$centroid[2])) %>%
    unnest() %>%
    select(name, x = centroid_x, y= centroid_y) %>%
    distinct() %>%
    filter(name != "c1")%>%
    mutate(vpa = map2(.x = x, .y = y, 
                      .f = ~ jh_calculate_vertex_angle(posterior_point_coord = return_list$vert_coord_list$sacrum$center,
                                                       ventral_point_coord = c(.x, .y),
                                                       vertex_coord = femoral_head_center,
                                                       spine_orientation = spine_facing
                      )
    )) %>%
    unnest() %>%
    mutate(vpa_label = paste0(name, "pa")) %>%
    select(vpa_label, vpa)
  
  ########### lines ################
  
  l1_to_fem_head_length <- jh_calculate_distance_between_2_points_function(st_coordinates(st_centroid(return_list$vert_geoms_square_list$l1)),
                                                                           femoral_head_center)
  
  # lines_list <- list()
  # l1pa_line <- st_linestring(rbind(st_centroid(return_list$vert_geoms_square_list$l1),
  #                                  femoral_head_center,
  #                                  s1_mid))
  # 
  # lines_list$l1pa <-   jh_plot_angle_curve_function(vertex_vector = femoral_head_center,
  #                                                   line_st_geometry = l1pa_line,
  #                                                   distance_of_curve = l1_to_fem_head_length/3)
  # 
  # t9pa_line <- st_linestring(rbind(st_centroid(return_list$vert_geoms_square_list$t9),
  #                                  femoral_head_center,
  #                                  s1_mid))
  # 
  # lines_list$t9pa <-   jh_plot_angle_curve_function(vertex_vector = femoral_head_center,
  #                                                   line_st_geometry = t9pa_line,
  #                                                   distance_of_curve = l1_to_fem_head_length/2.75)
  # 
  # 
  # t4pa_line <- st_linestring(rbind(st_centroid(return_list$vert_geoms_square_list$t4),
  #                                  femoral_head_center,
  #                                  s1_mid))
  # 
  # lines_list$t4pa <-   jh_plot_angle_curve_function(vertex_vector = femoral_head_center,
  #                                                   line_st_geometry = t4pa_line,
  #                                                   distance_of_curve = l1_to_fem_head_length/2.5)
  # 
  # 
  # c2pa_line <- st_linestring(rbind(st_centroid(dens),
  #                                  femoral_head_center,
  #                                  s1_mid))
  # 
  # lines_list$c2pa <-   jh_plot_angle_curve_function(vertex_vector = femoral_head_center,
  #                                                   line_st_geometry = c2pa_line,
  #                                                   distance_of_curve = l1_to_fem_head_length/2)
  
  
  lines_list <- jh_construct_vpa_lines_from_spine_coord_df_function(spine_coord_df = return_list$spine_coord_df, 
                                                                    fem_head_center_coord = femoral_head_center)
  
  
  return_list$lines_list <- lines_list
  
  
  return(  list(vert_coord_list = return_list$vert_coord_list,
                spine_coord_df = return_list$spine_coord_df,
                vert_geoms_square_list = return_list$vert_geoms_square_list,
                # united_c2 = return_list$united_c2,
                vert_geom_list = return_list$vert_geom_list,
                fem_head_sf = return_list$fem_head_sf,
                fem_head_center = return_list$fem_head_center,
                segment_angles_list = return_list$segment_angles_list,
                vpa_df = return_list$vpa_df,
                lines_list = return_list$lines_list, 
                buffer_amount = return_list$buffer_amount,
                vert_angles_df = vert_angles_df
  ))
  
}









jh_rotate_translate_vert_function <- function(vertebra,
                                              point_of_rotation,
                                              segment_angle_adjustment, 
                                              initial_translation_correction = c(0,0), 
                                              spine_orientation = "right") {
  
  spine_orientation_modifier = if_else(spine_orientation == "right", 1, -1)
  
  original_sp <- as.vector(st_coordinates(vertebra)[1, 1:2])
  
  # Convert the angle from degrees to radians
  angle_rad <- segment_angle_adjustment * pi / 180
  
  
  vertebra <- vertebra + initial_translation_correction*spine_orientation_modifier
  # Extract the coordinates of the vertebra geometry
  coords <- st_coordinates(vertebra)[, 1:2]
  
  # Translate the coordinates so the point of rotation is at the origin
  translated_coords <- sweep(coords, 2, point_of_rotation, FUN = "-")
  
  # Define the rotation matrix
  rotation_matrix <- matrix(c(cos(angle_rad), -sin(angle_rad),
                              sin(angle_rad), cos(angle_rad)), ncol = 2)
  
  # Apply the rotation matrix to the translated coordinates
  rotated_coords <- t(rotation_matrix %*% t(translated_coords))
  
  # Translate the coordinates back to the new rotated position (not the original position)
  final_coords <- sweep(rotated_coords, 2, point_of_rotation, FUN = "+")
  
  # Recreate the vertebra polygon with the new coordinates
  rotated_vertebra <- st_polygon(list(final_coords))
  
  # Ensure that the result is an 'sfc' object with the same CRS as the input
  rotated_vertebra <- st_sfc(rotated_vertebra, crs = st_crs(vertebra))
  
  final_coords <- st_zm(st_coordinates(rotated_vertebra))
  
  vert_rot_coord_list <- list()
  vert_rot_coord_list$sp <- final_coords[1,]
  vert_rot_coord_list$sa <- final_coords[2,]
  vert_rot_coord_list$ia <- final_coords[3,]
  vert_rot_coord_list$ip <- final_coords[4,]
  
  vert_rot_coord_list$centroid <-st_coordinates(st_centroid(rotated_vertebra))[1,]
  
  sp_translation <- original_sp - vert_rot_coord_list$sp
  
  return(list(vert_geom = rotated_vertebra, 
              vert_coord = vert_rot_coord_list, 
              sp_translation = sp_translation))
}



jh_rotate_spine_from_coordinates_by_segment_angles_function <- function(spine_build_list = list(), 
                                                                        # s1_posterior_superior = c(0,0), 
                                                                        sa_adjustment_df = tibble(spine_level = character(), 
                                                                                                  sa_adjustment = numeric()), 
                                                                        spine_orientation = "right"){
  vert_list <- list()
  
  # s1_posterior_superior
  vert_list$sacrum <- jh_rotate_translate_vert_function(vertebra = spine_build_list$vert_geoms_square_list$sacrum, 
                                                        point_of_rotation = spine_build_list$fem_head_center, 
                                                        segment_angle_adjustment = 0, 
                                                        initial_translation_correction = c(0,0))
  
  sacrum_coord_list <- (tibble(vert_point = names(st_coordinates(spine_build_list$vert_geoms_square_list$sacrum)[,2]),
                               x = st_coordinates(spine_build_list$vert_geoms_square_list$sacrum)[,1], 
                               y = st_coordinates(spine_build_list$vert_geoms_square_list$sacrum)[,2]
  ) %>%
    mutate(xy = map2(.x = x, .y = y, .f = ~ c(.x, .y))))$xy 
  
  sacrum_coord_list$s1_mid <- jh_get_point_along_line_function(coord_a = sacrum_coord_list$s1_posterior_superior, 
                                                               coord_b = sacrum_coord_list$s1_anterior_superior, 
                                                               percent_a_to_b = 0.5)
  
  
  vert_list$sacrum$vert_coord <- sacrum_coord_list
  
  vert_list$l5 <- jh_rotate_translate_vert_function(vertebra = spine_build_list$vert_geoms_square_list$l5, 
                                                    point_of_rotation = spine_build_list$vert_coord_list$l5$inferior_posterior_disc, 
                                                    segment_angle_adjustment = sum(sa_adjustment_df$sa_adjustment[1]), 
                                                    initial_translation_correction = c(0,0), 
                                                    spine_orientation = spine_orientation)
  
  
  vert_list$l4 <- jh_rotate_translate_vert_function(vertebra = spine_build_list$vert_geoms_square_list$l4, 
                                                    point_of_rotation = vert_list$l5$vert_coord$sp, 
                                                    segment_angle_adjustment = sum(sa_adjustment_df$sa_adjustment[1:2]), 
                                                    initial_translation_correction = vert_list$l5$sp_translation, 
                                                    spine_orientation = spine_orientation)
  
  
  vert_list$l3 <- jh_rotate_translate_vert_function(vertebra = spine_build_list$vert_geoms_square_list$l3, 
                                                    point_of_rotation = vert_list$l4$vert_coord$sp, 
                                                    segment_angle_adjustment = sum(sa_adjustment_df$sa_adjustment[1:3]), 
                                                    initial_translation_correction = vert_list$l4$sp_translation, 
                                                    spine_orientation = spine_orientation)
  
  
  vert_list$l2 <- jh_rotate_translate_vert_function(vertebra = spine_build_list$vert_geoms_square_list$l2, 
                                                    point_of_rotation = vert_list$l3$vert_coord$sp, 
                                                    segment_angle_adjustment = sum(sa_adjustment_df$sa_adjustment[1:4]), 
                                                    initial_translation_correction = vert_list$l3$sp_translation, 
                                                    spine_orientation = spine_orientation)
  
  
  vert_list$l1 <- jh_rotate_translate_vert_function(vertebra = spine_build_list$vert_geoms_square_list$l1, 
                                                    point_of_rotation = vert_list$l2$vert_coord$sp, 
                                                    segment_angle_adjustment = sum(sa_adjustment_df$sa_adjustment[1:5]), 
                                                    initial_translation_correction = vert_list$l2$sp_translation, 
                                                    spine_orientation = spine_orientation)
  
  vert_list$t12 <- jh_rotate_translate_vert_function(vertebra = spine_build_list$vert_geoms_square_list$t12, 
                                                     point_of_rotation = vert_list$l1$vert_coord$sp, 
                                                     segment_angle_adjustment = sum(sa_adjustment_df$sa_adjustment[1:6]), 
                                                     initial_translation_correction = vert_list$l1$sp_translation, 
                                                     spine_orientation = spine_orientation)
  
  vert_list$t11 <- jh_rotate_translate_vert_function(vertebra = spine_build_list$vert_geoms_square_list$t11, 
                                                     point_of_rotation = vert_list$t12$vert_coord$sp, 
                                                     segment_angle_adjustment = sum(sa_adjustment_df$sa_adjustment[1:7]), 
                                                     initial_translation_correction = vert_list$t12$sp_translation, 
                                                     spine_orientation = spine_orientation)
  
  vert_list$t10 <- jh_rotate_translate_vert_function(vertebra = spine_build_list$vert_geoms_square_list$t10, 
                                                     point_of_rotation = vert_list$t11$vert_coord$sp, 
                                                     segment_angle_adjustment = sum(sa_adjustment_df$sa_adjustment[1:8]), 
                                                     initial_translation_correction = vert_list$t11$sp_translation, 
                                                     spine_orientation = spine_orientation)
  #######
  
  vert_list$t9 <- jh_rotate_translate_vert_function(vertebra = spine_build_list$vert_geoms_square_list$t9, 
                                                    point_of_rotation = vert_list$t10$vert_coord$sp, 
                                                    segment_angle_adjustment = sum(sa_adjustment_df$sa_adjustment[1:9]), 
                                                    initial_translation_correction = vert_list$t10$sp_translation, 
                                                    spine_orientation = spine_orientation)
  
  vert_list$t8 <- jh_rotate_translate_vert_function(vertebra = spine_build_list$vert_geoms_square_list$t8, 
                                                    point_of_rotation = vert_list$t9$vert_coord$sp, 
                                                    segment_angle_adjustment = sum(sa_adjustment_df$sa_adjustment[1:10]), 
                                                    initial_translation_correction = vert_list$t9$sp_translation, 
                                                    spine_orientation = spine_orientation)
  
  vert_list$t7 <- jh_rotate_translate_vert_function(vertebra = spine_build_list$vert_geoms_square_list$t7, 
                                                    point_of_rotation = vert_list$t8$vert_coord$sp, 
                                                    segment_angle_adjustment = sum(sa_adjustment_df$sa_adjustment[1:11]), 
                                                    initial_translation_correction = vert_list$t8$sp_translation, 
                                                    spine_orientation = spine_orientation)
  
  vert_list$t6 <- jh_rotate_translate_vert_function(vertebra = spine_build_list$vert_geoms_square_list$t6, 
                                                    point_of_rotation = vert_list$t7$vert_coord$sp, 
                                                    segment_angle_adjustment = sum(sa_adjustment_df$sa_adjustment[1:12]), 
                                                    initial_translation_correction = vert_list$t7$sp_translation, 
                                                    spine_orientation = spine_orientation)
  
  vert_list$t5 <- jh_rotate_translate_vert_function(vertebra = spine_build_list$vert_geoms_square_list$t5, 
                                                    point_of_rotation = vert_list$t6$vert_coord$sp, 
                                                    segment_angle_adjustment = sum(sa_adjustment_df$sa_adjustment[1:13]), 
                                                    initial_translation_correction = vert_list$t6$sp_translation, 
                                                    spine_orientation = spine_orientation)
  
  vert_list$t4 <- jh_rotate_translate_vert_function(vertebra = spine_build_list$vert_geoms_square_list$t4, 
                                                    point_of_rotation = vert_list$t5$vert_coord$sp, 
                                                    segment_angle_adjustment = sum(sa_adjustment_df$sa_adjustment[1:14]), 
                                                    initial_translation_correction = vert_list$t5$sp_translation, 
                                                    spine_orientation = spine_orientation)
  
  vert_list$t3 <- jh_rotate_translate_vert_function(vertebra = spine_build_list$vert_geoms_square_list$t3, 
                                                    point_of_rotation = vert_list$t4$vert_coord$sp, 
                                                    segment_angle_adjustment = sum(sa_adjustment_df$sa_adjustment[1:15]), 
                                                    initial_translation_correction = vert_list$t4$sp_translation, 
                                                    spine_orientation = spine_orientation)
  
  vert_list$t2 <- jh_rotate_translate_vert_function(vertebra = spine_build_list$vert_geoms_square_list$t2, 
                                                    point_of_rotation = vert_list$t3$vert_coord$sp, 
                                                    segment_angle_adjustment = sum(sa_adjustment_df$sa_adjustment[1:16]), 
                                                    initial_translation_correction = vert_list$t3$sp_translation, 
                                                    spine_orientation = spine_orientation)
  
  vert_list$t1 <- jh_rotate_translate_vert_function(vertebra = spine_build_list$vert_geoms_square_list$t1, 
                                                    point_of_rotation = vert_list$t2$vert_coord$sp, 
                                                    segment_angle_adjustment = sum(sa_adjustment_df$sa_adjustment[1:17]), 
                                                    initial_translation_correction = vert_list$t2$sp_translation, 
                                                    spine_orientation = spine_orientation)
  
  vert_list$c7 <- jh_rotate_translate_vert_function(vertebra = spine_build_list$vert_geoms_square_list$c7, 
                                                    point_of_rotation = vert_list$t1$vert_coord$sp, 
                                                    segment_angle_adjustment = sum(sa_adjustment_df$sa_adjustment[1:18]), 
                                                    initial_translation_correction = vert_list$t1$sp_translation, 
                                                    spine_orientation = spine_orientation)
  
  ####
  
  vert_list$c6 <- jh_rotate_translate_vert_function(vertebra = spine_build_list$vert_geoms_square_list$c6, 
                                                    point_of_rotation = vert_list$c7$vert_coord$sp, 
                                                    segment_angle_adjustment = sum(sa_adjustment_df$sa_adjustment[1:19]), 
                                                    initial_translation_correction = vert_list$c7$sp_translation, 
                                                    spine_orientation = spine_orientation)
  
  vert_list$c5 <- jh_rotate_translate_vert_function(vertebra = spine_build_list$vert_geoms_square_list$c5, 
                                                    point_of_rotation = vert_list$c6$vert_coord$sp, 
                                                    segment_angle_adjustment = sum(sa_adjustment_df$sa_adjustment[1:20]), 
                                                    initial_translation_correction = vert_list$c6$sp_translation, 
                                                    spine_orientation = spine_orientation)
  
  vert_list$c4 <- jh_rotate_translate_vert_function(vertebra = spine_build_list$vert_geoms_square_list$c4, 
                                                    point_of_rotation = vert_list$c5$vert_coord$sp, 
                                                    segment_angle_adjustment = sum(sa_adjustment_df$sa_adjustment[1:21]), 
                                                    initial_translation_correction = vert_list$c5$sp_translation, 
                                                    spine_orientation = spine_orientation)
  
  vert_list$c3 <- jh_rotate_translate_vert_function(vertebra = spine_build_list$vert_geoms_square_list$c3, 
                                                    point_of_rotation = vert_list$c4$vert_coord$sp, 
                                                    segment_angle_adjustment = sum(sa_adjustment_df$sa_adjustment[1:22]), 
                                                    initial_translation_correction = vert_list$c4$sp_translation, 
                                                    spine_orientation = spine_orientation)
  
  vert_list$c2 <- jh_rotate_translate_vert_function(vertebra = spine_build_list$vert_geoms_square_list$c2, 
                                                    point_of_rotation = vert_list$c3$vert_coord$sp, 
                                                    segment_angle_adjustment = sum(sa_adjustment_df$sa_adjustment[1:23]), 
                                                    initial_translation_correction = vert_list$c3$sp_translation, 
                                                    spine_orientation = spine_orientation)
  
  vert_list$c1 <- jh_rotate_translate_vert_function(vertebra = spine_build_list$vert_geoms_square_list$c1, 
                                                    point_of_rotation = vert_list$c2$vert_coord$sp, 
                                                    segment_angle_adjustment = sum(sa_adjustment_df$sa_adjustment[1:24]), 
                                                    initial_translation_correction = vert_list$c2$sp_translation, 
                                                    spine_orientation = spine_orientation)
  
  vpa_list <- list()
  
  vpa_list$l1pa <- jh_calculate_vertex_angle(vertex_coord = spine_build_list$fem_head_center,
                                             posterior_point_coord = vert_list$sacrum$vert_coord$s1_mid,
                                             ventral_point_coord = vert_list$l1$vert_coord$centroid,
                                             spine_orientation = spine_orientation)
  
  vpa_list$t9pa <- jh_calculate_vertex_angle(vertex_coord = spine_build_list$fem_head_center,
                                             posterior_point_coord = vert_list$sacrum$vert_coord$s1_mid,
                                             ventral_point_coord = vert_list$t9$vert_coord$centroid,
                                             spine_orientation = spine_orientation)
  
  vpa_list$t4pa <- jh_calculate_vertex_angle(vertex_coord = spine_build_list$fem_head_center,
                                             posterior_point_coord = vert_list$sacrum$vert_coord$s1_mid,
                                             ventral_point_coord = vert_list$t4$vert_coord$centroid,
                                             spine_orientation = spine_orientation)
  
  vpa_list$c2pa <- jh_calculate_vertex_angle(vertex_coord = spine_build_list$fem_head_center,
                                             posterior_point_coord = vert_list$sacrum$vert_coord$s1_mid,
                                             ventral_point_coord = vert_list$c2$vert_coord$centroid,
                                             spine_orientation = spine_orientation)
  
  return(list(vert_list = vert_list, 
              vpa_list = vpa_list))
}


