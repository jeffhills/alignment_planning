# jh_calculate_slopes_perpendicular_to_spline_function <- function(df) {
#   # df should have columns x and y in ascending vertebra order
#   # Use spline interpolation to pass exactly through each centroid
#   t <- seq_len(nrow(df))
#   fx <- splinefun(t, df$x, method = "fmm")
#   fy <- splinefun(t, df$y, method = "fmm")
#   
#   # Compute derivatives at each point
#   dx <- fx(t, deriv = 1)
#   dy <- fy(t, deriv = 1)
#   
#   # Slope of tangent line
#   slope_tangent <- dy / dx
#   
#   # Slope of perpendicular line
#   slope_perp <- -1 / slope_tangent
#   
#   slope <- slope_perp*(180/pi)
#   
#   # Return slopes (and optionally the original coordinates)
#   # data.frame(x = df$x, y = df$y, slope_perp = slope_perp)
#   return(df %>% mutate(slope = slope))
# }

jh_calculate_slopes_perpendicular_to_spline_function <- function(df) {
  t <- seq_len(nrow(df))
  
  fx <- splinefun(t, df$x, method = "monoH.FC")
  fy <- splinefun(t, df$y, method = "monoH.FC")
  
  dx <- fx(t, deriv = 1)
  dy <- fy(t, deriv = 1)
  
  tangent_angle_deg <- atan2(dy, dx) * 180 / pi
  perp_angle_deg <- tangent_angle_deg + 90
  
  tibble::as_tibble(df) %>%
    mutate(
      dx = dx,
      dy = dy,
      slope = perp_angle_deg
    )
}

# compute_square_coordinates <- function(sp, ip, endplate_width) {
#   # Compute direction vector from sp to ip
#   dir_vector <- ip - sp
#   dir_vector <- dir_vector / sqrt(sum(dir_vector^2)) # Normalize
#   
#   # Compute perpendicular direction vector (90-degree rotation)
#   perp_vector <- c(-dir_vector[2], dir_vector[1]) * endplate_width
#   
#   # Compute missing coordinates
#   sa <- sp - perp_vector
#   ia <- ip - perp_vector
#   
#   # Return as a list
#   list(sp = sp, ip = ip, ia = ia, sa = sa)
# }
compute_square_coordinates <- function(sp, ip, endplate_width, spine_orientation = "left") {
  # spine_orientation <- match.arg(spine_orientation)
  
  dx <- ip[1] - sp[1]
  dy <- ip[2] - sp[2]
  length <- sqrt(dx^2 + dy^2)
  
  # Perpendicular unit vector
  perp_x <- -dy / length
  perp_y <- dx / length
  
  # Direction of anterior offset depends on orientation
  if ((spine_orientation == "right" && perp_x < 0) ||
      (spine_orientation == "left" && perp_x > 0)) {
    perp_x <- -perp_x
    perp_y <- -perp_y
  }
  
  # offset_x <- (endplate_width / 2) * perp_x
  # offset_y <- (endplate_width / 2) * perp_y
  offset_x <- (endplate_width) * perp_x
  offset_y <- (endplate_width) * perp_y
  
  sa <- c(sp[1] + offset_x, sp[2] + offset_y)
  ia <- c(ip[1] + offset_x, ip[2] + offset_y)
  
  return(list(sp = sp, ip = ip, ia = ia, sa = sa))
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
                                                fade_baseline_spine = FALSE, 
                                                return_vert_geoms_list = FALSE) {
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



jh_calculate_perp_unit_vector_function <- function(df, spine_orientation = "left") {
  t <- seq_len(nrow(df))
  
  fx <- splinefun(t, df$x, method = "monoH.FC")
  fy <- splinefun(t, df$y, method = "monoH.FC")
  
  dx <- fx(t, deriv = 1)
  dy <- fy(t, deriv = 1)
  
  mag <- sqrt(dx^2 + dy^2)
  tx <- dx / mag
  ty <- dy / mag
  
  # one of the 2 possible perpendicular directions
  nx <- -ty
  ny <-  tx
  
  # choose initial direction using global spine orientation
  if (spine_orientation == "left") {
    if (nx[1] < 0) {
      nx <- -nx
      ny <- -ny
    }
  } else {
    if (nx[1] > 0) {
      nx <- -nx
      ny <- -ny
    }
  }
  
  # enforce continuity level to level
  for (i in 2:length(nx)) {
    if ((nx[i] * nx[i - 1] + ny[i] * ny[i - 1]) < 0) {
      nx[i] <- -nx[i]
      ny[i] <- -ny[i]
    }
  }
  
  df %>%
    mutate(
      dx = dx,
      dy = dy,
      tx = tx,
      ty = ty,
      nx = nx,
      ny = ny
    )
}

jh_get_vertebral_corners <- function(centroids_sup_inf_centers_df, s1_width, spine_orientation = "right") {
  library(dplyr)
  library(tibble)
  library(tidyr)
  
  # Vertebrae order from inferior to superior (for width interpolation)
  vertebra_order <- c("l5", "l4", "l3", "l2", "l1",
                      "t12", "t11", "t10", "t9", "t8", "t7", "t6", "t5", "t4", "t3", "t2", "t1",
                      "c7", "c6", "c5", "c4", "c3", "c2")
  
  n_verts <- length(vertebra_order)
  
  # Smooth widths from s1_width (at L5) to s1_width*0.5 (at C2)
  vertebra_widths <- tibble(
    spine_level = vertebra_order,
    width = seq(s1_width, s1_width * 0.4, length.out = n_verts)
  )
  
  inferior_endplate_center_label <- (filter(centroids_sup_inf_centers_df, spine_level == "l5") %>%
                                       filter( spine_index == min(spine_index)))$spine_point
  
  superior_endplate_center_label <- (filter(centroids_sup_inf_centers_df, spine_level == "l5") %>%
                                       filter( spine_index == max(spine_index)))$spine_point
  
  # Extract relevant points
  inf_centroids <- centroids_sup_inf_centers_df %>%
    filter(spine_point == inferior_endplate_center_label, spine_level %in% vertebra_order) %>%
    select(spine_level, x_inf = x, y_inf = y)
  
  sup_centroids <- centroids_sup_inf_centers_df %>%
    filter(spine_point == superior_endplate_center_label, spine_level %in% vertebra_order) %>%
    select(spine_level, x_sup = x, y_sup = y)
  
  centroids <- centroids_sup_inf_centers_df %>%
    filter(spine_point == "centroid", spine_level %in% vertebra_order) %>%
    select(spine_level, x_c = x, y_c = y)
  
  # Join everything
  df <- vertebra_widths %>%
    left_join(inf_centroids, by = "spine_level") %>%
    left_join(sup_centroids, by = "spine_level") %>%
    left_join(centroids, by = "spine_level") %>%
    mutate(spine_level = factor(spine_level, levels = vertebra_order)) %>%
    arrange(spine_level)
  
  # Helper: get tangent direction at a point using neighboring centroids
  # We use the centroid spine curve for tangent estimation
  all_centroids <- centroids_sup_inf_centers_df %>%
    filter(spine_point %in% c("centroid", "inf", "center")) %>%
    # Use centroid where available, else inf (s1) or center (head)
    mutate(order = case_when(
      spine_level == "s1" ~ 0,
      spine_level == "head" ~ 99,
      TRUE ~ match(spine_level, vertebra_order)
    )) %>%
    filter(!is.na(order)) %>%
    arrange(order)
  
  # Build smooth spline through all centroids for tangent estimation
  spline_x <- splinefun(all_centroids$order, all_centroids$x, method = "natural")
  spline_y <- splinefun(all_centroids$order, all_centroids$y, method = "natural")
  
  # For each vertebra, compute corners
  get_perp_offset <- function(t_val, half_width, spline_x, spline_y) {
    dx <- spline_x(t_val, deriv = 1)
    dy <- spline_y(t_val, deriv = 1)
    len <- sqrt(dx^2 + dy^2)
    # Perpendicular unit vector (rotate 90 degrees)
    px <- -dy / len
    py <-  dx / len
    list(px = px * half_width, py = py * half_width)
  }
  
  results <- purrr::map_dfr(seq_len(nrow(df)), function(i) {
    row <- df[i, ]
    lvl <- as.character(row$spine_level)
    t_val <- match(lvl, vertebra_order)
    w <- row$width
    
    # Inferior endplate: perpendicular at inferior centroid
    # We need t-value for the inferior centroid on the spline
    # Use the vertebra's index but offset slightly inferior (toward lower t)
    # Approximate by using t_val - 0.5 for inferior, t_val + 0.5 for superior
    off_inf <- get_perp_offset(t_val - 0.35, w / 2, spline_x, spline_y)
    off_sup <- get_perp_offset(t_val + 0.35, w / 2, spline_x, spline_y)
    
    tibble(
      spine_level = lvl,
      spine_point = c("inferior_posterior", "inferior_anterior", "superior_anterior", "superior_posterior"),
      x = c(
        row$x_inf + off_inf$px,
        row$x_inf - off_inf$px,
        row$x_sup - off_sup$px,
        row$x_sup + off_sup$px
      ),
      y = c(
        row$y_inf + off_inf$py,
        row$y_inf - off_inf$py,
        row$y_sup - off_sup$py,
        row$y_sup + off_sup$py
      )
    )
  })
  
  # Enforce polygon winding order: inf_posterior, inf_anterior, sup_anterior, sup_posterior
  if(spine_orientation == "left"){
    results %>%
      mutate(
        spine_level = factor(spine_level, levels = vertebra_order),
        spine_point = factor(spine_point, levels = c("inferior_posterior", "inferior_anterior",
                                                     "superior_anterior", "superior_posterior"))
      ) %>%
      arrange(spine_level, spine_point) %>%
      mutate(spine_point = case_when(
        spine_point == "inferior_posterior" ~ "inferior_anterior", 
        spine_point == "inferior_anterior" ~ "inferior_posterior", 
        spine_point == "superior_anterior" ~ "superior_posterior", 
        spine_point == "superior_posterior" ~ "superior_anterior" 
      ))
    
  }else{
    results %>%
      mutate(
        spine_level = factor(spine_level, levels = vertebra_order),
        spine_point = factor(spine_point, levels = c("inferior_posterior", "inferior_anterior",
                                                     "superior_anterior", "superior_posterior"))
      ) %>%
      arrange(spine_level, spine_point) 
  }
}

###############################################################
###############################################################
###############################################################

###############################################################
###############################################################
###############################################################
jh_find_s1_inferior_posterior_function <- function(s1_anterior,
                                                   s1_posterior,
                                                   spine_orientation) {
  
  # Vector from s1_posterior to s1_anterior
  dx <- s1_anterior[[1]] - s1_posterior[[1]]
  dy <- s1_anterior[[2]] - s1_posterior[[2]]
  
  # Perpendicular vector (rotate 90 degrees)
  # Two options: (-dy, dx) or (dy, -dx)
  # Choose based on spine_orientation and enforce y < s1_anterior y
  leg_length <- sqrt(dx^2 + dy^2)
  
  perp1 <- c(-dy, dx)
  perp2 <- c(dy, -dx)
  
  candidate_1 <- s1_posterior + perp1
  candidate_2 <- s1_posterior + perp2
  
  # Select direction based on spine_orientation
  if (spine_orientation == "right") {
    s1_inf_post <- if (candidate_1[[2]] < candidate_2[[2]]) candidate_1 else candidate_2
  } else {
    s1_inf_post <- if (candidate_1[[2]] < candidate_2[[2]]) candidate_1 else candidate_2
  }
  
  # Normalize and scale to enforce hypotenuse = 3 * leg_length
  # Right angle at s1_posterior: legs are (s1_posterior->s1_anterior) and (s1_posterior->s1_inf_post)
  # leg_length already = dist(s1_posterior, s1_anterior)
  # hypotenuse = dist(s1_inf_post, s1_anterior) = sqrt(leg^2 + leg^2) = leg * sqrt(2) naturally
  # To make hypotenuse = 3 * leg_length, the perpendicular leg must be sqrt(9-1) * leg = sqrt(8) * leg
  
  perp_leg_length <- sqrt(9 - 1) * leg_length  # = sqrt(8) * leg_length
  
  perp_dir <- s1_inf_post - s1_posterior
  perp_dir_normalized <- perp_dir / sqrt(sum(perp_dir^2))
  
  s1_inf_post_scaled <- s1_posterior + perp_dir_normalized * perp_leg_length
  
  # Enforce y < s1_anterior y
  if (s1_inf_post_scaled[[2]] >= s1_anterior[[2]]) {
    perp_dir_alt <- -perp_dir_normalized
    s1_inf_post_scaled <- s1_posterior + perp_dir_alt * perp_leg_length
  }
  
  return(s1_inf_post_scaled)
}


jh_calculate_scaled_centered_spine_coordinates_from_centroids_function <- function(clicked_coord_df,

                                                                                   spine_orientation,
                                                                                   calibration_modifier = 1){
  
  clicked_coord_list <- (clicked_coord_df %>%
                           mutate(coord = map2(.x = x, .y = y, .f = ~ c(.x, .y))))$coord
  
  names(clicked_coord_list) <- clicked_coord_df$spine_point
  
  femoral_head_center <- clicked_coord_list$fem_head_center
  s1_anterior_superior <- clicked_coord_list$s1_anterior_superior
  s1_posterior_superior <- clicked_coord_list$s1_posterior_superior
  
  s1_superior_center <- jh_get_point_along_line_function(coord_a = s1_anterior_superior, s1_posterior_superior, percent_a_to_b = 0.5)
  
  s1_inferior_mid_point <- jh_find_sacrum_inf_point_function(s1_posterior_sup = s1_posterior_superior,
                                                             s1_anterior_sup = s1_anterior_superior,
                                                             inf_length_multiplier = 1,
                                                             spine_facing =  spine_orientation
  )
  
  
  clicked_coord_df <-tibble(spine_point = "s1_inferior_center", x = s1_inferior_mid_point[1], y = s1_inferior_mid_point[2]) %>%
    bind_rows(tibble(spine_point = "s1_superior_center", x = s1_superior_center[1], y = s1_superior_center[2]))   %>%
    bind_rows(clicked_coord_df %>%
    filter(spine_point %in% get_spine_labels(centroid_labels_for_spline = TRUE)))%>%
    distinct()
  
  #adjust for calibration#  
  if(calibration_modifier != 1){
    femoral_head_center <- femoral_head_center*calibration_modifier
    s1_anterior_superior <- s1_anterior_superior*calibration_modifier
    s1_posterior_superior <- s1_posterior_superior*calibration_modifier
    
    clicked_coord_df <- clicked_coord_df %>%
      mutate(x = x*calibration_modifier, 
             y = y*calibration_modifier)
  }
  
  ## adjust all coordinates so that fem heads are at 0,0
  s1_posterior_superior <- s1_posterior_superior - femoral_head_center 
  s1_anterior_superior <- s1_anterior_superior - femoral_head_center 
  centroid_points_centered_df <- clicked_coord_df %>%
    mutate(x = x - femoral_head_center[[1]], 
           y = y - femoral_head_center[[2]])
  
  s1_width <- jh_calculate_distance_between_2_points_function(point_1 = s1_anterior_superior, point_2 = s1_posterior_superior)
  
  femoral_head_center <- c(0,0)
  
  return_list <- list()
  
  ############## 
  centroid_points_centered_df <- centroid_points_centered_df %>%
    bind_rows(filter(centroid_points_centered_df, spine_point == "c2_centroid") %>%
    mutate(spine_point = "head_centroid", y = y + s1_width))
    
  

  # clicked_df <- tibble(spine_point = "s1_inferior_center", x = s1_inferior_mid_point[1], y = s1_inferior_mid_point[2]) %>%
  #   union_all(clicked_coord_df) %>%
  #   distinct()
  
  inf_sup_centroids_df <- expand_grid(spine_level = c('s1', 'l5', 'l4', 'l3', 'l2', 'l1', 't12', 't11', 't10', 't9', 't8', 't7', 't6', 't5', 't4', 't3', 't2', 't1', 'c7', 'c6', 'c5', 'c4', 'c3', 'c2', 'head'),
                                      point = c('inferior_center', 'centroid', 'superior_center')
  ) %>%
    mutate(segment_index = case_when(
      point == "inferior_center" ~ 0.2, 
      point == "centroid" ~ 0.8, 
      point == "superior_center" ~ 1 
    )) %>%
    mutate(spine_index = cumsum(segment_index)) %>%
    mutate(spine_point = paste0(spine_level, "_", point)) %>%
    left_join(centroid_points_centered_df)  %>%
    mutate(y = zoo::na.spline(y, x = spine_index)) %>%
    mutate(x = zoo::na.spline(x, x = spine_index)) %>%
    select(spine_level, spine_point = point, spine_index, x, y)
  
  # return_list$centroid_points_centered_df <- centroid_points_centered_df
  # 
  # return_list$inf_sup_centroids_df <- inf_sup_centroids_df
  
  return_list$centroids_df <- inf_sup_centroids_df %>%
    mutate(point_type = case_when(
      spine_level == "s1" & spine_point == "superior_center" ~ "key_point",
      spine_level == "s1" & spine_point != "superior_center" ~ "non_key",
      spine_level == "head" ~ "non_key",
      spine_point == "centroid" ~ "key_point",
      TRUE ~ "non_key"
    )) %>%
    filter(point_type == "key_point") %>%
    select(-point_type)
  
  
  ##############
  vertebral_corners_df <- jh_get_vertebral_corners(inf_sup_centroids_df, s1_width, spine_orientation = spine_orientation)
  
  s1_inferior_posterior_point <- jh_find_s1_inferior_posterior_function(s1_anterior = s1_anterior_superior, s1_posterior = s1_posterior_superior, spine_orientation = spine_orientation)
  
  s1_inferior_mid_point <- c(filter(inf_sup_centroids_df, spine_level == "s1", spine_point == "inferior_center")$x, filter(inf_sup_centroids_df, spine_level == "s1", spine_point == "inferior_center")$y)
  
  sacrum_df <- tibble(spine_level = "s1", spine_point = c("ip", 
                                                          "ia",
                                                          "sa",
                                                          "sp"), 
                      x = c(s1_inferior_posterior_point[1], s1_inferior_mid_point[1], s1_anterior_superior[1], s1_posterior_superior[1]),
                      y = c(s1_inferior_posterior_point[2], s1_inferior_mid_point[2], s1_anterior_superior[2], s1_posterior_superior[2])
  )
  
  spine_corner_coordinates_df <- sacrum_df %>%
    union_all(vertebral_corners_df %>%
                mutate(spine_point = case_when(
                  spine_point == "inferior_posterior" ~ "ip",
                  spine_point == "inferior_anterior" ~ "ia",
                  spine_point == "superior_anterior" ~ "sa",
                  spine_point == "superior_posterior" ~ "sp"
                )))
  
  vert_coord_nested_df <-  spine_corner_coordinates_df %>%
    mutate(xy = map2(.x = x, .y = y, .f = ~ c(.x, .y))) %>%
    select(-x, -y) %>%
    pivot_wider(names_from = spine_point, values_from = xy) %>%
    mutate(vert_coord_list = pmap(.l = list(..1 = sp, ..2 = ip, ..3 = ia, ..4 = sa), 
                                  .f = ~ list(sp = ..1, ip = ..2, ia = ..3, sa = ..4))) %>%
    select(spine_level, vert_coord_list)
  
  vert_coord_list <- vert_coord_nested_df$vert_coord_list
  
  names(vert_coord_list) <- vert_coord_nested_df$spine_level
  
  # vert_coord_list <- c(sup_center = c(filter(return_list$centroids_df, spine_point == "superior_center", spine_level == "s1")$x, 
  #                                     filter(return_list$centroids_df, spine_point == "superior_center", spine_level == "s1")$y),
  #                      vert_coord_list$s1)
  
  vert_coord_list$s1$sup_center <- c(filter(return_list$centroids_df, spine_point == "superior_center", spine_level == "s1")$x, 
                                    filter(return_list$centroids_df, spine_point == "superior_center", spine_level == "s1")$y)
  
  
  # fix C1
  vert_coord_list$c1$sp <- vert_coord_list$c2$sp
  vert_coord_list$c1$ip <- jh_get_point_along_line_function(coord_a = vert_coord_list$c2$sp, coord_b = vert_coord_list$c2$ip,percent_a_to_b = 0.3)
  vert_coord_list$c1$ia <- jh_get_point_along_line_function(coord_a = vert_coord_list$c2$sa, coord_b = vert_coord_list$c2$ia,percent_a_to_b = 0.3)
  vert_coord_list$c1$sa <- vert_coord_list$c2$sa
  
  #fix c2
  vert_coord_list$c2 <- jh_construct_odontoid_c2_coordinates_function(c2_vert_coord_list = vert_coord_list$c2)
  
  
  ## assemble coordinates into df
  return_list$vert_coord_df <- enframe(vert_coord_list) %>%
    unnest() %>%
    mutate(vert_point = names(value)) %>%
    unnest_wider(value, names_sep = "_") %>%
    select(spine_level = name, vert_point, x = value_1, y = value_2) 
  
  # return_list$vert_coord_list <- vert_coord_list <- c(
  #   list(femoral_head_center = c(0, 0)),
  #   vert_coord_list
  # )
  return_list$vert_coord_list <- vert_coord_list
  
  centroids_coord_df_for_list <- return_list$centroids_df %>%
    mutate(spine_level_point = paste0(spine_level, "_", spine_point), 
           coord = map2(.x = x, .y = y, .f = ~ c(.x, .y))) %>%
    select(spine_level_point, coord)
  
  return_list$centroids_coord_list <- centroids_coord_df_for_list$coord
  
  names(return_list$centroids_coord_list) <- centroids_coord_df_for_list$spine_level_point
  
  # return_list$vert_coord_df  %>%
  #   ggplot(aes(x = x, y = y, group = spine_level)) + 
  #   geom_polygon()+
  #   coord_fixed()
  
  return(return_list)
  
}

###############################################################
###############################################################
###############################################################


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
  
  centroids_with_scaled_order_df <- centroid_df %>%
    mutate(distance_to_cranial_vert =
             pmap(.l = list(..1 = x, ..2 = y, ..3 = lead(x), ..4 = lead(y)),
                  .f = ~ jh_calculate_distance_between_2_points_function(point_1 = c(..1, ..2), point_2 = c(..3, ..4))
             )
    ) %>%
    unnest() %>%
    mutate(distance_to_cranial_vert = if_else(is.na(distance_to_cranial_vert), max(distance_to_cranial_vert, na.rm = TRUE), distance_to_cranial_vert)) %>%
    mutate(spine_index = cumsum(distance_to_cranial_vert))%>%
    select(spine_point, x, y, spine_index) %>%
    mutate(distance_to_inferior_centroid = spine_index - lag(spine_index))%>%
    mutate(distance_to_superior_centroid = lead(spine_index) - spine_index) %>%
    mutate(distance_to_inferior_centroid = if_else(is.na(distance_to_inferior_centroid), distance_to_superior_centroid, distance_to_inferior_centroid))%>%
    mutate(distance_to_superior_centroid = if_else(is.na(distance_to_superior_centroid), distance_to_inferior_centroid, distance_to_superior_centroid)) %>%
    mutate(vert_inf_index = spine_index - distance_to_inferior_centroid*0.4,
           vert_sup_index = spine_index + distance_to_superior_centroid*0.4) %>%
    select(spine_point, x, y, spine_index, vert_inf_index, vert_sup_index)%>%
    separate(spine_point, into = c("spine_level", "spine_point"), sep = "_")


  inferior_center_empty_df <- centroids_with_scaled_order_df %>%
    select(spine_level, spine_point, x, y, spine_index = vert_inf_index) %>%
    filter(spine_level %in% c("s1", "head") == FALSE) %>%
    mutate(spine_point = "inferior_center", x = NA, y = NA)

  superior_center_empty_df <- centroids_with_scaled_order_df %>%
    select(spine_level, spine_point, x, y, spine_index = vert_sup_index) %>%
    filter(spine_level %in% c("s1", "head") == FALSE) %>%
    mutate(spine_point = "superior_center", x = NA, y = NA)


  inf_sup_centroids_df <- centroids_with_scaled_order_df %>%
    select(spine_level, spine_point, x, y, spine_index) %>%
    union_all(inferior_center_empty_df)%>%
    union_all(superior_center_empty_df) %>%
    arrange(spine_index)%>%
    mutate(
      y = zoo::na.spline(y, x = spine_index),
      x = zoo::na.spline(x, x = spine_index)
    )


  vertebral_corners_df <- jh_get_vertebral_corners(inf_sup_centroids_df, s1_width)
  
  
  ### First, fit a spline to locate all interspaces
  # centroid_interspaces_df <- centroid_df %>%
  #   mutate(spine_order = row_number()) %>%
  #   add_row(spine_point = "head_center", x = tail(centroid_df, 1)$x, y = tail(centroid_df, 1)$y + 100, spine_order = 30)%>%
  #   add_row(spine_point = "s1_inf", x = s1_inf_point[[1]], y = s1_inf_point[[2]], spine_order = 0) %>%
  #   arrange(spine_order) %>%
  #   select(-spine_order) %>%
  #   # add_row(spine_point = "head_center", x = tail(centroid_df, 1)$x, y = tail(centroid_df, 1)$y + 100)%>%
  #   # add_row(spine_point = "s1_inf", x = s1_inf_point[[1]], y = s1_inf_point[[2]]) %>%
  #   # arrange(y) %>%
  #   separate(spine_point, into = c("spine_level", "spine_point"), sep = "_") %>%
  #   mutate(spine_numeric = row_number() -1 )%>%
  #   bind_rows(tibble(spine_level = str_to_lower(str_replace_all(rev(jh_spine_levels_factors_df$interspace), "-", "_"))) %>%
  #               mutate(spine_point = "interspace") %>%
  #               mutate(spine_numeric = row_number() + 0.5)) %>%
  #   arrange(spine_numeric) %>%
  #   mutate(y = zoo::na.spline(y)) %>%
  #   mutate(x = zoo::na.spline(x)) 
  
  
  
  # coord_w_post_wall_df <- jh_calculate_perp_unit_vector_function(
  #   df = centroid_interspaces_df,
  #   spine_orientation = spine_orientation
  # ) %>%
  #   mutate(endplate_width = seq(from = s1_width, to = s1_width * 0.5, length = nrow(.))) %>%
  #   mutate(
  #     posterior_wall_x = x + nx * (endplate_width * 0.5),
  #     posterior_wall_y = y + ny * (endplate_width * 0.5),
  #     anterior_wall_x  = x - nx * (endplate_width * 0.5),
  #     anterior_wall_y  = y - ny * (endplate_width * 0.5)
  #   ) %>%
  #   mutate(
  #     posterior_wall_x = if_else(spine_level == "s1" & spine_point == "center", s1_posterior_superior[[1]], posterior_wall_x),
  #     posterior_wall_y = if_else(spine_level == "s1" & spine_point == "center", s1_posterior_superior[[2]], posterior_wall_y),
  #     anterior_wall_x  = if_else(spine_level == "s1" & spine_point == "center", s1_anterior_superior[[1]], anterior_wall_x),
  #     anterior_wall_y  = if_else(spine_level == "s1" & spine_point == "center", s1_anterior_superior[[2]], anterior_wall_y)
  #   )
  

  
  # anterior_wall_coord_for_wedged_vert_df  <- coord_w_post_wall_df %>%
  #   filter(str_detect(spine_point, "center|interspace")) %>%
  #   filter(spine_level != "l5_s1") %>%
  #   select(spine_level, anterior_wall_x, anterior_wall_y) %>%
  #   mutate(spine_level = str_replace_all(spine_level, "s1", "l5_s1")) %>%
  #   mutate(disc_level = spine_level) %>%
  #   separate(spine_level, into = c("spine_level", "caudal")) %>%
  #   select(-caudal) %>%
  #   mutate(anterior_wall_x = if_else(spine_level == "l5", s1_anterior_superior[[1]], anterior_wall_x),
  #          anterior_wall_y = if_else(spine_level == "l5", s1_anterior_superior[[2]], anterior_wall_y)) %>%
  #   mutate(anterior_wall_coord = map2(anterior_wall_x, anterior_wall_y, ~ c(.x, .y))) %>%
  #   mutate(ia_by_disc = map2(anterior_wall_coord, lead(anterior_wall_coord),
  #                            ~ jh_get_point_along_line_function(coord_a = .x, coord_b = .y, percent_a_to_b = 0.1))) %>%
  #   mutate(sa_by_disc = map2(anterior_wall_coord, lead(anterior_wall_coord),
  #                            ~ jh_get_point_along_line_function(coord_a = .x, coord_b = .y, percent_a_to_b = 0.9))) %>%
  #   select(spine_level, ia_by_disc, sa_by_disc) %>%
  #   filter(spine_level != "head") %>%
  #   mutate(anterior_wall_height_by_disc = map2_dbl(ia_by_disc, sa_by_disc, jh_calculate_distance_between_2_points_function))
  
  
  # return_list$coord_w_post_wall_df <- coord_w_post_wall_df
  
  ## computer posterior corner coordinates
  # posterior_corners_nested_df <- coord_w_post_wall_df %>%
  #   filter(str_detect(spine_point, "center|interspace")) %>%
  #   filter(spine_level != "l5_s1") %>%
  #   select(spine_level, posterior_wall_x, posterior_wall_y) %>%
  #   mutate(spine_level = str_replace_all(spine_level, "s1", "l5_s1")) %>%
  #   mutate(disc_level = spine_level) %>%
  #   separate(spine_level, into = c("spine_level", "caudal"))%>%
  #   select(-caudal) %>%
  #   mutate(posterior_wall_x = if_else(spine_level == "l5", s1_posterior_superior[[1]], posterior_wall_x),
  #          posterior_wall_y = if_else(spine_level == "l5", s1_posterior_superior[[2]], posterior_wall_y)) %>%
  #   mutate(posterior_wall_coord = map2(.x = posterior_wall_x, .y = posterior_wall_y, 
  #                                      .f = ~ c(.x, .y))) %>%
  #   mutate(ip = map2(.x = posterior_wall_coord, .y = lead(posterior_wall_coord),
  #                    .f = ~ jh_get_point_along_line_function(coord_a = .x, 
  #                                                            coord_b = .y,
  #                                                            percent_a_to_b = 0.1)))%>%
  #   mutate(sp = map2(.x = posterior_wall_coord, .y = lead(posterior_wall_coord),
  #                    .f = ~ jh_get_point_along_line_function(coord_a = .x, 
  #                                                            coord_b = .y,
  #                                                            percent_a_to_b = 0.9))) %>%
  #   filter(spine_level != "head") 
  
  
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
                                  .f = ~ compute_square_coordinates(sp = ..1, ip = ..2, endplate_width = ..3, spine_orientation = spine_orientation))) %>%
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


# jh_construct_spine_coord_df_from_centroids_function <- function(s1_posterior_superior = NULL , 
#                                                                 s1_anterior_superior = NULL, 
#                                                                 centroid_df = tibble(),
#                                                                 femoral_head_center = c(0,0),
#                                                                 calibration_modifier = 1,
#                                                                 spine_orientation = "left"){
#   
#   if(calibration_modifier != 1){
#     femoral_head_center <- femoral_head_center*calibration_modifier
#     s1_anterior_superior <- s1_anterior_superior*calibration_modifier
#     s1_posterior_superior <- s1_posterior_superior*calibration_modifier
#     centroid_df <- centroid_df %>%
#       mutate(x = x*calibration_modifier, 
#              y = y*calibration_modifier)
#   }
#   
#   ## adjust all coordinates so that fem heads are at 0,0
#   s1_posterior_superior <- s1_posterior_superior - femoral_head_center 
#   s1_anterior_superior <- s1_anterior_superior - femoral_head_center 
#   centroid_df <- centroid_df %>%
#     mutate(x = x - femoral_head_center[[1]], 
#            y = y - femoral_head_center[[2]])
#   
#   femoral_head_center <- c(0,0)
#   
#   return_list <- list()
#   
#   s1_width <- jh_calculate_distance_between_2_points_function(s1_posterior_superior, s1_anterior_superior)
#   
#   s1_inf_point <- jh_find_sacrum_inf_point_function(s1_posterior_sup = s1_posterior_superior, s1_anterior_sup = s1_anterior_superior, inf_length_multiplier = 1)
#   
#   ### First, fit a spline to locate all interspaces
#   centroid_interspaces_df <- centroid_df %>%
#     mutate(spine_order = row_number()) %>%
#     add_row(spine_point = "head_center", x = tail(centroid_df, 1)$x, y = tail(centroid_df, 1)$y + 100, spine_order = 30)%>%
#     add_row(spine_point = "s1_inf", x = s1_inf_point[[1]], y = s1_inf_point[[2]], spine_order = 0) %>%
#     arrange(spine_order) %>%
#     select(-spine_order) %>%
#     # add_row(spine_point = "head_center", x = tail(centroid_df, 1)$x, y = tail(centroid_df, 1)$y + 100)%>%
#     # add_row(spine_point = "s1_inf", x = s1_inf_point[[1]], y = s1_inf_point[[2]]) %>%
#     # arrange(y) %>%
#     separate(spine_point, into = c("spine_level", "spine_point"), sep = "_") %>%
#     mutate(spine_numeric = row_number() -1 )%>%
#     bind_rows(tibble(spine_level = str_to_lower(str_replace_all(rev(jh_spine_levels_factors_df$interspace), "-", "_"))) %>%
#                 mutate(spine_point = "interspace") %>%
#                 mutate(spine_numeric = row_number() + 0.5)) %>%
#     arrange(spine_numeric) %>%
#     mutate(y = zoo::na.spline(y)) %>%
#     mutate(x = zoo::na.spline(x)) 
#   
#   # spine_orientation <- "left"
#   
#   
#   ### With known endplate width, calculate the posterior wall or posterior disc space coordinate that is perpendicular 
#   # coord_w_post_wall_df <- jh_calculate_slopes_perpendicular_to_spline_function(df = centroid_interspaces_df) %>%
#   #   mutate(endplate_width = seq(from = s1_width, to = s1_width*0.5, length = nrow(.))) %>%
#   #   mutate(posterior_wall_point = pmap(.l = list(..1 = x,
#   #                                                ..2 = y,
#   #                                                ..3 = slope,
#   #                                                ..4 = endplate_width*0.5),
#   #                                      .f = ~ jh_calculate_coordinate_from_slope_hypotenuse_function(x = ..1,
#   #                                                                                                    y = ..2,
#   #                                                                                                    slope_deg = ..3,
#   #                                                                                                    hypotenuse = ..4,
#   #                                                                                                    spine_facing = spine_orientation,
#   #                                                                                                    posterior_or_anterior_coordinate = "posterior")))  %>%
#   #   mutate(posterior_wall_x = map(.x = posterior_wall_point, .f = ~ .x[[1]]))%>%
#   #   mutate(posterior_wall_y = map(.x = posterior_wall_point, .f = ~ .x[[2]])) %>%
#   #   select(-posterior_wall_point) %>%
#   #   unnest() %>%
#   #   mutate(posterior_wall_x = if_else(spine_point == "s1_center", s1_posterior_superior[[1]], posterior_wall_x),
#   #          posterior_wall_y = if_else(spine_point == "s1_center", s1_posterior_superior[[2]], posterior_wall_y))
#   # 
#   
# coord_w_post_wall_df <- jh_calculate_perp_unit_vector_function(
#   df = centroid_interspaces_df,
#   spine_orientation = spine_orientation
# ) %>%
#   mutate(endplate_width = seq(from = s1_width, to = s1_width * 0.5, length = nrow(.))) %>%
#   mutate(
#     posterior_wall_x = x + nx * (endplate_width * 0.5),
#     posterior_wall_y = y + ny * (endplate_width * 0.5),
#     anterior_wall_x  = x - nx * (endplate_width * 0.5),
#     anterior_wall_y  = y - ny * (endplate_width * 0.5)
#   ) %>%
#   mutate(
#     posterior_wall_x = if_else(spine_level == "s1" & spine_point == "center", s1_posterior_superior[[1]], posterior_wall_x),
#     posterior_wall_y = if_else(spine_level == "s1" & spine_point == "center", s1_posterior_superior[[2]], posterior_wall_y),
#     anterior_wall_x  = if_else(spine_level == "s1" & spine_point == "center", s1_anterior_superior[[1]], anterior_wall_x),
#     anterior_wall_y  = if_else(spine_level == "s1" & spine_point == "center", s1_anterior_superior[[2]], anterior_wall_y)
#   )
#   
#   
#   ## construct anterior interspace coordinates for thoracic wedging correction later:
#   ## construct anterior interspace coordinates for thoracic wedging correction later:
#   # anterior_wall_coord_for_wedged_vert_df  <- coord_w_post_wall_df %>%
#   #   mutate(anterior_wall_point = pmap(.l = list(..1 = x,
#   #                                               ..2 = y, 
#   #                                               ..3 = slope, 
#   #                                               ..4 = endplate_width*0.5), 
#   #                                     .f = ~ jh_calculate_coordinate_from_slope_hypotenuse_function(x = ..1, 
#   #                                                                                                   y = ..2,
#   #                                                                                                   slope_deg = ..3, 
#   #                                                                                                   hypotenuse = ..4,
#   #                                                                                                   spine_facing = spine_orientation,                                                                                                posterior_or_anterior_coordinate = "anterior"))) %>%
#   #   mutate(anterior_wall_x = map(.x = anterior_wall_point, .f = ~ .x[[1]]))%>%
#   #   mutate(anterior_wall_y = map(.x = anterior_wall_point, .f = ~ .x[[2]])) %>%
#   #   select(-anterior_wall_point) %>%
#   #   unnest()  %>%
#   #   filter(str_detect(spine_point, "center|interspace")) %>%
#   #   filter(spine_level != "l5_s1") %>%
#   #   select(spine_level, anterior_wall_x, anterior_wall_y) %>%
#   #   mutate(spine_level = str_replace_all(spine_level, "s1", "l5_s1")) %>%
#   #   mutate(disc_level = spine_level)%>%
#   #   separate(spine_level, into = c("spine_level", "caudal"))%>%
#   #   select(-caudal) %>%
#   #   mutate(anterior_wall_x = if_else(spine_level == "l5", s1_anterior_superior[[1]], anterior_wall_x),
#   #          anterior_wall_y = if_else(spine_level == "l5", s1_anterior_superior[[2]], anterior_wall_y)) %>%
#   #   mutate(anterior_wall_coord = map2(.x = anterior_wall_x, .y = anterior_wall_y, 
#   #                                     .f = ~ c(.x, .y))) %>%
#   #   mutate(ia_by_disc = map2(.x = anterior_wall_coord, .y = lead(anterior_wall_coord),
#   #                            .f = ~ jh_get_point_along_line_function(coord_a = .x, 
#   #                                                                    coord_b = .y,
#   #                                                                    percent_a_to_b = 0.1)))%>%
#   #   mutate(sa_by_disc = map2(.x = anterior_wall_coord, .y = lead(anterior_wall_coord),
#   #                            .f = ~ jh_get_point_along_line_function(coord_a = .x, 
#   #                                                                    coord_b = .y,
#   #                                                                    percent_a_to_b = 0.9))) %>%
#   #   select(spine_level, ia_by_disc, sa_by_disc) %>%
#   #   filter(spine_level != "head") %>%
#   #   mutate(anterior_wall_height_by_disc = map2(.x = ia_by_disc, .y = sa_by_disc, .f = ~ jh_calculate_distance_between_2_points_function(point_1 = .x, point_2 = .y))) %>%
#   #   unnest(anterior_wall_height_by_disc)
#   
#   anterior_wall_coord_for_wedged_vert_df  <- coord_w_post_wall_df %>%
#     filter(str_detect(spine_point, "center|interspace")) %>%
#     filter(spine_level != "l5_s1") %>%
#     select(spine_level, anterior_wall_x, anterior_wall_y) %>%
#     mutate(spine_level = str_replace_all(spine_level, "s1", "l5_s1")) %>%
#     mutate(disc_level = spine_level) %>%
#     separate(spine_level, into = c("spine_level", "caudal")) %>%
#     select(-caudal) %>%
#     mutate(anterior_wall_x = if_else(spine_level == "l5", s1_anterior_superior[[1]], anterior_wall_x),
#            anterior_wall_y = if_else(spine_level == "l5", s1_anterior_superior[[2]], anterior_wall_y)) %>%
#     mutate(anterior_wall_coord = map2(anterior_wall_x, anterior_wall_y, ~ c(.x, .y))) %>%
#     mutate(ia_by_disc = map2(anterior_wall_coord, lead(anterior_wall_coord),
#                              ~ jh_get_point_along_line_function(coord_a = .x, coord_b = .y, percent_a_to_b = 0.1))) %>%
#     mutate(sa_by_disc = map2(anterior_wall_coord, lead(anterior_wall_coord),
#                              ~ jh_get_point_along_line_function(coord_a = .x, coord_b = .y, percent_a_to_b = 0.9))) %>%
#     select(spine_level, ia_by_disc, sa_by_disc) %>%
#     filter(spine_level != "head") %>%
#     mutate(anterior_wall_height_by_disc = map2_dbl(ia_by_disc, sa_by_disc, jh_calculate_distance_between_2_points_function))
#   
#   
#   # return_list$coord_w_post_wall_df <- coord_w_post_wall_df
#   
#   ## computer posterior corner coordinates
#   posterior_corners_nested_df <- coord_w_post_wall_df %>%
#     filter(str_detect(spine_point, "center|interspace")) %>%
#     filter(spine_level != "l5_s1") %>%
#     select(spine_level, posterior_wall_x, posterior_wall_y) %>%
#     mutate(spine_level = str_replace_all(spine_level, "s1", "l5_s1")) %>%
#     mutate(disc_level = spine_level) %>%
#     separate(spine_level, into = c("spine_level", "caudal"))%>%
#     select(-caudal) %>%
#     mutate(posterior_wall_x = if_else(spine_level == "l5", s1_posterior_superior[[1]], posterior_wall_x),
#            posterior_wall_y = if_else(spine_level == "l5", s1_posterior_superior[[2]], posterior_wall_y)) %>%
#     mutate(posterior_wall_coord = map2(.x = posterior_wall_x, .y = posterior_wall_y, 
#                                        .f = ~ c(.x, .y))) %>%
#     mutate(ip = map2(.x = posterior_wall_coord, .y = lead(posterior_wall_coord),
#                      .f = ~ jh_get_point_along_line_function(coord_a = .x, 
#                                                              coord_b = .y,
#                                                              percent_a_to_b = 0.1)))%>%
#     mutate(sp = map2(.x = posterior_wall_coord, .y = lead(posterior_wall_coord),
#                      .f = ~ jh_get_point_along_line_function(coord_a = .x, 
#                                                              coord_b = .y,
#                                                              percent_a_to_b = 0.9))) %>%
#     filter(spine_level != "head") 
#   
# 
#   vert_coord_nested_df <-  posterior_corners_nested_df %>%
#     select(spine_level, ip, sp) %>%
#     left_join(coord_w_post_wall_df %>%
#                 filter(spine_point == "centroid") %>%
#                 select(spine_level, endplate_width)) %>%
#     mutate(posterior_wall_height = map2(.x = ip, .y = sp, .f = ~ jh_calculate_distance_between_2_points_function(point_1 = .x, point_2 = .y))) %>%
#     unnest(posterior_wall_height) %>%
#     left_join(anterior_wall_coord_for_wedged_vert_df) %>%
#     mutate(vert_shape = if_else(anterior_wall_height_by_disc > posterior_wall_height, "square", "wedged")) %>%
#     select(spine_level, endplate_width, vert_shape, ip, sp, ia_by_disc, sa_by_disc) %>%
#     mutate(vert_coord_list_wedged = pmap(.l = list(..1 = sp, ..2 = ip, ..3 = ia_by_disc, ..4 = sa_by_disc), 
#                                          .f = ~ list(sp = ..1, ip = ..2, ia = ..3, sa = ..4))) %>%
#     mutate(vert_coord_list = pmap(.l = list(..1 = sp,
#                                             ..2 = ip, 
#                                             ..3 =endplate_width,
#                                             ..4 = vert_shape), 
#                                   .f = ~ compute_square_coordinates(sp = ..1, ip = ..2, endplate_width = ..3, spine_orientation = spine_orientation))) %>%
#     mutate(vert_coord_list = if_else(vert_shape == "wedged", vert_coord_list_wedged, vert_coord_list))%>%
#     select(spine_level, vert_shape, vert_coord_list)
#   
#   
#   vert_coord_list <- vert_coord_nested_df$vert_coord_list
#   
#   names(vert_coord_list) <- vert_coord_nested_df$spine_level
#   
#   
#   
#   # fix C1
#   vert_coord_list$c1$sp <- vert_coord_list$c2$sp
#   vert_coord_list$c1$ip <- jh_get_point_along_line_function(coord_a = vert_coord_list$c2$sp, coord_b = vert_coord_list$c2$ip,percent_a_to_b = 0.3)
#   vert_coord_list$c1$ia <- jh_get_point_along_line_function(coord_a = vert_coord_list$c2$sa, coord_b = vert_coord_list$c2$ia,percent_a_to_b = 0.3)
#   vert_coord_list$c1$sa <- vert_coord_list$c2$sa
#   
#   #fix c2
#   vert_coord_list$c2 <- jh_construct_odontoid_c2_coordinates_function(c2_vert_coord_list = vert_coord_list$c2)
#   
#   
#   #add sacrum
#   sac_inf_far <- jh_find_sacrum_inf_point_function(s1_posterior_sup = s1_posterior_superior, 
#                                                    s1_anterior_sup = s1_anterior_superior, 
#                                                    spine_facing = spine_orientation,
#                                                    inf_length_multiplier = 2.5)
#   
#   
#   sacrum <- list()
#   sacrum$sa <- s1_anterior_superior
#   sacrum$sp <- s1_posterior_superior
#   sacrum$inf <- c(sac_inf_far[[1]], sac_inf_far[[2]])
#   
#   # Prepend the sacrum list to the beginning of vert_coord_list
#   return_list$vert_coord_list <- c(list(sacrum = sacrum), vert_coord_list)
#   
# 
#   ## construct interspace coord df
# 
#   
#   ## assemble coordinates into df
#   return_list$vert_coord_df <- enframe(return_list$vert_coord_list) %>%
#     unnest() %>%
#     mutate(vert_point = names(value)) %>%
#     unnest_wider(value, names_sep = "_") %>%
#     select(spine_level = name, vert_point, x = value_1, y = value_2) 
#   
#   ## construct interspace coord df
#   return_list$interspace_coord_df <- jh_calculate_posterior_interspace_coord_from_coord_df_function(vert_coord_df = return_list$vert_coord_df)
#   
#   return_list$interspace_list <- (return_list$interspace_coord_df %>%
#                                     mutate(coord = map2(.x = x, .y = y, .f = ~ c(.x, .y))))$coord
#   
#   names(return_list$interspace_list) <- return_list$interspace_coord_df$interspace
#   
#   # return_list$interspace_list
#   
#   ## add the new centroids to the return list 
#   centroid_coord_list <- (centroid_df %>%
#                             mutate(centroid_coord = map2(.x = x, .y = y, .f = ~ c(.x, .y))))$centroid_coord
#   
#   names(centroid_coord_list) <- str_remove_all(centroid_df$spine_point, pattern = "_center|_centroid")
#   
#   return_list$centroid_coord_list <- c(list(femoral_head = c(0,0)), centroid_coord_list)
#   
#   return(return_list)
#   
# } 




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
  sp <- vertebral_coord_list$sp
  ip <- vertebral_coord_list$ip
  ia <- vertebral_coord_list$ia
  sa <- vertebral_coord_list$sa
  
  # Vector from sa to sp and sa to ip
  sa_sp_vec <- c(sp[[1]] - sa[[1]], sp[[2]] - sa[[2]])
  sa_ip_vec <- c(ip[[1]] - sa[[1]], ip[[2]] - sa[[2]])
  
  # Maximum angle = angle at sa between old sp and ip
  cos_max <- sum(sa_sp_vec * sa_ip_vec) / (sqrt(sum(sa_sp_vec^2)) * sqrt(sum(sa_ip_vec^2)))
  max_angle_deg <- acos(clamp(cos_max, -1, 1)) * 180 / pi
  
  # Clamp pso_angle to maximum
  pso_angle_clamped <- min(pso_angle, max_angle_deg)
  theta <- pso_angle_clamped * pi / 180
  
  # Build both rotation directions
  rot_ccw <- matrix(c(cos(theta), -sin(theta),  sin(theta), cos(theta)), nrow = 2, byrow = TRUE)
  rot_cw  <- matrix(c(cos(theta),  sin(theta), -sin(theta), cos(theta)), nrow = 2, byrow = TRUE)
  
  new_sp_ccw <- as.numeric(c(sa[[1]], sa[[2]]) + rot_ccw %*% sa_sp_vec)
  new_sp_cw  <- as.numeric(c(sa[[1]], sa[[2]]) + rot_cw  %*% sa_sp_vec)
  
  # Correct rotation moves sp closer to ip
  dist_ccw <- sqrt(sum((new_sp_ccw - c(ip[[1]], ip[[2]]))^2))
  dist_cw  <- sqrt(sum((new_sp_cw  - c(ip[[1]], ip[[2]]))^2))
  
  new_sp <- if(dist_ccw < dist_cw) new_sp_ccw else new_sp_cw
  
  list(sp = new_sp, ip = ip, ia = ia, sa = sa)
}

# Helper to clamp a value between min and max
clamp <- function(x, min_val, max_val) max(min_val, min(max_val, x))


# 
# jh_calculate_pso_coordinates_function <- function(vertebral_coord_list, pso_angle) {
#   # Extract coordinates
#   sp <- vertebral_coord_list$sp
#   ip <- vertebral_coord_list$ip
#   ia <- vertebral_coord_list$ia
#   sa <- vertebral_coord_list$sa
#   
#   # Convert angle to radians
#   theta <- pso_angle * pi / 180
#   
#   # Compute the new sp coordinate
#   # Define the vector from sa to sp
#   sa_sp_vec <- c(sp[[1]] - sa[[1]], sp[[2]] - sa[[2]])
#   
#   # Compute the rotation matrix
#   rot_matrix <- matrix(c(cos(theta), -sin(theta), sin(theta), cos(theta)), nrow = 2)
#   
#   # Apply the rotation to sa_sp_vec
#   new_sp_vec <- rot_matrix %*% sa_sp_vec
#   
#   # Compute the new sp coordinate
#   new_sp <- c(sa[[1]] + new_sp_vec[1], sa[[2]] + new_sp_vec[2])
#   # new_sp <- list(x = sa[[1]] + new_sp_vec[1], y = sa$y + new_sp_vec[2])
#   
#   # Return updated coordinate list
#   updated_vertebral_coords <- list(sp = new_sp, ip = ip, ia = ia, sa = sa)
#   
#   return(updated_vertebral_coords)
# }


jh_construct_adjusted_spine_list_function <- function(segment_angle_adjustment_df = tibble(), 
                                                      pso_list = list(),
                                                 spine_list = list(),
                                                 spine_orientation = "left", 
                                                 adjust_for_pt_change = TRUE){
  
  
  spine_level_adjustment_df <- segment_angle_adjustment_df %>%
    mutate(spine_interspace = str_to_lower(str_replace_all(spine_interspace, "-", "_"))) %>%
    separate(remove = FALSE, spine_interspace, into = c("cranial", "caudal"))%>%
    mutate(caudal = if_else(caudal == "s1", "s1", caudal)) %>%
    select(spine_level = cranial, adjustment)
  
  if(spine_orientation == "right"){
    pso_list <- map(.x = pso_list, .f = ~ .x*-1)
    
    spine_level_adjustment_df <- spine_level_adjustment_df %>%
      mutate(adjustment = adjustment*-1)
  }
  spine_orientation <- "left"
  
  
  sa_adjustment_list <- as.list(spine_level_adjustment_df$adjustment)
  names(sa_adjustment_list) <- spine_level_adjustment_df$spine_level
  
  rotated_translated_spine_list <- list()
  
  ## s1
  rotated_translated_spine_list$s1 <- spine_list$vert_coord_list$s1
  ##### next vert:
  
  spine_vert_list <- spine_list$vert_coord_list

  if(any(names(pso_list)== "l5")){
    spine_vert_list$l5 <- jh_calculate_pso_coordinates_function(vertebral_coord_list = spine_vert_list$l5, pso_angle = pso_list$l5)
    sa_adjustment_list$l4 <- sa_adjustment_list$l4 + pso_list$l5
  }
  
  rotated_translated_spine_list$l5 <- jh_rotate_translate_vert_list_coord_function(vert_coord_list = spine_vert_list$l5, 
                                                                    point_of_rotation = spine_list$vert_coord_list$s1$sp,
                                                                    segment_angle_adjustment = sa_adjustment_list$l5,
                                                                    inferior_vertebra_original_list = spine_list$vert_coord_list$s1,
                                                                    inferior_vertebra_new_list = spine_list$vert_coord_list$s1,
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
  

  rotated_translated_spine_list$c1 <- c1_rot_trans_list
  

  return_list <- list()
  if(adjust_for_pt_change == TRUE){
    
    preop_pt <- jh_calculate_vertebral_tilt_function(vertebral_centroid = spine_list$centroids_coord_list$s1_superior_center, 
                                                     femoral_head_center = c(0,0),
                                                     # femoral_head_center = spine_list$centroids_coord_list$femoral_head, 
                                                     pelvic_tilt_modifier = TRUE,
                                                     spine_orientation = spine_orientation)
    
    preop_c2_tilt <- jh_calculate_vertebral_tilt_function(vertebral_centroid = spine_list$centroids_coord_list$c2, 
                                                          femoral_head_center = c(0,0),
                                                          # femoral_head_center = spine_list$centroids_coord_list$femoral_head, 
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
    
    return_list$centroids_coord_list <- map2(.x = return_list$vert_coord_list, .y = names(return_list$vert_coord_list),
                                            .f = ~ jh_calculate_vertebra_centroid_function(vertebra_coords = .x, vertebral_level = .y))
    
    names(return_list$centroids_coord_list) <- str_replace_all(names(return_list$vert_coord_list), "sacrum", "s1")
    
    return_list$centroids_coord_list <- c(list(femoral_head = c(0,0)), return_list$centroids_coord_list)
    
    return_list$centroids_coord_list$c1 <- NULL
    
    
    
  }else{
    return_list$vert_coord_df <- jh_convert_spine_coord_list_to_df_function(rotated_translated_spine_list)
    
    return_list$vert_coord_list <- rotated_translated_spine_list
    
    return_list$interspace_coord_df <- jh_calculate_posterior_interspace_coord_from_coord_df_function(vert_coord_df = return_list$vert_coord_df)
    
    return_list$interspace_list <- (return_list$interspace_coord_df %>%
                                      mutate(coord = map2(.x = x, .y = y, .f = ~ c(.x, .y))))$coord
    
    return_list$centroids_coord_list <- map2(.x = return_list$vert_coord_list, .y = names(return_list$vert_coord_list),
                                            .f = ~ jh_calculate_vertebra_centroid_function(vertebra_coords = .x, vertebral_level = .y))
    
    names(return_list$centroids_coord_list) <- str_replace_all(names(return_list$vert_coord_list), "sacrum", "s1")
    
    return_list$centroids_coord_list <- c(list(femoral_head = c(0,0)), return_list$centroids_coord_list)
    
    return_list$centroids_coord_list$c1 <- NULL
    
    
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



jh_spine_build_sacrum_function <- function(pelvic_incidence = 50,
                                           pelvic_tilt = 10,
                                           fem_head_center_coord = c(0,0), 
                                           fh_to_s1_center = 10, 
                                           s1_endplate_length = 3){
  
  ## compute radians
  pelvic_incidence_radians <- (pi / 180) * (pelvic_incidence)
  pelvic_tilt_radians <- (pi / 180) * (pelvic_tilt)
  sacral_slope_radians <- (pi / 180) * (pelvic_incidence - pelvic_tilt)
  
  sacrum_list <- list()
  # S1 mid (center of superior endplate) located along the PT ray from FH
  sacrum_list$mid <- c(
    fem_head_center_coord[1] - fh_to_s1_center * sin(pelvic_tilt_radians),
    fem_head_center_coord[2] + fh_to_s1_center * cos(pelvic_tilt_radians)
  )
  
  # Unit vector along superior endplate, from SP → SA
  u <- c(cos(sacral_slope_radians), -sin(sacral_slope_radians))
  # Superior corners
  sacrum_list$sp <- sacrum_list$mid - 0.5 * s1_endplate_length * u
  sacrum_list$sa <- sacrum_list$mid + 0.5 * s1_endplate_length * u
  
  posterior_sacrum_length <- jh_calculate_distance_between_2_points_function(point_1 = sacrum_list$sp, point_2 = fem_head_center_coord)*0.8
  
  perp1 <- c(-u[2],  u[1])
  perp2 <- c( u[2], -u[1])
  
  if(pelvic_incidence - pelvic_tilt < 91){
    ip = if(perp1[2] < 0) perp1 else perp2
  }else{
    ip = if(perp1[2] > 0) perp1 else perp2
  }
  
  sacrum_list$ip = c(sacrum_list$sp[[1]] + posterior_sacrum_length * ip[1], 
                     sacrum_list$sp[[2]] + posterior_sacrum_length * ip[2])
  
  sacrum_list$ia <- c(sacrum_list$ip[[1]] + s1_endplate_length*0.1 * cos(sacral_slope_radians), 
                      sacrum_list$ip[[2]] - s1_endplate_length*0.1 * sin(sacral_slope_radians))
  
  sacrum_sf <- st_polygon(list(rbind(sacrum_list$sp,
                                     sacrum_list$sa, 
                                     sacrum_list$ia,
                                     sacrum_list$ip, 
                                     sacrum_list$sp)))
  
  sacrum_df <- tibble(
    spine_level = "s1",
    spine_point = c("sp","sa","ia","ip","mid"),
    x     = c(sacrum_list$sp[1],  sacrum_list$s1_mid[1], sacrum_list$sa[1], sacrum_list$ia[1], sacrum_list$ip[1], sacrum_list$sp[1]),
    y     = c(sacrum_list$sp[2], sacrum_list$s1_mid[2], sacrum_list$sa[2], sacrum_list$ia[2], sacrum_list$ip[2], sacrum_list$sp[2])
  )
  
  return(list(sacrum_list = sacrum_list, 
              sacrum_df = sacrum_df,
              sacrum_sf = sacrum_sf 
  ))
}

jh_construct_vpa_line_geoms_from_vert_coord_function <- function(vertebral_level, 
                                                                 centroid_coord_list, 
                                                                 line_size = 2, 
                                                                 line_color = "blue"){
  
  fem_head_center <- centroid_coord_list$femoral_head
  
  if(any(names(centroid_coord_list) == "s1")){
    s1_center <- centroid_coord_list$s1
  }else{
    s1_center <- centroid_coord_list$s1_superior_center
  }
  
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
