
# predict_postop_pt_function <- function(preop_pt = 25.203136,
#                                        preop_c2_tilt = 1.5912083,
#                                        postop_c2pa = 19.231308) {
#   -0.49752004+0.10970428* preop_pt+0.00056298497*pmax(preop_pt-10.719671,0)^3-0.0038851919*pmax(preop_pt-20.0131,0)^3+0.0065365808*pmax(preop_pt-25.203136,0)^3-0.0038450855*pmax(preop_pt-31.171827,0)^3+0.00063071159*pmax(preop_pt-42.548329,0)^3-0.44307495* preop_c2_tilt+0.0017666557*pmax(preop_c2_tilt+5.8137583,0)^3-0.0035561278*pmax(preop_c2_tilt+1.7091905,0)^3+0.0016668741*pmax(preop_c2_tilt-1.5912083,0)^3+2.5888476e-05*pmax(preop_c2_tilt-5.5568369,0)^3+9.6709482e-05*pmax(preop_c2_tilt-14.441265,0)^3+0.92251864* postop_c2pa-0.00030340652*pmax(postop_c2pa-3.9065663,0)^3+0.0011766656*pmax(postop_c2pa-13.597867,0)^3-0.0014909534*pmax(postop_c2pa-19.231308,0)^3+0.00079454989*pmax(postop_c2pa-25.269772,0)^3-0.0001768555*pmax(postop_c2pa-35.169804,0)^3 
# }

predict_postop_pt_function <-  function(preop_c2_tilt = 1.5912083,
         preop_pt = 25.203136,
         postop_c2pa = 19.231308) {
  -0.018071724-0.26959853*preop_c2_tilt+0.17554658*preop_pt+0.83058112*postop_c2pa 
  }



target_l1pa_function <- function(pelvic_incidence = 51.813768) {-19 + 0.5*pelvic_incidence}


# Function to create vertebra as an sf polygon
jh_create_vert_coord_list_slope_0_from_centroid_function <- function(centroid_x = 0, centroid_y = 0, width = 5, height = 4, spine_orientation = "right") {
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
  
  # coord_list <- list()
  # coord_list$sp <- sp
  # coord_list$sa <- sa
  # coord_list$ia <- ia
  # coord_list$ip <- ip
  # return(coord_list)
  
  coord_matrix <- rbind(sa, sp, ip, ia) 
  
  return(coord_matrix)
}


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



jh_rotate_polygon_around_centroid <- function(polygon, angle_degrees) {
  # Step 1: Get the centroid of the polygon
  centroid <- st_centroid(polygon)
  centroid_coords <- st_coordinates(centroid)[1, 1:2]  # Ensure this is a 2D vector (x, y)
  
  # Step 2: Translate the polygon to have the centroid at the origin
  coords <- st_coordinates(polygon)[, 1:2]  # Extract the polygon's coordinates
  translated_coords <- sweep(coords, 2, centroid_coords)  # Subtract centroid from coordinates
  
  # Step 3: Create a rotation matrix
  angle_radians <- angle_degrees * pi / 180  # Convert angle to radians
  rotation_matrix <- matrix(c(cos(angle_radians), -sin(angle_radians),
                              sin(angle_radians),  cos(angle_radians)),
                            ncol = 2, byrow = TRUE)
  
  # Step 4: Apply the rotation matrix to the translated coordinates
  rotated_coords <- translated_coords %*% rotation_matrix
  
  # Step 5: Translate the polygon back to its original position
  final_coords <- sweep(rotated_coords, 2, centroid_coords, "+")
  
  # Step 6: Rebuild the rotated polygon
  rotated_polygon <- st_polygon(list(final_coords))
  
  return(st_sfc(rotated_polygon, crs = st_crs(polygon)))
}



jh_rotate_polygon_around_centroid <- function(polygon, angle_degrees) {
  # Step 1: Get the centroid of the polygon
  centroid <- st_centroid(polygon)
  centroid_coords <- st_coordinates(centroid)[1, 1:2]  # Ensure this is a 2D vector (x, y)
  
  # Step 2: Translate the polygon to have the centroid at the origin
  coords <- st_coordinates(polygon)[, 1:2]  # Extract the polygon's coordinates
  translated_coords <- sweep(coords, 2, centroid_coords)  # Subtract centroid from coordinates
  
  # Step 3: Create a rotation matrix
  angle_radians <- angle_degrees * pi / 180  # Convert angle to radians
  rotation_matrix <- matrix(c(cos(angle_radians), -sin(angle_radians),
                              sin(angle_radians),  cos(angle_radians)),
                            ncol = 2, byrow = TRUE)
  
  # Step 4: Apply the rotation matrix to the translated coordinates
  rotated_coords <- translated_coords %*% rotation_matrix
  
  # Step 5: Translate the polygon back to its original position
  final_coords <- sweep(rotated_coords, 2, centroid_coords, "+")
  
  # Step 6: Rebuild the rotated polygon
  rotated_polygon <- st_polygon(list(final_coords))
  
  return(st_sfc(rotated_polygon, crs = st_crs(polygon)))
}


rotate_spine_function <- function(spine_df, angle_degrees, point_of_rotation = c(0, 0)) {
  # Convert angle to radians
  angle_rad <- angle_degrees * pi / 180
  
  # Extract rotation center
  x_center <- point_of_rotation[1]
  y_center <- point_of_rotation[2]
  
  # Rotation matrix components
  cos_theta <- cos(angle_rad)
  sin_theta <- sin(angle_rad)
  
  # Apply rotation to x, y coordinates relative to the rotation point
  spine_df <- spine_df %>%
    mutate(
      x_shifted = x - x_center,
      y_shifted = y - y_center,
      x_rot = x_shifted * cos_theta - y_shifted * sin_theta + x_center,
      y_rot = x_shifted * sin_theta + y_shifted * cos_theta + y_center
    ) %>%
    select(spine_level, vert_point, x_rot, y_rot) %>%
    rename(x = x_rot, y = y_rot)
  
  return(spine_df)
}


# jh_construct_geoms_after_planning_function <- function(vertebral_level_tibble, buffer_amount = 0){
#   coord_list_df <- vertebral_level_tibble %>%
#     mutate(vert_point_coord = map2(.x = x, .y = y, .f = ~ c(.x, .y))) %>%
#     select(vert_point, vert_point_coord)
# 
#   vert_coord_point_list <- coord_list_df$vert_point_coord
# 
#   names(vert_coord_point_list) <- coord_list_df$vert_point
# 
#   if(unique(vertebral_level_tibble$spine_level) == "sacrum"){
#     vertebral_geom <- st_polygon(list(rbind(vert_coord_point_list$s1_anterior_superior,
#                                             vert_coord_point_list$sac_inf_1,
#                                             vert_coord_point_list$s1_posterior_superior,
#                                             vert_coord_point_list$s1_anterior_superior)))
#   }else{
#     vertebral_geom <- st_polygon(list(rbind(vert_coord_point_list$sp,
#                                             vert_coord_point_list$sa,
#                                             vert_coord_point_list$ia,
#                                             vert_coord_point_list$ip,
#                                             vert_coord_point_list$sp)))
#   }
# 
#   if(buffer_amount > 0){
#     geom <- jh_safely_buffer_vert_function(vert_geom = vertebral_geom,
#                                            buffer_amount = buffer_amount)
#     return(geom)
#   }else{
#     return(vertebral_geom)
#   }
# 
# }

jh_construct_geoms_after_planning_function <- function(vertebral_level_tibble, buffer_amount = 0){
  coord_list_df <- vertebral_level_tibble %>%
    mutate(vert_point_coord = map2(.x = x, .y = y, .f = ~ c(.x, .y))) %>%
    select(vert_point, vert_point_coord)
  
  vert_coord_point_list <- coord_list_df$vert_point_coord
  
  names(vert_coord_point_list) <- coord_list_df$vert_point
  
  if(unique(vertebral_level_tibble$spine_level) == "sacrum"){
    vertebral_geom <- st_polygon(list(rbind(vert_coord_point_list$s1_anterior_superior,
                                            vert_coord_point_list$sac_inf_1,
                                            vert_coord_point_list$s1_posterior_superior,
                                            vert_coord_point_list$s1_anterior_superior)))
  }else{
    vert_coord_point_list <- jh_calculate_points_for_square_vertebra_function(vert_coord_list = vert_coord_point_list)
    
    vertebral_geom <- st_polygon(list(rbind(vert_coord_point_list$sp, 
                                            vert_coord_point_list$sa,
                                            vert_coord_point_list$ia, 
                                            vert_coord_point_list$ip, 
                                            vert_coord_point_list$sp)))
  }
  
  if(buffer_amount > 0){
    geom <- jh_safely_buffer_vert_function(vert_geom = vertebral_geom,
                                           buffer_amount = buffer_amount)
    return(geom)
  }else{
    return(vertebral_geom)
  }
  
}


jh_calculate_coord_at_intersection_function <- function(line_1_point_a, line_1_point_b, line_2_point_a, line_2_point_b) {
  x1 <- line_1_point_a[[1]];      y1 <- line_1_point_a[[2]]
  x2 <- line_1_point_b[[1]];  y2 <- line_1_point_b[[2]]
  x3 <- line_2_point_a[[1]];  y3 <- line_2_point_a[[2]]
  x4 <- line_2_point_b[[1]];  y4 <- line_2_point_b[[2]]
  
  dx1 <- x2 - x1
  dy1 <- y2 - y1
  dx2 <- x4 - x3
  dy2 <- y4 - y3
  
  denom <- dx1 * dy2 - dy1 * dx2
  if (abs(denom) < .Machine$double.eps) return(NA)
  
  t <- ((x3 - x1) * dy2 - (y3 - y1) * dx2) / denom
  c(x1 + t * dx1, y1 + t * dy1)
}



jh_calculate_points_for_square_vertebra_function <- function(vert_coord_list) {
  
  coord_list_return <- list()
  
  posterior_body_height <- jh_calculate_distance_between_2_points_function(point_1 = vert_coord_list$sp, point_2 = vert_coord_list$ip)
  
  anterior_body_height <- jh_calculate_distance_between_2_points_function(point_1 = vert_coord_list$sa, point_2 = vert_coord_list$ia)
  
  
  if(posterior_body_height > anterior_body_height){
    coord_list_return$sp <- vert_coord_list$sp
    coord_list_return$sa <- vert_coord_list$sa
    coord_list_return$ia <- vert_coord_list$ia
    coord_list_return$ip <- vert_coord_list$ip

    return(coord_list_return)
  }else{
    sp <- as.list(vert_coord_list$sp)
    ip <- as.list(vert_coord_list$ip)
    
    base_length <- posterior_body_height*2
    
    names(sp) <- c("x", "y")
    names(ip) <- c("x", "y")
    dx <- ip$x - sp$x
    dy <- ip$y - sp$y
    
    # Compute unit perpendicular vector (rotated 90 degrees counterclockwise)
    perp_x <- -dy
    perp_y <- dx
    
    # Normalize the perpendicular vector to unit length
    norm_factor <- sqrt(perp_x^2 + perp_y^2)
    perp_x <- perp_x / norm_factor
    perp_y <- perp_y / norm_factor
    
    # Scale to base_length
    perp_x <- perp_x * base_length
    perp_y <- perp_y * base_length
    
    if(vert_coord_list$sa[[1]] < vert_coord_list$sp[[1]]){
      sup_point <- list(x = sp$x - perp_x, y = sp$y - perp_y)
      inf_point <- list(x = ip$x - perp_x, y = ip$y - perp_y)
    }else{
      sup_point <- list(x = sp$x + perp_x, y = sp$y + perp_y)
      inf_point <- list(x = ip$x + perp_x, y = ip$y + perp_y)
    }
    

    
    coord_list_return$sp <- vert_coord_list$sp
    coord_list_return$sa <- jh_calculate_coord_at_intersection_function(line_1_point_a = vert_coord_list$sp,
                                                          line_1_point_b = as.vector(sup_point),
                                                          line_2_point_a = vert_coord_list$sa, 
                                                          line_2_point_b = vert_coord_list$ia)
    
    coord_list_return$ia <- jh_calculate_coord_at_intersection_function(line_1_point_a = vert_coord_list$ip,
                                                          line_1_point_b = as.vector(inf_point),
                                                          line_2_point_a = vert_coord_list$sa, 
                                                          line_2_point_b = vert_coord_list$ia)
    coord_list_return$ip <- vert_coord_list$ip
    
      
    return(coord_list_return)
    
  }
}
