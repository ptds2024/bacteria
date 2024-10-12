# Web App for Bacteria motility
# Definition of functions

# Applications of Random walk - Bacteria Motility
# load packages
#library(plotrix)
#library(emojifont)
#library(circular)

# define fct update position
#' @importFrom stats runif
update_position_uniform <- function(x1,y1){
  # generate an angle
  angle <- runif(1,0,2*pi)
  speed <- runif(1, 0, 2)
  x2 <- x1 + speed * cos(angle)
  y2 <- y1 + speed * sin(angle)
  return(c(x2,y2))
}

compute_angle <- function(x, circle_center){
  # compute the angle in whihc the bacteria should move to reach the center
  if(x[2] >= circle_center[2]){
    res <- atan2(x = x[1], y = x[2]) - pi
  }else if(x[2] < circle_center[2]){
    res <- atan2(x = x[1], y = x[2]) + pi
  }
  return(res)
}

#' @importFrom circular rvonmises
update_position_von_mises <- function(x1, y1, circle_center, constant_kappa){

  # compute true angle between point and circle center
  x_pos <- c(x1,y1)
  circle_center_exo <- c(0,0)
  true_angle <- compute_angle(x = x_pos, circle_center = circle_center_exo)

  # compute distance between center and point
  distance_to_center <- norm((x_pos-circle_center_exo), type = "2")

  # define kappa parameter
  kappa_pos <- constant_kappa / distance_to_center

  # generate an angle
  angle <- rvonmises(n = 1, mu = true_angle, kappa = kappa_pos)
  speed <- runif(1, 0, 2)
  x2 <- x1 + speed * cos(angle)
  y2 <- y1 + speed * sin(angle)
  return(c(x2,y2))
}

# define fct identify_furthest_point_in_circle
#' @importFrom stats approx
identify_furthest_point_in_circle <- function(x1, y1, x2, y2, circle_x, circle_y, radius){
  # check if initial position is in the center
  euclidean_norm_init_pos <- norm(c(x1-circle_x,y1-circle_y), type = "2")
  # check if initial position is in the circle
  if(euclidean_norm_init_pos >= radius){
    return(c(x2, y2))
    # if was in the circle, return max point in the circle
  }else if(euclidean_norm_init_pos < radius){
    direction_line <- approx(x= c(x1,x2), y = c(y1,y2), n = 100)

    # find the last point in the line that respect the circle constraint

    if(x1 > x2){
      mat_of_traj = cbind(rev(direction_line$x), rev(direction_line$y))

    }else{mat_of_traj <- cbind( direction_line$x, direction_line$y)}

    vec_of_norms <- sqrt(mat_of_traj[,1]^2 + mat_of_traj[,2]^2)
    mat_of_traj <- cbind(mat_of_traj, vec_of_norms)
    index_last_pt <- max(which(mat_of_traj[,3]<radius))
    last_pos_in_circle <- c(mat_of_traj[index_last_pt,1],  mat_of_traj[index_last_pt,2])

    # return last position in circle
    return(last_pos_in_circle)
  }
}

# define fct check_position
check_position <- function(x1,y1,x2,y2, circle_x, circle_y, radius){
  # check if initial position is in the center
  euclidean_norm_init_pos <- norm(c(x1-circle_x,y1-circle_y), type = "2")
  # check if initial position is in the circle
  if(euclidean_norm_init_pos >= radius){
    return(c(x2, y2))
    # if was in the circle, return the initial position
  }else if(euclidean_norm_init_pos < radius){
    return(c(x1, y1))
  }
}

# define color palette
#' @importFrom grDevices hcl
gg_color_hue <- function(n, alpha = 1) {
  hues <- seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100, alpha = alpha)[1:n]
}

# define function compute all positions
compute_all_positions <- function(circle_coordinate, circle_radius = 3,
                                 move_in_sugar = T, N = 20, M = 150,
                                 initial_seed = 123, constant_kappa = 0){
  # define circle coordinate
  circle_coor_x <- circle_coordinate[1]
  circle_coor_y <- circle_coordinate[2]

  # define colors
  color_pal <- gg_color_hue(n=N, alpha = 0.4)

  # create a matrix of initial values
  # define the first position as uniform
  set.seed(initial_seed)
  current_position_mat <- matrix(data = runif(N*2, min = -9, max = 9), ncol = 2, nrow = N)

  # create array of all position
  all_position <- array(NA, dim = c(N, 2, M+1))
  all_position[,,1] <- current_position_mat

  # compute all trajectories
  for(step in seq(M)){
    for(bacteria_i in seq(N)){
      current_pos <- current_position_mat[bacteria_i,]

      # compute new position
      new_pos <- update_position_von_mises(x1 = current_pos[1], y1 = current_pos[2],
                                          circle_center = c(0,0), constant_kappa = constant_kappa)


      # check last position in circle if already in the circle
      # if bacteria keeps moving in sugar
      if(move_in_sugar == T){
        new_pos_verified <- identify_furthest_point_in_circle(x1 = current_pos[1],
                                                             y1 = current_pos[2],
                                                             x2 = new_pos[1],
                                                             y2 = new_pos[2],
                                                             circle_x = circle_coor_x,
                                                             circle_y = circle_coor_y,
                                                             radius = circle_radius)
        # if bacteria do not keep moving in sugar
      }else if(move_in_sugar == F){
        new_pos_verified <- check_position(x1 = current_pos[1],
                                          y1 = current_pos[2],
                                          x2 = new_pos[1],
                                          y2 = new_pos[2],
                                          circle_x = circle_coor_x,
                                          circle_y = circle_coor_y,
                                          radius = circle_radius)

      }
      all_position[bacteria_i,,step + 1] <- c(new_pos_verified)
      current_position_mat[bacteria_i,] <- new_pos_verified
    }
  }
  return(all_position)
}

#' @importFrom plotrix draw.circle
#' @importFrom emojifont emoji
compute_animation_path <- function(circle_coordinate = c(0,0), circle_radius = 3,
                                  array_all_positions, sys_sleep_time = 0.7){

  # compute steps and bacterias from array_all_positions
  M <- dim(array_all_positions)[3]
  N <- dim(array_all_positions)[1]
  all_position <- array_all_positions

  # define colors
  color_pal <- gg_color_hue(n=N, alpha = 0.4)

  # Compute an animation of the path of the bacterias
  for(step in seq(M)){
    # plot now all of the trajectories with the point each time
    # plot the map
    plot(NA, ylim = c(-10,10), xlim = c(-10,10), asp =1,
         xlab = "X Position", ylab = "Y Position", main = "Bacteria Motility")
    grid(lty="solid")


    # define circle coordinate
    circle_coor_x <- circle_coordinate[1]
    circle_coor_y <- circle_coordinate[2]

    # add circle of sugar
    draw.circle(x = circle_coor_x, y = circle_coor_y, radius = circle_radius, col = "orange")

    # add legend of sugar
    text(x=0, y=0, "SUGAR", col="white", cex = 1.5)

    for(bacteria_i in seq(N)){
      text(x = all_position[bacteria_i,1,step], y = all_position[bacteria_i,2,step], labels=emoji('alien'), cex=1.75, family='EmojiOne')
      lines(x = all_position[bacteria_i, 1, 1:step], y = all_position[bacteria_i, 2, 1:step], col =  color_pal[bacteria_i] )
    }
    Sys.sleep(sys_sleep_time)
  }
}

compute_animation_path_one_step <- function(circle_coordinate = c(0,0), circle_radius = 3,
                                           array_all_positions, step_i){

  # compute steps and bacterias from array_all_positions
  M <- dim(array_all_positions)[3]
  N <- dim(array_all_positions)[1]
  all_position = array_all_positions

  # define colors
  color_pal <- gg_color_hue(n=N, alpha = 0.4)

  # Compute an animation of the path of the bacterias
  step <- step_i
  # plot now all of the trajectories with the point each time
  # plot the map
  plot(NA, ylim = c(-15,15), xlim = c(-15,15), asp =1,
       xlab = "X Position", ylab = "Y Position", main = "")
  grid(lty="solid")


  # define circle coordinate
  circle_coor_x <- circle_coordinate[1]
  circle_coor_y <- circle_coordinate[2]

  # add circle of sugar
  draw.circle(x = circle_coor_x, y = circle_coor_y, radius = circle_radius, col = "orange")

  # add legend of sugar
  text(x=0, y=0, "SUGAR", col="white", cex = 1)

  for(bacteria_i in seq(N)){
    text(x = all_position[bacteria_i,1,step], y = all_position[bacteria_i,2,step], labels=emoji('alien'), cex=1.25, family='EmojiOne')
    lines(x = all_position[bacteria_i, 1, 1:step], y = all_position[bacteria_i, 2, 1:step], col =  color_pal[bacteria_i] , lwd = 1.2)
  }
  # compute nbr of bacteria inside circiel of sugar
  bacteria_inside <- sum(sqrt(rowSums(array_all_positions[, , step]^2)) < 3)
  text(x = 10, 14, labels = paste("Bacteria inside sugar:", bacteria_inside), cex = 1.1)
}
