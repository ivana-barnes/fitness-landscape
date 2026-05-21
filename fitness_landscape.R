fl_plot <- function(w11 = 10,
                    w12 = 60,
                    w22 = 20,
                    file,
                    rel_fit = T,
                    h_lines_color_scheme = "basic_eq", 
                    y_axis1_color_scheme = h_lines_color_scheme,
                    y_axis2_range_ticks = F,
                    add_legend = F,
                    color_stable = "chocolate4",
                    color_unstable = "forestgreen",
                    color_background = "grey40",
                    color_neutral = "grey40",
                    color_w11 = "hotpink",
                    color_w12 = "mediumpurple",
                    color_w22 = "royalblue",
                    width = 8,
                    height = 4,
                    plot_type = "pop_out"){

##### Setup for figures ##### ----------------------- #####

if(class(c(w11,w12,w22)) != "numeric"){
  cat("Error: w11, w12, and w22 must all be numeric")
} else if (w11 < 0 | w12 < 0 | w22 < 0){
  cat("Error: w11, w12, and w22 must all be >= 0")
} else { 
  
if(plot_type %in% c("pop_out","plot_window","pdf") == F){
  cat("Warning: invalid plot type. Default (plot_type = \"pop_out\") was used")
  plot_type <- "pop_out"
}  
  
# define function to use later - calculates mean fitness from p and ws
w_bar_fun <- function(p,w11,w12,w22){
  w_bar_res <- ((p^2)*w11) + (2*p*(1-p)*w12) + (((1-p)^2)*w22)
  return(w_bar_res)
  }  

# rescale fitnesses

if(w11 == 0 & w12 == 0 & w22 == 0){all_zero <- T} else {all_zero <- F}

if(rel_fit == T & all_zero == F){  
max_og_fit <- max(c(w11,w12,w22))
w11 <- w11/max_og_fit
w12 <- w12/max_og_fit
w22 <- w22/max_og_fit
}

w_max <- max(w11,w12,w22)
if(all_zero == T){w_max <- 1} # when w_max is 0 it might cause plotting issues

# makes a special condition for if there are no fitness differences
if(w11 == w12 & w12 == w22){flat_plot <- T} else {flat_plot <- F}
  
# initialize the p axis
p <- seq(0,1,by = .0001)

# calculate w_bar and delta_p for the vector of p values  
w_bar <- ((p^2)*w11) + (2*p*(1-p)*w12) + (((1-p)^2)*w22)
delta_p <- (((p*(w11-w12)) + ((1-p)*(w12-w22)))*p*(1-p))/w_bar

# The only cases where w_bar is 0 is if p = 0 and w22 = 0 or q = 0 and w11 = 0. 
# This causes delta_p to be 0/0 which creates errors, but in this case it should 
# converge to 0, so this just fixes that manually
delta_p[w_bar == 0] <- 0
  
# find potential equilibria (note, only works when ws are constant)
p_hat <- (w22 - w12)/(w11 - (2*w12) + w22)
  
# if there are no equilibria, the expression above will give a 
  # value for p_hat that is not between 0 and 1

# evaluate if there are any equilibria

# p_hat_exists = T if there is an eq, F if not.
p_hat_exists <- (p_hat > 0) & (p_hat < 1) 
# p_hat_exists might be NA if there are weird cases like NaN, this fixes that
if(is.na(p_hat_exists)){p_hat_exists <- F} 

# set p_hat to NA if there isn't an equilibrium 
if(!p_hat_exists){p_hat <- NA}
  
# find the average fitness at the equilibrium value of p
if(p_hat_exists){p_hat_w_bar <- w_bar_fun(p_hat,w11,w12,w22)} else {
  p_hat_w_bar <- NA}
  
# evaluate whether equilibria (0,1,p_hat) are stable or unstable
  
# for p_hat, stable if second derivative of delta_p is negative
# for 0, stable if w12 < w22 (increasing f(A1) from 0 lowers avg fitness)
  # when w12 - w22 is negative
# for 1, stable if w12 < w11 (decreasing f(A1) from 1 lowers avg fitness)
  # when w12 - w11 is negative

# make the test values
if(p_hat_exists){sec_deriv <- (w11 - (2*w12) + w22)}
test0 <- w12 - w22
test1 <- w12 - w11

# tiebreaker if w12 = w11 or w12 = w22
if(test0 == 0){test0 <- w11 - w22}
if(test1 == 0){test1 <- w22 - w11}

# now these test0 and test1 only = 0 if all 3 values are equal
  
# function to return stable color if test stat is negative
 # and unstable color if it is positive
set_color <- function(value){
  if(value < 0){
    result <- color_stable
  } else if (value > 0){
    result <- color_unstable
  } else {result <- color_neutral}  
  return(result)
}  

# set the colors for the equilibria based on if they are stable or unstable    
if(p_hat_exists){col_p_hat <- set_color(sec_deriv)} else {col_p_hat <- NA}
col0 <- set_color(test0)
col1 <- set_color(test1)  

##### Plotting the figures ##### ----------------------- #####

if(plot_type == "pop_out"){quartz(width = width, height = height)}
if(plot_type == "plot_window"){quartz.options(width = width, height = height)}
if(plot_type == "pdf"){pdf(width = width, height = height, file = file)}
plot.new()
par(mfrow = c(1, 2), oma = c(2,2,1,1))
  
  
##### LEFT PLOT #####

### call plot
plot.new()
par(mar = c(1,1,1,2))
plot.window(xlim = c(-0.2,1), ylim = c(-0.2*w_max,w_max))

### organizing x axis parameters

# make vector of values for x axis ticks, and set the names of that 
  # vector as what I want the labels to be 
x_axis <- c(0,1,p_hat)
names(x_axis) <- c(as.character(round(x_axis,2)))

# make vector of corresponding colors, with the same names so that it is easy 
  # to keep track of which color is which value
x_axis_cols <- c(col0,col1,col_p_hat)
names(x_axis_cols) <- names(x_axis)

# sort these vectors (and remove p_hat if it doesn't exist) so that 
  # they are in increasing order for the axis() function  
x_axis <- sort(x_axis[!is.na(x_axis)])
x_axis_cols <- x_axis_cols[names(x_axis)]
  
### organizing y axis parameters
  
# make vector of values for y axis ticks, and set the names of that 
  # vector as what I want the labels to be 
  
y_axis1 <- c(0, w11, w12, w22, p_hat_w_bar)
names(y_axis1) <- as.character(round(y_axis1,2))
  
# make vector of corresponding colors, with the same names so that it is easy 
  # to keep track of which color is which value. There are 3 ways to do this
  # depending on the y_axis1_color_scheme parameter
  
if(y_axis1_color_scheme == "eq"){
  y_axis1_cols <- c("black",col1,color_background,col0, col_p_hat)
  } 
if(y_axis1_color_scheme == "w") {
  y_axis1_cols <- c("black",color_w11,color_w12,color_w22,col_p_hat)
}
if(y_axis1_color_scheme == "w_no_eq") {
  y_axis1_cols <- c("black",color_w11,color_w12,color_w22,color_background)
}
if(y_axis1_color_scheme == "basic") {
  y_axis1_cols <- c("black","black","black","black","black")
}
if(y_axis1_color_scheme == "basic_eq") {
  y_axis1_cols <- c("black",color_background,color_background,
                    color_background,col_p_hat)
}

names(y_axis1_cols) <- names(y_axis1)

# sort these vectors (and remove p_hat_w_bar if it doesn't exist) so that 
  # they are in increasing order for the axis() function
  
y_axis1 <- sort(y_axis1[!is.na(y_axis1)])
y_axis1_cols <- y_axis1_cols[names(y_axis1)]
  
### organizing colors for horizontal lines (depending on chosen color scheme)

if(h_lines_color_scheme == "eq"){
  col_w11 <- col1
  col_w12 <- color_background
  col_w22 <- col0
  col_eq <- col_p_hat
}

if(h_lines_color_scheme == "w"){
  col_w11 <- color_w11
  col_w12 <- color_w12
  col_w22 <- color_w22
  col_eq <- col_p_hat
}

if(h_lines_color_scheme == "w_no_eq"){
  col_w11 <- color_w11
  col_w12 <- color_w12
  col_w22 <- color_w22
  col_eq <- color_background
}

if(h_lines_color_scheme == "basic"){
  col_w11 <- color_background
  col_w12 <- color_background
  col_w22 <- color_background
  col_eq <- color_background
}

if(h_lines_color_scheme == "basic_eq"){
  col_w11 <- color_background
  col_w12 <- color_background
  col_w22 <- color_background
  col_eq <- col_p_hat
}
  
### start plotting elements

# add axes
axis(1,at = x_axis, labels = F, cex.axis = .75, tcl = -.5, pos = 0, padj = -1)
axis(2,at = y_axis1, labels = F, las = 1, cex.axis = .75, tcl = -.5, pos = 0, hadj = -.5)
  
# add axis tick labels
text(names(y_axis1), x = -.075, y = y_axis1, cex = .75, adj = 1, 
      col = y_axis1_cols)
text(names(x_axis), x = x_axis, y = -0.1*w_max, cex = .75, col = x_axis_cols)
  
# add x and y axis labels
mtext(expression(paste("A" ["1"]," Frequency (p)")), 1, line = 0, at = .5, cex = .75)
mtext(expression(paste("Mean Fitness (", bar(w) ,")")), 2, line = 0, at = .5*w_max, cex = .75)

if(flat_plot == F){ # flat_plot = T is a special case that will have different elements   
# add the w_bar vs p curve  
lines(w_bar ~ p) #curve


# add dashed horizontal lines for w11, w12, w22, and equilibrium
lines(x = c(0,1), y = c(w11,w11), col = col_w11, lty = "dashed") #w11 line
lines(x = c(0,1), y = c(w12,w12), col = col_w12, lty = "dashed") #w12 line
lines(x = c(0,1), y = c(w22,w22), col = col_w22, lty = "dashed") #w22 line
if(p_hat_exists){lines(x = c(0,1), y = c(p_hat_w_bar,p_hat_w_bar), 
                      col = col_eq, lty = "dashed")} #equilibrium line


# add equilibrium points
points(x = c(0,1), y = c(w22,w11), col = c(col0,col1), 
        cex = .75, pch = 19) # 0 and 1
if(p_hat_exists){points(x = p_hat, y = p_hat_w_bar, 
                        col = col_p_hat, pch = 19, cex = .75)} # p_hat


# add labels for dotted horizontal lines

# if they are all different
if(w11 %in% c(0,w12,w22) == F){
  text(expression("w"["11"]),x = 1, y = w11 - .025*w_max, adj = c(1,1), 
      col = col_w11, cex = .75)}
if(w11 %in% c(w12,w22) == F & w11 == 0){
  text(expression(paste("w"["11"]," = 0")),x = 1, y = .075*w_max, adj = c(1,1), 
       col = col_w11, cex = .75)}

if(w12 %in% c(0,w11,w22) == F){
  text(expression("w"["12"]),x = 1, y = w12 - .025*w_max, adj = c(1,1), 
      col = col_w12, cex = .75)}
if(w12 %in% c(w11,w22) == F & w12 == 0){
  text(expression(paste("w"["12"]," = 0")),x = 1, y = .075*w_max, adj = c(1,1), 
       col = col_w12, cex = .75)}

if(w22 %in% c(0,w12,w11) == F){
  text(expression("w"["22"]),x = 1, y = w22 - .025*w_max, adj = c(1,1), 
      col = col_w22, cex = .75)}
if(w22 %in% c(w11,w12) == F & w22 == 0){
  text(expression(paste("w"["22"]," = 0")),x = 1, y = .075*w_max, adj = c(1,1), 
       col = col_w22, cex = .75)}

# if two are the same
# w11 = w12
if(w11 == w12 & w11 != 0){
  text(expression(paste("w"["11"]," = ","w"["12"])),x = 1, y = w11 - .025*w_max, adj = c(1,1), 
       col = col_w12, cex = .75)
}
if(w11 == w12 & w11 == 0){
  text(expression(paste("w"["11"]," = ","w"["12"]," = 0")),x = 1, y = .075*w_max, adj = c(1,1), 
       col = col_w12, cex = .75)
}

# w11 = w22
if(w11 == w22 & w11 != 0){
  text(expression(paste("w"["11"]," = ","w"["22"])),x = 1, y = w11 - .025*w_max, adj = c(1,1), 
       col = col_w11, cex = .75)
}
if(w11 == w22 & w11 == 0){
  text(expression(paste("w"["11"]," = ","w"["22"]," = 0")),x = 1, y = .075*w_max, adj = c(1,1), 
       col = col_w11, cex = .75)
}

# w12 = w22
if(w12 == w22 & w12 != 0){
  text(expression(paste("w"["12"]," = ","w"["22"])),x = 1, y = w11 - .025*w_max, adj = c(1,1), 
       col = col_w12, cex = .75)
}
if(w12 == w22 & w12 == 0){
  text(expression(paste("w"["12"]," = ","w"["22"]," = 0")),x = 1, y = .075*w_max, adj = c(1,1), 
       col = col_w12, cex = .75)
}


# add a label for the equilibrium if it exists + there is no legend
if(add_legend == F & p_hat_exists){
if(sec_deriv < 0){
  text("stable equilibrium",
       x = p_hat, 
       y = p_hat_w_bar + .0375*w_max, 
       adj = c(.5,0), 
       col = col_eq, 
       cex = .65)
  }

if(sec_deriv > 0){
  text("unstable equilibrium",
       x = p_hat, 
       y = p_hat_w_bar - .0375*w_max, 
       adj = c(.5,1), 
       col = col_eq, 
       cex = .65)
  } 
}

} else if (flat_plot == T){ # special case plot for when w11=w12=w22
  if(all_zero == T){
    # makes the y-axis with no ticks since when all are 0 the y - axis gets messed up
    lines(x = c(0,0), y = c(0,w_max))
    text_y_pos <- 0.075 # says to place text above the line (b/c line is at 0)
    } else if(all_zero == F){
    text_y_pos <- .9*w_max  # says to place text below the line (b/c line is at max)
    }
  lines(w_bar ~ p, lwd = 2, col = color_neutral) #curve
  points(x = c(0,1), y = c(w22,w11), col = c(col0,col1), 
         cex = .75, pch = 19) # 0 and 1
  if(add_legend == F){
    text("neutral equilibrium",
         x = .5,
         y = text_y_pos,
         adj = c(.5,1),
         col = color_neutral,
         cex = .65)} # legend
  
  # text labeling w11 = w12 = w22
  if(all_zero == F){
    text(expression(paste("w"["11"]," = ","w"["12"]," = ","w"["22"])),
         x = 1, y = w11 - .025*w_max, adj = c(1,1), 
         col = color_neutral, cex = .75)
  }
  
  if(all_zero == T){
    text(expression(paste("w"["11"]," = ","w"["12"]," = ","w"["22"]," = 0 ")),
         x = 1, y = w11 - .025*w_max, adj = c(1,1), 
         col = color_neutral, cex = .75)
  } 
}

##### RIGHT PLOT #####

### set limit for y axis based of range of the data
if(add_legend == T){scalar <- 2} else if(add_legend == F){scalar <- 1.5}
y_limit <- max(abs(delta_p)) * scalar

if(flat_plot == T){y_limit <- 1}

### call plot
plot.new()
par(mar = c(1,1,1,2))
plot.window(xlim = c(-0.2,1), ylim = c(-1.4*y_limit,y_limit))
               
### if relevant, calculate minima and maxima - if not set as NA

if(y_axis2_range_ticks == T){
  # determine if there are minima or maxima (besides 0) and if so, define them
  if(min(delta_p) < 0){ymin <- min(delta_p)} else {ymin <- NA}
  if(max(delta_p) > 0){ymax <- max(delta_p)} else {ymax <- NA}
} 
  
if(y_axis2_range_ticks == F){
  ymin <- NA
  ymax <- NA
}

### organizing y axis parameters

# make vector of values for y axis ticks, and set the names of that 
# vector as what I want the labels to be 
y_axis2 <- c(0, ymin,ymax)
names(y_axis2) <- as.character(round(y_axis2,2))

# sort the vector (and remove min and max if they are NA) so that 
# it is in increasing order for the axis() function
y_axis2 <- sort(y_axis2[!is.na(y_axis2)])

### start plotting elements

# add axes

# x axis (taken from left figure)
axis(1,at = x_axis, labels = F, cex.axis = .75, tcl = -.5, 
     pos = -1*y_limit, padj = -1) 

# y axis with no ticks - extends full range of the plot window
lines(x = c(0,0), y = c(-1*y_limit,y_limit))

# y axis ticks
axis(2,at = y_axis2, labels = F, las = 1, cex.axis = .75, tcl = -.5, pos = 0, hadj = -.5)


# add axis tick labels
text(names(y_axis2), x = -.075, y = y_axis2, cex = .75, adj = 1)
text(names(x_axis), x = x_axis, y = -1.2*y_limit, cex = .75, 
     col = x_axis_cols)

# add x and y axis labels
mtext(expression(paste("A" ["1"]," Frequency (p)")), 1, line = 0, at = .5, cex = .75)
mtext(expression(paste(Delta,"p")), 2, line = -1, at = 0, cex = .75)

if(flat_plot == F){ # flat_plot = T is a special case that will have different elements

# add horizontal line at 0
lines(x = c(0,1), y = c(0,0), col = "darkgrey", lty = "dashed") 

# add delta_p ~ p curve
lines(delta_p ~ p)


# add arrows

# around 0
if(test0 > 0){
  arrows(x0 = .03,y0 = 0,x1 = .1,y1 = 0, length = .05, col = col0)
} else {arrows(x0 = .1,y0 = 0,x1 = .03,y1 = 0, length = .05, col = col0)}

# around 1
if(test1 > 0){
  arrows(x0 = .97,y0 = 0,x1 = .9,y1 = 0, length = .05, col = col1)
} else {arrows(x0 = .9,y0 = 0,x1 = .97,y1 = 0, length = .05, col = col1)}

# around p_hat (if it exists)
if(p_hat_exists){

if(sec_deriv > 0){
  arrows(x0 = p_hat + .03,y0 = 0,x1 = p_hat + .1,y1 = 0, length = .05, col = col_p_hat)
  arrows(x0 = p_hat - .03,y0 = 0,x1 = p_hat - .1,y1 = 0, length = .05, col = col_p_hat)
} else if(sec_deriv < 0){
  arrows(x0 = p_hat + .1,y0 = 0,x1 = p_hat + .03,y1 = 0, length = .05, col = col_p_hat)
  arrows(x0 = p_hat - .1,y0 = 0,x1 = p_hat - .03,y1 = 0, length = .05, col = col_p_hat)
}
}
  
# add equilibrium points (0,1,p_hat)
points(x = 0, y = delta_p[1],col = col0,pch = 19, cex = .75)
points(x = 1, y = delta_p[length(p)], col = col1, pch = 19, cex = .75)
points(x = p_hat, y = 0, col = col_p_hat, pch = 19, cex = .75)


# add legend
if(add_legend){

legend(x = .65, y = y_limit, xjust = 0, yjust = 1, 
       legend = c("stable","unstable"), 
       fill = c(color_stable,color_unstable),
       cex = .75, bty = "n")

}
} else if(flat_plot == T){ # special case plot for when w11=w12=w22
  # add delta_p ~ p curve
  lines(delta_p ~ p, lwd = 2, col = color_neutral)
  
  # add equilibrium points (0,1,p_hat)
  points(x = 0, y = delta_p[1],col = color_neutral,pch = 19, cex = .75)
  points(x = 1, y = delta_p[length(p)], col = color_neutral, pch = 19, cex = .75)
  
  if(add_legend){
    
    legend(x = .4, y = y_limit, xjust = 0, yjust = 1, 
           legend = c("neutral equilibrium"), 
           fill = c(color_neutral),
           cex = .75, bty = "n")
    
  }
}

if(plot_type == "pdf"){dev.off()}
}
}