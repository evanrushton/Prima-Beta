# Submit pen answer / resize pen events (score and visualize)
# Authors: Evan Rushton, Seth Corrigan

# Library Imports
library(tidyverse)

# Load in game answer submissions
pen <- read.table("./Data/resize_pen_submit_pen_answer.csv", fill = TRUE, header = TRUE, sep = ",")
# sapply(pen, function(y) sum(length(which(is.na(y)))))

#### Score resize_pen events Success/Fail ####
pen <- pen[ order(pen[,2], pen[,1]),] # Order by userId (asc) and clientTimeStamp (asc)

# if actionName is resize_pen AND next actionName is submit_pen_answer AND same userId, then assign success as same value as next success
index <- pen$actionName == "resize_pen" & lead(pen$actionName) == "submit_pen_answer" & pen$userId == lead(pen$userId)
pen$success[which(index)] <- lead(pen$success)[which(index)] 

pen <- pen[order(pen$userId, -rank(pen$clientTimeStamp)), ] # Reorder in descending chronological
# if actionName is resize_pen AND previous actionName is resize_pen AND same userId, then assign my success as same value as previous success
last <- nrow(pen)
for (i in 2:last) {
  if (pen$actionName[i-1] == "resize_pen" & pen$actionName[i]=="resize_pen" & pen$userId[i] == pen$userId[i-1]) {
    pen$success[i] <- pen$success[i-1]
  }
}

#### Visualize Success/Fail ####
for (loc in levels(pen$gameLevel)) {
  level <- pen[which(pen$gameLevel == loc),]
  level$success[which(level$success == 0)] <- 'Fail'
  level$success[which(level$success == 1)] <- 'Pass'
  level$success <- as.factor(level$success)
  p <- level %>%
    ggplot(aes(pendim_x, pendim_y, color = success, shape = success)) +
    geom_point() +
    geom_density2d() +
    ggtitle(loc) +
    theme_light()
  save_plot(paste0("./Output/", loc, "_penEvent.png"), p, base_height=6)
}


level %>%
  ggplot(aes(pendim_x, pendim_y, color = success, shape = success)) +
  geom_point() +
  geom_density2d() +
  ggtitle(loc) +
  theme_light()
####
subset <- subset(level, pendim_x < 100 & pendim_y < 100) 
subset %>%
  ggplot(aes(pendim_x, pendim_y, color = success, shape = success)) +
  geom_point() +
  geom_density2d() +
  ggtitle(loc) +
  theme_light()
####
subset2 <- subset(level, pendim_x < 50 & pendim_y < 50) 
subset2 %>%
  ggplot(aes(pendim_x, pendim_y, color = success, shape = success)) +
  geom_point() +
  geom_density2d() +
  ggtitle(loc) +
  theme_light()

#### Cowplot ####
library("ggplot2")
install.packages("cowplot")
library("cowplot")

#Create each plot seperately
gg_scatter = ggplot(level, aes(pendim_x, pendim_z, color=success, shape=success)) + geom_point(alpha=.8)

gg_dist_g1 = ggplot(level, aes(pendim_x, fill=success)) + geom_density(alpha=.5) 
gg_dist_g1 = gg_dist_g1 + ylab("Pendimension X density")

gg_dist_g2 = ggplot(level, aes(pendim_z, fill=success)) + geom_density(alpha=.5) 
gg_dist_g2 = gg_dist_g2 + ylab("Pendimension Z density")

#Different ways to combine the plots
plot_grid(gg_scatter, gg_dist_g1, gg_dist_g2, nrow=1, labels=c('A', 'B', 'C')) #Or labels="AUTO"

#Integrate
# Flip axis of gg_dist_g2
gg_dist_g2 = gg_dist_g2 + coord_flip()

# Remove some duplicate axes
gg_dist_g1 = gg_dist_g1 + theme(axis.title.x=element_blank(),
                                axis.text=element_blank(),
                                axis.line=element_blank(),
                                axis.ticks=element_blank())

gg_dist_g2 = gg_dist_g2 + theme(axis.title.y=element_blank(),
                                axis.text=element_blank(),
                                axis.line=element_blank(),
                                axis.ticks=element_blank())

# Modify margin c(top, right, bottom, left) to reduce the distance between plots
#and align G1 density with the scatterplot
gg_dist_g1 = gg_dist_g1 + theme(plot.margin = unit(c(0.5, 0, 0, 0.7), "cm"))
gg_scatter = gg_scatter + theme(plot.margin = unit(c(0, 0, 0.5, 0.5), "cm"))
gg_dist_g2 = gg_dist_g2 + theme(plot.margin = unit(c(0, 0.5, 0.5, 0), "cm"))


# Combine all plots together and crush graph density with rel_heights
first_col = plot_grid(gg_dist_g1, gg_scatter, ncol = 1, rel_heights = c(1, 3))
second_col = plot_grid(NULL, gg_dist_g2, ncol = 1, rel_heights = c(1, 3))
perfect = plot_grid(first_col, second_col, ncol = 2, rel_widths = c(3, 1))

save_plot("./perfect.png", perfect, base_height=6)
perfect

#### 3D Plots ####
install.packages("rgl")
install.packages("magick")
library("rgl")
library("magick")
z=iris
#colors vector
x=rep(0,150)

for (i in 1:150) {
  if(z$Species[i] == "setosa") x[i]="royalblue1"
  if(z$Species[i] == "versicolor") x[i]="darkcyan"
  if(z$Species[i] == "virginica") x[i]="oldlace"
}
z$Color=x

plot3d( z[,1], z[,2], z[,3], col = z$Color, type = "s", radius = .02 )

#We can indicate the axis and the rotation velocity

play3d( spin3d( axis = c(0, 0, 1), rpm = 7), duration = 10 )

#Save like gif

movie3d( spin3d( axis = c(0, 0, 1), rpm = 7),
         duration = 10, dir = getwd(),
         type = "gif", clean = TRUE )

