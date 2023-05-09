# create doughnut plots for seagrass management levels
# uses the 'circlize' package

# function to generate the doughnut plot
# agruments:
# prop.1: status-quo proportion (as a %)
# prop.2: increase proportion due to management option (as a %)
# col.1: colour to represent status-quo proportion
# col.2: colour to represent management proportion
doughnut <- function(prop.1, prop.2, col.1, col.2){
  require(circlize)
  par(mar = c(0, 0, 0, 0))
  plot(c(-0.93, 0.93), c(-0.93, 0.93), type = "n", axes = FALSE, ann = FALSE, 
       asp = 1)
  draw.sector(0, 360, rou1 = 1, rou2 = 0.55, clock.wise = TRUE, col = grey(0.9),
              lwd = 5, border = 0)
  draw.sector(90, 90 - (prop.1 * 3.6), rou1 = 1, rou2 = 0.55, clock.wise = TRUE,
              col = col.1, lwd = 5, border = 0)
  draw.sector(90 - (prop.1 * 3.6), 90 - ((prop.1 + prop.2) * 3.6), rou1 = 1, 
              rou2 = 0.55, clock.wise = TRUE, col = col.2, lwd = 5, border = 0)
  text(0, 0.15, paste0(prop.1 + prop.2, "%"), cex = 3, col = grey(0.55), 
       font = 2)
  text(0, -0.225, paste0("+", prop.2, "%"), cex = 2, col = grey(0.7), font = 2)
}

# function to save the plot to a svg file 
# note: stored in folder 'img' and takes the nae from the levels
doughnut.svg <- function(prop.1, prop.2, col.1, col.2){
  svg(paste0("img/level-", prop.1, "-", prop.2, ".svg"), width = 2, height = 2)
  doughnut(prop.1, prop.2, col.1, col.2)
  dev.off()   
}

# function to save the plot to a png file 
# note: stored in folder 'img' and takes the nae from the levels
doughnut.png <- function(prop.1, prop.2, col.1, col.2){
  svg(paste0("img/level-", prop.1, "-", prop.2, ".png"), width = 2, height = 2)
  doughnut(prop.1, prop.2, col.1, col.2)
  dev.off()   
}


# colours for status-quo and management levels
col.1 <- rgb(0.9, 0, 0)
col.2 <- rgb(0, 0.9, 0)

# set the range for status-quo and the maximum after management
min.sq <- 0 # as a %
max.sq <- 50 # as a %
max.after.policy <- 90 # as a %
# set the increment level
level.increment <- 10 # as a %

# generate the status-quo levels (based on constraints above)
sq.levs <- seq(ceiling(min.sq / level.increment) * level.increment,
               floor(max.sq / level.increment) * level.increment, 
               by = level.increment)
# generate the management levels (based on constraints above)
policy.levs <- seq(0,
                   floor(max.after.policy / level.increment) * level.increment, 
                   by = level.increment)

# generate the full list of (status-quo and management) level combinations
proportion_combns <- expand.grid(sq.levs, policy.levs)

# sum the combinations
proportion_combns$sum <- apply(proportion_combns, 1, sum)
# exclude combinations greater than maximum allowed (set above)
# drop the sum column
proportion_combns <- proportion_combns[proportion_combns$sum <= max.after.policy, -3]

# plot and save the doughnut plots 
for(i in 1:nrow(proportion_combns)){
  doughnut.svg(proportion_combns[i, 1], proportion_combns[i, 2], col.1, col.2)
  #  doughnut.png(proportion_combns[i, 1], proportion_combns[i, 2], col.1, col.2)
}

