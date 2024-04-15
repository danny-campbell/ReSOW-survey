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
  par(mar = c(0, 0, 0, 0), bg = rgb(1, 1, 1, 0))
  plot(c(-0.96, 0.96), c(-0.96, 0.96), type = "n", axes = FALSE, ann = FALSE, 
       asp = 1)
  draw.sector(0, 360, rou1 = 1, rou2 = , clock.wise = TRUE, 
              col = rgb(1, 1, 1, 0.85), lwd = 3, border = rgb(1, 1, 1, 0.85))
  draw.sector(0, 360, rou1 = 1, rou2 = 0.45, clock.wise = TRUE, col = grey(0.9),
              lwd = 3, border = "white")
  draw.sector(90, 90 - (prop.1 * 3.6), rou1 = 1, rou2 = 0.45, clock.wise = TRUE,
              col = col.1, lwd = 3, border = "white")
  draw.sector(90 - (prop.1 * 3.6), 90 - ((prop.1 + prop.2) * 3.6), rou1 = 1, 
              rou2 = 0.45, clock.wise = TRUE, col = col.2, lwd = 3, 
              border = "white")
  
  
  A <- abs(90 - abs(180 - (360 * ((prop.1 * 0.5) / 100))))
  prop.1.y <- 0.725 * sin(A * (pi/180))
  prop.1.x <- sqrt((0.725^2 - prop.1.y^2))
  if(prop.1/2 > 50) prop.1.x <- prop.1.x * -1
  if((prop.1/2 > 25) & (prop.1/2 < 75)) prop.1.y <- prop.1.y * -1
  if(prop.1 > 0) text(prop.1.x, prop.1.y, paste0(prop.1, "%"), cex = 1.15, col = "white", font = 2)
  
  B <- abs(90 - abs(180 - (360 * ((prop.1 + (prop.2 / 2)) / 100))))
  prop.2.y <- 0.725 * sin(B * (pi/180))
  prop.2.x <- sqrt((0.725^2 - prop.2.y^2))
  if(prop.1 + prop.2/2 > 50) prop.2.x <- prop.2.x * -1
  if((prop.1 + prop.2/2 > 25) & (prop.1 + prop.2/2 < 75)) prop.2.y <- prop.2.y * -1
  if(prop.2 > 0) text(prop.2.x, prop.2.y, paste0(prop.2, "%"), cex = 1.15, col = "white", font = 2)
  text(0, 0, paste0(prop.1 + prop.2, "%"), cex = 2.3, col = grey(0.55), font = 2)
}

# function to save the plot to a svg file 
# note: stored in folder 'img' and takes the name from the levels
doughnut.svg <- function(prop.1, prop.2, col.1, col.2){
  require(stringr)
  svg(paste0("img/level-", str_pad(prop.1, 3, pad = "0"), "-",
             str_pad(prop.2, 3, pad = "0"), ".svg"), 
      width = 2, height = 2, bg = "transparent")
  doughnut(prop.1, prop.2, col.1, col.2)
  dev.off()   
}

# function to save the plot to a png file 
# note: stored in folder 'img' and takes the name from the levels
doughnut.png <- function(prop.1, prop.2, col.1, col.2){
  svg(paste0("img/level-", str_pad(prop.1, 3, pad = "0"), "-",
             str_pad(prop.2, 3, pad = "0"), ".png"), 
      width = 2, height = 2, bg = "transparent")
  doughnut(prop.1, prop.2, col.1, col.2)
  dev.off()   
}

# colours for status-quo and management levels
col.1 <- "#d73027"
col.2 <- "#1a9850"

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

