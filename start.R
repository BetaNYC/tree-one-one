library(ggmap)
library(dplyr)
library(reshape2)
library(rjson)

trees.full <- read.csv("data/2015_Street_Tree_Census_-_Tree_Data.csv")

decimal_round <- function(x, level=1, direction = "up")  {
  if (direction == "up"){
    round(x+ 5*10^(-level-1), level)
  } else{
    round(x- 5*10^(-level-1), level)
  }
}

trees <- trees.full[, c("tree_id", "block_id", "tree_dbh", "status", "health", "spc_latin", "spc_common", "latitude", "longitude")]
trees$corner.se.lat <- decimal_round(trees$latitude, 3, "down")
trees$corner.se.lng <- decimal_round(trees$longitude, 3, "down")
trees$corner.ne.lat <- decimal_round(trees$latitude, 3, "up")
trees$corner.ne.lng <- decimal_round(trees$longitude, 3, "down")
trees$corner.nw.lat <- decimal_round(trees$latitude, 3, "up")
trees$corner.nw.lng <- decimal_round(trees$longitude, 3, "up")
trees$corner.sw.lat <- decimal_round(trees$latitude, 3, "down")
trees$corner.sw.lng <- decimal_round(trees$longitude, 3, "up")

square <- unique(trees[, c("corner.se.lat", "corner.se.lng", "corner.ne.lat", "corner.ne.lng", "corner.nw.lat", "corner.nw.lng", "corner.sw.lat", "corner.sw.lng")])
square$id <- seq_along(square$corner.se.lat)
trees.merged <- inner_join(trees, square)

trees.grouped <- trees.merged %>% group_by(id) %>% summarize(alive_count = sum(status == "Alive"), total_count = n(),
                                                             dbh_avg = mean(tree_dbh), 
                                                             species_count = length(unique(spc_latin)))
trees.grouped$aliveness <- round(trees.grouped$alive_count / trees.grouped$total_count, 2)
trees.grouped$aliveness_group <- .bincode(trees.grouped$aliveness, seq(0, 1, by = 0.1))
colfunc <- colorRampPalette(c("red", "green"))
cols <- data.frame(hex = colfunc(10), col_id= 1:10)


trees.list <- list()
for (i in 1: nrow(trees.grouped)) {
  trees.list[[length(trees.list) + 1]] <- trees.grouped[i, ]$id
  trees.list[[i]]$bounds <- paste0('[[', square[square$id == trees.grouped[i, ]$id, ]$corner.ne.lat, ', ',
                                   square[square$id == trees.grouped[i, ]$id, ]$corner.ne.lng, '], [',
                                   square[square$id == trees.grouped[i, ]$id, ]$corner.sw.lat, ', ', 
                                   square[square$id == trees.grouped[i, ]$id, ]$corner.sw.lng, ']]')
  trees.list[[i]]$aliveness <- trees.grouped[i,]$aliveness
  trees.list[[i]]$average_size <- trees.grouped[i,]$dbh_avg
  trees.list[[i]]$diversity <- trees.grouped[i,]$species_count
}

json <- toJSON(trees.list)


###
melted <- melt(square, id = c("id"))
melted.ne <- melted[melted$variable == "corner.ne.lat" | melted$variable == "corner.ne.lng",  ]
melted.se <- melted[melted$variable == "corner.se.lat" | melted$variable == "corner.se.lng",  ]
melted.sw <- melted[melted$variable == "corner.sw.lat" | melted$variable == "corner.sw.lng",  ]
melted.nw <- melted[melted$variable == "corner.nw.lat" | melted$variable == "corner.nw.lng",  ]
casted <- as.data.frame(rbind(as.matrix(dcast(melted.ne, id ~ variable)), as.matrix(dcast(melted.se, id ~ variable)),
                              as.matrix(dcast(melted.sw, id ~ variable)), as.matrix(dcast(melted.nw, id ~ variable))))
names(casted) <- c("square_id", "lat", "lng")

#merge(casted, trees, by = c("square_id"))
plotdata <- inner_join(casted, trees.merged, by = c("square_id" = "id"))

p <- ggplot(casted, aes(x = lng, y = lat)) + geom_polygon(aes(fill = ))

###
map <- get_map(location = "40.705873, -74.013200", zoom = 12, maptype = "toner-lines")
ggmap(map) + 
#   geom_point(data = trees.err,
#              aes(x = lon, y = lat, color = cluster), alpha = 0.4, size = 8) +
#   geom_text(data = means, aes(x = lon, y = lat, label = cluster), size = 8, color = "red4", 
#             alpha = 0.8) +
  geom_polygon()
  theme_nothing() 
  
  
  % alivE