library(dplyr)
library(rjson)

# exported from https://data.cityofnewyork.us/Environment/2015-Street-Tree-Census-Tree-Data/uvpi-gqnh
trees.full <- read.csv("data/2015_Street_Tree_Census_-_Tree_Data.csv")

decimal_round <- function(x, level=1, direction = "up")  {
# via http://stackoverflow.com/questions/35807523/r-decimal-ceiling#comment59284649_35807523
  if (direction == "up"){
    round(x+ 5*10^(-level-1), level)
  } else{
    round(x- 5*10^(-level-1), level)
  }
}

# trim down (ha) data set to only columns used
trees <- trees.full[, c("tree_id", "block_id", "tree_dbh", "status", "health", "spc_latin", 
                        "spc_common", "latitude", "longitude")]

# assign each tree a square
trees$corner.se.lat <- decimal_round(trees$latitude, 3, "down")
trees$corner.se.lng <- decimal_round(trees$longitude, 3, "down")
trees$corner.ne.lat <- decimal_round(trees$latitude, 3, "up")
trees$corner.ne.lng <- decimal_round(trees$longitude, 3, "down")
trees$corner.nw.lat <- decimal_round(trees$latitude, 3, "up")
trees$corner.nw.lng <- decimal_round(trees$longitude, 3, "up")
trees$corner.sw.lat <- decimal_round(trees$latitude, 3, "down")
trees$corner.sw.lng <- decimal_round(trees$longitude, 3, "up")

# get all the unique squares and give them an id
square <- unique(trees[, c("corner.se.lat", "corner.se.lng", "corner.ne.lat", "corner.ne.lng", 
                           "corner.nw.lat", "corner.nw.lng", "corner.sw.lat", "corner.sw.lng")])
square$id <- seq_along(square$corner.se.lat)
trees.merged <- inner_join(trees, square)

# aggregate tree data by square id
trees.grouped <- trees %>% inner_join(square) %>% group_by(id) %>% 
                 summarize(alive_count = sum(status == "Alive"), total_count = n(), 
                           dbh_avg = mean(tree_dbh), species_count = length(unique(spc_latin)))
trees.grouped$aliveness <- round(trees.grouped$alive_count / trees.grouped$total_count, 2)

# assign each variable a bucket for gradient coloring
trees.grouped$aliveness_group <- .bincode(trees.grouped$aliveness, seq(0, 1, by = 0.05))
trees.grouped$species_group <- .bincode(trees.grouped$species_count, seq(0, 22, by = 2))
#trees.grouped$dbh_group <- .bincode(trees.grouped$dbh_avg, seq(0, 60, by = 1))
trees.grouped$dbh_group <- .bincode(trees.grouped$dbh_avg, seq(0, 60, by = 1))

# clean up
trees.grouped <- trees.grouped[trees.grouped$dbh_avg <= 60,]
trees.grouped[is.na(trees.grouped$aliveness_group), "aliveness_group"] <- 0
trees.grouped[is.na(trees.grouped$species_group), "species_group"] <- 0
trees.grouped[is.na(trees.grouped$dbh_group), "dbh_group"] <- 0

# create hex color ramps for javascript viz
alive_col <- data.frame(hex_alive = colorRampPalette(c("red", "green"))(21), 
                   col_id= 0:20, stringsAsFactors = F)
species_col <- data.frame(hex_species = colorRampPalette(c("lightblue", "darkblue"))(21), 
                          col_id= 0:20, stringsAsFactors = F)
dbh_col <- data.frame(hex_dbh = colorRampPalette(c("lightgreen", "darkgreen"))(61),
                      col_id= 0:60, stringsAsFactors = F)

trees.grouped <- trees.grouped %>% 
                  inner_join(alive_col, by = c("aliveness_group" = "col_id")) %>%
                  inner_join(species_col, by = c("species_group" = "col_id")) %>%
                  inner_join(dbh_col, by = c("dbh_group" = "col_id"))

# create list object to turn into json
trees.list <- list()
for (i in 1:nrow(trees.grouped)) {
  trees.list[[length(trees.list) + 1]] <- trees.grouped[i, ]$id
  trees.list[[i]]$bounds <- paste0('[[', square[square$id == trees.grouped[i, ]$id, ]$corner.ne.lat, ', ',
                                   square[square$id == trees.grouped[i, ]$id, ]$corner.ne.lng, '], [',
                                   square[square$id == trees.grouped[i, ]$id, ]$corner.sw.lat, ', ', 
                                   square[square$id == trees.grouped[i, ]$id, ]$corner.sw.lng, ']]')
  trees.list[[i]]$aliveness <- trees.grouped[i,]$aliveness
  trees.list[[i]]$average_size <- trees.grouped[i,]$dbh_avg
  trees.list[[i]]$diversity <- trees.grouped[i,]$species_count
  trees.list[[i]]$aliveness_col <- trees.grouped[i,]$hex_alive
  trees.list[[i]]$species_col <- trees.grouped[i,]$hex_species
  trees.list[[i]]$dbh_col <- trees.grouped[i,]$hex_dbh
}

json <- toJSON(trees.list)
writeLines(json, "data/square.json")


### todo
melted.ne <- melted[melted$variable == "corner.ne.lat" | melted$variable == "corner.ne.lng",  ]
melted.se <- melted[melted$variable == "corner.se.lat" | melted$variable == "corner.se.lng",  ]
melted.sw <- melted[melted$variable == "corner.sw.lat" | melted$variable == "corner.sw.lng",  ]
melted.nw <- melted[melted$variable == "corner.nw.lat" | melted$variable == "corner.nw.lng",  ]
casted <- as.data.frame(rbind(as.matrix(dcast(melted.ne, id ~ variable)), as.matrix(dcast(melted.se, id ~ variable)),
                              as.matrix(dcast(melted.sw, id ~ variable)), as.matrix(dcast(melted.nw, id ~ variable))))
names(casted) <- c("square_id", "lat", "lng")

plotdata <- inner_join(casted, trees.merged, by = c("square_id" = "id"))
p <- ggplot(casted, aes(x = lng, y = lat)) + geom_polygon(aes(fill = ))

map <- get_map(location = "40.705873, -74.013200", zoom = 12, maptype = "toner-lines")
ggmap(map) + 
  geom_polygon()
  theme_nothing() 
  
  
