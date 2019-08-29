library(rgdal)
library(rgeos)
library(raster)
library(maptools)

#Load shapefile and raster for COG
COG_shp <- readOGR("data/shp/COG_adm1.shp", stringsAsFactors = FALSE)
COG_raster <- raster("data/raster/COG_raster.tif")

#Plot to see everything
windows()
plot(COG_shp)
plot(COG_raster, add = TRUE, legend = FALSE)

#Cropland values
crop_values <- c(10, 11, 12, 20, 30)

#Give crop_values the same values that are not taken by anything else and remove the rest
COG_crop_raster <- COG_raster
COG_crop_raster[COG_crop_raster %in% crop_values] <- 9999
COG_crop_raster[COG_crop_raster != 9999] <- NA

#Plot to check
plot(COG_shp)
plot(COG_crop_raster, add = TRUE, col = "blue", legend = FALSE)

#Give crop_values the same values that are not taken by anything else and remove the rest
#Aggregate values - this is done so the map looks better at lower resolutions (aka global or continental)
crop_finding_function <- function(x, crop_threshold = 0.1, ...){
  if(any(x %in% crop_values)){
    if(length(which(x %in% crop_values))/length(x) >= crop_threshold) 9999 else NA
  } else NA
}

#Change factor for size
COG_crop_raster_agg <- raster::aggregate(COG_raster, fact = 10, fun = crop_finding_function)

#Plot to check how it looks at the continet level
par(mfrow = c(1, 2))
plot(COG_shp, ylim = c(-36, 36), xlim = c(-19, 52), border = NA)
plot(COG_crop_raster_agg, col = "blue", legend = FALSE, add = TRUE)
mtext(side = 3, line = -5, text = "Aggregated")
plot(COG_shp, ylim = c(-36, 36), xlim = c(-19, 52), border = NA)
plot(COG_crop_raster, col = "blue", legend = FALSE, add = TRUE)
mtext(side = 3, line = -5, text = "Non-aggregated")


#Now find which admin units different crop colours are in
#Generate fake disease burde df
disease_burden <- data.frame("ISO" = COG_shp$ISO, "adm_id" = COG_shp$ID_1, "burden" = 1:12)
disease_burden_raster <- COG_crop_raster_agg

all_adm_burden <- sapply(disease_burden$adm_id, function(x){
  these_intersect <- intersect(rasterize(COG_shp[COG_shp$ID_1 == x, ], disease_burden_raster, mask = TRUE), COG_shp[COG_shp$ID_1 == x, ])
  these_intersect[these_intersect == 9999] <- disease_burden[disease_burden$adm_id == x, ]$burden
  these_intersect
}, simplify = F)


#Merge everything to the same raster
total_burden_country <- do.call(merge, all_adm_burden)

par(mfrow = c(1, 1))
plot(total_burden_country)
plot(COG_shp, add = T)

