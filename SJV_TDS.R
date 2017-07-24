# Tulare Basin Groundwater Quality Map (TDS)

# set wd, load required packages, and load data
setwd("C:/Users/rpauloo/Desktop/GW Qual/GIS workshop 2017")
required.packages <- c("sp", "XML", "rgdal", "rgeos", "raster", 
                       "dismo", "leaflet", "RColorBrewer", "classInt")
lapply(required.packages, library, character.only = TRUE)

dat = read.csv(file = "tulare_gw_qual_subset.csv", stringsAsFactors = FALSE)

# explore plotting data
plot(dat$DEC_LONG_VA, dat$DEC_LAT_VA)

# create spatial points df
well_coords = cbind(dat$DEC_LONG_VA, dat$DEC_LAT_VA)
pts = SpatialPoints(well_coords) # still needs a CRS (coordinate reference system)
df = dat[,1:6]

# add a level to df that indicates if the change in TDS is + (pos) or - (neg)
df$direction = as.factor(ifelse(df$P70300_CY2 > df$P70300_CY1, "pos", "neg"))
# calculate decadal change in TDS
df$dec_change = df[, 6] - df[, 4] 

# define radius size based on absolute TDS change
small = abs(df$dec_change) < 9.99 # small changes
medium = abs(df$dec_change) > 9.99 & abs(df$dec_change) < 99.99 # medium changes
large = abs(df$dec_change) > 99.99 # large changes
## substitute TRUE values from logical indexing with numeric radius sizes for plotting
w = as.numeric(sub("TRUE", 5, small))
y = as.numeric(sub("TRUE", 10, medium))
z = as.numeric(sub("TRUE", 15, large))
## create sizes data frame and then radius column in df 
sizes = cbind(w,y,z)
df$radius = rowSums(sizes[,1:3], na.rm = TRUE)


# create spatial points data frame
ptsdf = SpatialPointsDataFrame(pts, data = df)

# create map
map = leaflet(data = ptsdf)  #%>% setView(lng = -158, lat = 21.5, zoom = 8) # I set the view because the automatic view given my dataset would be all Hawaiian islands
map = addTiles(map) # add tiles for zooming
map = addCircleMarkers(map,
    lng = well_coords[,1], 
    lat = well_coords[,2],
    radius = df$radius,
    color = ifelse(ptsdf$direction == "pos", "red", "blue"),
    stroke = FALSE, fillOpacity = 0.5,
    popup = paste(as.character(ptsdf$dec_change), "mg/L TDS", by = " "))
map
# map %>% addProviderTiles(providers$Stamen.Watercolor) #overwrites default map



