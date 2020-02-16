# Inspection of the groud truth polygons

source("import.R")
library(leaflet)
library(RColorBrewer)

# (1) calc area of the polygons, (2) calc sums of the areas
gt.stats = gt %>%
    mutate(area = st_area(.)) %>%
    group_by(Name) %>%
    st_transform(4326)
gt.stats$area = as.numeric(gt.stats$area)

gt.sum = gt.stats %>%
    summarise(sum = sum(area))

# plot -------------------------------------------------------------------------

mypal = colorNumeric(
    palette = "YlOrRd",
    domain = gt.stats$Name
)

mypopup = paste0("ID: ", gt.stats$Name, "<br>", "size: ", round(gt.stats$area,0))

mymap = leaflet() %>%
    addProviderTiles("Stamen.Toner") %>%
    addPolygons(data = gt.stats,
                fillColor = ~mypal(area),
                color = "#b2aeae", # you need to use hex colors
                fillOpacity = 0.7,
                weight = 1,
                smoothFactor = 0.2,
                popup = mypopup) %>%
    addLegend(pal = mypal,
              values = gt.stats$area,
              position = "bottomright",
              title = "Size",
              labFormat = labelFormat(suffix = " m²"))
mymap
