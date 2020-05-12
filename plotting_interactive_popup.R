#' Goal of this script is to generate static, or best, dynamic html popup stats graphs on a mapview/leaflet
#' Map.
#' Konstantin Schellenberg, 12.05.2020
#'
#' Depends on plotting.R


map = function(c, n){
    # load classificatin palette
    qgis_pal = get_qgis_palette("D:/Geodaten/#Jupiter/GEO402/layout/classification_palette.csv")

    # get geographical position and identify of reference plot
    sf = position(c, n)

    description = data.frame(asdf = "Increase", "Continuous", "Breakpoint", "Agriculture", "Bare Soil", "Grassland", "Forest", "Urban", "Water")


    s = stats(list.dataframes = list.dataframes, c, n)
    g = plt1(a = s, all = F)

    popup = leafpop::popupGraph(g, type = "html")
    # aggregate overview median raster
    # r = raster::aggregate(vh_med, fact = 2, fun = mean)

    # optional: classification
    # pred.classif = pred[[3]]
    # pred.classif = raster::aggregate(pred.classif, fact = 2, fun = modal)
    # pred.classif = pred[[3]]
    # vrn = raster::aggregate(pred[[3]], fact = 2, fun = max)

    mypopup = paste0("ID: ", gt$descrip, "<br>", "Number: ", gt$number, "<br> Area: ",
                     round(st_area(gt)), " m²")

    rasterpal = colorNumeric(c("black", "white"), values(r), na.color = "transparent")
    factpal = colorFactor(palette = qgis_pal, domain = gt$Name)
    # classifpal = colorFactor(palette = qgis_pal, domain = 1:9, na.color = "transparent")

    # if zoomed to gt example
    lon = st_bbox(st_centroid(sf))[1] %>% as.vector()
    lat = st_bbox(st_centroid(sf))[2] %>% as.vector()

    mymap = leaflet() %>%
        addProviderTiles("Esri.WorldImagery") %>%
        addRasterImage(x = r, opacity = 0.9, colors = rasterpal, group = "VH Median") %>%
        # addRasterImage(x = pred.classif, opacity = 0.9, colors = classifpal, group = "Classification") %>%
        setView(lon, lat, zoom = 17) %>%
        addPolygons(data = st_transform(study_area, 4326),
                    fillOpacity = 0,
                    weight = 3,
                    color = "black") %>%
        addPolygons(data = st_transform(gt, 4326),
                    fillColor = ~factpal(Name),
                    color = ~factpal(Name), # you need to use hex colors
                    fillOpacity = 0,
                    weight = 3,
                    smoothFactor = 0.1,
                    popup = mypopup, group = "Reference plots") %>%
        # addLegend(position = "bottomright", pal = rasterpal, values = values(r), title = "S-1 VH backscatter,<br><sub>5-year median</sub>",
        #           labFormat = labelFormat(suffix = " [db]")) %>%
        addLegend(position = "bottomright", colors = qgis_pal, values = gt$Name, title = "Classes",
                  labels = c("Increase", "Continuous", "Breakpoint", "Agriculture", "Bare Soil", "Grassland", "Forest", "Urban", "Water")) %>%
        addLayersControl(
            overlayGroups = c("VH Median", "Reference plots"),
            options = layersControlOptions(collapsed = FALSE)) %>%
        hideGroup(c("VH Median"))
    return(mymap)
}

map(c, n)

# save leaflets
# mapview::mapshot(map(a, i), file = paste0("hiwi/", a, "/map_", a, "-", i, ".png"), remove_controls = c("zoomControl", "layersControl"))
