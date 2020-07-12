#' Goal of this script is to generate static, or best, dynamic html popup stats graphs on a mapview/leaflet
#' Map.
#' Konstantin Schellenberg, 12.05.2020
#'
#' Depends on plotting.R


source("import.R")
source("functions_plotting.R")

library(leafpop)

# ------------------------------------------------------------------------------

graph.concat = function(c, n, graphid, suffix){
    src = file.path(env, "hiwi", c)
    pattern = paste0(c, "-", n, "_", graphid, ".*\\.", suffix, "$")
    graphs = list.files(src, full.names = T)[grepl(pattern = pattern, list.files(src))]
    return(graphs)
}

graphid = "bscVH-cohVV"

imap = function(graphid = "bscVH-cohVV", suffix = "html"){
    # load classificatin palette
    qgis_pal = get_qgis_palette("D:/Geodaten/#Jupiter/GEO402/layout/classification_palette.csv")

    # Zoom to Central Study Area
    lon = st_bbox(st_centroid(study_area))[1] %>% as.vector()
    lat = st_bbox(st_centroid(study_area))[2] %>% as.vector()

    # Import Graphs ------------------------------------------------------------
    vec = c()
    for (a in 1:9){
        numbers = elements[a]
        cat("Printing class", a, "with", numbers, "reference plots\n")
        for (i in 1:numbers){
            graph_path = graph.concat(a, i, graphid, suffix)
            vec = append(vec, graph_path)
        }
    }

    # append graph ids to gt_graphs
    gt_graphs = arrange(gt, Name, number) %>%
        cbind(graphs = vec)
    gt_graphs$graphs = as.character(gt_graphs$graphs)

    l = lapply(gt_graphs$graphs, function(x) list(x))
    popupImage(l[[1]])

    # Concat Popups ------------------------------------------------------------

    mypopup = paste0("ID: ", gt_graphs$descrip, "<br>", "Number: ", gt_graphs$number, "<br> Area: ",
                     round(st_area(gt_graphs)), " m²",
                     popupGraph(gt_graphs$graphs, type = "html"))

    factpal = colorFactor(palette = qgis_pal, domain = gt_graphs$Name)



    mymap = leaflet() %>%
        addProviderTiles("Esri.WorldImagery") %>%
        # setView(lon, lat, zoom = 13) %>%
        addPolygons(data = st_transform(study_area, 4326),
                    fillOpacity = 0,
                    weight = 3,
                    color = "black") %>%
        addPolygons(data = st_transform(gt_graphs, 4326),
                    fillColor = ~factpal(Name),
                    color = ~factpal(Name), # you need to use hex colors
                    fillOpacity = 0,
                    weight = 3,
                    smoothFactor = 0.1,
                    popup = mypopup, group = "Reference plots") %>%
        addLegend(position = "bottomright", colors = qgis_pal, values = gt_graphs$Name, title = "Classes",
                  labels = c("Increase", "Continuous", "Breakpoint", "Arable Land", "Bare Soil", "Grassland", "Forest", "Urban", "Water")) %>%
        addLayersControl(
            overlayGroups = c("Reference plots"),
            options = layersControlOptions(collapsed = FALSE))
    return(mymap)
}

imap()

# save leaflets
# mapview::mapshot(map(a, i), file = paste0("hiwi/", a, "/map_", a, "-", i, ".png"), remove_controls = c("zoomControl", "layersControl"))


# find the plots

c = 1
n = 5




