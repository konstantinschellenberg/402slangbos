if (interactive()) {
    ### example: svg -----

    library(sp)
    library(lattice)

    data(meuse)
    coordinates(meuse) = ~ x + y
    proj4string(meuse) = CRS("+init=epsg:28992")
    meuse = spTransform(meuse, CRS("+init=epsg:4326"))

    ## create plots with points colored according to feature id
    library(lattice)
    p = xyplot(copper ~ cadmium, data = meuse@data, col = "grey")
    p = mget(rep("p", length(meuse)))

    clr = rep("grey", length(meuse))
    p = lapply(1:length(p), function(i) {
        clr[i] = "red"
        update(p[[i]], col = clr)
    })

    leaflet() %>%
        addTiles() %>%
        addCircleMarkers(data = meuse, popup = popupGraph(p, type = "svg"))

    ### example: png -----
    pt = data.frame(x = 174.764474, y = -36.877245)

    coordinates(pt) = ~ x + y
    proj4string(pt) = "+init=epsg:4326"

    p2 = levelplot(t(volcano), col.regions = terrain.colors(100))

    leaflet() %>%
        addTiles() %>%
        addCircleMarkers(data = pt, popup = popupGraph(p2, width = 300, height = 400))

    ### example: html -----
    leaflet() %>%
        addTiles() %>%
        addCircleMarkers(
            data = breweries91[1, ],
            popup = popupGraph(
                leaflet() %>%
                    addProviderTiles("Esri.WorldImagery") %>%
                    addMarkers(data = breweries91[1, ],
                               popup = popupTable(breweries91[1, ])),
                type = "html"
            )
        )

}

if (interactive()) {
    ## remote images -----
    ### one image
    library(sf)

    pnt = st_as_sf(data.frame(x = 174.764474, y = -36.877245),
                   coords = c("x", "y"),
                   crs = 4326)

    img = "http://bit.ly/1TVwRiR"

    leaflet() %>%
        addTiles() %>%
        addCircleMarkers(data = pnt, popup = popupImage(img, src = "remote"))

    ### multiple file (types)
    library(sp)
    images = c(img,
               "https://upload.wikimedia.org/wikipedia/commons/9/91/Octicons-mark-github.svg",
               "https://www.r-project.org/logo/Rlogo.png",
               "https://upload.wikimedia.org/wikipedia/commons/d/d6/MeanMonthlyP.gif")

    pt4 = data.frame(x = jitter(rep(174.764474, 4), factor = 0.01),
                     y = jitter(rep(-36.877245, 4), factor = 0.01))
    coordinates(pt4) = ~ x + y
    proj4string(pt4) = "+init=epsg:4326"

    leaflet() %>%
        addTiles() %>%
        addMarkers(data = pt4, popup = popupImage(images)) # NOTE the gif animation

    ## local images -----
    pnt = st_as_sf(data.frame(x = 174.764474, y = -36.877245),
                   coords = c("x", "y"), crs = 4326)
    img = system.file("img","Rlogo.png",package="png")
    leaflet() %>%
        addTiles() %>%
        addCircleMarkers(data = pnt, popup = popupImage(img))
}

library(leaflet)

leaflet() %>% addTiles() %>% addCircleMarkers(data = breweries91)
leaflet() %>%
    addTiles() %>%
    addCircleMarkers(data = breweries91,
                     popup = popupTable(breweries91))
leaflet() %>%
    addTiles() %>%
    addCircleMarkers(data = breweries91,
                     popup = popupTable(breweries91,
                                        zcol = c("brewery", "zipcode", "village"),
                                        feature.id = FALSE,
                                        row.numbers = FALSE))

