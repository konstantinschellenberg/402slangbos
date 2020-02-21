#' Data exploration of the ground truth data
#'
#' Good classes (15.02.2020)
#' 1: 3
#' 2: 3
#' 3: 13
#' 4: 4
#' 5: 13
#' 6: 3
#' 7: 5
#' 8: 3
#' 9: 1

source("import.R")

# ------------------------------------------------------------------------------

gt.files = list.files(path_rds, pattern = "gt_", full.names = TRUE)

gt_vv = readRDS(gt.files[grep(x = gt.files, "vv")])
gt_vh = readRDS(gt.files[grep(x = gt.files, "vh")])
gt_red = readRDS(gt.files[grep(x = gt.files, "red")])
gt_nir = readRDS(gt.files[grep(x = gt.files, "nir")])

# accessing a list content
# plot(gt_vv$"1"$"32"$sd)

# Wrapper to receive a specific plot and show it spatially ---------------------

# function to retrieve the data

cat("Do you need the data normalised or not?", "Append `scale_custom` on the grep2 call if needed", sep = "\n")

# load plot components
one = grep2(gt_vh, 1, 7)
two = grep2(gt_vh, 2, 1)
thr = grep2(gt_vh, 3, 4)
# one = grep2(gt_vh, 4, 4)
# two = grep2(gt_vh, 5, 3)
# thr = grep2(gt_vh, 6, 3)
# one = grep2(gt_vh, 7, 5)
# two = grep2(gt_vh, 8, 3)
# thr = grep2(gt_vh, 9, 1)

c = 4
n = 4

# gettin bulk data

for (i in 1){
    one = grep2(gt_vh, c, n) %>% scale_custom(center = T)
    two = grep2(gt_red, c, n) %>% scale_custom(center = T)
    thr = grep2(gt_nir, c, n) %>% scale_custom(center = T)
    ndvi.pre.red = grep2(gt_red, c, n)
    ndvi.pre.nir = grep2(gt_nir, c, n)


    ndvi = data.frame(matrix(nrow = nrow(ndvi.pre.nir), ncol = 2))
    ndvi$X2 = fun.ndvi(ndvi.pre.red$median, ndvi.pre.nir$median)
    ndvi$X1 = two$date
    # optional scaling of the ndvi:
    ndvi$X2 = scale(ndvi$X2, center = T) %>% as.vector()

    # we should parse POSTIX earlier, really... BAUSTELLE!
    # grep all of the one class
    all = grep2(gt_vh, c)

    sf = position(c, n)
}


# Visualisation ----------------------------------------------------------------

# gt overview map (interactive)

mypopup = paste0("ID: ", gt$Name, "<br>", "Number: ", gt$number)
pal = brewer.pal(9, "RdYlBu")
factpal = colorFactor(palette = pal,
                      domain = gt$Name)

# if zoomed to gt example
lon = st_bbox(st_centroid(sf))[1] %>% as.vector()
lat = st_bbox(st_centroid(sf))[2] %>% as.vector()

# if to zoom on the whole area:
# lon = st_bbox(st_centroid(st_transform(study_area, 4326)))[1] %>% as.vector()
# lat = st_bbox(st_centroid(st_transform(study_area, 4326)))[2] %>% as.vector()

mymap = leaflet() %>%
    addProviderTiles("Esri.WorldImagery") %>%
    setView(lon, lat, zoom = 17) %>%
    addPolygons(data = st_transform(study_area, 4326),
                fillOpacity = 0,
                weight = 1,
                color = "darkgrey") %>%
    addPolygons(data = sf,
                fillOpacity = 0,
                weight = 1) %>%
    addPolygons(data = st_transform(gt, 4326),
                fillColor = ~factpal(Name),
                color = ~factpal(Name), # you need to use hex colors
                fillOpacity = 1,
                weight = 3,
                smoothFactor = 0.2,
                popup = mypopup)
mymap

# library(htmlwidgets)
# setwd("data/")
# saveWidget(mymap, file="overview.png")
# setwd("D:/Geodaten/Master/projects/402slangbos")

ggplot()+
    geom_line(one, mapping = aes(x = date, y = median), color = "red") +
    # geom_line(two, mapping = aes(x = date, y = median), color = "blue") +
    # geom_line(thr, mapping = aes(x = date, y = median), color = "darkgreen")+
    geom_line(ndvi, mapping = aes(x = X1, y = X2), color = "black")


# this loop contains all the settings required to plot the graphs with non-default graphic sets.
for (i in 1){

pal2 = brewer.pal(8, "Dark2")

# one
x_1 = one$date
med_1 = one$median
upsd_1 = one$lower_sd
losd_1 = one$upper_sd

# two
x_2 = two$date
med_2 = two$median
upsd_2 = two$lower_sd
losd_2 = two$upper_sd

# three
x_3 = thr$date
med_3 = thr$median
upsd_3 = thr$lower_sd
losd_3 = thr$upper_sd

# ndvi
x_4 = ndvi[[1]]
med_4 = ndvi[[2]]

# curve smoothing---------------------------------------------------------------

pr_1 = supsmu(x_1, med_1)# smoothing curve
pr_2 = supsmu(x_2, med_2)
pr_3 = supsmu(x_3, med_3)
pr_4 = supsmu(x_4, med_4)
pr_losd_1 = supsmu(x_1, losd_1) # making smoothed line for standard deviation
pr_upsd_1 = supsmu(x_1, upsd_1)
pr_losd_2 = supsmu(x_2, losd_2)
pr_upsd_2 = supsmu(x_2, upsd_2)
pr_losd_3 = supsmu(x_3, losd_3)
pr_upsd_3 = supsmu(x_3, upsd_3)

# linear reg
lin_1 = lm(x_1 ~ med_1)
lin_2 = lm(x_2 ~ med_2)
lin_3 = lm(x_3 ~ med_3)


# Colours-----------------------------------------------------------------------

mycolour = (20)

blue_backgroud = "#6ec3f7"
red_background = "#fc8d59"
green_background = "#007f00"
grey_background = "#909090"

# Format edits
data.fmt = list(color="#878787", width=1)
line_1.fmt = list(dash="solid", width = 0.5, color="#b6e1fb") # blue
pr_1.fmt = list(dash="solid", width = 1.5, color="#2c6487")
line_2.fmt = list(dash="solid", width = 0.5, color="#fc8d59") # red
pr_2.fmt = list(dash="solid", width = 1.5, color="#fa5305")
line_3.fmt = list(dash="solid", width = 0.5, color="#007f00")
pr_3.fmt = list(dash="solid", width = 1, color="#007f00")
interval.fmt = list(dash="solid", width = 1, color="grey")

vh.fmt = list(dash="solid", width = 3, color="#2c6487") # blue
red.fmt = list(dash="solid", width = 2, color="#fc8d59") # red
nir.fmt = list(dash="solid", width = 2, color="#007f00") # green
ndvi.fmt = list(dash = "do", width = 2, color = "red")

# dash = "do"
# Axes
f1 <- list(
    family = "Arial, sans-serif",
    size = 15,
    color = "grey"
)

f2 <- list(
    family = "Arial, sans-serif",
    size = 17,
    color = "grey90"
)

x_axis <- list(
    title = "Date",
    titlefont = f2,
    tickfont = f1,
    showticklabels = TRUE,
    range = c(min(x_1), max(x_1))
)

y_axis <- list(
    title = "VH backscatter [db]",
    titlefont = f2,
    tickfont = f1,
    showticklabels = TRUE,
    exponentformat = "E",
    range = c(-25L, -12)
    # range = c(min(two$median), max(two$median))
    # range = c(-3, 3)
)
vrn_y_axis <- list(
    title = "Reflectance [W/m²]",
    titlefont = f2,
    tickfont = f1,
    showticklabels = TRUE,
    exponentformat = "E",
    side = "right",
    # range = c(-25L, -14)
    range = c(min(two$median), max(thr$median))
)
}

1
# PLOTLY--------------------------------------------------------------------------

plt = plot_ly(data = one,
              x = ~date,
              y = ~median,
              width = 700,
              height = 400) %>%

    layout(title = "",
           yaxis = y_axis,
           xaxis = x_axis,
           legend = list(font = f1, traceorder = "reversed", yanchor = "bottom",
                         xanchor = "right")) %>%

    add_ribbons(x = pr_losd_1$x,
                ymin = pr_losd_1$y,
                ymax = pr_upsd_1$y,
                color = I(blue_backgroud), line = list(width = 0), opacity = 0.4,
                name = "SB: Increase",
                showlegend = FALSE) %>%

    # add_ribbons(x = pr_losd_2$x,
    #             ymin = pr_losd_2$y,
    #             ymax = pr_upsd_2$y,
    #             color = I(green_background), line = list(width = 0), opacity = 0.3,
    #             name = "SB: Continuous",
    #             showlegend = FALSE) %>%

    add_ribbons(x = pr_losd_3$x,
                ymin = pr_losd_3$y,
                ymax = pr_upsd_3$y,
                color = I(red_background), line = list(width = 0), opacity = 0.2,
                name = "SB: Breakpoint",
                showlegend = FALSE) %>%
    add_lines(x = x_1, y = pr_1$y, line = pr_1.fmt, name = "(1) SB: Increase") %>%
    # add_lines(x = x_2, y = pr_2$y, line = pr_3.fmt, name = "(2) SB: Continuous") %>%
    add_lines(x = x_3, y = pr_3$y, line = pr_2.fmt, name = "(3) SB: Breakpoint") %>%


print(plt)

# export of image -> has to be in the project directory!
plotly::orca(plt, file = "data/plot13.svg")


# PLot for S1 vs S2 plots ------------------------------------------------------
# smoothing curve
# pr_1 = supsmu(x_1, med_1, span = 0.1)
# pr_2 = supsmu(x_2, med_2, span = 0.1)
# pr_3 = supsmu(x_3, med_3, span = 0.1)
# pr_4 = supsmu(x_4, med_4, span = 0.1)

plt = plot_ly(width = 700,
              height = 600) %>%

    layout(title = "Comparison of Sentinel signals to Slangbos",
           yaxis = y_axis,
           xaxis = x_axis,
           legend = list(font = f1, orientation = "v")) %>%

    add_annotations(text = "Agriculture (Class 4)",
                    x = 0.5,
                    y = 1,
                    yref = "paper",
                    xref = "paper",
                    xanchor = "middle",
                    yanchor = "top",
                    showarrow = FALSE,
                    font = list(size = 15)) %>%

    add_trace(x = x_1,
              y = med_1,
              name = "S1 - VH",  marker = list(color = "#3858E7", size = 3)) %>%
    # add_trace(x = x_2,
    #           y = med_2,
    #           name = "S2 red median", marker = list(color = "#fc8d59", size = 3)) %>%
    # add_trace(x = x_3,
    #           y = med_3,
    #           name = "S2 NIR median", marker = list(color = "green", size = 3)) %>%
    add_trace(x = x_4,
              y = med_4,
              name = "S2 - NDVI", marker = list(color = "red", size = 3)) %>%


    add_lines(x = x_1,
              y = pr_1$y,
              line = vh.fmt, name = "S1 - VH [moving avg.]") %>%
#
#     add_lines(x = x_2,
#               y = pr_2$y,
#               line = red.fmt, name = "S2 red smooth") %>%
#
#     add_lines(x = x_3,
#               y = pr_3$y,
#               line = nir.fmt, name = "S2 NIR smooth") %>%

    add_lines(x = x_4,
              y = pr_4$y,
              line = ndvi.fmt, name = "S2 - NDVI [moving avg.]") %>%

    add_ribbons(x = x_1, ymin = -1, ymax = 1,
                color = I(grey_background), line = list(width = 0), opacity = 0.2,
                name = "",
                showlegend = FALSE)

plt
plotly::orca(plt, file = "data/plot_vrn_cls4_4.svg")

# Plot all of one class: -------------------------------------------------------

# get the data
all = grep2(gt_vh, 4)

# init plot
plt = plot_ly(width = 600, height = 600) %>%

    layout(xaxis = x_axis,
           yaxis = y_axis,
           title = "Agriculture",
           showlegend = F)

counter = 1
for (n in all){

    # new format for each line:
    massplt.fmt = list(dash = "solid", width = 1, color = sample(pal2, 1))

    # calc stats for each
    x = n$date
    med = n$median
    upsd = n$lower_sd
    losd = n$upper_sd

    # soothing
    pr = supsmu(x, med) # smoothing curve
    pr_losd = supsmu(x, losd) # making smoothed line for standard deviation
    pr_upsd = supsmu(x, upsd)

    plt = add_lines(plt,
                    x = x,
                    y = pr$y,
                    # name = paste("Increase plot (", counter, ")", sep = ""),
                    line = massplt.fmt)
    counter = counter + 1

    if (counter == 15){break}
}

plt

plotly::orca(plt, file = "data/massplt_cls4.svg")
