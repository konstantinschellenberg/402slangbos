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

class = 1
number = 4

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
grep2 = function(source, class, number = NULL){

    # if there is no number given -> return the whole class
    if (is.null(number)){
        cls = as.character(class)
        cat("Ground truth set: ", deparse(substitute(source)), cls, sep = "\n")
        table = source[[cls]] # indexing task

    } else {

        # if there is a number given -> return just one

        cls = as.character(class) # convert to string (list headers are strings not int)
        num = as.character(number) # same here
        cat("Ground truth set: ", deparse(substitute(source)), cls, num, sep = "\n") # tidy print
        table = source[[cls]][[num]] # indexing task
    }

    if (is.null(table)){stop("This number doesn't exist")}

    # # spatial plotting
    # subset = gt %>%
    #     filter(Name == class) %>%
    #     .[number, ]
    #
    # pal = brewer.pal(9, "YlGnBu")[class] # get colour for class from brewer pal
    #
    # g = ggplot() +
    #     geom_sf(data = subset, fill = pal) +
    #     ggtitle(label = paste("Plot:", number, "\t", "Class:", class)) +
    #     theme_map()
    # print(g)

    # convert date to POSTix
    table$date = as.POSIXct(substr(table$date, start = nchar(table$date) -9, stop = nchar(table$date)),
               format = "%Y.%m.%d", tz = "")

    return(table)
}
position = function(class, number = Null){
    gt %>%
        filter(Name == class) %>%
        .[number, ] %>%
        st_transform(4326)
}
scale_custom = function(x, fun = NULL, center = FALSE){
    m = as.matrix(x[2:6])

    # main scale function
    if (is.null(fun)){m = scale(m, center=center, scale=colSums(m))
    } else if (!is.null(fun)){m = scale(m, center=center, scale=colSums(m)) * fun}

    out = as.data.frame(m) %>%
        mutate(date = x$date, count = x$count)
    cat("data normalised!")
    return(out)
} # scaling the parameters

cat("Do you need the data normalised or not?", "Append `scale_custom` on the grep2 call if needed", sep = "\n")

# load plot components
one = grep2(gt_vh, 1, 26) #%>% scale_custom(fun = -1, center = T)
two = grep2(gt_vh, 2, 3) #%>% scale_custom(center = T)
thr = grep2(gt_vh, 3, 13) #%>% scale_custom(center = T)
# one = grep2(gt_vh, 4, 4) #%>% scale_custom(fun = -1, center = T)
# two = grep2(gt_vh, 5, 3) #%>% scale_custom(center = T)
# thr = grep2(gt_vh, 6, 3) #%>% scale_custom(center = T)
# one = grep2(gt_vh, 7, 5) #%>% scale_custom(fun = -1, center = T)
# two = grep2(gt_vh, 8, 3) #%>% scale_custom(center = T)
# thr = grep2(gt_vh, 9, 1) #%>% scale_custom(center = T)

one = grep2(gt_vh, 1, 3) %>% scale_custom(fun = -1, center = T)
two = grep2(gt_red, 1, 3) %>% scale_custom(center = T)
thr = grep2(gt_nir, 1, 3) %>% scale_custom(center = T)

sf = position(1, 26)

all_increase = grep2(gt_vh, 1)

# Visualisation ----------------------------------------------------------------

# gt overview map (interactive)

mypopup = paste0("ID: ", gt$Name, "<br>", "Number: ", gt$number)
pal = brewer.pal(9, "RdYlBu")
factpal = colorFactor(palette = pal,
                      domain = gt$Name)
lon = st_bbox(st_centroid(sf))[1] %>% as.vector()
lat = st_bbox(st_centroid(sf))[2] %>% as.vector()

mymap = leaflet() %>%
    addProviderTiles("Esri.WorldImagery") %>%
    setView(lon, lat, zoom = 16) %>%
    addPolygons(data = st_transform(study_area, 4326),
                fillOpacity = 0.1,
                color = "darkgrey") %>%
    addPolygons(data = sf,
                fillOpacity = 0,
                weight = 1) %>%
    addPolygons(data = st_transform(gt, 4326),
                # fillColor = ~factpal(Name),
                color = ~factpal(Name), # you need to use hex colors
                fillOpacity = 0.1,
                weight = 1,
                smoothFactor = 0.2,
                popup = mypopup)
mymap

ggplot()+
    geom_line(one, mapping = aes(x = date, y = median), color = "red") +
    geom_line(two, mapping = aes(x = date, y = median), color = "blue") +
    geom_line(thr, mapping = aes(x = date, y = median), color = "darkgreen")

for (i in 1){

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

# curve smoothing---------------------------------------------------------------

pr_1 = supsmu(x_1, med_1) # smoothing curve
pr_2 = supsmu(x_2, med_2)
pr_3 = supsmu(x_3, med_3)
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

# Format edits
data.fmt = list(color="#878787", width=1)
line_1.fmt = list(dash="solid", width = 0.5, color="#b6e1fb") # blue
pr_1.fmt = list(dash="solid", width = 1.5, color="#2c6487")
line_2.fmt = list(dash="solid", width = 0.5, color="#fc8d59") # red
pr_2.fmt = list(dash="solid", width = 1.5, color="#fa5305")
line_3.fmt = list(dash="solid", width = 0.5, color="#007f00")
pr_3.fmt = list(dash="solid", width = 1, color="#007f00")
interval.fmt = list(dash="solid", width = 1, color="grey")


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
    title = "Normalised signal (Reflectance/backscatter)",
    titlefont = f2,
    tickfont = f1,
    showticklabels = TRUE,
    exponentformat = "E",
    # range = c(-25L, -14)
    range = c(min(two$median), max(thr$median))
)
}

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
                name = "",
                showlegend = FALSE) %>%

    add_ribbons(x = pr_losd_2$x,
                ymin = pr_losd_2$y,
                ymax = pr_upsd_2$y,
                color = I(red_background), line = list(width = 0), opacity = 0.4,
                name = "",
                showlegend = FALSE) %>%

    add_ribbons(x = pr_losd_3$x,
                ymin = pr_losd_3$y,
                ymax = pr_upsd_3$y,
                color = I(green_background), line = list(width = 0), opacity = 0.2,
                name = "",
                showlegend = FALSE) %>%
    add_lines(x = x_1, y = pr_1$y, line = pr_1.fmt, name = "(1)") %>%
    add_lines(x = x_2, y = pr_2$y, line = pr_2.fmt, name = "(2)") %>%
    add_lines(x = x_3, y = pr_3$y, line = pr_3.fmt, name = "(3)") %>%


print(plt)

# export of image -> has to be in the project directory!
plotly::orca(plt, file = "data/plottest.png")


# PLot for S1 vs S2 plots ------------------------------------------------------

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

    add_trace(x = x_1, y = med_1, marker = list(color = "#3858E7"), name = "vh") %>%
    add_trace(x = x_2, y = med_2, marker = list(color = "red"), name = "red") %>%
    add_trace(x = x_3, y = med_3, marker = list(color = "green"), name = "nir")Y
    # add_lines(x = lin_1$qr$qr$, y = lin_1$model$med_1)

plt
plotly::orca(plt, file = "data/plot_vrn.png")
