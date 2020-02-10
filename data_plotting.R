# Konstantin Schellenberg, WS 2019/20
# University of Jena, Chair of remote sensing
# supervisor: Dr. Marcel Urban

# Script to plot the aggregated pixel time series information retrieved by the functions defined in raster_functions.R.

source("import.R")
library(viridis)
library(plotly)

################################################################################
# Data digesting----------------------------------------------------------------
################################################################################

# get the pixel based data from the raster brick and the user polygons
if (!file.exists("data/vv.rds")) {
    vv = list_summaries(sentinel1_brick = s1vv,
                        polygon = gt)
    saveRDS(vv, "data/vv.rds")
} else {
    vv = readRDS("data/vv.rds")
}

if (!file.exists("data/vh.rds")) {
    vh = list_summaries(sentinel1_brick = s1vh,
                        polygon = gt)
    saveRDS(vh, "data/vh.rds")
} else {
    vh = readRDS("data/vh.rds")
}

# debug: check if different
if(identical(vv$plot1_1$median,
          vh$plot1_1$median)){
    warning("the input raster are the same")
}

# violinplot
rasterVis::bwplot(vh[[1:5]])

################################################################################
# Data digesting 2 new function ------------------------------------------------
################################################################################

gt_list = readRDS("D:\\Geodaten\\#Jupiter\\GEO402\\03_develop\\rda\\training_list.rds")

df1 = gt_list[[1]][[1]]
df2 = gt_list[[1]][[2]]
#grepl listname 1: rbind -> aggregate information
rbind(df1, df2)

################################################################################
# Plotting----------------------------------------------------------------------
################################################################################

# raster
# plot(s1vv[[1]],
#      breaks = c(-25:-5),
#      col = viridis(20),
#      axes = TRUE
# )
#
# # gt
# plot(gt[1], main = "All gts", col = "red", border = "red", add = TRUE)

### init for plotly-------------------------------------------------------------
# overview plots-----------------------------------------------------------------

stelle = 1
code = 1

# get gt
example = gt %>%
    filter(gt$Name == code) %>%
    .[stelle, ]

# plot(example[1], main = paste0(code, ": ", code, "_", stelle), col = "grey")

# Plotly graphs-----------------------------------------------------------------

vv = vv # whole summary
vh = vh

grep = function(summary = vv, stelle = 1, code = 1){

    grep = summary[grepl(paste0("plot", code, "_"), names(summary))]
    # indexing
    fetched = grep[[stelle]]
    # remove na
    ready = na.omit(fetched)
    nrow(ready)
    return(ready)
}

# get data for the double plot
agro = grep(summary = vh, stelle = 2, code = 4)
incr = grep(summary = vh, stelle = 1, code = 1)
brk = grep(summary = vh, stelle = 2, code = 12)

# Assignment--------------------------------------------------------------------

# Agro
x_1 = agro$date
med_1 = agro$median
upsd_1 = agro$lower_sd
losd_1 = agro$upper_sd

# Increase
x_2 = incr$date
med_2 = incr$median
upsd_2 = incr$lower_sd
losd_2 = incr$upper_sd

# Breakpoint
x_3 = brk$date
med_3 = brk$median
upsd_3 = brk$lower_sd
losd_3 = brk$upper_sd

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


# Colours-----------------------------------------------------------------------

mycolour = (20)

blue_backgroud = "#6ec3f7"
red_background = "#fc8d59"
black_background = "#333333"

# Format edits
data.fmt = list(color="#878787", width=1)
line_1.fmt = list(dash="solid", width = 0.5, color="#b6e1fb") # blue
pr_1.fmt = list(dash="solid", width = 1.5, color="#2c6487")
line_2.fmt = list(dash="solid", width = 0.5, color="#fc8d59") # red
pr_2.fmt = list(dash="solid", width = 1.5, color="#fa5305")
line_3.fmt = list(dash="solid", width = 0.5, color="#333333")
pr_3.fmt = list(dash="dot", width = 1, color="#333333")
interval.fmt = list(dash="dot", width = 1, color="grey")

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
    title = "Sentinel-1 VH backscatter (median of polygon area)",
    titlefont = f2,
    tickfont = f1,
    showticklabels = TRUE,
    exponentformat = "E",
    range = c(min(agro$median) - 3, max(agro$median) + 3)
)


# PLOTLY--------------------------------------------------------------------------

plt = plot_ly(data = agro,
              x = ~date,
              y = ~median,
              width = 600,
              height = 600) %>%

    layout(title = "",
           yaxis = y_axis,
           xaxis = x_axis,
           legend = list(font = f1, traceorder = "reversed", yanchor = "top",
                         xanchor = "right")) %>%

    add_ribbons(x = x_1,
                ymin = pr_losd_1$y,
                ymax = pr_upsd_1$y,
                color = I(blue_backgroud), line = list(width = 0), opacity = 0.4,
                name = "",
                showlegend = FALSE) %>%

    add_ribbons(x = x_2,
                ymin = pr_losd_2$y,
                ymax = pr_upsd_2$y,
                color = I(red_background), line = list(width = 0), opacity = 0.4,
                name = "",
                showlegend = FALSE) %>%

    add_lines(x = x_1, y = pr_1$y, line = pr_1.fmt, name = "Agriculture") %>% # median
    add_lines(x = x_2, y = pr_2$y, line = pr_2.fmt, name = "Slangbos increase site") %>%
    add_lines(x = x_3, y = pr_3$y, line = pr_3.fmt, name = "Slangbos burnt site")

plt
