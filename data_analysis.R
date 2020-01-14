# Script to call already assigned functions

source("import_parse.R")

library(viridis)


################################################################################
# Data digesting----------------------------------------------------------------
################################################################################

# s1_vv_summaries
vv = list_summaries(sentinel1_brick = s1vv,
                    polygon = roi)
vh = list_summaries(sentinel1_brick = s1vh,
                    polygon = roi)

# debug: check if different
if(!identical(vv$plot1_1$median,
          vh$plot1_1$median)){
    warning("the input raster are the same")
}

################################################################################
# Calculations------------------------------------------------------------------
################################################################################
# UNDER CONSTRUCTION
class(vv)
class(vv$plot1_1)
class(vv$plot1_1$mean)


# initialise empty dataframe, nrow
# empty = data.frame(matrix(NA, nrow = length(sb_1$plot1_1$date)))



# Function definition-----------------------------------------------------------
summarise_roi = function(polarisation, category = "1", argument = "median"){

    # function to summaries the data with the given argument (must be a col name from carve_brick. E.g mean, median, sd)
    # http://www.endmemo.com/program/R/grepl.php ## regular expression syntax
    roi_vv = polarisation[grepl(paste0("plot", category, "_"), namer(polarisation))]

    ####

    return(roi_vv)
}
summarise_roi(polarisation = vv,
              category = 2,
              argument = "median")

# ------------------------------------------------------------------------------

# medians transmuted
# sb_1 = vv[grepl(paste0("plot", 1, "_"), names(vv))]
#
#
#
# n = sb_1 %>%
#     map_dfr("median") %>%
#     mutate(date = datenames) %>%
#     group_by(date)
#
# n_2 = n %>%
#     select(plot1_1, plot1_2) %>%
#     summarise_all(mean)
# n_2
#
#     summarise_at(vars(1:5), mean, rm.na = TRUE)
#
#
# n_2
#
# n[grepl("plot", names(n))]
#
# plot(t(n))
# mutate(n, . ~mean)

# parse datenames

# map(increase, mean)

################################################################################
# Plotting----------------------------------------------------------------------
################################################################################

# raster
plot(s1vv[[1]],
     breaks = c(-25:-5),
     col = mycolour,
     axes = TRUE
)

# ROI
plot(roi[1], main = "All ROIs", col = "red", add = TRUE)

### init for plotly-------------------------------------------------------------
# overview plots-----------------------------------------------------------------

stelle = 1
code = 1

# get roi
example = roi %>%
    filter(roi$Name == code) %>%
    .[stelle, ]

plot(example[1], main = paste0(namer(code), ": ", code, "_", stelle), col = "grey")

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

identical(nrow(agro),nrow(incr))
identical(nrow(agro),nrow(brk))

# x_vv = vv_plot$date
# med_vv = vv_plot$median
# losd_vv = vv_plot$lower_sd
# upsd_vv = vv_plot$upper_sd
#
# x_vh = vh_plot$date
# med_vh = vh_plot$median
# losd_vh = vh_plot$lower_sd
# upsd_vh = vh_plot$upper_sd

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
    # add_ribbons(x = x_3,
    #             ymin = pr_losd_3$y,
    #             ymax = pr_upsd_3$y,
    #             color = I(black_background), line = list(width = 0), opacity = 0.4,
    #             name = "") %>%


    add_lines(x = x_1, y = pr_1$y, line = pr_1.fmt, name = "Agriculture") %>% # median
    add_lines(x = x_2, y = pr_2$y, line = pr_2.fmt, name = "Slangbos site") %>%
    add_lines(x = x_3, y = pr_3$y, line = pr_3.fmt, name = "Slangbos burnt site")

plt


# plt = add_lines(plt, x = x, y = pr_losd$y, line = interval.fmt, name = "Lower standard deviation")
# plt = add_lines(plt, x = x, y = pr_upsd$y, line = interval.fmt, name = "Upper standard deviation")


#####################OLD add vh


#
# add_lines(x = x_1, y = med_1, line = line_1.fmt, name = "agro") %>% # main data
# add_lines(x = x_2, y = med_2, line = line_2.fmt, name = "incr") %>%
#
# , # main data
# type = 'scatter',
# line = data.fmt,
# mode = "lines"
#
#
# plt = add_ribbons(plt, x = x_1, ymin = losd_1, ymax = upsd_1,
#                   color = I("grey80"), line = list(width = 0), opacity = 0.9,
#                   name = "VV 1 sigma standard deviation")
# plt = add_lines(plt,
#                 x = x_vh,
#                 y = med_vh,
#                 name = "VH")
#
# plt = add_lines(plt,
#                 x = x_vh,
#                 y = pr_vh$y,
#                 line = linevh.fmt,
#                 name = "Smooth VH")
#
# plt = add_ribbons(plt, x = x_vh, ymin = losd_vh, ymax = upsd_vh,
#                   color = I("grey80"), line = list(width = 0), opacity = 0.9,
#                   name = "VH 1 sigma standard deviation")
#
#
# plt
#
# # exporting graphics to dst

# plotly::orca(plt, "D:\\Geodaten\\#Jupiter\\GEO402\\work progress\\plotly")

# plt = add_lines(plt, # median line for 1
#                 x = x_1,
#                 y = pr_1$y,
#                 line = line_1.fmt,
#                 name = "Agriculture",
# )


