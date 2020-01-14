# Script to call already assigned functions

source("import_parse.R")



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
    roi_vv = polarisation[grepl(paste0("plot", category, "_"), names(polarisation))]

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



mycolour = terrain.colors(20)

# raster
plot(s1vv[[1]],
     breaks = c(-25:-5),
     col = mycolour,
     axes = TRUE
)

# ROI
plot(roi[1], main = "All ROIs", col = "red", add = TRUE)

### init for plotly

# overview plots-----------------------------------------------------------------

stelle = 1
code = 1

# get roi
example = roi %>%
    filter(roi$Name == code) %>%
    .[stelle, ]

plot(example[1], main = paste0(names(code), ": ", code, "_", stelle), col = "grey")

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
agro = grep(vh, 1, code = 4)
incr = grep(vh, 1, code = 1)

# ------------------------------------------------------------------------------

# format edits
data.fmt = list(color="#878787", width=1)
linevv.fmt = list(dash="solid", width = 1.5, color="red")
linevh.fmt = list(dash="solid", width = 1.5, color="blue")
interval.fmt = list(dash="dot", width = 1, color="grey")

# save stats of vv and vh

x_1 = agro$date
med_1 = agro$median
upsd_1 = agro$lower_sd
losd_1 = agro$upper_sd

x_2 = incr$date
med_2 = incr$median
upsd_2 = incr$lower_sd
losd_2 = incr$upper_sd

# x_vv = vv_plot$date
# med_vv = vv_plot$median
# losd_vv = vv_plot$lower_sd
# upsd_vv = vv_plot$upper_sd
#
# x_vh = vh_plot$date
# med_vh = vh_plot$median
# losd_vh = vh_plot$lower_sd
# upsd_vh = vh_plot$upper_sd

pr_1 = supsmu(x_1, med_1) # smoothing curve
pr_2 = supsmu(x_2, med_2)
pr_losd = supsmu(x_1, losd_1) # making smoothed line for standard deviation
pr_upsd = supsmu(x_2, upsd_2)

# PLOTLY--------------------------------------------------------------------------

plt = plot_ly(data = agro,
              x = ~date,
              y = ~median,
              type = 'scatter',
              line = data.fmt,
              mode = "lines")
# name = paste(codename))

plt = plotly::layout(plt, title = paste0()
                     yaxis = list(range = c(-27, -8)))

plt = add_lines(plt,
                x = x_vv,
                y = pr_vv$y,
                line = linevv.fmt,
                name = "Smooth VV",
)

# plt = add_lines(plt, x = x, y = pr_losd$y, line = interval.fmt, name = "Lower standard deviation")
# plt = add_lines(plt, x = x, y = pr_upsd$y, line = interval.fmt, name = "Upper standard deviation")
plt = add_ribbons(plt, x = x_vv, ymin = losd_vv, ymax = upsd_vv,
                  color = I("grey80"), line = list(width = 0), opacity = 0.9,
                  name = "VV 1 sigma standard deviation")

## add vh
plt = add_lines(plt,
                x = x_vh,
                y = med_vh,
                name = "VH")

plt = add_lines(plt,
                x = x_vh,
                y = pr_vh$y,
                line = linevh.fmt,
                name = "Smooth VH")

plt = add_ribbons(plt, x = x_vh, ymin = losd_vh, ymax = upsd_vh,
                  color = I("grey80"), line = list(width = 0), opacity = 0.9,
                  name = "VH 1 sigma standard deviation")


print(plt)

# exporting graphics to dst

# plotly::orca(plt, "D:\\Geodaten\\#Jupiter\\GEO402\\work progress\\plotly")

