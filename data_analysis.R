# Script to call already assigned functions

source("import_parse.R")



################################################################################
# Data digesting----------------------------------------------------------------
################################################################################

# s1_vv_summaries
vv = list_summaries(sentinel1_brick = s1vv,
                    polygon = roi)

# s1_vh_summaries
vh = list_summaries(sentinel1_brick = s1vh,
                    polygon = roi)

# debug: check if different
identical(vv$plot1_1$median,
          vh$plot1_1$median)

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

# 1st Raster
mycolour = terrain.colors(20)

# raster
plot(s1vv[[1]],
     breaks = c(-25:-5),
     col = mycolour,
     axes = TRUE
)

# ROI
plot(roi[1], main = "All ROIs", col = "red", add = TRUE)

# Plotly graphs-----------------------------------------------------------------

stelle = 1
code = 4

# get roi
example = roi %>%
    filter(roi$Name == code) %>%
    .[stelle, ]

plot(example[1], main = paste0(namer(code), ": ", code, "_", stelle), col = "grey")
plot(s1vh[[1]], add = F)
################ unbedingt besser coden!!

vv_grep = vv[grepl(paste0("plot", code, "_"), names(vv))]
# indexing
vv_fetched = vv_grep[[stelle]]
# remove na
vv_plot = na.omit(vv_fetched)
nrow(vv_plot)

vh_grep = vh[grepl(paste0("plot", code, "_"), names(vv))]
# indexing
vh_fetched = vh_grep[[stelle]]
# remove na
vh_plot = na.omit(vh_fetched)
nrow(vh_plot)

# ------------------------------------------------------------------------------

# format edits
data.fmt = list(color="#878787", width=1)
linevv.fmt = list(dash="solid", width = 1.5, color="red")
linevh.fmt = list(dash="solid", width = 1.5, color="blue")
# interval.fmt = list(dash="dot", width = 1, color="grey")


# save stats of vv and vh
x_vv = vv_plot$date
med_vv = vv_plot$median
losd_vv = vv_plot$lower_sd
upsd_vv = vv_plot$upper_sd

x_vh = vh_plot$date
med_vh = vh_plot$median
losd_vh = vh_plot$lower_sd
upsd_vh = vh_plot$upper_sd

# smoothing curve
pr_vv = supsmu(x_vv, med_vv)
pr_vh = supsmu(x_vh, med_vh)

# making smoothed line for standard deviation
# pr_losd = supsmu(x, losd)
# pr_upsd = supsmu(x, upsd)

# PLOTLY--------------------------------------------------------------------------

plt = plot_ly(data = vv_plot,
              x = ~date,
              y = ~median,
              type = 'scatter',
              line = data.fmt,
              mode = "lines")
# name = paste(codename))

plt = plotly::layout(plt, title = paste0("Median Backscatter for site no. ", code," ", "_", stelle, ": ", namer(code)),
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

