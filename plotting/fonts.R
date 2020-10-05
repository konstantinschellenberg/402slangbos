# fonts and formats for data plotting

mycolour = (20)

blue_background = "#6ec3f7"
red_background = "#fc8d59"
green_background = "#007f00"
grey_background = "#909090"
orange_background = "#eb9c4d"

# Format edits
data.fmt = list(color="#878787", width=1)
line_1.fmt = list(dash="solid", width = 0.5, color="#b6e1fb") # blue
pr_1.fmt = list(dash="solid", width = 1.5, color="#2c6487")
line_2.fmt = list(dash="solid", width = 0.5, color="#fc8d59") # red
pr_2.fmt = list(dash="solid", width = 1.5, color="#fa5305")
line_3.fmt = list(dash="solid", width = 0.5, color="#007f00")
pr_3.fmt = list(dash="solid", width = 1, color="#007f00")
interval.fmt = list(dash="solid", width = 1, color="grey")

vh.fmt = list(dash="solid", width = 2, color="#2c6487") # blue
vv.fmt = list(dash="solid", width = 2, color="#007f00") # green
red.fmt = list(dash="solid", width = 2, color="#fc8d59") # red
red2.fmt = list(dash="solid", width = 1, color="#fc8d59") # red
nir.fmt = list(dash="solid", width = 2, color="#007f00") # green
nir2.fmt = list(dash="solid", width = 1, color="#007f00") # green
ndvi.fmt = list(dash = "do", width = 2, color = "red")
black.fmt = list(dash = "solid", width = 2, color = "black")
classes.fmt = map(c(increase = "#ff0040", continuous = "#fd8d3c", clearing = "black",
                         grass = "#009933", agro = "#bf8040"), ~ list(dash="solid", width = 2, color = .x))
classes.fmt$grass$dash = "dash"; classes.fmt$agro$dash = "dash"

vh.big.fmt = list(dash="solid", width = 3, color="#2c6487") # blue
red.big.fmt = list(dash="solid", width = 3, color="#fc8d59") # red

vh.fmt.slim = list(dash="solid", width = 0.3, color="#2c6487") # blue
red.fmt.slim = list(dash="solid", width = 0.7, color="#fc8d59") # red
nir.fmt.slim = list(dash="solid", width = 0.7, color="#007f00") # green
ndvi.fmt.slim = list(dash = "do", width = 0.7, color = "red")
black.fmt.slim = list(dash = "solid", width = 0.3, color = "black")
classes.fmt.slim = map(c(increase = "#ff0040", continuous = "#fd8d3c", clearing = "black",
                         grass = "#009933", agro = "#bf8040"), ~ list(dash="solid", width = 1, color = .x))
# classes.fmt.slim$grass$dash = "dash"; classes.fmt.slim$agro$dash = "dash"
# Fonts ------------------------------------------------------------------------
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

# bigger: for papers to be able to read still
f3 = list(
    family = "Times New Roman",
    size = 18,
    color = "black"
)

f4= list(
    family = "Times New Roman",
    size = 22,
    color = "black"
)
f5 <- list(
    family = "Arial, sans-serif",
    size = 24,
    color = "grey"
)

f6 <- list(
    family = "Arial, sans-serif",
    size = 27,
    color = "grey"
)

# X-Axis -----------------------------------------------------------------------
date.axis <- list(
    type = "date",
    title = "Date",
    titlefont = f2,
    tickfont = f1,
    anchor = "free",
    position = 0)

date.axis.big <- list(
    type = "date",
    title = "Date",
    titlefont = list(
        family = "Arial, sans-serif",
        size = 28,
        color = "grey"
    ),
    tickfont = f5,
    anchor = "free",
    position = 0)

# Y-Axes------------------------------------------------------------------------

y.s1 <- list(
    title = list(text = "S1 VH backscatter [dB]", standoff = 50),
    titlefont = f2,
    tickfont = f1,
    showline = F,
    showgrid = FALSE,
    anchor = "free",
    position = 0)

y.s1.big <- list(
    title = list(text = "S1 VH backscatter [dB]", standoff = 50),
    titlefont = list(
        family = "Arial, sans-serif",
        size = 28,
        color = vh.big.fmt$color
    ),
    tickfont = f5,
    showline = F,
    showgrid = FALSE,
    anchor = "free",
    position = 0)

# c(y.s1, range = list(c(-99L, 0L)))

y.s1.vv <- list(
    title = list(text = "S1 VV backscatter [dB]", standoff = 50),
    titlefont = f2,
    tickfont = f1,
    showline = F,
    showgrid = FALSE,
    anchor = "free",
    position = 0)

y.s1.general <- list(
    title = list(text = "Backscatter [dB]", standoff = 30),
    titlefont = f2,
    tickfont = f1,
    showline = F,
    showgrid = FALSE,
    anchor = "free",
    position = 0)

y.co <- list(
    title = "Coherence",
    titlefont = f2,
    tickfont = f1,
    showticklabels = TRUE,
    exponentformat = "E",
    range = c(0, 1),
    zeroline = FALSE)

y.s2 <- list(
    title = "reflectance",
    titlefont = f2,
    tickfont = f1,
    showticklabels = TRUE,
    exponentformat = "E")

# Y-Axes right -----------------------------------------------------------------

y.co_2 = list(
    title = list(text = "S1 VV coherence", standoff = 50),
    tickfont = f1,
    titlefont = f2,
    overlaying = "y",
    showline = F,
    side = "right",
    range = c(0,1),
    zeroline = FALSE)

y.co_2.big = list(
    title = list(text = "S1 VV coherence", standoff = 50),
    tickfont = f5,
    titlefont = list(
        family = "Arial, sans-serif",
        size = 28,
        color = red.big.fmt$color
    ),
    overlaying = "y",
    showline = F,
    side = "right",
    range = c(0,1),
    zeroline = FALSE)

y.s1.vv <- list(
    title = list(text = "S-1 VV backscatter [db]", standoff = 80),
    titlefont = f2,
    tickfont = f1,
    showticklabels = TRUE,
    side = "right",
    exponentformat = "E",
    overlaying = "y",
    anchor = "x",
    position = 0.5)

y.s2_2 <- list(
    title = "NDVI",
    titlefont = f2,
    tickfont = f1,
    showline = F,
    overlaying = "y",
    side = "right",
    range = c(0,1),
    anchor = "free",
    position = 1)

y.indizes <- list(
    titlefont = f2,
    tickfont = f1,
    showline = F,
    overlaying = "y",
    side = "right",
    anchor = "free",
    position = 1)

# LEGENDS ----------------------------------------------------------------------

# plot_ly() %>% layout(xaxis = x, yaxis = y.s1, yaxis2 = y.co_2,
#        yaxis3 = y.s2_2, # NDVI Axis
#        legend = list(font = f1, orientation = "h", xanchor = "center", yanchor = "bottom", y = -0.7, x = 0.5),
#        margin = list(pad = 80, b = 200, l = 0, r = 200, automargin = TRUE),
#        title = title,
#        images = list(source = paste0("hiwi/", b, "/map_", b, "-", i, ".png")))

# for LaTeX text in labels
# plot_ly(x = c(1, 2, 3, 4), y = c(1, 4, 9, 16)) %>%
#     layout(title = TeX("\\text{Some mathjax: }\\alpha+\\beta x")) %>%
#     config(mathjax = "cdn")
