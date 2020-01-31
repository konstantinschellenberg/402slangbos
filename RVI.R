source("import.R")

library(terra)
library(RColorBrewer)
library(stars)
library(sf)

pal = brewer.pal(8, "RdYlGn")

a = s1vv[[1]]
b = s1vh[[2]]

ratio = a - b
ratio2 = b - a

ratio2 %>%
    st_as_stars() %>%
    plot()


df = data.frame(x = c(1.2, 1.4, 1.5),
                y = c(54.4, 54.2, 54.8))

df_new_col = df %>%
    mutate(z = c(3, 2, 5)) %>%
    cbind(.)

mutate(df, new_col = c(...))


    plot(col = pal)

plot(ratio)

ratio %>%
    rast() %>%
    plot()
