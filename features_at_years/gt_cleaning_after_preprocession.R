# splitting the gt data into years


setwd("D:/Geodaten/Projects/402slangbos")
source("import_samples.R")
source("functions.R")

library(mapview)

# all available classes mapped so far:
cgt[c("class", "new_class")] %>% st_set_geometry(NULL) %>% unique()

# for classification keep
# shrubs (ehem. continuous)
# grassland
# cultivated
#
# bare
# woodland
# water
# urban


mapview(cgt)

cgt %>% filter(class == "11") %>% st_geometry() %>% plot(main = c("slangbos_increase_oldfield"))
cgt %>% filter(class == "12") %>% st_geometry() %>% plot(main = "slangbos_increase_grassland     ")
cgt %>% filter(class == "21") %>% st_geometry() %>% plot(main = "slangbos_continuous_oldfield    ")
cgt %>% filter(class == "32") %>% st_geometry() %>% plot(main = "slangbos_continuous_grassland   ")

library(stars)
a = read_stars("F:/geodata/geo402/S2/xx_raws/S2A_MSIL1C_20160108T075312_N0201_R135_T35JNH_20160108T081418_atm_20m.tif")
plot(a)
