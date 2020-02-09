# making predction data set

source("import.R")
# prediction dataset

red = as.data.table.raster(red_small, xy = TRUE)
nir = as.data.table.raster(nir_small, xy = TRUE)
vh = as.data.table.raster(vh_small, xy = TRUE)

#################### FUNCTION HERE
# merge matrices, find out with cols are dublicates
dts3 = as_tibble(cbind(vh, red, nir), .name_repair = "unique")

# remove cols with x, y and class from the data frame, rename vars from the last binded data frame to x, y and class
# e.g. -vh_x, -vh_y, -class...356, -class...557, class = class...758, -red_x, -red_y, x = nir_x, y = nir_y
dts2 = dts3 %>%
    .[,3:(length(.))] %>%
    dplyr::select(-starts_with("x"), -starts_with("y")) %>%
    cbind(dts3[,1:2]) %>%
    dplyr::rename(x = starts_with("x"),
                  y = starts_with("y"))

# remove cols with NA (prerequisit for random forest input)
dts = dts2 %>%
    as.data.frame() %>%
    .[, colSums(is.na(.)) == 0] %>%
    as.data.table()

# number of variables:
length(names(dts))
class(dts)
