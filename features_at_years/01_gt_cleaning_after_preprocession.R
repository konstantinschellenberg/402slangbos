# splitting the gt data into years


# setwd("D:/Geodaten/Projects/402slangbos")
source("import_samples.R")
source("functions.R")

library(mapview)

#' for classification keep
#' 1: Slangbos
#' 2: Pasture
#' 3: Grassland
#'
#' 4: Cultivated
#' 5: Bare Soil
#' 6: Woodland
#' 7: Water
#' 8: Urban

# IMPORT SAMPLES WITH ALL CLASSES ----------------------------------------------

cgt = st_read("D:/Geodaten/GEO402/02_features/features.gpkg", layer = "LADYBRAND_gt_stats_full_simple")

# all available classes mapped so far:
cgt[c("class", "new_class")] %>% st_set_geometry(NULL) %>% unique() %>% arrange(class)

# basic plotting
mapview(cgt)

cgt %>% filter(class == "11") %>% st_geometry() %>% plot(main = c("slangbos_increase_oldfield"))
cgt %>% filter(class == "12") %>% st_geometry() %>% plot(main = "slangbos_increase_grassland     ")
cgt %>% filter(class == "21") %>% st_geometry() %>% plot(main = "slangbos_continuous_oldfield    ")
cgt %>% filter(class == "32") %>% st_geometry() %>% plot(main = "slangbos_continuous_grassland   ")

# PREPROCESSING CGT ------------------------------------------------------------

# selecting 15th of month as dummy day
df = cgt

df$class = as.numeric(df$class)
df$break_date = df$break_date %>%
    paste0("15", .) %>%
    as.Date(format = "%d%m%Y")



 # FILTERING --------------------------------------------------------------------

break.date.after = as.Date("15-10-2018", format = "%d-%m-%Y")
break.date.before = as.Date("15-10-2017", format = "%d-%m-%Y")

df %>% filter(class == 22)
df$break_date %>% unique()
df$break_date[df$break_date >= break.date.after] %>% unique()

# 21 & 22 -> 1 (Slangbos)
# 31 & 32 after 15.10.2018 -> 1 (Slangbos)
# 32 break before 15.10.2017 -> 2 (Pasture)
# 31 break before 15.10.2017 -> 4 (Cultivated)
df.mut = df %>%
    mutate(classif = case_when(class == 21 | class == 22 ~ 1,
                               class == 32 & break_date > break.date.after ~ 1,
                               class == 31 & break_date > break.date.after ~ 1,
                               class == 32 & break_date < break.date.before ~ 2,
                               class == 31 & break_date < break.date.before ~ 4,
                               class == 4 ~ 3,
                               class == 5 ~ 4,
                               class == 6 ~ 5,
                               class == 7 ~ 6,
                               class == 8 ~ 7,
                               class == 9 ~ 8)) %>%
    mutate_at("classif", factor)


# deleting others
df.mut = filter(df.mut, !is.na(classif))
mapview(df.mut)

names(df.mut)
df.mut$classif %>% unique()

df.out = df.mut %>% dplyr::select(c(class, break_date, classif)) %>% print(n = 50)

g1 = ggplot(df.out) +
    geom_bar(aes(classif)) +
    theme_light()
ggsave("Verteilung_Sampling_Sites.png", plot = g1, path = "D:/Geodaten/GEO402/06_plots/Classification/Samples", device = "png")

# st_write(df.out, "D:/Geodaten/GEO402/02_features/classif.gpkg", layer = "pre_mapping", append = FALSE)

# end()
