# calculating the interferometric pairs
# init: 15.10.2020, Konstantin Schelleberg

library(tidyverse)

source("D:/Geodaten/Master/projects/402slangbos/import.R")

# ------------------------------------------------------------------------------

df = summary$co$`1` %>%
    arrange(date)

df2 = df %>% mutate(diff = (date - lead(date, 1))) %>%
    dplyr::select(c(date, diff))

# round date
df2$diff = df2$diff %>% round()

# which have longer temporal baseline?
long_interval = df2 %>% filter(diff != -12)


# write out
write_excel_csv(df2, "F:/geodata/geo402/S1_SLC/xx_new/interferometric_pairs.csv", append = FALSE)
write_excel_csv(long_interval, "F:/geodata/geo402/S1_SLC/xx_new/interval_24d.csv", append = FALSE)

# write to drive
write_excel_csv(df2, "D:/Geodaten/#Jupiter/GEO402/06_plots/Interferometric pairs/interferometric_pairs.csv", append = FALSE)
write_excel_csv(long_interval, "D:/Geodaten/#Jupiter/GEO402/06_plots/Interferometric pairs/interval_24d.csv", append = FALSE)

