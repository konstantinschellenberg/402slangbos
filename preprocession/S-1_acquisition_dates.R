# Sentinel-1 Acquisition Dates:


# Import date Sentinel-1 Acquisitions

S1_acquisition_dates = n.vh %>%
    read_csv() %>%
    .$x %>%
    substring(first = 4) %>%
    as.Date(format = "%Y.%m.%d")

df_S1_acquisition_dates = S1_acquisition_dates %>%
    as.data.frame() %>%
    `names<-`("acqu_date_S1")

names(S1_acquisition_dates) = "acqu_date_S-1"

# to Zhenyu!

write_csv(df_S1_acquisition_dates, path = "F:/geodata/geo402/S1_GRD/xx_new/dates.txt", append = FALSE)
