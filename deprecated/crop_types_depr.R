#####################
# deprecated
# ------------------------------------------------------------------------------
# OKAY, aber nicht alle plots dri
# Immer left-join auf den ersten Datensatz

c12 = st_join(crops[[1]], crops[[2]], join = st_intersects, suffix = c("_1415", "_1516"))
print(c12, n = 100)
nrow(c12)

c123 = st_join(c12, crops[[3]], join = st_intersects, suffix = c("_1617"))
list(names = names(c123),
     rows = nrow(c123),
     print(c123, n = 50))

plot(c123)

crop_boundaries %>%
    filter(st_intersection(., crops[[1]], sparse = F)[1,])

i = st_intersection(crop_boundaries, crops[[1]])
i[i == F]

# ------------------------------------------------------------------------------
# full join non-spatial = fail
crops2 = map(crops, function(x) as.data.frame(x))

c12_full = full_join(crops2[[1]], crops2[[2]], by = "geom") %>%
    st_as_sf()
list(names = names(c12_full),
     rows = nrow(c12_full),
     print(c12_full, n = 50))

i = st_intersection(c12_full)

plot(c12_full)
