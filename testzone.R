library(raster)

f <- system.file("external/rlogo.grd", package="raster")
f

r1 = raster(f)
r1

r2 = raster(f, band=2)
r2
plot(r1)

b = brick(f)
plot(b)

writeRaster(b, "\\ignored\\output.tif")

# reading
pathname = "output.tif"

b = brick(pathname)
# writing
writeRaster(b, "\\ignored\\output_overwritten.tif", overwrite = TRUE)

# ------------------------------------------------------------------------------

#convert from dB to linear values, already done.
file = 10^(file/10)

# subsetting in two halves
half1 = subset(file, 1 : (n/2))

half2 = subset(file, (n/2+1) : n)

#save as data frame without spatial information
# df = as.data.frame(a) #xy=TRUE
# ------------------------------------------------------------------------------

library(zoo)

z <- read.zoo(text = Lines, header = TRUE, format = "%m/%d/%Y")
#
# ------------------------------------------------------------------------------

a = letters[1:5]

map_chr(a, ~ "z")
a %>%
    map(3)
