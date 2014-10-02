library(XML)
library(OpenStreetMap)
library(lubridate)
library(raster)
library(RgoogleMaps)

shift.vec <- function (vec, shift) {
  if(length(vec) <= abs(shift)) {
    rep(NA ,length(vec))
  }else{
    if (shift >= 0) {
      c(rep(NA, shift), vec[1:(length(vec)-shift)]) }
    else {
      c(vec[(abs(shift)+1):length(vec)], rep(NA, abs(shift))) } } }

# Parse the GPX file
pfile <- htmlTreeParse("run.gpx",
                       error = function (...) {}, useInternalNodes = T)

# Get all elevations, times and coordinates via the respective xpath
elevations <- as.numeric(xpathSApply(pfile, path = "//trkpt/ele", xmlValue))
times <- xpathSApply(pfile, path = "//trkpt/time", xmlValue)
coords <- xpathSApply(pfile, path = "//trkpt", xmlAttrs)
# Extract latitude and longitude from the coordinates
lats <- as.numeric(coords["lat",])
lons <- as.numeric(coords["lon",])
# Put everything in a dataframe and get rid of old variables
geodf <- data.frame(lat = lats, lon = lons, ele = elevations, time = times)
rm(list=c("elevations", "lats", "lons", "pfile", "times", "coords"))
head(geodf)

# Shift vectors for lat and lon so that each row also contains the next position.
geodf$lat.p1 <- shift.vec(geodf$lat, -1)
geodf$lon.p1 <- shift.vec(geodf$lon, -1)
# Calculate distances (in metres) using the function pointDistance from the 'raster' package.
# Parameter 'lonlat' has to be TRUE!
geodf$dist.to.prev <- apply(geodf, 1, FUN = function (row) {
  pointDistance(c(as.numeric(row["lat.p1"]),
                  as.numeric(row["lon.p1"])),
                c(as.numeric(row["lat"]), as.numeric(row["lon"])),
                lonlat = T)
})
# Transform the column 'time' so that R knows how to interpret it.
geodf$time <- strptime(geodf$time, format = "%Y-%m-%dT%H:%M:%OS")
# Shift the time vector, too.
geodf$time.p1 <- shift.vec(geodf$time, -1)
# Calculate the number of seconds between two positions.
geodf$time.diff.to.prev <- as.numeric(difftime(geodf$time.p1, geodf$time))
# Calculate metres per seconds, kilometres per hour and two LOWESS smoothers to get rid of some noise.
geodf$speed.m.per.sec <- geodf$dist.to.prev / geodf$time.diff.to.prev
geodf$speed.km.per.h <- geodf$speed.m.per.sec * 3.6
geodf$speed.km.per.h <- ifelse(is.na(geodf$speed.km.per.h), 0, geodf$speed.km.per.h)
geodf$lowess.speed <- lowess(geodf$speed.km.per.h, f = 0.2)$y
geodf$lowess.ele <- lowess(geodf$ele, f = 0.2)$y

#Now, let's plot all the stuff!

# Plot elevations and smoother
plot(geodf$ele, type = "l", bty = "n", xaxt = "n", ylab = "Elevation", xlab = "", col = "grey40")
lines(geodf$lowess.ele, col = "red", lwd = 3)
legend(x="bottomright", legend = c("GPS elevation", "LOWESS elevation"),
       col = c("grey40", "red"), lwd = c(1,3), bty = "n")

# Plot speeds and smoother
plot(geodf$speed.km.per.h, type = "l", bty = "n", xaxt = "n", ylab = "Speed (km/h)", xlab = "",
     col = "grey40")
lines(geodf$lowess.speed, col = "blue", lwd = 3)
legend(x="bottom", legend = c("GPS speed", "LOWESS speed"),
       col = c("grey40", "blue"), lwd = c(1,3), bty = "n")
abline(h = mean(geodf$speed.km.per.h), lty = 2, col = "blue")

# Plot the track without any map, the shape of the track is already visible.
#plot(rev(geodf$lon), rev(geodf$lat), type = "l", col = "red", lwd = 3, bty = "n", ylab = "Latitude", xlab = "Longitude")

lon<- rev(geodf$lon)
lat<-rev(geodf$lat)
center = c(mean(lat), mean(lon));
zoom <- min(MaxZoom(range(lat), range(lon)));
MyMap <- GetMap(center=center, zoom=zoom);
PlotOnStaticMap(MyMap)
PlotOnStaticMap(MyMap, lat, lon,  lwd = 5, col="red", FUN = lines, add=TRUE)

