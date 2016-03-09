library(RUnit)
rm(list = ls())
source("utils.r")
test.poly_data_make = function()
{
  poly_data_make(sampling = "irregular") %>% head
  poly_data_make(sampling = "sparse")
  poly_data_make(sampling = "dense")
  poly_data_make(sampling = "thibaux")
}
test.poly_data_make()

track <- tracker(); ## initialize a tracking "object"
track$init(); ## initialize the tracker
sampling = "irregular"
resPolyDataMake <- inspect(poly_data_make(sampling = sampling), track=track); ## execute the test function and track
resTrack <- track$getTrackInfo(); ## get the result of Code Inspector (a list)
resTrack %>% head




