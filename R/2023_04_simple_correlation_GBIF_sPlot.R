



g <- brick("/net/home/tkattenborn/data_global_maps/trait_maps_gbif/Shrub_Tree/2deg/GBIF_TRYgapfilled_X78_2deg.gri")
g
plot(g)

s <- brick("/net/home/tkattenborn/data_global_maps/trait_maps_gbif/Shrub_Tree/2deg/sPlot_TRYgapfilled_X78_2deg.gri")
s
plot(s)


cor.test(getValues(g$mean), getValues(s$mean))
