

require(data.table)

#define PFT:
pfts = c("Shrub", "Tree")
#pfts = c("Shrub", "Tree", "Grass")
#pfts = c("Grass")



species = fread("/net/home/tkattenborn/data_global_maps/sPlotOpen/sPlotOpen_DT(1).txt", sep = "\t")
head(species)


check_abun <- species[, sum(Relative_cover), by = PlotObservationID]

max(check_abun$V1)
min(check_abun$V1)


TRY <- fread("/net/home/tkattenborn/data_global_maps/TRY_5_gapfilled/TRY_50_2020_01/gapfilled_data/species_means.csv")
TRY_pft <- fread("/net/home/tkattenborn/data_global_maps/try_pft_v1.csv")
TRY_pft <- TRY_pft[TRY_pft$pft %in% pfts]
TRY <- TRY[TRY$Species %in% TRY_pft$AccSpeciesName]


sPlot_TRY_1 <- merge(species, TRY, by.x = "Species", by.y = "Species", all.x = TRUE)
length(which(complete.cases(sPlot_TRY_1)))/nrow(species)

length(which(complete.cases(sPlot_TRY_1)==FALSE))
length(which(complete.cases(sPlot_TRY_1)==TRUE))



sPlot_TRY_2 <- merge(species[which(complete.cases(sPlot_TRY_1) == FALSE),], TRY, by.x = "Original_species", by.y = "Species", all.x = FALSE)
#sPlot_TRY_2 <- merge(species[which(is.na(sPlot_TRY_1$Species)),], TRY, by.x = "Original_species", by.y = "Species", all.x = FALSE)
sPlot_TRY_1 <- sPlot_TRY_1[-which(complete.cases(sPlot_TRY_1) == FALSE),] # remove not-merged for the moment

sPlot_TRY <- rbind(sPlot_TRY_1, sPlot_TRY_2)
length(which(complete.cases(sPlot_TRY)))/nrow(species)

length(unique(sPlot_TRY$PlotObservationID))
length(unique(species$PlotObservationID))

sPlot_TRY[sPlot_TRY == 0] <- NA
0.0 %in% unique(sPlot_TRY$Relative_cover)

sum(duplicated(sPlot_TRY))
sPlot_TRY <- unique(sPlot_TRY)
nrow(sPlot_TRY)/nrow(species)


sPlot = fread("/net/home/tkattenborn/data_global_maps/sPlotOpen/sPlotOpen_header(2).txt", sep= "\t")
colnames(sPlot)
loc <- sPlot[, c("PlotObservationID", "Releve_area", "Latitude", "Longitude")]

sPlot_TRY <- merge(sPlot_TRY, loc, by = "PlotObservationID", all.x = FALSE)


fwrite(sPlot_TRY, file = paste0("/net/home/tkattenborn/data_global_maps/sPlotOpen_TRYgapfilled", paste0(pfts, collapse = "_"),".csv"))
sPlot_TRY <- fread(file = paste0("/net/home/tkattenborn/data_global_maps/sPlotOpen_TRYgapfilled", paste0(pfts, collapse = "_"),".csv"))


# Group the data.table by the plot number

# Select columns for weighted mean calculation
selected_columns <- colnames(sPlot_TRY)[which(grepl("X", colnames(sPlot_TRY)))]

# Calculate the weighted mean for each selected column grouped by Category
weighted_means <- sPlot_TRY[, lapply(.SD, function(x) weighted.mean(x, w = Relative_cover)),by = PlotObservationID, .SDcols = selected_columns]
head(weighted_means)

sPlot_cwm <- merge(loc, weighted_means, by = "PlotObservationID")
fwrite(sPlot_cwm, paste0("/net/home/tkattenborn/data_global_maps/sPlotOpen_TRYgapfilled_cwm", paste0(pfts, collapse = "_"), ".csv"), row.names = FALSE)
