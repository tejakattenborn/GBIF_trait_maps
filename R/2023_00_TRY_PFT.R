
require(dplyr)
require(data.table)

setwd("/net/data/iNaturalist/Tracheophyta")


dat = fread("0091819-210914110416597.csv", nrows = 100)
head(dat)

setwd("/net/home/swolf/iNaturalist/Data/TRY/Life_Forms")

#dat = fread("19233.txt", nrows = 300000)
dat = fread("19233.txt")
head(dat)

search_terms_tree = c("tree", "Tree", "TREE", "seedling", "hardwood", "softwood", "Hardwood", "Softwood", "Tree_Fern", "Tree_", "Small_Tree",
                      "trees/T/Tree", "Tree/Tree", "Tree V", "Tree VII", "Tree IX", "Tree V", "Tree IV", "Tree III", "Tree II", "Tree I", "Tree/Treelet",
                      "Treen", "T/Tree", "T/tree/Tree", "tree/Tree", "trees/Shrub", "trees/T/tree/Tree", "Tree/Shrub", "Trees", "trees", "Tree_Shrub",
                      "Shrub_Tree","Tree | Shrub", "Shrub | Tree", "Tree | Tree", "USforestTrees")

search_terms_grass = c("herb","Herb", "HERB", "herbs", "graminoid", "Graminoid","GRAMINOID", "Forb","forb", "Grasses&Sedges",
                     "Grass","grass", "GRASS", "sedge","SEDGE", "fern", "Fern", "FERN", "Grassland", "Annual Grass", "Perennial Grass", "grassland")

search_terms_shrub = c("shrub","Shrub", "SHRUB", "seedling","vine", "Vine", "VINE", "liana", "Liana", "LIANA", "Terrestrial_Shrub", "Shrub forest belt",
                       "Dwarf Shrub community", "Shrub/Aquatic", "Shrub/Aquatic", "Shrub/Parasite", "Shrubs", "Shrubland", "shrubland", "Shrub, Subshrub", "Shrub")


dat$OrigValueStr[dat$OrigValueStr %in% search_terms_tree] = "Tree"
dat$OrigValueStr[dat$OrigValueStr %in% search_terms_grass] = "Grass"
dat$OrigValueStr[dat$OrigValueStr %in% search_terms_shrub] = "Shrub"


# Subset the data frame to valid Tee, Srhub, Grass
characters_to_include <- c("Tree", "Shrub", "Grass")
#dat <- dat[grepl(paste(characters_to_include, collapse = "|"), dat$OrigValueStr), ]
dat <- dat[OrigValueStr %in% characters_to_include]
unique(dat$OrigValueStr)


# majority vote
dat_pft = data.frame(AccSpeciesID = unique(dat$AccSpeciesID), AccSpeciesName = unique(dat$AccSpeciesName), pft = rep(NA, length(unique(dat$AccSpeciesID))))
for(i in 1:nrow(dat_pft)){
  sspecies = dat[AccSpeciesID == dat_pft$AccSpeciesID[i]]
  freq_table <- table(sspecies$OrigValueStr)
  dat_pft$pft[i] <- sspecies$OrigValueStr[which.max(freq_table)]
  if (i %% 100 == 0) {
    print(paste0(round(i/nrow(dat_pft)*100), " % finished."))
  }
}


# check
dat_pft[grepl("Quercus", dat_pft$AccSpeciesName),]
dat_pft[grepl("Taraxacum", dat_pft$AccSpeciesName),]
dat_pft[grepl("Carex", dat_pft$AccSpeciesName),]
dat_pft[grepl("Ilex", dat_pft$AccSpeciesName),]
unique(dat_pft$pft)

# output
fwrite(dat_pft, file = "/net/home/tkattenborn/data_global_maps/try_pft_v1.csv", row.names = F)
