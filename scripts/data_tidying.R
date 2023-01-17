##Data tyding R Script ###
#issues with data tidying export in rmd)

#Jack A .Goldman

#required packages
library(tidyverse)
library(sf)


##


#SDD
pathIn <- "C:/Users/jackg/Documents/PhD/Data/SDDperFire/SDDperFire_raw/"
pathOut <- "C:/Users/jackg/Documents/PhD/Data/SDDperFire/SDDperFire_clipped/"
list1 <- list.files(pathIn, pattern = glob2rx("*SDD.tif"))
fa = fa %>% 
  add_column(SDD = "SDD",.after = "raster_id") %>%
  unite("sdd_id", c('Fire_ID', 'SDD'), sep = "_", remove = FALSE) %>% 
  filter(Fire_Year > 2001)

#make sure to coerce as sp object again
shpFires <- st_as_sf(fa)

setwd(pathOut)

for (i in 1:length(list1)){
  
  #read in your raster
  ras1 <- raster::raster(paste0(pathIn,list1[i]))
  fireID1 <- unlist(strsplit(list1[i],"[.]"))[1]
  shpFires1 <- shpFires[shpFires$sdd_id == fireID1,] #make sure its raster_id2
  crop1 = raster::crop(ras1, shpFires1)
  mask1 = raster::mask(crop1, shpFires1)
  outFile <- paste0(fireID1 , '.tif') 
  raster::writeRaster(mask1, outFile, overwrite = TRUE  )
  rm(ras1,fireID1,shpFires1,crop1,outFile)
}

#SCF
pathIn <- "C:/Users/jackg/Documents/PhD/Data/SCFperFire/SCFperFire_raw/"
pathOut <- "C:/Users/jackg/Documents/PhD/Data/SCFperFire/SCFperFire_clipped/"
list1 <- list.files(pathIn, pattern = glob2rx("*SCF.tif"))
fa = fa %>% 
  add_column(SCF = "SCF",.after = "raster_id") %>%
  unite("scf_id", c('Fire_ID', 'SCF'), sep = "_", remove = FALSE) %>% 
  filter(Fire_Year > 2001)

#make sure to coerce as sp object again
shpFires <- st_as_sf(fa)

setwd(pathOut)

for (i in 1:length(list1)){
  
  #read in your raster
  ras1 <- raster::raster(paste0(pathIn,list1[i]))
  fireID1 <- unlist(strsplit(list1[i],"[.]"))[1]
  shpFires1 <- shpFires[shpFires$scf_id == fireID1,] #make sure its raster_id2
  crop1 = raster::crop(ras1, shpFires1)
  mask1 = raster::mask(crop1, shpFires1)
  outFile <- paste0(fireID1 , '.tif') 
  raster::writeRaster(mask1, outFile, overwrite = TRUE  )
  rm(ras1,fireID1,shpFires1,crop1,outFile)
}

#FC
pathIn <- "C:/Users/jackg/Documents/PhD/Data/FCperFire/FCperFire_raw/"
pathOut <- "C:/Users/jackg/Documents/PhD/Data/FCperFire/FCperFire_clipped/"
list1 <- list.files(pathIn, pattern = glob2rx("*FC.tif"))
fa = fa %>% 
  add_column(FC = "FC",.after = "raster_id") %>%
  unite("FC_id", c('Fire_ID', 'FC'), sep = "_", remove = FALSE) %>% 
  filter(Fire_Year > 2001)

#make sure to coerce as sp object again
shpFires <- st_as_sf(fa)

setwd(pathOut)

for (i in 1:length(list1)){
  
  #read in your raster
  ras1 <- raster::raster(paste0(pathIn,list1[i]))
  fireID1 <- unlist(strsplit(list1[i],"[.]"))[1]
  shpFires1 <- shpFires[shpFires$FC_id == fireID1,] #make sure its raster_id2
  crop1 = raster::crop(ras1, shpFires1)
  mask1 = raster::mask(crop1, shpFires1)
  outFile <- paste0(fireID1 , '.tif') 
  raster::writeRaster(mask1, outFile, overwrite = TRUE  )
  rm(ras1,fireID1,shpFires1,crop1,outFile)
}

#PFC
pathIn <- "C:/Users/jackg/Documents/PhD/Data/PFCperFire/PFCperFire_raw/"
pathOut <- "C:/Users/jackg/Documents/PhD/Data/PFCperFire/PFCperFire_clipped/"
list1 <- list.files(pathIn, pattern = glob2rx("*PFC.tif"))
fa = fa %>% 
  add_column(PFC = "PFC",.after = "raster_id") %>%
  unite("PFC_id", c('Fire_ID', 'PFC'), sep = "_", remove = FALSE) %>% 
  filter(Fire_Year > 2001)

#make sure to coerce as sp object again
shpFires <- st_as_sf(fa)

setwd(pathOut)

for (i in 1:length(list1)){
  
  #read in your raster
  ras1 <- raster::raster(paste0(pathIn,list1[i]))
  fireID1 <- unlist(strsplit(list1[i],"[.]"))[1]
  shpFires1 <- shpFires[shpFires$PFC_id == fireID1,] #make sure its raster_id2
  crop1 = raster::crop(ras1, shpFires1)
  mask1 = raster::mask(crop1, shpFires1)
  outFile <- paste0(fireID1 , '.tif') 
  raster::writeRaster(mask1, outFile, overwrite = TRUE  )
  rm(ras1,fireID1,shpFires1,crop1,outFile)
}

#NDMI
pathIn <- "C:/Users/jackg/Documents/PhD/Data/NDMIperFire/NDMIperFire_raw/"
pathOut <- "C:/Users/jackg/Documents/PhD/Data/NDMIperFire/NDMIperFire_clipped/"
list1 <- list.files(pathIn, pattern = glob2rx("*NDMI.tif"))
fa = fa %>% 
  add_column(NDMI = "NDMI",.after = "raster_id") %>%
  unite("NDMI_id", c('Fire_ID', 'NDMI'), sep = "_", remove = FALSE) %>% 
  filter(Fire_Year > 2001)

#make sure to coerce as sp object again
shpFires <- st_as_sf(fa)

setwd(pathOut)

for (i in 1:length(list1)){
  
  #read in your raster
  ras1 <- raster::raster(paste0(pathIn,list1[i]))
  fireID1 <- unlist(strsplit(list1[i],"[.]"))[1]
  shpFires1 <- shpFires[shpFires$NDMI_id == fireID1,] #make sure its raster_id2
  crop1 = raster::crop(ras1, shpFires1)
  mask1 = raster::mask(crop1, shpFires1)
  outFile <- paste0(fireID1 , '.tif') 
  raster::writeRaster(mask1, outFile, overwrite = TRUE  )
  rm(ras1,fireID1,shpFires1,crop1,outFile)
}


# -------------- Now lets extract the values ---------- 

#Snow Disappearance date
path.In <- "C:/Users/jackg/Documents/PhD/Data/SDDperFire/SDDperFire_clipped/"
sdd.list <- list.files(path.In, pattern = glob2rx("*SDD.tif"))

#create empty list

datalist = list()
##extract rbr means
for (i in 1:length(sdd.list)){
  
  #read in your raster
  ras.1 <- raster::raster(paste0(path.In,sdd.list[i]))
  fireID2 <- unlist(strsplit(sdd.list[i],"[.]"))[1] # get fire ID
  df <- raster::rasterToPoints(ras.1)
  df1 <- df %>%  as_tibble()
  df2 <- df1 %>%  mutate(fireID = fireID2)
  datalist[[i]] <- df2

  
}
#bind lists together
df_sdd = bind_rows(datalist)
#rename columns
df_sdd = df_sdd %>%  rename(sdd = layer, lon = x, lat = y )
df_sdd1 = df_sdd %>%  filter( sdd >0)
df_sdd1$fireID <- df_sdd1$fireID %>%   str_remove( "_SDD")

#Snow cover frequency
path.In <- "C:/Users/jackg/Documents/PhD/Data/SCFperFire/SCFperFire_clipped/"
scf.list <- list.files(path.In, pattern = glob2rx("*SCF.tif"))

#create empty list

datalist = list()
##extract rbr means
for (i in 1:length(scf.list)){
  
  #read in your raster
  ras.1 <- raster::raster(paste0(path.In,scf.list[i]))
  fireID2 <- unlist(strsplit(scf.list[i],"[.]"))[1] # get fire ID
  df <- raster::rasterToPoints(ras.1)
  df1 <- df %>%  as_tibble()
  df2 <- df1 %>%  mutate(fireID = fireID2)
  datalist[[i]] <- df2
  
  
}
#bind lists together and rename columns
df_scf = datalist %>% bind_rows() %>%  
  rename(sscf = layer, lon = x, lat = y )
df_scf$fireID <- df_scf$fireID %>%   str_remove( "_SCF")

#PFC
path.In <- "C:/Users/jackg/Documents/PhD/Data/PFCperFire/PFCperFire_clipped/"
pfc.list <- list.files(path.In, pattern = glob2rx("*PFC.tif"))

#create empty list

datalist = list()
##extract rbr means
for (i in 1:length(pfc.list)){
  
  #read in your raster
  ras.1 <- raster::raster(paste0(path.In,pfc.list[i]))
  fireID2 <- unlist(strsplit(pfc.list[i],"[.]"))[1] # get fire ID
  df <- raster::rasterToPoints(ras.1)
  df1 <- df %>%  as_tibble()
  df2 <- df1 %>%  mutate(fireID = fireID2)
  datalist[[i]] <- df2
  
  
}
#bind lists together and rename columns
df_pfc = datalist %>% bind_rows() %>%  
  rename(pfc = layer, lon = x, lat = y )
df_pfc$fireID <- df_pfc$fireID %>%   str_remove( "_PFC")


#FC
path.In <- "C:/Users/jackg/Documents/PhD/Data/FCperFire/FCperFire_clipped/"
fc.list <- list.files(path.In, pattern = glob2rx("*FC.tif"))

#create empty list

datalist = list()
##extract rbr means
for (i in 1:length(fc.list)){
  
  #read in your raster
  ras.1 <- raster::raster(paste0(path.In,fc.list[i]))
  fireID2 <- unlist(strsplit(fc.list[i],"[.]"))[1] # get fire ID
  df <- raster::rasterToPoints(ras.1)
  df1 <- df %>%  as_tibble()
  df2 <- df1 %>%  mutate(fireID = fireID2)
  datalist[[i]] <- df2
  
  
}
#bind lists together and rename columns
df_fc = datalist %>% bind_rows() %>%  
  rename(fc = layer, lon = x, lat = y )
df_fc$fireID <- df_fc$fireID %>%   str_remove( "_FC")

#NDMI
path.In <- "C:/Users/jackg/Documents/PhD/Data/NDMIperFire/NDMIperFire_clipped/"
ndmi.list <- list.files(path.In, pattern = glob2rx("*NDMI.tif"))

#create empty list

datalist = list()
##extract rbr means
for (i in 1:length(ndmi.list)){
  
  #read in your raster
  ras.1 <- raster::raster(paste0(path.In,ndmi.list[i]))
  fireID2 <- unlist(strsplit(ndmi.list[i],"[.]"))[1] # get fire ID
  df <- raster::rasterToPoints(ras.1)
  df1 <- df %>%  as_tibble()
  df2 <- df1 %>%  mutate(fireID = fireID2)
  datalist[[i]] <- df2
  
  
}
#bind lists together and rename columns
df_ndmi = datalist %>% bind_rows() %>%  
  rename(ndmi = layer, lon = x, lat = y )
df_ndmi$fireID <- df_ndmi$fireID %>%   str_remove( "_NDMI")

#RBR
path.In <- "C:/Users/jackg/Documents/PhD/Data/RBR_500m/rbr_500_clipped/"
rbr.list <- list.files(path.In, pattern = glob2rx("*rbr.tif"))

#create empty list

datalist = list()
##extract rbr means
for (i in 1:length(rbr.list)){
  
  #read in your raster
  ras.1 <- raster::raster(paste0(path.In,rbr.list[i]))
  fireID2 <- unlist(strsplit(rbr.list[i],"[.]"))[1] # get fire ID
  df <- raster::rasterToPoints(ras.1)
  df1 <- df %>%  as_tibble()
  df2 <- df1 %>%  mutate(fireID = fireID2)
  datalist[[i]] <- df2
  
  
}
#bind lists together and rename columns
df_rbr = datalist %>% bind_rows() %>%  
  rename(rbr = layer, lon = x, lat = y )
df_rbr$fireID <- df_rbr$fireID %>%   str_remove( "_rbr")

#### join all dataframes together
snow_wf_data = df_sdd1 %>% 
  left_join(df_scf,by = c(lon = "lon", 
                   lat = "lat", 
                   fireID = "fireID")) %>% 
  left_join(.,df_pfc,by = c(lon = "lon", 
                            lat = "lat", 
                            fireID = "fireID")) %>% 
  left_join(.,df_fc,by = c(lon = "lon", 
                            lat = "lat", 
                            fireID = "fireID")) %>% 
  left_join(.,df_ndmi,by = c(lon = "lon", 
                            lat = "lat", 
                            fireID = "fireID")) %>% 
  left_join(., df_rbr, by = c(lon = "lon", 
                              lat = "lat", 
                              fireID = "fireID"))


#save data set
write.table(snow_wf_data, "snow_sev_data.txt")
