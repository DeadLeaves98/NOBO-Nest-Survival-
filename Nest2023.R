#### Title: Creating Complete Raw Nest File ----
#### Section: Part 1 and 2 -- Adding covariates: bURN STAT AND IND COV
# Author: Autumn Randall 
# Original Code: Dr. DJ McNeil
# Date Started: 11/26/2023 
# Date Last Editted: 11/26/2023 
#############################################################################3
# goal: rerun the code for nests in 2023.
# This is will need to be updated with newer data 

# packages 
# May not need all of these. 
library(rgdal); library(raster); library(adehabitatHR); library(rgeos); library(sf); library(dplyr)

# Setwd and read in data 
setwd("E:/NOBO Project Data/Analyses/Breeding Season/Summer 2023/Nest/Code")
nestdat = read.csv("E:/NOBO Project Data/Analyses/Breeding Season/Summer 2023/Nest/Code/BobwhiteNestingData_29-08-2023/NestData.csv", header = T)
head(nestdat)
nrow(nestdat)
length(unique(nestdat$Bird.ID)) #n = 136 
max(nestdat$Date.Found) # "8/9/2023 4:00:00 PM"
min(nestdat$Date.Found) #"5/10/2023 4:00:00 PM"


# Transform into spatial point 
nest_sp <- SpatialPoints(coords = data.frame("x" = nestdat$x, "y" = nestdat$y)) # convert DF to Spatial Points
crs(nest_sp) <- CRS("+init=epsg:4326") # define CRS for the spatial points (EPSG 4326 == lat/lon WGS84 etc)

# checking to make sure it looks right 
course <- readOGR("E:/NOBO Project Data/Analyses/shapefiles/OrtonCourses.shp")
crs(course)
nest_sp <- spTransform(nest_sp, crs(course))
crs(nest_sp)
plot(course)
plot(nest_sp, add=T)

# Looks good; you can tell because bill jones fucking sucks 


############################################
############################################ Adding Burn Status

# read in OP shapefile
Burn <- readOGR("E:/NOBO Project Data/Analyses/shapefiles/BurnMap2023.shp")
head(Burn)
#Burn <- readOGR("D:/NOBO Project Data/Orton_Courses_Shapefile/Shapefiles/Other/Master_Burn_Plan.shp")

# make nest match burn map
nest_sp <- spTransform(nest_sp, crs(Burn)) # transform nobo_sp from WGS84 to match "burn"

# Check Coordinate systems by plotting 
plot(Burn); plot(nest_sp, add = TRUE)

extraction <- over(nest_sp, Burn)# extract burn status for points

# turn all nas within burnstatus.extract column into no 
extraction["Burns_2023"][is.na(extraction["Burns_2023"])] <- "no" 
unique(extraction$Burns_2023)


# add burn status to nest
nestdat$burn_stat <- extraction$Burns_2023

# Look and check 
head(nestdat) # Looks good!

#   BURN STAT COVARIATE ADDED 

######################################
######## ADDING IN INDIVIDUAL COVARIATES ----

# Read in data 
indata = read.csv("E:/NOBO Project Data/Analyses/Breeding Season/Summer 2023/Trap_data_0_21Nov2023.csv", header = T)
head(indata)
nrow(indata)
nestdat <- merge(x = nestdat, by.x = "Bird.ID", y = indata, by.y = "BirdID", all.x = TRUE) # merge by birdid
# View(nest.merge)

# Add treatment 
#######################   ADDING TREATMENT TO NEST ----
head(nestdat)

nobo_blower <- subset(nestdat, Course.ID == "BB" | Course.ID == "BP" | Course.ID == "BJ") # 1 = blower, 0 = spinner 
nobo_blower$method = "1"
nobo_spinner = subset(nestdat, Course.ID == "FC" | Course.ID == "AC" | Course.ID == "CC" | Course.ID == "DB")
nobo_spinner$method = "0"


nest.treatment = rbind(nobo_blower, nobo_spinner) # 1 = blower, 0 = spinner 

bush4 = subset(nest.treatment, Course.ID == "DB" | Course.ID == "BJ" | Course.ID == "FC") # 1 = 4bushels/ac/yr, 0 = 2bushels/ac/yr 
bush4$quantity = "1"
bush2 = subset(nest.treatment, Course.ID == "CC" | Course.ID == "BB" | Course.ID == "AC" | Course.ID == "BP")
bush2$quantity = "0"

nest.treatment = rbind(bush4, bush2)


######################## ADDING RASTER VALUES TO NEST ----
# Transform into spatial point 
nest_sp <- SpatialPoints(coords = data.frame("x" = nest.treatment$x, "y" = nest.treatment$y)) # convert DF to Spatial Points
crs(nest_sp) <- CRS("+init=epsg:4326") # define CRS for the spatial points (EPSG 4326 == lat/lon WGS84 etc)
head(nest_sp)


# read in rasters
list.files("E:/NOBO Project Data/Analyses/Breeding Season/Summer 2023/Adult/rasters")
NDVI <- raster("E:/NOBO Project Data/Analyses/Breeding Season/Summer 2023/Adult/rasters/2019NDVI.tif")
perc_mpine <- raster("E:/NOBO Project Data/Analyses/Breeding Season/Summer 2023/Adult/rasters/PercentMaturePine.tif")
perc_grassy <- raster("E:/NOBO Project Data/Analyses/Breeding Season/Summer 2023/Adult/rasters/PercentGrassy.tif")
perc_decid <- raster("E:/NOBO Project Data/Analyses/Breeding Season/Summer 2023/Adult/rasters/PercentDeciduous.tif")
perc_bf <- raster("E:/NOBO Project Data/Analyses/Breeding Season/Summer 2023/Adult/rasters/PercentBroodField.tif")
perc_water <- raster("E:/NOBO Project Data/Analyses/Breeding Season/Summer 2023/Adult/rasters/PercentWater.tif")
DTN_road <- raster("E:/NOBO Project Data/Analyses/Breeding Season/Summer 2023/Adult/rasters/DTN_road.tif")
DTN_mpine <- raster("E:/NOBO Project Data/Analyses/Breeding Season/Summer 2023/Adult/rasters/DTN_maturepine.tif")
DTN_grassy <- raster("E:/NOBO Project Data/Analyses/Breeding Season/Summer 2023/Adult/rasters/DTN_grassy.tif")
DTN_decid <- raster("E:/NOBO Project Data/Analyses/Breeding Season/Summer 2023/Adult/rasters/DTN_deciduous.tif")
DTN_bf <- raster("E:/NOBO Project Data/Analyses/Breeding Season/Summer 2023/Adult/rasters/DTN_broodfield1.tif")
DTN_water <- raster("E:/NOBO Project Data/Analyses/Breeding Season/Summer 2023/Adult/rasters/DTN_water.tif")



# reproject nest  and extract
nest_sp1 <- spTransform(nest_sp, crs(NDVI)) # transform to NAD83
plot(NDVI); plot(nest_sp1, add = TRUE)
ndv1_ex <- raster::extract(x = NDVI, y = nest_sp1)
perc_mpine_ex <- raster::extract(x = perc_mpine, y = nest_sp1)
perc_grassy_ex <- raster::extract(x = perc_grassy, y = nest_sp1)
perc_decid_ex <- raster::extract(x = perc_decid, y = nest_sp1)
perc_bf_ex <- raster::extract(x = perc_bf, y = nest_sp1)
perc_water_ex <- raster::extract(x = perc_water, y = nest_sp1)

nest.sp2 <- spTransform(nest_sp, crs(DTN_road)) # transform to pseudo-mercator
plot(DTN_road); plot(nest.sp2, add = TRUE)
DTN_road_ex <- raster::extract(x = DTN_road, y = nest.sp2)
DTN_mpine_ex <- raster::extract(x = DTN_mpine, y = nest.sp2)
DTN_grassy_ex <- raster::extract(x = DTN_grassy, y = nest.sp2)
DTN_decid_ex <- raster::extract(x = DTN_decid, y = nest.sp2)
DTN_bf_ex <- raster::extract(x = DTN_bf, y = nest.sp2)
DTN_water_ex <- raster::extract(x = DTN_water, y = nest.sp2)

hist(DTN_water_ex)
hist(DTN_road_ex)

# add columns to NOBO and export
nest.treatment$ndvi <- ndv1_ex
nest.treatment$perc_mpine <- perc_mpine_ex
nest.treatment$perc_grassy <- perc_grassy_ex
nest.treatment$perc_decid <- perc_decid_ex
nest.treatment$perc_bf <- perc_bf_ex
nest.treatment$perc_water <- perc_water_ex
nest.treatment$DTN_road <- DTN_road_ex
nest.treatment$DTN_mpine <- DTN_mpine_ex
nest.treatment$DTN_grassy <- DTN_grassy_ex
nest.treatment$DTN_decid <- DTN_decid_ex
nest.treatment$DTN_bf <- DTN_bf_ex
nest.treatment$DTN_water <- DTN_water_ex 

# Check 
head(nest.treatment)



write.csv(nest.treatment, "E:/NOBO Project Data/Analyses/Breeding Season/Summer 2023/Nest/Code/nest.x.2023.csv", row.names = FALSE)

# manually removed columns and formatted the date column to match datconv

nest.treatment = read.csv("E:/NOBO Project Data/Analyses/Breeding Season/Summer 2023/Nest/Code/nest.y.2023.csv", header = T)
# date 

head(nest.treatment)
datconv = read.csv("E:/NOBO Project Data/Analyses/Breeding Season/Summer 2023/Nest/Code/NestDateConversion.csv")
head(datconv)

N <- merge(x = nest.treatment, by.x = "Date.Found", y = datconv, by.y = "Date", all.x = TRUE) # merge by date to create the i value
View(N) 





# export
write.csv(N, "E:/NOBO Project Data/Analyses/Breeding Season/Summer 2023/Nest/Code/nest.z.2023.csv", row.names = FALSE)
A = read.csv("E:/NOBO Project Data/Analyses/Breeding Season/Summer 2023/Nest/Code/nest.a.2023.csv", header = T)
A 
B <- merge(x = A, by.x = "Fate.Date", y = datconv, by.y = "Date", all.x = TRUE) # merge by date to create the i value
B
write.csv(B, "E:/NOBO Project Data/Analyses/Breeding Season/Summer 2023/Nest/Code/nest.b.2023.csv", row.names = FALSE)


#write.csv(nest.treatment, "E:/NOBO Project Data/Analyses/Breeding Season/Summer 2023/Nest/Code/nest2023.csv", row.names = FALSE)
# creating the inp file ---- 

# need to create the dataframe 

# View(nest.treatment)

x = nest.treatment[-c(3:5, 7:8, 14:109, 112:127, 130, 132:137)]
x = x[-c(4:8, 14:23)]
View(x)

#write.csv(nest.treatment, "E:/NOBO Project Data/Analyses/Breeding Season/Summer 2023/Nest/Code/nest.x.2023.csv", row.names = FALSE)

# Manually in excel I removed unnecessary columns 

nests = read.csv("E:/NOBO Project Data/Analyses/Breeding Season/Summer 2023/Nest/Code/nest.x.2023.csv", header = TRUE)
head(nests)









#########################################################################
# Fixing Date 
############ modifying the dates to something easier to handle
library(tidyr)
head(nest.treatment)
# view and pull out three date columns 
datefound <- data.frame("Date" = nest.treatment$Date.Found)
epe <- data.frame("Date" = nest.treatment$Date.Found)
nobodates <- data.frame("Date" = nest.treatment$Date.Found)
nobodates <- separate(nobodates, Date, into = c("month", "day", "year"), sep = "/") # split up month, day, year
nobodates <- separate(nobodates, time, into = c("hour", "min", "Sec"), sep = ":") # split up hour, minute, and second
nobo.merge2 <- cbind(nobo.merge1, nobodates)
# make numeric
nobo.merge2$month <- as.numeric(nobo.merge2$month)
nobo.merge2$day <- as.numeric(nobo.merge2$day)
nobo.merge2$year <- as.numeric(nobo.merge2$year)
nobo.merge2$hour <- as.numeric(nobo.merge2$hour)
nobo.merge2$min <- as.numeric(nobo.merge2$min)
nobo.merge2$Sec <- as.numeric(nobo.merge2$Sec)
nobo.merge2$CombinedDate <- paste0(nobo.merge2$day, "_", nobo.merge2$month, "_", nobo.merge2$year)

# Figure for Nest Success by Treatment
counts = table(nest.treatment$treatment)
counts

barplot(counts, main = "Nest Distribution by Treatment", 
        xlab="Treatment", 
        ylab= "Number of Nests")

counts = table(nest.treatment$treatment, nest.treatment$f)
counts

barplot(counts, main="Nest Success by Treatment", 
        xlab="Treatment", 
        col=c("darkblue","red"), 
        legend =(rownames(counts)), beside=T)

#########################################################################