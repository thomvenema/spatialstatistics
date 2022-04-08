# packages 
library(sf)
library(tmap)
library(dplyr)
library(rgdal)

# set working directory 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Clear workspace
rm(list=ls())

# Load Turn-out data
turnout = read.csv('data/turnout_per_station_XY.csv')
turnout_point <- st_as_sf(turnout, coords = c("X", "Y"), crs = 28992)
turnout_point <- turnout_point[,"Zipcode"]

turnout = turnout %>% 
  group_by(Zipcode) %>% 
  summarise(Invited = sum(Invited),
            Turnout = sum(Turnout))

# Load mapping data
mapcodes = read.csv('data/pc6-gwb2020.csv', sep = ';')
mapcodes = mapcodes[, c('PC6','Wijk2020')]
mapcodes = distinct(mapcodes)

# merge wijkcodes with pc6
turnout = left_join(turnout, mapcodes, by = c('Zipcode' = 'PC6'))
turnout = turnout %>% 
  group_by(Wijk2020) %>% 
  summarise(Invited = sum(Invited),
            Turnout = sum(Turnout))

turnout[, "Wijk2020"] = as.numeric(turnout$Wijk2020)
turnout <- turnout[,c('Wijk2020', 'Turnout')]

######################################3
wijk <- read_sf('data/wijken_2019.shp')
features <- c(1,6,6,9:21,31:32,45,47,78:79,81:82,87:90,129,164)
wijk <- wijk[,features]

newnames <- c('wijkcode', 'Water', 'gem_naam', 'bev_dichth_km2', 'aantal_inw', 'man', 'vrouw', 
             'p_0_15_jaar', 'p_15_25_jaar', 'p_25_45_jaar', 'p_45_65_jaar', 'p_>65_jaar',
             'p_ongehuwd', 'p_gehuwd', 'p_gescheid', 'p_verweduwd', 'p_ia_west', 'p_ia_n_west',
             'aantal_bedr', 'gem_won_waarde', 'aant_ink_ont', 'gem_ink_p_o',
             'p_laag_ink', 'p_hoog_ink', 'ao_uitk', 'ww_uitk', 'alg_bijst_uitk_tot',
             'aow_uitk', 'afst_levensm_5km', 'voortg_ond_3km', 'geometry')

names(wijk) <- newnames

# filter water out of the dataset
wijk <- wijk[wijk$Water == 'NEE',]
wijk <- wijk[names(wijk) != 'Water']

# manipulate Wijkcode
wijk[, "wijkcode"] <- gsub('WK','', wijk$wijkcode)
wijk[, "wijkcode"] <- gsub("^0+([1-9])","\\1",wijk$wijkcode)
wijk[, "wijkcode"] <- as.numeric(wijk$wijkcode)

# make ID
wijk[,'ID'] <- 1:nrow(wijk)

# geometry
wijkgeo <- wijk[,c('ID', 'geometry')]

# Get wijkdata: drop geometry
wijkdata <- st_drop_geometry(wijk)

# change -99999999 value to NA
wijkdata[wijkdata == -99999999] <- NA

# Make data raletive (decimal numbers)
# percentages to decimal numbers
p_cols <- names(wijkdata)[grepl('^p_', names(wijkdata))]
wijkdata[p_cols] <- wijkdata[p_cols]/100

# Calculate total uitekering
wijkdata[,'tot_uitk'] <- wijkdata$ao_uitk + wijkdata$ww_uitk + wijkdata$alg_bijst_uitk_tot
wijkdata <- wijkdata[!names(wijkdata) %in% c('ao_uitk', 'ww_uitk', 'alg_bijst_uitk_tot')]

# Calculate ratios
wijkdata[wijkdata$aantal_inw == 0 & !is.na(wijkdata$aantal_inw), 'aantal_inw'] <- NA
cols_to_ratio <- c('man','vrouw','aant_ink_ont','aow_uitk','tot_uitk')
wijkdata[cols_to_ratio] <- wijkdata[cols_to_ratio]/wijkdata$aantal_inw

# Calculate adults
# assumption of 18 year old
wijkdata[,"abs_15_25_jaar"] <- (wijkdata$p_15_25_jaar*wijkdata$aantal_inw)*0.7
wijkdata[,"abs_25_45_jaar"] <- wijkdata$p_25_45_jaar*wijkdata$aantal_inw
wijkdata[,"abs_45_65_jaar"] <- wijkdata$p_45_65_jaar*wijkdata$aantal_inw
wijkdata[,"abs_>65_jaar"] <- wijkdata$`p_>65_jaar`*wijkdata$aantal_inw

wijkdata[,'adults'] <- wijkdata$abs_15_25_jaar + wijkdata$abs_25_45_jaar + wijkdata$abs_45_65_jaar + wijkdata$`abs_>65_jaar`
wijkdata[,'adults'] <- round(wijkdata$adults, digits = 0) 

dropcols = c('abs_15_25_jaar','abs_25_45_jaar', 'abs_45_65_jaar', 'abs_>65_jaar')
wijkdata <- wijkdata[!names(wijkdata) %in% dropcols]
# Merge turnout percentage with the wijkdata
wijkdata <- left_join(wijkdata, turnout, by = c('wijkcode'='Wijk2020'))

# Caldulate turnout ratios
wijkdata[,'to_ratio'] <- round(wijkdata$Turnout/wijkdata$adults, digits = 3)

# calculate distance to nearest voting station
# calculate centroids
wijk_centroids <- st_centroid(wijkgeo)

# order on ID
wijk_centroids <- wijk_centroids[order(wijk_centroids$ID),]

# Caculate disntance matrix
m_dist <- st_distance(wijk_centroids$geometry, turnout_point$geometry)
# Calculate minimul distance to polling station
nn_voting_stat <- apply(m_dist, 1, FUN = min, na.rm = TRUE)

# bind rows again, but first arrange
wijkdata <- wijkdata[order(wijkdata$ID),]
wijkdata <- cbind(wijkdata, nn_voting_stat)

# Merge data with location
wijk_final <- merge(wijkdata, wijkgeo)
wijk_final <- st_as_sf(wijk_final)

# Write data
st_write(wijk_final, 'CBS_turnout_Wijk.shp')

# map turnout 
tmap_mode('plot')
tm_shape(wijk_final)+
  tm_fill('nn_voting_stat',
          breaks = c(0,1000,2000,3000,4000,5000, Inf),
          pallete = 'Greens')

#####################################
missing.values <- wijkdata %>%
  gather(key = "key", value = "val") %>%
  mutate(isna = is.na(val)) %>%
  group_by(key) %>%
  mutate(total = n()) %>%
  group_by(key, total, isna) %>%
  summarise(num.isna = n()) %>%
  mutate(pct = num.isna / total * 100)

levels <- (missing.values  %>% filter(isna == T) %>% arrange(desc(pct)))$key

percentage.plot <- missing.values %>%
  ggplot() +
  geom_bar(aes(x = reorder(key, desc(pct)), y = pct, fill=isna), stat = 'identity', alpha=0.8) +
  scale_x_discrete(limits = levels) +
  scale_fill_manual(name = "",  values = c('steelblue', 'tomato3'), labels = c("Present", "Missing")) +
  coord_flip() +
  labs(title = "Percentage of missing values", x = 'Variable', y = "% of missing values")

row.plot <- wijkdata %>%
  mutate(id = row_number()) %>%
  gather(-id, key = "key", value = "val") %>%
  mutate(isna = is.na(val)) %>%
  ggplot(aes(key, id, fill = isna)) +
  geom_raster(alpha=0.8) +
  scale_fill_manual(name = "", values = c('steelblue', 'tomato3'), labels = c("Present", "Missing")) +
  scale_x_discrete(limits = levels) +
  labs(x = "Variable", y = "Row Number", title = "Missing values in rows") +
  coord_flip()

patchwork::wrap_plots(row.plot, percentage.plot)


