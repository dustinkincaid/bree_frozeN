# Compile 2014 & 2015 data from Missisquoi Bay and 2015 data from Shelburne Pond

# Load packages----
  library("tidyverse")
  library("lubridate")
  library("data.table")

# Read in data----
  {
  # Read in 2014 grab sample chemistry data for Missisquoi Bay
    mb_2014.raw <- read.csv("01_raw data/Copy of Lake Grab 2014_2-6-15_sb.csv", header=T, stringsAsFactors = F, na.strings = c("", " ", "#N/A"))
  
  # Read in 2014 manual sonde cast data for Missisquoi Bay
    mb_sensor_2014 <- read.csv("01_raw data/2014_Manual_Sonde_Casts_Winter.csv", header=T, stringsAsFactors = F)

  # Read in 2015 under ice water chemistry data for Missisquoi Bay & Shelburne Pond
  # Missisquoi Bay
    mb_n.raw <- read.csv("01_raw data/WINTER 2015 MB_TN-TP.csv", header=T, stringsAsFactors = F)
  # Shelburne Pond
    sp_n.raw <- read.csv("01_raw data/WINTER 2015 SP_TN-TP.csv", header=T, stringsAsFactors = F)
  
  # Read in 2015 supplementary data from Joung et al. 2017
  # Keep the sensor data
    mb_sensor <- read.csv("01_raw data/WINTER 2015 MB_supp_data.csv", header=T, stringsAsFactors = F, na.strings = " ") %>% 
      filter(Location == "Lake") %>% 
      select(Cal.Date, Depth_sensor:BGA) %>% 
      rename(date=Cal.Date, depth=Depth_sensor, temp=Temp., cond=Cond., turb=Turb., chla=Chl.a) %>% 
      mutate(date = mdy(as.character(date), tz="US/Eastern"),
             site = "mb")
    sp_sensor <- read.csv("01_raw data/WINTER 2015 SP_supp_data.csv", header=T, stringsAsFactors = F, na.strings = " ") %>% 
      select(Cal.Date, Depth_sensor:BGA) %>% 
      rename(date=Cal.Date, depth=Depth_sensor, temp=Temp., cond=Cond., turb=Turb., chla=Chl.a) %>% 
      mutate(date = mdy(as.character(date), tz="US/Eastern"),
             site = "sp")
    
  # Read in ice depth data compiled from Schroth et al. 2015 and Joung et al. 2017
    ice <- read.csv("01_raw data/iceDepths.csv", header = T, stringsAsFactors = F, na.strings = "NA") %>% 
      mutate(date = mdy(as.character(date), tz="US/Eastern"))
  }

# Tidy the data----
  {
  # Tidy the 2014 data
    mb_2014 <- mb_2014.raw %>% 
      filter(Lab.ID.Bottle.ID != "LG14-013") %>% 
      select(Depth.or.BLANK, depth_cat, Date.Collected, TP.Corrected, Nox.Corrected, NH4.Corrected, TN.Corrected) %>% 
      filter(Depth.or.BLANK != "blank", Depth.or.BLANK != "blank ", Depth.or.BLANK != "Blank", !is.na(Depth.or.BLANK)) %>% 
      rename(date=Date.Collected, samp_depth_cat=depth_cat, samp_depth_desc=Depth.or.BLANK, TP=TP.Corrected, NO3=Nox.Corrected, NH4=NH4.Corrected, TN=TN.Corrected) %>% 
      mutate(date = mdy(as.character(date), tz="US/Eastern"),
             site = "mb") %>% 
      # Filter for dates on or before 4/25/2014; last on ice sample = 4/3/2014
      filter(date <= "2014-04-25 EDT")
    
    # Add depths
      # NOTE: the bottom 3 depths are referenced to the bottom depth; DK estimated these bottom depths from manual sonde cast
      #       data, so these are approximate; 4/3/18 is a guess b/c no sonde cast data; adjust bottom 3 depths if necessary
      mb2014_depths = c(0.5, 1, 2.1, 2.6, 3.1, #2014-01-10
                        0.5, 1, 2.3, 2.8, 3.3, #2014-01-31
                        0.5, 1, 2.2, 2.7, 3.2, #2014-02-13
                        0.5, 1, 2.3, 2.8, 3.3, #2014-03-05
                        0.5, 1, 2.1, 2.6, 3.1, #2014-03-19
                        0.5, 1, 2.1, 2.6, 3.1, #2014-04-03
                        0.5, NA, 3.1, NA, 4.1  #2014-04-25
                        )    
      # Create a df with values to match and merge with mb_2014
        mb2014_depths_df <- data.frame(date = rep(unique(mb_2014$date), each=5), samp_depth_cat = rep(unique(mb_2014$samp_depth_cat), 7), depth = mb2014_depths)
      # Join to add depths to mb_2014
        mb_2014 <- left_join(mb_2014, mb2014_depths_df, by = c("date", "samp_depth_cat"))
      
    # Add another samp_depth_cat2 that defines 0.5 as top, 3.5 as bottom, and the middle depths as Mid-1, Mid-2, Mid-3
      depth_index_2014 <- c("D1", "D2", "D3", "D4", "D5")
      depth_cat2 <- c("Top", "Mid-1", "Mid-2", "Mid-3", "Bottom")
      mb_2014$samp_depth_cat2 <- depth_cat2[match(mb_2014$samp_depth_cat, depth_index_2014)]
    
    # Update Julian day
      mb_2014 <- mb_2014 %>% 
        mutate(yday = yday(date)) %>% 
        # Re-order columns
        select(date, yday, site, depth, everything()) %>% select(-samp_depth_desc)
    
  # Tidy the 2014 manual sonde cast data for Missisquoi Bay
      # I've made the assumption that cond in 2015 sensor data is cond, not specfic cond  
        mb_sensor_2014 <- mb_sensor_2014 %>% 
          # Select the same variables as 2015 data
          select(DateTime, Depth_m, Temp_C, Cond_mscm, ODO_mgl, pH, Turbidity_ntu, Chlorophyll_ugl, BGA_PC_cellsml) %>% 
          rename(date = DateTime, depth = Depth_m, temp = Temp_C, cond = Cond_mscm, DO = ODO_mgl, 
                 turb = Turbidity_ntu, chla = Chlorophyll_ugl, BGA = BGA_PC_cellsml) %>% 
          # Convert date format & drop time
          mutate(date = floor_date(mdy_hm(date, tz="US/Eastern"), unit = "day")) %>% 
          # Add year day
          mutate(yday = yday(date)) %>% 
          # Filter data for dates after 12/31
          filter(date > "2013-12-31") %>% 
          # Convert conductivity to uS/cm (currently mS/cm)
          mutate(cond = cond*1000) %>% 
          # Make negative temps = 0
          mutate(temp = replace(temp, temp < 0, 0.0)) %>% 
          # Sort by date then depth
          arrange(date, depth) %>% 
          # Round depths to the nearest tenth
          mutate(depth = round(depth, digits = 1))
        
      # Deal with duplicate measurements made on same date & depth
        # Set the keys to be date and depth
        # keys <- colnames(mb_sensor_2014)[(names(mb_sensor_2014) %in% c("date", "depth"))]
          keys <- c("date", "depth")
        # Set as data.table
          setDT(mb_sensor_2014)
        # Take the average of duplicate date & depth combinations
          mb_sensor_2014 <- mb_sensor_2014[, lapply(.SD, mean), keys] %>% 
            # Round the columns to their original precision  
            mutate(temp = round(temp, digits = 2),
                   cond = round(cond, digits = 0),
                   DO = round(DO, digits = 2),
                   pH = round(pH, digits = 2),
                   turb = round(turb, digits = 1),
                   chla = round(chla, digits = 1),
                   BGA = round(BGA, digits = 0))
          
    # Join the 2014 nutrient and sensor data
        mb_2014 <- full_join(mb_2014, mb_sensor_2014, by = c("date", "yday", "depth")) %>%
          # Sort by date a depth
          arrange(date, depth) %>% 
          # Add a site column
          mutate(site = "mb")
  
  # Tidy the 2015 data and combine into one
    # Missisquoi Bay  
      mb_n <- mb_n.raw %>% 
        select(Date.Collected, Depth.or.BLANK, TP.Corrected, Nox.Corrected, NH4.Corrected, TN.Corrected) %>% 
        rename(date=Date.Collected, samp_depth_cat=Depth.or.BLANK, TP=TP.Corrected, NO3=Nox.Corrected, NH4=NH4.Corrected, TN=TN.Corrected) %>% 
        mutate(date = mdy(as.character(date), tz="US/Eastern"),
               yday = yday(date),
               site = "mb")
      # Correct one sampling date
        mb_n$date[mb_n$date == "2015-03-27"] <- "2015-03-28"
      
      # Add measured depths as column
      # Currently using the depths as reported in 2015 Winter -All sensor data.xlsx Sheet4 column AD
        mb_depths = c(0.5, 2.5, 3.0, 3.3, 3.5, #1/15/2015
                      0.5, 2.8, 3.0, 3.2, 3.5, #2/4/2015
                      0.7, 2.3, 2.8, 3.2, 3.3, #2/17/2015
                      0.7, 2.1, 2.6, 2.75, 3.05, #3/10/2015 
                      0.7, 2.1, 2.6, 2.85, 3.1, #3/19/2015 
                      0.7, 2.1, 2.6, 2.85, 3.05, #3/25/2015
                      0.7, 2.1, 2.6, 2.85, 3.12, #3/28/2015
                      0.6, 2.38, 2.88, 3.13, 3.38 #4/6/2015
                      )
      # Create a df with values to match and merge with mb_n
        mb_depths_df <- data.frame(date = rep(unique(mb_n$date), each=5), samp_depth_cat = rep(unique(mb_n$samp_depth_cat), 8), depth = mb_depths)
      # Join to add depths to mb_n
        mb_n <- full_join(mb_n, mb_depths_df, by = c("date", "samp_depth_cat"))
      # Add sensor data from supplementary material
        mb_n <- full_join(mb_n, mb_sensor, by = c("date", "site", "depth")) %>% arrange(date, depth)
  
    # Repeat for Shelburne Pond data
      sp_n <- sp_n.raw %>% 
        select(Date.Collected, Depth.or.BLANK, TP.Corrected, Nox.Corrected, NH4.Corrected, TN.Corrected) %>% 
        rename(date=Date.Collected, samp_depth_cat=Depth.or.BLANK, TP=TP.Corrected, NO3=Nox.Corrected, NH4=NH4.Corrected, TN=TN.Corrected) %>% 
        mutate(date = mdy(as.character(date), tz="US/Eastern"),
               site = "sp")
      # Correct sampling dates
        sp_n$date[sp_n$date == "2015-02-04"] <- "2015-02-06"
        sp_n$date[sp_n$date == "2015-02-17"] <- "2015-02-18"
  
      # Add measured depths as column
      # Currently using the depths as reported in 2015 Winter -All sensor data.xlsx Sheet4 column AD
        sp_depths = c(0.4, 3.8, 4.3, 4.5, 4.8, #1/15/2015
                      0.5, 3.8, 4.2, 4.4, 4.6, #2/6/2015
                      0.45, 3.6, 4.1, 4.35, 4.64, #2/18/2015
                      0.54, 3.7, 4.2, 4.45, 4.65, #3/10/2015 
                      0.7, 3.6, 4.1, 4.35, 4.6, #3/19/2015 
                      0.45, 3.8, 4.25, 4.5, 4.75, #3/25/2015
                      0.4, 3.75, 4.25, 4.5, 4.75, #3/27/2015
                      0.3, 3.8, 4.3, 4.55, 4.8 #4/6/2015
                      )
  
      # Create a df with values to match and merge with mb_n
        sp_depths_df <- data.frame(date = rep(unique(sp_n$date), each=5), samp_depth_cat = rep(unique(sp_n$samp_depth_cat), 8), depth = sp_depths)
      # Join to add depths to mb_n
        sp_n <- full_join(sp_n, sp_depths_df, by = c("date", "samp_depth_cat"))
      # Add sensor data from supplementary material
        sp_n <- full_join(sp_n, sp_sensor, by = c("date", "site", "depth")) %>% arrange(date, depth)
      
      # Here's the final dataframe with 2015 data from both lakes
        winter2015_chem_all <- 
          bind_rows(mb_n, sp_n) %>% 
          mutate(yday = yday(date)) %>% 
          select(date, yday, site, depth, everything())
      
      # Add another samp_depth_cat2 that defines D1 as top, D5 as bottom, and the middle depths as Mid-1, Mid-2, Mid-3
        depth_index_2015 <- c("D1", "D2", "D3", "D4", "D5")
        depth_cat2 <- c("Top", "Mid-1", "Mid-2", "Mid-3", "Bottom")
        winter2015_chem_all$samp_depth_cat2 <- depth_cat2[match(winter2015_chem_all$samp_depth_cat, depth_index_2015)]
      
  # Combine 2014 & 2015 data & 
  # convert N & P data from mg/L to umol/L
    alldata <-
      bind_rows(mb_2014, winter2015_chem_all) %>% 
      # Convert N & P data from mg/L to umol/L
      mutate(TP = TP*1000/30.974,
             NO3 = NO3*1000/14.007,
             NH4 = NH4*1000/14.007,
             TN = TN*1000/14.007,
             tntp = TN/TP) %>% 
      select(site, date, yday, depth, samp_depth_cat, samp_depth_cat2, NH4, NO3, TN, TP, tntp, temp:BGA)
      # select(date:TN, tntp, temp:BGA, samp_depth_cat2) 

  # Remove unnecessary objects
    rm(depth_cat2, depth_index_2014, depth_index_2015, keys, mb_2014.raw, mb_depths, mb_depths_df, 
       mb_sensor_2014, mb2014_depths_df, mb_n, mb_n.raw, sp_depths_df, sp_n, sp_n.raw, mb_sensor, sp_sensor)
    
  # Add ice depth data
    alldata <- alldata %>% 
      full_join(ice, by = c("site", "date", "yday")) %>% 
      select(site, date, yday, ice_depth = ice_depth_m, everything())
    
  # Write these data to a CSV file
    alldata %>%
      mutate(date = as.character(date)) %>% 
      write_csv("01_raw data/alldata_2014_2015_mb_sp_compiled.csv")
  }