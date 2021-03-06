# Compile 2014 & 2015 data from Missisquoi Bay and 2015 data from Shelburne Pond

# Load packages----
  library("tidyverse")
  library("lubridate")
  library("data.table")

# Read in data----
  {
  # Read in 2014 grab sample chemistry data for Missisquoi Bay
    mb_2014.raw <- read.csv("01_raw data/Copy of Lake Grab 2014_2-6-15_sb.csv", header=T, stringsAsFactors = F, na.strings = c("", " ", "#N/A"))
    
  # Read in 2014 SRP Miss Bay data from grab samples
    # SRP are ug P/L
    mb_SRP <- read.csv("01_raw data/SRP Data 2014.csv", header = T, stringsAsFactors = F) %>% 
      mutate(date = mdy(date)) %>% 
      mutate(site = replace(site, site == "S087", "mb")) %>% 
      mutate(samp_depth_cat = ifelse(depth %in% c("Below ice", "Surface"), "D1",
                                     ifelse(depth == "1m", "D2",
                                            ifelse(depth == "B+1m", "D3",
                                                   ifelse(depth == "B+0.5m", "D4", "D5"))))) %>% 
      mutate(samp_depth_cat2 = ifelse(depth %in% c("Below ice", "Surface"), "Top",
                                     ifelse(depth == "1m", "Mid-1",
                                            ifelse(depth == "B+1m", "Mid-2",
                                                   ifelse(depth == "B+0.5m", "Mid-3", "Bottom"))))) %>%   
      select(-depth) %>% 
      mutate(rep = 1) %>% 
      rename(depth = depth_m, SRP = PO4_ugL) %>% 
      # Convert SRP to mg/l from ug/L to match other grab sample data
      mutate(SRP = SRP/1000)
  
  # Read in 2014 manual sonde cast data for Missisquoi Bay
    mb_sensor_2014 <- read.csv("01_raw data/2014_Manual_Sonde_Casts_Winter.csv", header=T, stringsAsFactors = F)

  # Read in 2015 under ice water chemistry data for Missisquoi Bay & Shelburne Pond
  # Missisquoi Bay
    mb_n.raw <- read.csv("01_raw data/WINTER 2015 MB_TN-TP.csv", header=T, stringsAsFactors = F)
  # Shelburne Pond
    sp_n.raw <- read.csv("01_raw data/WINTER 2015 SP_TN-TP.csv", header=T, stringsAsFactors = F)
  
  # Read in 2015 supplementary data from Joung et al. 2017
  # Keep the sensor
    mb_sensor <- read.csv("01_raw data/WINTER 2015 MB_supp_data.csv", header=T, stringsAsFactors = F, na.strings = " ") %>% 
      filter(Location == "Lake") %>% 
      select(Cal.Date, Depth_sensor:BGA) %>% 
      rename(date=Cal.Date, depth=Depth_sensor, temp=Temp., cond=Cond., turb=Turb., chla=Chl.a) %>% 
      mutate(date = mdy(as.character(date)),
             site = "mb") %>% 
      mutate(rep = 1)
    sp_sensor <- read.csv("01_raw data/WINTER 2015 SP_supp_data.csv", header=T, stringsAsFactors = F, na.strings = " ") %>% 
      select(Cal.Date, Depth_sensor:BGA) %>% 
      rename(date=Cal.Date, depth=Depth_sensor, temp=Temp., cond=Cond., turb=Turb., chla=Chl.a) %>% 
      mutate(date = mdy(as.character(date)),
             site = "sp") %>% 
      mutate(rep = 1)
  # Keep the 2015 P and NO3 data from supp info file
  
  # All P concentrations are umol/kg, which is umol/L (assuming 1 kg = 1 L for freshwater)
    mb_P_15 <- read.csv("01_raw data/WINTER 2015 MB_supp_data.csv", header=T, stringsAsFactors = F, na.strings = " ") %>% 
      filter(Location == "Lake") %>% 
      select(date = Cal.Date, depth = Depth_grab, SRP, DOP, PP, TDP, TP) %>% 
      filter(!is.na(SRP)) %>% 
      mutate(date = mdy(as.character(date)),
             site = "mb") %>% 
      # For joining purposes below, let's convert umol/L to mg/L
      mutate(SRP = SRP/1000*30.974,
             DOP = DOP/1000*30.974,
             PP = PP/1000*30.974,
             TDP = TDP/1000*30.974,
             TP = TP/1000*30.974) %>% 
      mutate(rep = 1)
    sp_P_15 <- read.csv("01_raw data/WINTER 2015 SP_supp_data.csv", header=T, stringsAsFactors = F, na.strings = " ") %>%
      select(date = Cal.Date, depth = Depth_grab, SRP, DOP, PP, TDP, TP) %>% 
      filter(!is.na(SRP)) %>% 
      mutate(date = mdy(as.character(date)),
             site = "sp") %>% 
      # For joining purposes below, let's convert umol/L to mg/L
      mutate(SRP = SRP/1000*30.974,
             DOP = DOP/1000*30.974,
             PP = PP/1000*30.974,
             TDP = TDP/1000*30.974,
             TP = TP/1000*30.974) %>% 
      mutate(rep = 1)
    
  # Read in 2015 Missisquoi River grab samples
    # First from 2015 supplementary data from Joung et al. 2017
    missRiv_chem_SI <- read.csv("01_raw data/WINTER 2015 MB_supp_data.csv", header=T, stringsAsFactors = F, na.strings = " ") %>% 
      filter(Location == "River") %>% 
      # select(-c(ends_with("SD"), ends_with("02"), ends_with("45"), ends_with("coll")))
      select(-(ends_with("SD"))) %>% 
      select(Cal.Date, Jul.Day, Location, `Temp.`:DOC) %>% 
      rename(date=Cal.Date, yday = Jul.Day, depth=Depth_grab, temp = `Temp.`, cond = `Cond.`, turb = `Turb.`, chla = `Chl.a`) %>% 
      mutate(date = mdy(as.character(date)),
             site = "mb") %>% 
      # NO3 here is from s::can in mg N/L; let's drop it
      select(-NO3) %>% 
      # Convert P data from umol/L to mg P/L for joining purposes; NO3 is already mg N/L; DOC is mg C/L
      mutate(SRP = SRP/1000*30.974,
             DOP = DOP/1000*30.974,
             PP = PP/1000*30.974,
             TDP = TDP/1000*30.974,
             TP = TP/1000*30.974) %>% 
      # Drop the empty TP column for joining purposes below
      select(-TP)
      
    
    # Then from lab file from Saul
    # Concentrations are mg P/L or mg N/L
    missRiv_chem_lab <- read_csv("01_raw data/2015_Spring_MissR_Grabs.csv", col_types = cols()) %>% 
      select(date = `Date Collected`, TP = `TP Corrected mg P /L`, NO3 = `Nox Corrected mg N/ L`, NH4 = `NH4 Corrected mg N/ L`, TN = `TN Correctedmg N/ L`) %>% 
      # Correct the date of the March grab sample
      mutate(date = ifelse(date == "3/27/15", "3/28/15", date)) %>% 
      # Parse columns into correct type
      type_convert() %>% 
      mutate(date = mdy(date)) %>% 
      # Calculate mean of the replicates
      group_by(date) %>% 
      summarize(across(TP:TN, ~ round(mean(.x, na.rm = T), 3))) %>% 
      ungroup()
    
    # Combine river dfs & add samp_depth_cat2 = "River"
    missRiv_chem <- full_join(missRiv_chem_SI, missRiv_chem_lab) %>% 
      # Calculate PP
      mutate(PP = TP - TDP) %>% 
      # Add a samp_depth_cat2
      mutate(samp_depth_cat2 = "River")
    rm(missRiv_chem_SI, missRiv_chem_lab)
       
  # Read in ice depth data compiled from Schroth et al. 2015 and Joung et al. 2017
    ice <- read.csv("01_raw data/iceDepths.csv", header = T, stringsAsFactors = F, na.strings = "NA") %>% 
      mutate(date = mdy(as.character(date)))
  }

# Tidy the data----
  {
  # Tidy the 2014 grab sample data 
    mb_2014 <- mb_2014.raw %>% 
      filter(Lab.ID.Bottle.ID != "LG14-013") %>% 
      select(Depth.or.BLANK, depth_cat, Date.Collected, TP.Corrected, Nox.Corrected, NH4.Corrected, TN.Corrected) %>% 
      filter(Depth.or.BLANK != "blank", Depth.or.BLANK != "blank ", Depth.or.BLANK != "Blank", !is.na(Depth.or.BLANK)) %>% 
      rename(date=Date.Collected, samp_depth_cat=depth_cat, samp_depth_desc=Depth.or.BLANK, TP=TP.Corrected, NO3=Nox.Corrected, NH4=NH4.Corrected, TN=TN.Corrected) %>% 
      mutate(date = mdy(as.character(date)),
             site = "mb") %>% 
      # Filter for dates on or before 4/25/2014; last on ice sample = 4/3/2014
      filter(date <= "2014-04-25") %>% 
      # Add rep no.
      group_by(date, samp_depth_cat) %>% 
      mutate(rep = row_number())
    
    # Add depths
      # NOTE: the bottom 3 depths are referenced to the bottom depth; DK estimated these bottom depths from manual sonde cast
      #       data, so these are approximate; 4/3/18 is a guess b/c no sonde cast data; adjust bottom 3 depths if necessary
      mb2014_depths = c(0.5, 1, 2.1, 2.6, 3.1, #2014-01-10
                        0.5, 1, 2.3, 2.8, 3.3, #2014-01-31
                        0.5, 1, 2.2, 2.7, 3.2, #2014-02-13
                        0.5, 1, 2.3, 2.8, 3.3, #2014-03-05
                        0.5, 1, 2.1, 2.6, 3.1, #2014-03-19
                        0.5, 1, 2.1, 2.6, 3.1, #2014-04-03
                        0.5, 1, 3.1, 3.6, 4.1  #2014-04-25
                        )    
      # Create a df with values to match and merge with mb_2014
        mb2014_depths_df <- data.frame(date = rep(unique(mb_2014$date), each=5), samp_depth_cat = rep(unique(mb_2014$samp_depth_cat), 7), depth = mb2014_depths)
      # Join to add depths to mb_2014
        mb_2014 <- left_join(mb_2014, mb2014_depths_df, by = c("date", "samp_depth_cat"))
      
    # Add another samp_depth_cat2 that defines 0.5 as top, 3.5 as bottom, and the middle depths as Mid-1, Mid-2, Mid-3
      depth_index_2014 <- c("D1", "D2", "D3", "D4", "D5")
      depth_cat2 <- c("Top", "Mid-1", "Mid-2", "Mid-3", "Bottom")
      mb_2014$samp_depth_cat2 <- depth_cat2[match(mb_2014$samp_depth_cat, depth_index_2014)]
    
    # Add 2014 SRP data to mb_2014 & update day of year
      mb_2014 <- mb_2014 %>% 
        full_join(mb_SRP, by = c("date", "site", "depth", "rep")) %>% 
        mutate(samp_depth_cat.x = ifelse(is.na(samp_depth_cat.x), samp_depth_cat.y, samp_depth_cat.x),
               samp_depth_cat2.x = ifelse(is.na(samp_depth_cat2.x), samp_depth_cat2.y, samp_depth_cat2.x)) %>% 
        mutate(yday = yday(date)) %>% 
        # Re-order columns
        select(date, yday, site, depth, samp_depth_cat = samp_depth_cat.x, samp_depth_cat2 = samp_depth_cat2.x, everything()) %>% select(-c(samp_depth_desc, samp_depth_cat.y, samp_depth_cat2.y))
    
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
                   BGA = round(BGA, digits = 0)) %>% 
            mutate(rep = 1) %>% 
            mutate(site = "mb")
          
  # Join mb_2014 & mb_sensor_2014
    mb_2014 <- full_join(mb_2014, mb_sensor_2014, by = c("date", "yday", "site", "depth", "rep")) %>% 
      select(-rep) %>% 
      arrange(date, depth)
  
  # Tidy the 2015 data and combine into one
    # Missisquoi Bay  
      mb_n <- mb_n.raw %>% 
        select(Date.Collected, Depth.or.BLANK, Nox.Corrected, NH4.Corrected, TN.Corrected) %>% 
        rename(date=Date.Collected, samp_depth_cat=Depth.or.BLANK, NO3=Nox.Corrected, NH4=NH4.Corrected, TN=TN.Corrected) %>% 
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
      # Add depths to mb_n & join 2015 P data
        mb_n <- full_join(mb_n, mb_depths_df, by = c("date", "samp_depth_cat")) %>% 
          # Add rep no.
          group_by(date, samp_depth_cat) %>% 
          mutate(rep = row_number()) %>% 
          # Join 2015 MP P data
          full_join(mb_P_15, by = c("date", "site", "rep", "depth"))
      # Add sensor data from supplementary material
        mb_n <- full_join(mb_n, mb_sensor, by = c("date", "site", "rep", "depth")) %>% arrange(date, depth) %>% select(-rep)
  
    # Repeat for Shelburne Pond data
      sp_n <- sp_n.raw %>% 
        select(Date.Collected, Depth.or.BLANK, Nox.Corrected, NH4.Corrected, TN.Corrected) %>% 
        rename(date=Date.Collected, samp_depth_cat=Depth.or.BLANK, NO3=Nox.Corrected, NH4=NH4.Corrected, TN=TN.Corrected) %>% 
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
  
      # Create a df with values to match and merge with sp_n
        sp_depths_df <- data.frame(date = rep(unique(sp_n$date), each=5), samp_depth_cat = rep(unique(sp_n$samp_depth_cat), 8), depth = sp_depths)
      # Add depths to sp_n & join 2015 P data
        sp_n <- full_join(sp_n, sp_depths_df, by = c("date", "samp_depth_cat")) %>% 
          # Add rep no.
          group_by(date, samp_depth_cat) %>% 
          mutate(rep = row_number()) %>% 
          # Join 2015 SP P data
          full_join(sp_P_15, by = c("date", "site", "rep", "depth"))        
      # Add sensor data from supplementary material
        sp_n <- full_join(sp_n, sp_sensor, by = c("date", "site", "rep", "depth")) %>% arrange(date, depth) %>% select(-rep)
      
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
      bind_rows(mb_2014 %>% mutate(Location = "Lake"), missRiv_chem, winter2015_chem_all %>% mutate(Location = "Lake")) %>% 
      # Convert N & P data from mg/L to umol/L
      mutate(TP = TP*1000/30.974,
             PP = PP*1000/30.974,
             TDP = TDP*1000/30.974,
             DOP = DOP*1000/30.974,
             SRP = SRP*1000/30.974,
             NO3 = NO3*1000/14.007,
             NH4 = NH4*1000/14.007,
             TN = TN*1000/14.007) %>% 
      select(site, location = Location, date, yday, depth, samp_depth_cat, samp_depth_cat2, NH4, NO3, TN, SRP, DOP, TDP, PP, TP, temp:BGA) %>% 
      arrange(site, location, date)
      # select(date:TN, tntp, temp:BGA, samp_depth_cat2) 

  # Remove unnecessary objects
    rm(depth_cat2, depth_index_2014, depth_index_2015, keys, mb_2014.raw, mb_depths, mb_depths_df, 
       mb_sensor_2014, mb2014_depths_df, mb_n, mb_n.raw, sp_depths_df, sp_n, sp_n.raw, mb_sensor, sp_sensor,
       mb_P_15, sp_P_15, mb_SRP)
    
  # Add ice depth data
    alldata <- alldata %>% 
      full_join(ice, by = c("site", "date", "yday")) %>% 
      select(site, location, date, yday, ice_depth = ice_depth_m, everything())
  }   

# Fix a few turbidity & nutrient outliers
  alldata <- 
    alldata %>% 
    mutate(turb = ifelse(site == "mb" & date == ymd("2015-02-04") & depth == 1.5, (2.3+4.4)/2,
                         ifelse(site == "mb" & date == ymd("2015-02-17") & depth == 3.1, 4.1,
                                ifelse(site == "mb" & date == ymd("2015-03-10") & depth == 1.0, (25.1+25.8)/2,
                                       ifelse(site == "sp" & date == ymd("2015-02-06") & depth == 4.6 & turb == 147.9, round((32.80+44.9+10.9)/3, 1),
                                              ifelse(site == "sp" & date == ymd("2015-02-18") & depth == 4.4, round((32.80+44.9+10.9)/3, 1), turb)))))) %>% 
    mutate(NO3 = ifelse(site == "sp" & samp_depth_cat2 == "Mid-3" & yday == 49 & NO3 > 10, NA, NO3)) %>% 
    mutate(NH4 = ifelse(site == "mb" & year(date) == 2015 & samp_depth_cat2 == "Mid-1" & yday == 48 & NH4 > 10, NA, NH4)) %>% 
    mutate(SRP = ifelse(site == "sp" & samp_depth_cat2 == "Mid-2" & yday == 78 & SRP > 0.02, NA, SRP)) %>%
    mutate(TP = ifelse(site == "sp" & samp_depth_cat2 == "Mid-2" & yday == 78 & TP > 2, NA, TP)) %>%
    mutate(DOP = ifelse(site == "sp" & samp_depth_cat2 == "Mid-2" & yday == 78 & DOP > 0.015, NA, DOP)) %>%
    mutate(PP = ifelse(site == "sp" & samp_depth_cat2 == "Mid-2" & yday == 78 & PP > 0.03, NA, PP))


# Write these data to a CSV file
  alldata %>%
    mutate(date = as.character(date)) %>% 
    write_csv("01_raw data/alldata_2014_2015_mb_sp_compiled.csv")
