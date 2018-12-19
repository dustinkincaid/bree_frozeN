# Load libraries
library(tidyverse)
library(lubridate)
library(data.table)
library(cowplot)

# TO DO: Do linear regressions of NO3 declines and add asterics to significant trends
#        Confirm depths for 2014 grab sample data
#        Plot DO & Chl a profiles
#        Figure out how to make filled contour plots like Ocean Data View plots in Joung paper
#        Calculate TN:TP molar ratios
#        Plot the ratios
#        Plot chla and oxygen data along with N species data
#          - will likely have to figure out how to no include rows with NA's if plotting at the same time as 

# Read in data
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
  }

# Tidy the data
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
      
  # Combine 2014 & 2015 data
    alldata <-
      bind_rows(mb_2014, winter2015_chem_all) %>% 
      select(date:TN, temp:BGA, samp_depth_cat2)
        
  # Remove unnecessary objects
    rm(depth_cat2, depth_index_2014, depth_index_2015, keys, mb_2014, mb_2014.raw, mb_depths, mb_depths_df, 
       mb_sensor_2014, mb2014_depths_df, mb_n, mb_n.raw, sp_depths_df, sp_n, sp_n.raw, mb_sensor, sp_sensor)
  }


# Interpolate 2015 sensor measurements & filter dataframe for rows with nutrient grab data
  # Here I use rule 2 on both left and right sides of the interval to get closest value for top and bottom depths if there was no value
  int_winter2015_chem_all <- winter2015_chem_all %>% 
    arrange(site, date, depth) %>% 
    group_by(site, date) %>% 
    mutate_at(vars(c(temp:BGA)),
              funs("i" = approx(x=depth, y=., xout=depth, rule=2, method="linear")[["y"]])) %>% 
    filter(!is.na(NO3)) %>% 
    select(date:samp_depth_cat, samp_depth_cat2, everything())


# ----Plot NO3 over time at each depth category----
  # Re-order samp_depth_cat2 levels
  mb_2014$samp_depth_cat2 <- factor(mb_2014$samp_depth_cat2, 
                                    levels = c("Top", "Mid-1", "Mid-2", "Mid-3", "Bottom"))
  
  # MB 2014
  p1 <-  mb_2014 %>% 
      filter(date < "2014-05-01") %>% 
      ggplot(aes(x=yday, y=NO3)) +
        geom_point() +
        geom_smooth(data = . %>% filter(yday < 79), method=lm, se=FALSE, color="black") +
        geom_vline(xintercept=79, linetype="dashed") + #Need to figure out exactly when thaw period began see Joung et al. 2017 & Schroth et al. 2015
        scale_y_continuous(limits=c(0, 0.8), 
                           breaks = seq(0, 0.8, by=0.2)) +
        facet_wrap(~samp_depth_cat2, ncol=1) +
        xlab("Julian Day") + ylab(expression("NO"[3]^-{}*" (mg N/L)")) +
        theme_bw() +
        theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())  
  
  # Re-order samp_depth_cat2 levels
  winter2015_chem_all$samp_depth_cat2 <- factor(winter2015_chem_all$samp_depth_cat2, 
                                                levels = c("Top", "Mid-1", "Mid-2", "Mid-3", "Bottom"))
  # MB 2015
  p2 <-  winter2015_chem_all %>% 
      filter(site == "mb", !is.na(NO3)) %>% 
      ggplot(aes(x=yday, y=NO3)) +
        geom_point() +
        geom_smooth(data = . %>% filter(yday < 70), method=lm, se=FALSE, color="black") +
        geom_vline(xintercept=70, linetype="dashed") +
        geom_vline(xintercept=85, linetype="dashed") +
        geom_vline(xintercept=95, linetype="dashed") +
        scale_y_continuous(limits=c(0, 0.8), 
                           breaks = seq(0, 0.8, by=0.2)) +
        facet_wrap(~samp_depth_cat2, ncol=1) +
        xlab("Julian Day") + ylab(expression("NO"[3]^-{}*" (mg N/L)")) +
        theme_bw() +
        theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())  

  # SP 2015
  p3 <-  winter2015_chem_all %>% 
      filter(site == "sp", !is.na(NO3)) %>% 
      ggplot(aes(x=yday, y=NO3)) +
        geom_point() +
        geom_smooth(data = . %>% filter(yday < 70), method=lm, se=FALSE, color="black") +
        geom_vline(xintercept=70, linetype="dashed") +
        geom_vline(xintercept=85, linetype="dashed") +
        geom_vline(xintercept=95, linetype="dashed") +
        scale_y_continuous(limits=c(0, 0.8), 
                           breaks = seq(0, 0.8, by=0.2)) +
        facet_wrap(~samp_depth_cat2, ncol=1) +
        xlab("Julian Day") + ylab(expression("NO"[3]^-{}*" (mg N/L)")) +
        theme_bw() +
        theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())
  
  # Combine these three plots into one plot
  fig_no3_ts <- plot_grid(p1, NULL, p2, p3, ncol = 2, align = "hv")
  save_plot("03_figures/fig_no3_ts.pdf", fig_no3_ts,
            base_height = 9, base_width = 8,
            dpi=150)
  save_plot("03_figures/fig_no3_ts.png", fig_no3_ts,
            base_height = 9, base_width = 8,
            dpi=150)  


# Which of the linear regressions are significant?
  # Not sure if I should get slopes (NO3 removal rates) from interaction term or individual regressions
  # They give similar significant trends, but slopes are slightly different
  # With the interaction term approach, the intercept is the mean for all depths
  # So, probably do individual regressions?
  summary(lm(NO3~yday:samp_depth_cat2, data = mb_2014 %>% filter(yday < 79)))
  summary(lm(NO3~yday, data = mb_2014 %>% filter(yday < 79, samp_depth_cat2=="Top")))
  summary(lm(NO3~yday, data = mb_2014 %>% filter(yday < 79, samp_depth_cat2=="Mid-1")))
  summary(lm(NO3~yday, data = mb_2014 %>% filter(yday < 79, samp_depth_cat2=="Mid-2")))
  summary(lm(NO3~yday, data = mb_2014 %>% filter(yday < 79, samp_depth_cat2=="Mid-3")))
  summary(lm(NO3~yday, data = mb_2014 %>% filter(yday < 79, samp_depth_cat2=="Bottom")))
  
  summary(lm(NO3~yday:samp_depth_cat2, data = winter2015_chem_all %>% filter(site == "mb", yday < 70)))
  summary(lm(NO3~yday, data = winter2015_chem_all %>% filter(site == "mb", samp_depth_cat2=="Top", yday < 70)))
  summary(lm(NO3~yday, data = winter2015_chem_all %>% filter(site == "mb", samp_depth_cat2=="Mid-1", yday < 70)))
  summary(lm(NO3~yday, data = winter2015_chem_all %>% filter(site == "mb", samp_depth_cat2=="Mid-2", yday < 70)))
  summary(lm(NO3~yday, data = winter2015_chem_all %>% filter(site == "mb", samp_depth_cat2=="Mid-3", yday < 70)))
  summary(lm(NO3~yday, data = winter2015_chem_all %>% filter(site == "mb", samp_depth_cat2=="Bottom", yday < 70)))
  
  summary(lm(NO3~yday:samp_depth_cat2, data = winter2015_chem_all %>% filter(site == "sp", yday < 70)))
  summary(lm(NO3~yday, data = winter2015_chem_all %>% filter(site == "sp", samp_depth_cat2=="Top", yday < 70)))
  summary(lm(NO3~yday, data = winter2015_chem_all %>% filter(site == "sp", samp_depth_cat2=="Mid-1", yday < 70)))
  summary(lm(NO3~yday, data = winter2015_chem_all %>% filter(site == "sp", samp_depth_cat2=="Mid-2", yday < 70)))
  summary(lm(NO3~yday, data = winter2015_chem_all %>% filter(site == "sp", samp_depth_cat2=="Mid-3", yday < 70)))
  summary(lm(NO3~yday, data = winter2015_chem_all %>% filter(site == "sp", samp_depth_cat2=="Bottom", yday < 70)))


# ----Plot winter2015_chem_all data to look for interesting trends----
# Profiles of each N species plotted by date
# Missisquoi Bay 2014
mb_2014 %>% 
  filter(date < "2014-05-01") %>% 
  select(-TP) %>% 
  gather(key="analyte", value="conc", c(NO3:TN)) %>% 
  group_by(yday, depth, analyte) %>% 
  summarize(mean.conc = mean(conc, na.rm = T)) %>% # average the replicate samples
  ggplot(aes(x=depth*-1, y=mean.conc, group=analyte, color=analyte)) + # depth*-1 makes depths negative
    geom_line() + geom_point() +
    coord_flip() + 
    scale_x_continuous(limits=c(-3.5, 0), 
                       breaks = seq(-3.5, 0, by=0.5)) +
    facet_wrap(~yday, ncol=8) +
    xlab("Depth (m)") + ylab("Conc. (mg N/L)") +
    theme_bw() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank())
  
ggsave("/Users/dustinkincaid/ownCloud/bree_frozeN/03_figures/2014_winterN_profiles_mp.png", 
       width=10, height=3, units="in", dpi=150)


# Missisquoi Bay 2015
# Plots of N species
winter2015_chem_all %>% 
  filter(site == "mb") %>% 
  select(-TP, -c(temp:BGA)) %>% 
  gather(key="analyte", value="conc", c(NO3:TN)) %>% 
  group_by(yday, depth, analyte) %>% 
  summarize(mean.conc = mean(conc, na.rm = T)) %>% #average the replicate samples
  filter(!is.na(mean.conc)) %>% 
  ggplot(aes(x=depth*-1, y=mean.conc, group=analyte, color=analyte)) + 
    geom_line() + geom_point() +
    coord_flip() + 
    scale_x_continuous(limits = c(-3.5, 0),
                       breaks = seq(-3.5, 0, by=0.5)) +
    facet_wrap(~yday, ncol=8) +
    xlab("Depth (m)") + ylab("Conc. (mg N/L)") +
    theme_bw() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank())

ggsave("/Users/dustinkincaid/ownCloud/bree_frozeN/03_figures/2015_winterN_profiles_mp.png", 
       width=10, height=3, units="in", dpi=150)

# MB - DO plots
winter2015_chem_all %>% 
  filter(site == "mb") %>% 
  select(date:samp_depth_cat, DO) %>% 
  #gather(key="param", value="conc", c(temp:BGA)) %>% 
  filter(!is.na(DO)) %>% 
  ggplot(aes(x=depth*-1, y=DO)) + 
    geom_line() + geom_point() +
    coord_flip() + 
    #scale_x_continuous(limits = c(-3.5, 0),
    #                   breaks = seq(-3.5, 0, by=0.5)) +
    facet_wrap(~yday, ncol=8) +
    xlab("Depth (m)") + ylab("Conc. (mg DO/L)") +
    theme_bw() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank())


# Shelburne Pond
# Plots of N species
winter2015_chem_all %>% 
  filter(site == "sp") %>% 
  select(-TP) %>% 
  gather(key="analyte", value="conc", c(NO3:TN)) %>% 
  group_by(yday, depth, analyte) %>% 
  summarize(mean.conc = mean(conc, na.rm = T)) %>% #average the replicate samples
  filter(!is.na(mean.conc)) %>% 
  ggplot(aes(x=depth*-1, y=mean.conc, group=analyte, color=analyte)) + 
    geom_line() + geom_point() +
    coord_flip() +
    scale_x_continuous(limits = c(-5, 0)) +
    facet_wrap(~yday, ncol=8) +
    xlab("Depth (m)") + ylab("Conc. (mg N/L)") +
    theme_bw() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank())

ggsave("/Users/dustinkincaid/ownCloud/bree_frozeN/03_figures/2015_winterN_profiles_sp.png", 
       width=10, height=3, units="in", dpi=150)


# SP - DO plots
winter2015_chem_all %>% 
  filter(site == "sp") %>% 
  select(date:samp_depth_cat, DO) %>% 
  #gather(key="param", value="conc", c(temp:BGA)) %>% 
  filter(!is.na(DO)) %>% 
  ggplot(aes(x=depth*-1, y=DO)) + 
    geom_line() + geom_point() +
    coord_flip() + 
    #scale_x_continuous(limits = c(-3.5, 0),
    #                   breaks = seq(-3.5, 0, by=0.5)) +
    facet_wrap(~yday, ncol=8) +
    xlab("Depth (m)") + ylab("Conc. (mg DO/L)") +
    theme_bw() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank())

# Plot N species over time grouped by depth
# Missisquoi Bay - 2014
mb_2014 %>% 
  filter(date < "2014-05-01") %>% 
  select(-TP) %>% 
  gather(key="analyte", value="conc", c(NO3:TN)) %>% 
  group_by(yday, depth, analyte) %>% 
  summarize(mean.conc = mean(conc, na.rm = T)) %>% 
  ggplot(aes(x=yday, y=mean.conc, group=analyte, color=analyte)) +
    geom_line() + geom_point() +
    geom_vline(xintercept=79, linetype="dashed") + #Need to figure out exactly when thaw period began see Joung et al. 2017 & Schroth et al. 2015
    facet_wrap(~depth, ncol=1) +
    xlab("Julian Day") + ylab("Conc. (mg N/L)") +
    theme_bw() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank())

ggsave("/Users/dustinkincaid/ownCloud/bree_frozeN/03_figures/2014_winterN_timeseries_mp.png", 
       width=5, height=6, units="in", dpi=150)

# Missisquoi Bay - 2015
winter2015_chem_all %>% 
  filter(site == "mb") %>% 
  select(-TP) %>% 
  gather(key="analyte", value="conc", c(NO3:TN)) %>% 
  group_by(yday, samp_depth_cat, analyte) %>% 
  summarize(mean.conc = mean(conc, na.rm = T)) %>% 
  filter(!is.na(samp_depth_cat)) %>% # removes NA sampling depth category from facetted plot
  ggplot(aes(x=yday, y=mean.conc, group=analyte, color=analyte)) +
    geom_line() + geom_point() +
    geom_vline(xintercept=70, linetype="dashed") +
    geom_vline(xintercept=85, linetype="dashed") +
    geom_vline(xintercept=95, linetype="dashed") +
    facet_wrap(~samp_depth_cat, ncol=1, drop=TRUE) +
    xlab("Julian Day") + ylab("Conc. (mg N/L)") +
    theme_bw() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank())      

ggsave("/Users/dustinkincaid/ownCloud/bree_frozeN/03_figures/2015_winterN_timeseries_mp.png", 
       width=5, height=6, units="in", dpi=150)

# Shelburne Pond
winter2015_chem_all %>% 
  filter(site == "sp") %>% 
  select(-TP) %>% 
  gather(key="analyte", value="conc", c(NO3:TN)) %>% 
  group_by(yday, samp_depth_cat, analyte) %>% 
  summarize(mean.conc = mean(conc, na.rm = T)) %>% 
  filter(!is.na(samp_depth_cat)) %>% # removes NA sampling depth category from facetted plot
  ggplot(aes(x=yday, y=mean.conc, group=analyte, color=analyte)) +
    geom_line() + geom_point() +
    geom_vline(xintercept=70, linetype="dashed") +
    geom_vline(xintercept=85, linetype="dashed") +
    geom_vline(xintercept=95, linetype="dashed") +
    facet_wrap(~samp_depth_cat, ncol=1) +
    xlab("Julian Day") + ylab("Conc. (mg N/L)") +
    theme_bw() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank())   
    
ggsave("/Users/dustinkincaid/ownCloud/bree_frozeN/03_figures/2015_winterN_timeseries_sp.png", 
       width=5, height=6, units="in", dpi=150)


# Profiles of TN:TP ratios plotted by date
# Missisquoi Bay 2014
mb_2014 %>% 
  filter(date < "2014-05-01") %>%
  select(-TP) %>% 
  gather(key="analyte", value="conc", c(NO3:TN)) %>% 
  group_by(yday, depth, analyte) %>% 
  summarize(mean.conc = mean(conc, na.rm = T)) %>% #average the replicate samples
  ggplot(aes(x=depth, y=mean.conc, group=analyte, color=analyte)) + 
    geom_line() + geom_point() +
    coord_flip() + scale_x_reverse() +
    facet_wrap(~yday, ncol=8) +
    xlab("Depth (m)") + ylab("Conc. (mg N/L)") +
    theme_bw() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank())
  
ggsave("/Users/dustinkincaid/ownCloud/bree_frozeN/03_figures/2014_winterN_profiles_mp.png", 
       width=10, height=3, units="in", dpi=150)



# --------------------------------------------------------------------