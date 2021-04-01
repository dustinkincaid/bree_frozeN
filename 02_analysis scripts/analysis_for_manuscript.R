# Use this script to visualize and analyze the data in Kincaid et al. submitted to Biogeochemistry Letters
# View outline to navigate sections
# Edited by DW Kincaid on 01 April 2021


# LOAD PACKAGES ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load("tidyverse", "lubridate", "dataRetrieval",
               "gridExtra", "gtable", "grid", "cowplot",
               "akima", "RColorBrewer", "directlabels",
               "broom", "patchwork")


# READ & TIDY DATA ----
# Data for Fig. 3 (mean temp, river stage, & ice thickness) ----
{
  # Daily air temperatures ----
  # Global Historical Climatology Network (GHCN) daily air temperatures from South Burlington, VT station
  # Downloaded on 2020-07-28 from NOAA's Nat'l Centers for Env. Info. Climate Date Online Search - https://www.ncdc.noaa.gov/cdo-web/search?datasetid=GHCND
  airT_eno <- read_csv("01_raw data/ghcn-daily_airTemp_enosburg.csv")
  airT_sb <- read_csv("01_raw data/ghcn-daily_airTemp_southBurlington.csv")
  airTemp <- bind_rows(airT_eno, airT_sb) %>% 
    # Make column names lower case
    rename_all(tolower) %>% 
    # Convert all temps from deg. F to deg. C
    mutate_at(vars(c(tmax:tavg)),
              ~((. - 32)*(5/9))) %>% 
    # Add yday
    mutate(yday = yday(date)) %>% 
    # Filter data by yday
    filter((yday >= 1 & yday <= 100) | (yday >= 335 & yday <= 365)) %>% 
    # Calculate an alternative temp average of t_max and t_min, because Enosburg data don't have daily temp average included, just max and min
    mutate(tavg_alt = ((tmax + tmin)/2)) %>% 
    # Provide a categorical for 2014 vs. 2015 winters
    mutate(winter = ifelse(date >= "2014-12-01", "winter2015", "winter2014")) %>% 
    # Add lake associated with each river
    mutate(lake = ifelse(name == "ENOSBURG FALLS 2, VT US", "mb", "sp"))
  rm(airT_eno, airT_sb)
  
  # Change yday 335-365 to -30 to 0
  # Create sequence
  yday_new <- data.frame(yday = seq(from = 335, to = 365, by = 1),
                         yday_new = seq(from = -30, to = 0, by = 1))
  
  # Join sequence to airTemp and sub -30 to 0 sequence for ydays 335 to 365
  airTemp <- airTemp %>% 
    full_join(yday_new, by = "yday") %>% 
    mutate(yday = ifelse(!is.na(yday_new), yday_new, yday)) %>% 
    select(-yday_new)
  
  # Discharge data from USGS ----
    # Provide information for data retrieval
      # 04293500 = East Berkshire
      # 04282795 = Laplatte River @ Shelburne Falls
    siteNos <- c("04293500", "04282795")
    pCodes <- c("00065", "00060")
    start.date <- "2013-12-01"
    end.date <- "2015-05-31"
  
    # Retrieve data as specified above
    str <- readNWISuv(siteNumbers = siteNos,
                      parameterCd = pCodes,
                      startDate = start.date,
                      endDate = end.date)
    
    # Tidy data
    str <- 
      # Function below is built in and should fix most/all of the column names
      renameNWISColumns(str) %>% 
      # Rename timestamp column 
      rename(timestamp = dateTime) %>% 
      # Provide column for stream names
      mutate(site_name = ifelse(site_no == "04293500", "eberk", "laplatte")) %>% 
      # Convert time from UTC to EST
      # The data are downloaded in UTC, so tell R this
      mutate(timestamp = ymd_hms(timestamp, tz = "UTC")) %>% 
      # Now convert the times from UTC to EST
      mutate(timestamp = force_tz(timestamp, tzone = "Etc/GMT+5")) %>% 
      # Select & arrange columns to keep
      select(agency_cd, site_no, site_name, timestamp, Flow_Inst, GH_Inst)
    
    # Calculate daily mean stage height & filter for just the 
    str_daily <- str %>% 
      mutate(date = date(timestamp)) %>% 
      group_by(site_name, date) %>% 
      summarize(GH_mean_ft = mean(GH_Inst, na.rm = TRUE)) %>% 
      mutate(GH_mean_m = GH_mean_ft*0.3048) %>% 
      # Add yday
      mutate(yday = yday(date)) %>% 
      # Filter data by yday
      filter((yday >= 1 & yday <= 100) | (yday >= 335 & yday <= 365))
    
    # Join sequence to airTemp and sub -30 to 0 sequence for ydays 335 to 365
    str_daily <- str_daily %>% 
      full_join(yday_new, by = "yday") %>% 
      mutate(yday = ifelse(!is.na(yday_new), yday_new, yday)) %>% 
      select(-yday_new)
    
  # Ice thickness ----
  ice <- read_csv("01_raw data/iceDepths.csv") %>% 
    mutate(date = mdy(date))
    
  # Read in the compiled sampling data ('alldata') to get sampling dates
  sampling_dates <- read_csv("01_raw data/alldata_2014_2015_mb_sp_compiled.csv") %>% 
    select(site, date, yday) %>% 
    distinct(site, date, yday)
 }  
  
  
# Data for Fig. 4 (contour plots of all vars); Fig. 5 (N profiles); ----
# Fig. S1 (distribution of N species for TN pool)
# Fig. S2 (compare lake vs. river NO3 concs)
  # All concentrations are in umol/L (uM)
  alldata <- read_csv("01_raw data/alldata_2014_2015_mb_sp_compiled.csv") %>% 
    # Focus on the lake (vs river grab) data
    filter(location == "Lake") %>% 
    # Calculate ratios
    mutate(no3_srp = NO3/SRP,
           din_srp = (NO3 + NH4)/SRP,
           tn_tp = TN/TP) 
  

# SUMMARY STATISTICS used throughout paper ----
# Look at max ice thickness
  alldata %>% 
    group_by(site, year(date)) %>% 
    slice(which.max(ice_depth))
  
# Look at ice thickness stats
  # We first calculate the mean for eaching sampling date, then the mean for the whole season
  alldata %>% 
    group_by(site, year(date), yday) %>% 
    summarize(mean_yday = mean(ice_depth, na.rm = T)) %>% 
    ungroup() %>% 
    group_by(site, `year(date)`) %>% 
    summarize(mean = mean(mean_yday, na.rm = T),
              sd = sd(mean_yday, na.rm = T))
  
  # Paired t-test to compare ice thicknesses b/w MB and SP in 2015
  # This is what Jason did in his MS
  ice2015 <- 
    alldata %>%
    filter(year(date) == 2015) %>% 
    filter(!is.na(ice_depth)) %>% 
    group_by(site, year(date), yday) %>% 
    summarize(mean_yday = mean(ice_depth, na.rm = T)) %>% 
    ungroup()
  
  t.test(mean_yday ~ site, data = ice2015, paired = TRUE)
  
# Look at chl a
  alldata %>% 
    group_by(site, year(date), yday) %>% 
    summarize(mean_yday = mean(chla, na.rm = T)) %>% 
    ungroup() %>% 
    group_by(site, `year(date)`) %>% 
    summarize(mean = mean(mean_yday, na.rm = T),
              sd = sd(mean_yday, na.rm = T))  
  
# Look at water temperatures
  alldata %>% 
    filter(yday < 100) %>% 
    group_by(site, year(date), yday) %>% 
    summarize(mean_yday = mean(temp, na.rm = T)) %>% 
    ungroup() %>% 
    group_by(site, `year(date)`) %>% 
    summarize(mean = mean(mean_yday, na.rm = T),
              sd = sd(mean_yday, na.rm = T))
    
  temp2015 <- 
    alldata %>%
    filter(year(date) == 2015) %>% 
    filter(!is.na(temp)) %>% 
    group_by(site, year(date), yday) %>% 
    summarize(mean_yday = mean(temp, na.rm = T)) %>% 
    ungroup()
  
  t.test(mean_yday ~ site, data = temp2015, paired = TRUE)    

# Look at DO  
  alldata %>%
    group_by(site, year(date)) %>%
    summarize(mean_do = mean(DO, na.rm = T),
              min_do = min(DO, na.rm = T),
              max_do = max(DO, na.rm = T))

  DO_mins_2015 <- alldata %>%
    filter(year(date) == 2015) %>% 
    filter(!is.na(DO)) %>%
    filter(depth >= 2) %>% 
    mutate(depth_range = cut_width(depth, width = 0.5, boundary = 0)) %>%
    group_by(site, yday, depth_range) %>% 
    slice(which.min(DO)) %>% 
    select(site:depth, depth_range, DO)
  
# Look at N data and means per depth
  # Overall means and SD
  alldata %>% 
    group_by(site, year(date), yday) %>% 
    summarize(across(c(NH4:TN), ~mean(.x, na.rm = T))) %>% 
    ungroup() %>% 
    group_by(site, `year(date)`) %>% 
    summarize(across(c(NH4:TN), list(mean = ~mean(.x, na.rm = T), sd = ~sd(.x, na.rm = T))))
  
  # Means per depth  
  justN <- alldata %>%
    filter(!is.na(samp_depth_cat)) %>%
    group_by(site, year(date), yday, samp_depth_cat) %>%
    summarize(NH4 = mean(NH4, na.rm = T),
              NO3 = mean(NO3, na.rm = T),
              TN = mean(TN, na.rm = T)) %>%
    arrange(site, `year(date)`, samp_depth_cat, yday)
  
  # Prop. of NO3 + NH4 as total N pool in SP and MB
  alldata %>% 
    group_by(site, year(date), yday) %>% 
    summarize(across(c(NH4:TN), ~mean(.x, na.rm = T))) %>% 
    ungroup() %>% 
    mutate(percent_DIN = round(((NH4 + NO3)/TN*100), 3)) %>% 
    group_by(site, `year(date)`) %>% 
    summarize(mean = mean(percent_DIN, na.rm = T))
  
# Look at N concentrations in MB on sampling day prior to the thaw period
  N_mb_preThaw <- alldata %>% 
    filter(site == "mb") %>% 
    filter((year(date) == 2014 & yday == 78) |
             (year(date) == 2015 & yday == 69)) %>% 
    filter(!is.na(NO3)) %>% 
    mutate(id = paste(site, substr(year(date), 3, 4), sep = "")) %>% 
    select(-c(location, date, yday, ice_depth, depth, SRP:tn_tp)) %>% 
    group_by(id, site, samp_depth_cat, samp_depth_cat2) %>% 
    summarize(across(.cols = c(NH4, NO3, TN), mean)) %>% 
    pivot_wider(names_from = id, values_from = NH4:TN) %>% 
    mutate(diffPer_NH4 = round((NH4_mb15 - NH4_mb14)/NH4_mb14 *100, 0),
           diffPer_NO3 = round((NO3_mb15 - NO3_mb14)/NO3_mb14 *100, 0),
           diffPer_TN = round((TN_mb15 - TN_mb14)/TN_mb14 *100, 0))


# CREATE Fig. 3 (mean temp, river stage, & ice thickness) ----
{
  # Set a theme for all plots
  theme1 <-
      theme_classic() +
      theme(legend.title = element_blank(),
            legend.text = element_text(size = 9),
            legend.spacing.x = unit(0.01, "in"),
            axis.text = element_text(size = 8),
            axis.title = element_text(size = 10),
            plot.tag = element_text(size = 12, face = "bold", vjust = -0.5),
            # Adjust plot margin: top, right, bottom, left
        plot.margin = unit(c(0, 0.05, 0, 0.05), "in"))
    
  # Air temperature ----
    p1 <- airTemp %>% 
      # Make a lake_winter code
      mutate(lake_winter = paste(lake, winter, sep = "_")) %>% 
      # Exclude sp_winter2014 & sp_winter2015
      filter(lake_winter != "sp_winter2014" & lake_winter != "sp_winter2015") %>% 
      # Plot
      ggplot(aes(x = yday, y = tavg_alt, group = lake_winter, color = lake_winter)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
      geom_line(size = 0.7) +
      geom_vline(xintercept = 78, linetype = "dashed", size = 0.7, color = "gray50") +
      geom_vline(xintercept = 70, linetype = "dashed", size = 0.7, color = "gray50") +
      scale_color_manual(breaks = c("mb_winter2014", "mb_winter2015", "sp_winter2015"),
                         labels = c("MB 2014", "MB 2015", "SP 2015"),
                         values = c("#56B4E9", "#0072B2", "#009E73")) +
      scale_x_continuous(limits = c(-10, 100), breaks = c(-30, 0, 30, 60, 90)) + 
      xlab("Day of year") +
      ylab(expression(Air~temperature~(degree*C))) +
      theme1 +
      theme(legend.position = "none",
            axis.text.x = element_blank(),
            axis.title.x = element_blank()) +
      labs(tag = "a")
    
  # River stage ----
    # Prepare the 2 data sets to plot
    # River stage
    str_daily_2 <- str_daily %>% 
      # Provide a categorical for 2014 vs. 2015 winters
      mutate(winter = ifelse(date >= "2014-12-01", "winter2015", "winter2014")) %>% 
      # Make a river_winter code
      mutate(river_winter = paste(site_name, winter, sep = "_")) %>% 
      # Exclude laplatte_winter2014
      filter(river_winter != "laplatte_winter2014")
  
    # Sampling dates
    sampling_dates_2 <- full_join(str_daily_2, sampling_dates %>% 
                                      mutate(site_name = ifelse(site == "mb", "eberk", "laplatte"),
                                      dummy_var = 1),
                          by = c("site_name", "date", "yday")) %>% 
      # Make dummy_var = gage height
      mutate(dummy_var = ifelse(!is.na(dummy_var), GH_mean_m, dummy_var)) %>% 
      # Filter out NA rows for dummy_var so rows include just sampling dates
      filter(!is.na(dummy_var)) %>% 
      # Drop original column GH_mean_m column
      select(-GH_mean_m) %>% 
      # Rename dummy_var as GH_mean_m
      rename(GH_mean_m = dummy_var)
    
    # Plot them together
    p2 <-
      ggplot() +
      geom_line(data = str_daily_2, aes(x = yday, y = GH_mean_m, group = river_winter, color = river_winter), size = 0.7) +
      geom_point(data = sampling_dates_2, aes(x = yday, y = GH_mean_m, group = river_winter, color = river_winter), shape = 4, size = 1.5, stroke = 1) +
      geom_vline(xintercept = 78, linetype = "dashed", size = 0.7, color = "gray50") +
      annotate("text", x=74.5, y=2.7, label="2014 thaw", size = 2.5, angle=90, color = "gray20") +
      geom_vline(xintercept = 70, linetype = "dashed", size = 0.7, color = "gray50") +
      annotate("text", x=66.5, y=2.7, label="2015 thaw", size = 2.5, angle=90, color = "gray20") +
      scale_color_manual(breaks = c("eberk_winter2014", "eberk_winter2015", "laplatte_winter2015"),
                         labels = c("MB 2014", "MB 2015", "SP 2015"),
                         values = c("#56B4E9", "#0072B2", "#009E73")) +
      scale_x_continuous(limits = c(-10, 100), breaks = c(-30, 0, 30, 60, 90)) + 
      xlab("Day of year") +
      ylab("River stage (m)") +
      theme1 +
      theme(legend.position = "none",
            axis.text.x = element_blank(),
            axis.title.x = element_blank()) +
      labs(tag = "b")
    
  # Ice thickness ----
    p3 <- ice %>% 
      # Provide a categorical for 2014 vs. 2015 winters
      mutate(winter = ifelse(date >= "2014-12-01", "winter2015", "winter2014")) %>% 
      # Make a site_winter code
      mutate(site_winter = paste(site, winter, sep = "_")) %>% 
      # Exclude laplatte_winter2014
      filter(site_winter != "sp_winter2014") %>% 
      # Filter out NA values
      filter(!is.na(ice_depth_m)) %>% 
      # Plot
      ggplot(aes(x = yday, y = ice_depth_m, group = site_winter, color = site_winter)) +
      geom_line(size = 0.7) +
      geom_point() +
      geom_vline(xintercept = 78, linetype = "dashed", size = 0.7, color = "gray50") +
      geom_vline(xintercept = 70, linetype = "dashed", size = 0.7, color = "gray50") +
      scale_color_manual(breaks = c("mb_winter2014", "mb_winter2015", "sp_winter2015"),
                         labels = c("MB 2014", "MB 2015", "SP 2015"),
                         values = c("#56B4E9", "#0072B2", "#009E73")) +
      scale_x_continuous(limits = c(-10, 100), breaks = c(-30, 0, 30, 60, 90)) + 
      xlab("Day of year") +
      ylab("Ice thickness (m)") +
      theme1 +
      theme(legend.position = c(0.2, 0.8)) +
      labs(tag = "c")    
      
  # Combine the subplots into one plot
  g1 <- ggplotGrob(p1)
  g2 <- ggplotGrob(p2)
  g3 <- ggplotGrob(p3)
  plot1 <- rbind(g1, g2, g3, size = "first")
  plot1$widths <- unit.pmax(g1$widths, g2$widths, g3$widths)
  grid.newpage()
  # grid.draw(g)
  grid::grid.draw(plot1)
 }  
  

# CREATE FIG. 4 (filled contour plots) ----
# This code will create plots with countour labels; the final figure in the MS was further edited using Inkscape
{
  # Set theme2 for plots
  theme2 <-
    theme_classic() +
    theme(
      # Adjust plot margin: top, right, bottom, left
      plot.margin = unit(c(0.05, 0.35, 0, 0.15), "in"),
      legend.title = element_blank(),
      legend.text = element_text(size = 9),
      legend.key.width = unit(0.1, "in"),
      legend.key.height = unit(0.18, "in"),
      legend.margin = margin(t=0, r=0, b=0, l=0, unit="in"),
      legend.position = c(1.07, 0.55),
      axis.title = element_blank())
  
  # Create plot function
  make_contourplot <- function(df, year, site_exp, var, leg_title, leg_lim_vec, leg_break_vec, cont_break_vec, 
                               col_guide = FALSE, ax_txt_x = FALSE, ax_txt_y = FALSE) {
     # Turn necessary variables into strings
     site_exp_enq <- enquo(site_exp)
     var_enq <- enquo(var)
     # Subset data by 
     df_sub <- df %>% 
       filter(year(date) == year & !!site_exp_enq & !is.na(!!var_enq) & yday < 100) %>%
       # filter(year(date) == year & !!site_exp_enq & !is.na(!!var_enq)) %>%
       select(date, yday, depth, !!var_enq) %>% 
       group_by(date, yday, depth) %>% 
       # Take the mean of any variable measurements at the same time and depth
       summarize(var = mean(!!var_enq, na.rm = T)) %>% 
       # Change the minimum depth (closest to 0) for each sampling date to 0; this is what D Joung did for figures in 2017 paper
       group_by(yday) %>% 
       mutate(depth = ifelse(depth == min(depth), 0, depth)) %>% 
       # Convert depths to negative for plotting purposes
       mutate(depth = depth*-1)
     
      # Interpolate data (akima::interp does gridded bivariate interpolation for irregular data)     
      int <- with(df_sub, interp(x = yday, y = depth, z = var, nx = 100, ny = 100))
      # Create a data.frame frame from the interp() result
      df_int <- interp2xyz(int, data.frame = TRUE) %>%
        filter(!is.na(z)) %>%
        rename(yday = x, depth = y, var = z) %>%
        arrange(yday, desc(depth))
      
      # Plot
      plot <- ggplot(data = df_int, aes(x = yday, y = depth, z = var)) +
        geom_tile(aes(fill = var)) +
        scale_fill_gradientn(colors = brewer.pal(n = 11, name = "Spectral"), trans = "reverse",
                             guide = if (col_guide) guide_colorbar(reverse = TRUE) else NULL,
                             limits = leg_lim_vec, # choose this range based on range of all plots (i.e., 2014 MB & 2015 SP)
                             breaks = leg_break_vec,
                             name = leg_title) +
        # Day separating cold from thaw period (78 for 2014, 70 for 2015)
        geom_vline(xintercept=ifelse(year == 2014, 78, 70), linetype="dashed", color = "black", size = 0.5) +
        geom_point(data = df_sub, aes(x = yday, y = depth), color = "white", shape = 1, size = 0.3) +
        geom_contour(color = "gray50", breaks = cont_break_vec, size = 0.25) +
        # This will add contour labels
        geom_dl(aes(label = ..level..), breaks = cont_break_vec, method = list("bottom.pieces", cex = 0.5), stat="contour", color = "gray20") +
        scale_x_continuous(limits = c(10, 100), breaks = c(20, 40, 60, 80, 100)) +
        xlab("Day of year") + ylab("Depth (m)") +
        theme2 +
        theme(axis.text.x = if (ax_txt_x) element_text(size = 9) else element_blank(),
              axis.text.y = if (ax_txt_y) element_text(size = 9) else element_blank())
     
     return(plot)
    }  

  # Create plots
  {
  # Temp
    # MB 2014
    p_temp_mb_2014 <- make_contourplot(df = alldata, year = 2014, site_exp = site == "mb", var = temp, leg_title = expression(Temp.~(degree*C)), 
                             leg_lim_vec = c(6,0), leg_break_vec = c(0, 2, 4, 6), cont_break_vec = c(0,1,2,3,4,5,6), ax_txt_y = TRUE)
    # MB 2015
    p_temp_mb_2015 <- make_contourplot(df = alldata, year = 2015, site_exp = site == "mb", var = temp, leg_title = expression(Temp.~(degree*C)),
                             leg_lim_vec = c(6,0), leg_break_vec = c(0, 2, 4, 6), cont_break_vec = c(0,1,2,3,4,5,6), ax_txt_y = FALSE)
    # SP 2015
    p_temp_sp_2015 <- make_contourplot(df = alldata, year = 2015, site_exp = site == "sp", var = temp, leg_title = expression(Temp.~(degree*C)),
                             leg_lim_vec = c(6,0), leg_break_vec = c(0, 2, 4, 6), cont_break_vec = c(0,1,2,3,4,5,6), ax_txt_x = FALSE, ax_txt_y = FALSE, col_guide = TRUE)
  # DO
    # MB 2014
    p_do_mb_2014 <- make_contourplot(df = alldata, year = 2014, site_exp = site == "mb", var = DO, leg_title = expression(DO~(mg~l^{-1})), 
                             leg_lim_vec = c(20,0), leg_break_vec = c(0,10,20), cont_break_vec = c(0,5,10,15,20), ax_txt_y = TRUE)
    # MB 2015
    p_do_mb_2015 <- make_contourplot(df = alldata, year = 2015, site_exp = site == "mb", var = DO, leg_title = expression(DO~(mg~l^{-1})),  
                             leg_lim_vec = c(20,0), leg_break_vec = c(0,10,20), cont_break_vec = c(0,5,10,15,20))
    # SP 2015
    p_do_sp_2015 <- make_contourplot(df = alldata, year = 2015, site_exp = site == "sp", var = DO, leg_title = expression(DO~(mg~l^{-1})),  
                             leg_lim_vec = c(20,0), leg_break_vec = c(0,10,20), cont_break_vec = c(0,5,10,15,20), ax_txt_x = FALSE, col_guide = TRUE)
  # chl a
    # MB 2014
    p_chla_mb_2014 <- make_contourplot(df = alldata, year = 2014, site_exp = site == "mb", var = chla, leg_title = expression(Chl~italic(a)~(mu*g~l^{-1})), 
                             leg_lim_vec = c(50,0), leg_break_vec = c(0,25,50), cont_break_vec = c(0,10,20,30,40,50), ax_txt_y = TRUE)
    # MB 2015
    p_chla_mb_2015 <- make_contourplot(df = alldata, year = 2015, site_exp = site == "mb", var = chla, leg_title = expression(Chl~italic(a)~(mu*g~l^{-1})), 
                             leg_lim_vec = c(50,0), leg_break_vec = c(0,25,50), cont_break_vec = c(0,10,20,30,40,50))
    # SP 2015
    p_chla_sp_2015 <- make_contourplot(df = alldata, year = 2015, site_exp = site == "sp", var = chla, leg_title = expression(Chl~italic(a)~(mu*g~l^{-1})),
                             leg_lim_vec = c(50,0), leg_break_vec = c(0,25,50), cont_break_vec = c(0,10,20,30,40,50), ax_txt_x = FALSE, col_guide = TRUE)
    
  # NH4 - umol N/L
    # MB 2014                                                                                                 
    p_nh4_mb_2014 <- make_contourplot(df = alldata, year = 2014, site_exp = site == "mb", var = NH4, leg_title = expression(paste("NH"["4"]^" +", " (",mu,"mol"," l"^"-1",")")), 
                             leg_lim_vec = c(40,0), leg_break_vec = c(0,20,40), cont_break_vec = c(0,10,20,30,40), ax_txt_y = TRUE)
    # MB 2015
    p_nh4_mb_2015 <- make_contourplot(df = alldata, year = 2015, site_exp = site == "mb", var = NH4, leg_title = expression(paste("NH"["4"]^" +", " (",mu,"mol"," l"^"-1",")")), 
                             leg_lim_vec = c(40,0), leg_break_vec = c(0,20,40), cont_break_vec = c(0,10,20,30,40), ax_txt_y = FALSE)
    # SP 2015
    p_nh4_sp_2015 <- make_contourplot(df = alldata, year = 2015, site_exp = site == "sp", var = NH4, leg_title = expression(paste("NH"["4"]^" +", " (",mu,"mol"," l"^"-1",")")),
                             leg_lim_vec = c(40,0), leg_break_vec = c(0,20,40), cont_break_vec = c(0,10,20,30,40), ax_txt_x = FALSE, ax_txt_y = FALSE, col_guide = TRUE)   
  # NO3 - umol N/L
    # MB 2014
    p_no3_mb_2014 <- make_contourplot(df = alldata, year = 2014, site_exp = site == "mb", var = NO3, leg_title = expression(paste("NO"["3"]^" -", " (",mu,"mol"," l"^"-1",")")), 
                             leg_lim_vec = c(60,0), leg_break_vec = c(0,20,40,60), cont_break_vec = c(0,10,20,30,40,50,60), ax_txt_y = TRUE)
    # MB 2015
    p_no3_mb_2015 <- make_contourplot(df = alldata, year = 2015, site_exp = site == "mb", var = NO3, leg_title = expression(paste("NO"["3"]^" -", " (",mu,"mol"," l"^"-1",")")), 
                             leg_lim_vec = c(60,0), leg_break_vec = c(0,20,40,60), cont_break_vec = c(0,10,20,30,40,50,60))
    # SP 2015
    p_no3_sp_2015 <- make_contourplot(df = alldata, year = 2015, site_exp = site == "sp", var = NO3, leg_title = expression(paste("NO"["3"]^" -", " (",mu,"mol"," l"^"-1",")")),
                             leg_lim_vec = c(60,0), leg_break_vec = c(0,20,40,60), cont_break_vec = c(0,10,20,30,40,50,60), ax_txt_x = FALSE, col_guide = TRUE)
  # TN - umol N/L
    # MB 2014
    p_tn_mb_2014 <- make_contourplot(df = alldata, year = 2014, site_exp = site == "mb", var = TN, leg_title = expression(TN~(mu*mol~l^{-1})), 
                             leg_lim_vec = c(120,0), leg_break_vec = c(0,40,80,120), cont_break_vec = c(0,20,40,60,80,100,120), ax_txt_y = TRUE, ax_txt_x = TRUE)
    # MB 2015
    p_tn_mb_2015 <- make_contourplot(df = alldata, year = 2015, site_exp = site == "mb", var = TN, leg_title = expression(TN~(mu*mol~l^{-1})), 
                             leg_lim_vec = c(120,0), leg_break_vec = c(0,40,80,120), cont_break_vec = c(0,20,40,60,80,100,120), ax_txt_x = TRUE)
    # SP 2015
    p_tn_sp_2015 <- make_contourplot(df = alldata, year = 2015, site_exp = site == "sp", var = TN, leg_title = expression(TN~(mu*mol~l^{-1})), 
                             leg_lim_vec = c(120,0), leg_break_vec = c(0,40,80,120), cont_break_vec = c(0,20,40,60,80,100,120), ax_txt_x = TRUE, col_guide = TRUE)
    
      
  # Create titles for figures
  title_2014MB <- textGrob("2014 MB", gp = gpar(fontsize = 10), vjust = 0.5)
  title_2015MB <- textGrob("2015 MB", gp = gpar(fontsize = 10), vjust = 0.5)
  title_2015SP <- textGrob("2015 SP", gp = gpar(fontsize = 10), vjust = 0.5)  
  
  # Arrange plots and add column titles in this first grob object
  grob1 <- grid.arrange(arrangeGrob(p_temp_mb_2014, p_do_mb_2014, p_chla_mb_2014, p_nh4_mb_2014, p_no3_mb_2014, p_tn_mb_2014, top = title_2014MB, ncol = 1),
                       arrangeGrob(p_temp_mb_2015, p_do_mb_2015, p_chla_mb_2015, p_nh4_mb_2015, p_no3_mb_2015, p_tn_mb_2015, top = title_2015MB, ncol = 1),
                       arrangeGrob(p_temp_sp_2015, p_do_sp_2015, p_chla_sp_2015, p_nh4_sp_2015, p_no3_sp_2015, p_tn_sp_2015, top = title_2015SP, ncol = 1),
                       ncol = 3)
  
  # Add common x and y axis titles
  y.grob <- textGrob("Depth (m)", gp = gpar(fontsize = 10), rot = 90, vjust = 1.25)
  x.grob <- textGrob("Day of year", gp = gpar(fontsize = 10))
  grob2 <- grid.arrange(arrangeGrob(grob1, left = y.grob, bottom = x.grob))
  
  }
  # The remaining edits (adding contour labels, etc) are done manually using Inkscape  
}
  
# LINEAR REGRESSIONS for Fig. 5 (NO3/NH4/TN ~ DOY) ----
{  
  # Fit the linear regression models for each site, year, depth, and analyte combo
  lm_results <- alldata %>%
    mutate(samp_depth_cat2 = factor(samp_depth_cat2, levels = c("Top", "Mid-1", "Mid-2", "Mid-3", "Bottom"))) %>% 
    filter(!is.na(samp_depth_cat2)) %>% 
    # Create a year column
    mutate(year = year(date)) %>% 
    # Let's filter the data to only do regressions on data before the thaw
    filter((site == "mb" & year == 2014 & yday < 79) |
             (site == "mb" & year == 2015 & yday < 70) |
             (site == "sp" & year == 2015 & yday < 70)) %>% 
    # To simplify the df, select relevant columns
    select(site, year, date, yday, depth, samp_depth_cat, samp_depth_cat2, NH4:TN, SRP, TP) %>% 
    # Pivot the measured variables into long format
    pivot_longer(cols = c(NH4:TN, SRP, TP), names_to = "analyte", values_to = "conc") %>% 
    # Group and nest the groupings - one regression for each transect date, reach section, and var
    group_by(site, year, samp_depth_cat2, analyte) %>% 
    nest() %>% 
    # Here is where we regress the value of the variable on distance along reach
    mutate(model = map(data, ~lm(conc ~ yday, data = .x)),
           tidied = map(model, tidy),
           glanced = map(model, glance))
  
  # Get the coefficient estimates and stats
  lm_results_coef <- lm_results %>% 
    # Unnesting 'tidied' will give you a summary of the coefficients
    unnest(tidied) %>%
    select(-c(data, model, glanced)) %>% 
    pivot_wider(names_from = term, values_from = c(estimate:p.value))
  
  # Get R^2 and other summary stats
  lm_results_r2 <- lm_results %>% 
    # Unnesting 'tidied' will give you a summary of the coefficients
    unnest(glanced) %>% 
    select(-c(data, model, tidied, sigma, statistic, p.value, df, logLik, AIC, BIC, deviance, df.residual))
  
  # Join these together
  lm_results <- full_join(lm_results_coef, lm_results_r2, by = c("site", "year", "samp_depth_cat2", "analyte")) %>% 
    # Drop unnecessary columns
    select(-c(`std.error_(Intercept)`, `statistic_(Intercept)`, `p.value_(Intercept)`)) %>% 
    # Round estimate, p-value, and r2
    mutate_at(vars(`estimate_(Intercept)`:adj.r.squared),
              ~round(., 3))
  rm(lm_results_coef, lm_results_r2)
 }  

# ESTIMATE FIRST-ORDER RATE CONSTANT (k) for Supp Info ----
  {  
# Also going to log-transform nutrient concentrations to estimate first-order rate constant (k) for Supp Info
lm_results_k <- alldata %>%
  mutate(samp_depth_cat2 = factor(samp_depth_cat2, levels = c("Top", "Mid-1", "Mid-2", "Mid-3", "Bottom"))) %>%
  filter(!is.na(samp_depth_cat2)) %>%
  # Create a year column
  mutate(year = year(date)) %>%
  # Let's filter the data to only do regressions on data before the thaw
  filter((site == "mb" & year == 2014 & yday < 79) |
           (site == "mb" & year == 2015 & yday < 70) |
           (site == "sp" & year == 2015 & yday < 70)) %>%
  # To simplify the df, select relevant columns
  select(site, year, date, yday, depth, samp_depth_cat, samp_depth_cat2, NH4:TN, SRP, TP) %>%
  # Pivot the measured variables into long format
  pivot_longer(cols = c(NH4:TN, SRP, TP), names_to = "analyte", values_to = "conc") %>%
  # Group and nest the groupings - one regression for each transect date, reach section, and var
  group_by(site, year, samp_depth_cat2, analyte) %>%
  nest() %>%
  # Here is where we regress the value of the variable on distance along reach
  mutate(model = map(data, ~lm(log(conc) ~ yday, data = .x)),
         tidied = map(model, tidy),
         glanced = map(model, glance))

# Get the coefficient estimates and stats
lm_results_k_coef <- lm_results_k %>%
  # Unnesting 'tidied' will give you a summary of the coefficients
  unnest(tidied) %>%
  select(-c(data, model, glanced)) %>%
  pivot_wider(names_from = term, values_from = c(estimate:p.value))

# Get R^2 and other summary stats
lm_results_k_r2 <- lm_results_k %>%
  # Unnesting 'tidied' will give you a summary of the coefficients
  unnest(glanced) %>%
  select(-c(data, model, tidied, sigma, statistic, p.value, df, logLik, AIC, BIC, deviance, df.residual))

# Join these together
lm_results_k <- full_join(lm_results_k_coef, lm_results_k_r2, by = c("site", "year", "samp_depth_cat2", "analyte")) %>%
  # Drop unnecessary columns
  select(-c(`std.error_(Intercept)`, `statistic_(Intercept)`, `p.value_(Intercept)`)) %>%
  # Round estimate, p-value, and r2
  mutate_at(vars(`estimate_(Intercept)`:adj.r.squared),
            ~round(., 3))
rm(lm_results_k_coef, lm_results_k_r2)

# Look at only relationships for N species w/ p < 0.05
lm_results_k_sig <-
  lm_results_k %>%
  filter(analyte %in% c("NH4", "NO3", "TN")) %>%
  filter(p.value_yday < 0.05)
}
    
# CREATE FIG. 5 (O3/NH4/TN ~ DOY) ----
# Note: regression eqs. were added to plots manually in Inkscape
{
# NO3 plots ----
  # MB 2014
  pl_mb14 <- alldata %>% 
    # Add year column
    mutate(year = year(date)) %>% 
    # Filter site and year
    filter(site == "mb" & year == 2014) %>% 
    filter(date < "2014-04-10") %>% 
    # Order the sample depth categories
    mutate(samp_depth_cat2 = factor(samp_depth_cat2, levels = c("Top", "Mid-1", "Mid-2", "Mid-3", "Bottom"))) %>% 
    filter(!is.na(samp_depth_cat2)) %>% 
    # Join NO3~yday linear regression results
    left_join(lm_results %>% filter(analyte == "NO3"), by = c("site", "year", "samp_depth_cat2")) %>% 
    ggplot(aes(x=yday, y=NO3)) +
      facet_wrap(~samp_depth_cat2, ncol=1) +
      geom_point() +
      geom_smooth(data = . %>% filter(p.value_yday < 0.05 & yday < 79), method=lm, se=FALSE, color="black") +
      geom_vline(xintercept=79, linetype="dashed") + #Need to figure out exactly when thaw period began see Joung et al. 2017 & Schroth et al. 2015
      scale_y_continuous(limits=c(0, 65),
                         breaks = seq(0, 60, by = 30)) +
      scale_x_continuous(limits=c(10, 100),
                         breaks = seq(0, 100, by = 20)) +  
      xlab("Day of year") + 
      ylab(expression(paste("NO"["3"]^" -", " (",mu,"mol"," l"^"-1",")"))) +
      theme2 +
      annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf) +
      annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf) +
      ggtitle("MB 2014")
  
  # MB 2015
  pl_mb15 <-  alldata %>% 
    # Add year column
    mutate(year = year(date)) %>%   
    # Filter site and year
    filter(site == "mb" & year == 2015) %>% 
    filter(!is.na(NO3)) %>% 
    # Order the sample depth categories
    mutate(samp_depth_cat2 = factor(samp_depth_cat2, levels = c("Top", "Mid-1", "Mid-2", "Mid-3", "Bottom"))) %>% 
    filter(!is.na(samp_depth_cat2)) %>% 
    # Join NO3~yday linear regression results
    left_join(lm_results %>% filter(analyte == "NO3"), by = c("site", "year", "samp_depth_cat2")) %>%   
    ggplot(aes(x=yday, y=NO3)) +
      facet_wrap(~samp_depth_cat2, ncol=1) +
      geom_point() +
      geom_smooth(data = . %>% filter(p.value_yday < 0.05 & yday < 70), method=lm, se=FALSE, color="black") +
      geom_vline(xintercept=70, linetype="dashed") +
      geom_vline(xintercept=85, linetype="dashed") +
      geom_vline(xintercept=95, linetype="dashed") +
      scale_y_continuous(limits=c(0, 65),
                         breaks = seq(0, 60, by = 30)) +
      scale_x_continuous(limits=c(10, 100),
                         breaks = seq(0, 100, by = 20)) +      
      xlab("Day of year") +
      ylab(expression(paste("NO"["3"]^" -", " (",mu,"mol"," l"^"-1",")"))) +
      theme2 +
      annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf) +
      annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf) +
      ggtitle("MB 2015")

  # SP 2015
  pl_sp15 <-  alldata %>% 
    # Add year column
    mutate(year = year(date)) %>%   
    # Filter site and year
    filter(site == "sp" & year == 2015) %>% 
    filter(!is.na(NO3)) %>% 
    # Order the sample depth categories
    mutate(samp_depth_cat2 = factor(samp_depth_cat2, levels = c("Top", "Mid-1", "Mid-2", "Mid-3", "Bottom"))) %>% 
    filter(!is.na(samp_depth_cat2)) %>% 
    # Join NO3~yday linear regression results
    left_join(lm_results %>% filter(analyte == "NO3"), by = c("site", "year", "samp_depth_cat2")) %>% 
    ggplot(aes(x=yday, y=NO3)) +
      geom_point() +
      geom_smooth(data = . %>% filter(p.value_yday < 0.05, yday < 70), method=lm, se=FALSE, color="black") +
      geom_vline(xintercept=70, linetype="dashed") +
      geom_vline(xintercept=85, linetype="dashed") +
      geom_vline(xintercept=95, linetype="dashed") +
      scale_y_continuous(limits=c(0, 65),
                         breaks = seq(0, 60, by = 30)) +
      scale_x_continuous(limits=c(10, 100),
                         breaks = seq(0, 100, by = 20)) +    
      facet_wrap(~samp_depth_cat2, ncol=1) +
      xlab("Day of year") + 
      ylab(expression(paste("NO"["3"]^" -", " (",mu,"mol"," l"^"-1",")"))) +
      theme2 +
      annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf) +
      annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf) +
      ggtitle("SP 2015")
  
# NH4 plots ----
  # MB 2014
  pl_mb14_nh4 <- alldata %>% 
    # Add year column
    mutate(year = year(date)) %>% 
    # Filter site and year
    filter(site == "mb" & year == 2014) %>% 
    filter(date < "2014-04-10") %>% 
    # Order the sample depth categories
    mutate(samp_depth_cat2 = factor(samp_depth_cat2, levels = c("Top", "Mid-1", "Mid-2", "Mid-3", "Bottom"))) %>% 
    filter(!is.na(samp_depth_cat2)) %>% 
    # Join NH4~yday linear regression results
    left_join(lm_results %>% filter(analyte == "NH4"), by = c("site", "year", "samp_depth_cat2")) %>% 
    ggplot(aes(x=yday, y=NH4)) +
      facet_wrap(~samp_depth_cat2, ncol=1) +
      geom_point() +
      geom_smooth(data = . %>% filter(p.value_yday < 0.05 & yday < 79), method=lm, se=FALSE, color="black") +
      geom_vline(xintercept=79, linetype="dashed") + #Need to figure out exactly when thaw period began see Joung et al. 2017 & Schroth et al. 2015
      scale_y_continuous(limits=c(0, 30),
                   breaks = seq(0, 30, by = 15)) +
      scale_x_continuous(limits=c(10, 100),
                         breaks = seq(0, 100, by = 20)) +        
      xlab("Day of year") + 
      ylab(expression(paste("NH"["4"]^" +", " (",mu,"mol"," l"^"-1",")"))) +
      theme2 +
      annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf) +
      annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf) +
      ggtitle("MB 2014")
  
  # MB 2015
  pl_mb15_nh4 <-  alldata %>% 
    # Add year column
    mutate(year = year(date)) %>%   
    # Filter site and year
    filter(site == "mb" & year == 2015) %>% 
    filter(!is.na(NH4)) %>% 
    # Order the sample depth categories
    mutate(samp_depth_cat2 = factor(samp_depth_cat2, levels = c("Top", "Mid-1", "Mid-2", "Mid-3", "Bottom"))) %>% 
    filter(!is.na(samp_depth_cat2)) %>% 
    # Join NH4~yday linear regression results
    left_join(lm_results %>% filter(analyte == "NH4"), by = c("site", "year", "samp_depth_cat2")) %>%   
    ggplot(aes(x=yday, y=NH4)) +
      facet_wrap(~samp_depth_cat2, ncol=1) +
      geom_point() +
      geom_smooth(data = . %>% filter(p.value_yday < 0.05 & yday < 70), method=lm, se=FALSE, color="black") +
      scale_y_continuous(limits=c(0, 30),
                   breaks = seq(0, 30, by = 15)) +  
      scale_x_continuous(limits=c(10, 100),
                         breaks = seq(0, 100, by = 20)) +      
      geom_vline(xintercept=70, linetype="dashed") +
      geom_vline(xintercept=85, linetype="dashed") +
      geom_vline(xintercept=95, linetype="dashed") +
      xlab("Day of year") +
      ylab(expression(paste("NH"["4"]^" +", " (",mu,"mol"," l"^"-1",")"))) +
      theme2 +
      annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf) +
      annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf) +
      ggtitle("MB 2015")

  # SP 2015
  pl_sp15_nh4 <-  alldata %>% 
    # Add year column
    mutate(year = year(date)) %>%   
    # Filter site and year
    filter(site == "sp" & year == 2015) %>% 
    filter(!is.na(NH4)) %>% 
    # Order the sample depth categories
    mutate(samp_depth_cat2 = factor(samp_depth_cat2, levels = c("Top", "Mid-1", "Mid-2", "Mid-3", "Bottom"))) %>% 
    filter(!is.na(samp_depth_cat2)) %>% 
    # Join NH4~yday linear regression results
    left_join(lm_results %>% filter(analyte == "NH4"), by = c("site", "year", "samp_depth_cat2")) %>% 
    ggplot(aes(x=yday, y=NH4)) +
      geom_point() +
      geom_smooth(data = . %>% filter(p.value_yday < 0.05, yday < 70), method=lm, se=FALSE, color="black") +
      scale_y_continuous(limits=c(0, 30),
                   breaks = seq(0, 30, by = 15)) +
      scale_x_continuous(limits=c(10, 100),
                         breaks = seq(0, 100, by = 20)) +      
      geom_vline(xintercept=70, linetype="dashed") +
      geom_vline(xintercept=85, linetype="dashed") +
      geom_vline(xintercept=95, linetype="dashed") +
      facet_wrap(~samp_depth_cat2, ncol=1) +
      xlab("Day of year") + 
      ylab(expression(paste("NH"["4"]^" +", " (",mu,"mol"," l"^"-1",")"))) +
      theme2 +
      annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf) +
      annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf) +
      ggtitle("SP 2015")
  
# TN plots ----
  # MB 2014
  pl_mb14_tn <- alldata %>% 
    # Add year column
    mutate(year = year(date)) %>% 
    # Filter site and year
    filter(site == "mb" & year == 2014) %>% 
    filter(date < "2014-04-10") %>%  
    # Order the sample depth categories
    mutate(samp_depth_cat2 = factor(samp_depth_cat2, levels = c("Top", "Mid-1", "Mid-2", "Mid-3", "Bottom"))) %>% 
    filter(!is.na(samp_depth_cat2)) %>% 
    # Join TN~yday linear regression results
    left_join(lm_results %>% filter(analyte == "TN"), by = c("site", "year", "samp_depth_cat2")) %>% 
    ggplot(aes(x=yday, y=TN)) +
      facet_wrap(~samp_depth_cat2, ncol=1) +
      geom_point() +
      geom_smooth(data = . %>% filter(p.value_yday < 0.05 & yday < 79), method=lm, se=FALSE, color="black") +
      geom_vline(xintercept=79, linetype="dashed") + #Need to figure out exactly when thaw period began see Joung et al. 2017 & Schroth et al. 2015
      scale_x_continuous(limits=c(10, 100),
                         breaks = seq(0, 100, by = 20)) +       
      xlab("Day of year") + 
      ylab(expression(paste("TN", " (",mu,"mol"," l"^"-1",")"))) +
      theme2 +
      annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf) +
      annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf) +
      ggtitle("MB 2014")
  
  # MB 2015
  pl_mb15_tn <-  alldata %>% 
    # Add year column
    mutate(year = year(date)) %>%   
    # Filter site and year
    filter(site == "mb" & year == 2015) %>% 
    filter(!is.na(TN)) %>% 
    # Order the sample depth categories
    mutate(samp_depth_cat2 = factor(samp_depth_cat2, levels = c("Top", "Mid-1", "Mid-2", "Mid-3", "Bottom"))) %>% 
    filter(!is.na(samp_depth_cat2)) %>% 
    # Join TN~yday linear regression results
    left_join(lm_results %>% filter(analyte == "TN"), by = c("site", "year", "samp_depth_cat2")) %>%   
    ggplot(aes(x=yday, y=TN)) +
      facet_wrap(~samp_depth_cat2, ncol=1) +
      geom_point() +
      geom_smooth(data = . %>% filter(p.value_yday < 0.05 & yday < 70), method=lm, se=FALSE, color="black") +
      geom_vline(xintercept=70, linetype="dashed") +
      geom_vline(xintercept=85, linetype="dashed") +
      geom_vline(xintercept=95, linetype="dashed") +
      scale_x_continuous(limits=c(10, 100),
                         breaks = seq(0, 100, by = 20)) +    
      xlab("Day of year") +
      ylab(expression(paste("TN", " (",mu,"mol"," l"^"-1",")"))) +
      theme2 +
      annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf) +
      annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf) +
      ggtitle("MB 2015")

  # SP 2015
  pl_sp15_tn <-  alldata %>% 
    # Add year column
    mutate(year = year(date)) %>%   
    # Filter site and year
    filter(site == "sp" & year == 2015) %>% 
    filter(!is.na(TN)) %>% 
    # Order the sample depth categories
    mutate(samp_depth_cat2 = factor(samp_depth_cat2, levels = c("Top", "Mid-1", "Mid-2", "Mid-3", "Bottom"))) %>% 
    filter(!is.na(samp_depth_cat2)) %>% 
    # Join TN~yday linear regression results
    left_join(lm_results %>% filter(analyte == "TN"), by = c("site", "year", "samp_depth_cat2")) %>% 
    ggplot(aes(x=yday, y=TN)) +
      geom_point() +
      geom_smooth(data = . %>% filter(p.value_yday < 0.05, yday < 70), method=lm, se=FALSE, color="black") +
      geom_vline(xintercept=70, linetype="dashed") +
      geom_vline(xintercept=85, linetype="dashed") +
      geom_vline(xintercept=95, linetype="dashed") +
      facet_wrap(~samp_depth_cat2, ncol=1) +
      scale_x_continuous(limits=c(10, 100),
                         breaks = seq(0, 100, by = 20)) +   
      xlab("Day of year") + 
      ylab(expression(paste("TN", " (",mu,"mol"," l"^"-1",")"))) +
      theme2 +
      annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf) +
      annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf) +
      ggtitle("SP 2015")

  # Combine plots into one
  pl_nh4_all_2 <- pl_mb14_nh4 + theme(plot.title = element_text(size = 14, hjust = 0.5, face = "plain"), axis.text.x = element_blank(), axis.title.y = element_text(size = 14), axis.text.y = element_text(size = 9)) +
                  pl_mb15_nh4 + theme(plot.title = element_text(size = 14, hjust = 0.5, face = "plain"), axis.text = element_blank()) +
                  pl_sp15_nh4 + theme(plot.title = element_text(size = 14, hjust = 0.5, face = "plain"), axis.text = element_blank())
  pl_no3_all_2 <- pl_mb14 + theme(plot.title = element_blank(), axis.text.x = element_blank(), axis.title.y = element_text(size = 14), axis.text.y = element_text(size = 9)) +
                  pl_mb15 + theme(plot.title = element_blank(), axis.text = element_blank()) +
                  pl_sp15 + theme(plot.title = element_blank(), axis.text = element_blank())
  pl_tn_all_2 <- pl_mb14_tn + theme(plot.title = element_blank(), axis.title.y = element_text(size = 14), axis.text.y = element_text(size = 9), axis.text.x = element_text(size = 9)) +
                 pl_mb15_tn + theme(plot.title = element_blank(), axis.title.x = element_text(margin = margin(t = 8), size = 15), axis.text.x = element_text(size = 9), axis.text.y = element_blank()) +
                 pl_sp15_tn + theme(plot.title = element_blank(), axis.text.x = element_text(size = 9), axis.text.y = element_blank())
  
  # Resulting plot
  pl_nh4_all_2 / pl_no3_all_2 / pl_tn_all_2
  
  # Equations and plot letters are added manually in Inkscape
}    
    

# CREATE FIG. S1 (distribution of N species in TN pool) ----
# Each figure represents the average concentration for the water column
{
  # Set labels for facets
  labels <- c(mb = "MB", sp = "SP")  
  
  # N species
  alldata %>% 
      # filter out 2014 sample date 115
      filter(yday != 115) %>% 
      # calculate unmeasured N concentration
      mutate(N_other = TN - (NO3 + NH4)) %>% 
      # wide to long format
      pivot_longer(cols = c(NO3, NH4, N_other), names_to = "analyte", values_to = "conc") %>% 
      # calculate whole water column mean
      group_by(site, year(date), yday, analyte) %>% 
      summarize(conc_mean = mean(conc, na.rm = T)) %>% 
      # plot
      ggplot(aes(x = as.factor(yday), y = conc_mean, fill = analyte)) +
      facet_wrap(site~`year(date)`, scales = "free_x", labeller = labeller(site = labels)) +
      geom_bar(position = "stack", stat = "identity") +
      scale_fill_manual(name = "N species",
                        breaks = c("N_other", "NH4", "NO3"),
                        labels = c("Unmeasured", "NH4", "NO3"),
                        values = c("gray80", "#1b9e77", "#7570b3")) +
      ylab(expression(paste("Conc.", " (",mu,"mol"," l"^"-1",")"))) +
      xlab("Day of year") +
      theme_bw() +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            strip.background = element_rect(fill = "white"))
 }
  
  
# Create FIG. S2 (lake vs. river conc.) ----
{  
# Create summary of N concs (means and SE) for MB 2015 and 2015 River
n_summ <-
  alldata %>% 
  filter((site == "mb" & year(date) == 2015) | location == "River") %>% 
  filter(!is.na(samp_depth_cat2) | location == "River") %>% 
  select(date, yday, location, samp_depth_cat, samp_depth_cat2, NH4:TN) %>% 
  pivot_longer(cols = NH4:TN, names_to = "var", values_to = "conc") %>% 
  group_by(date, yday, location, samp_depth_cat, samp_depth_cat2, var) %>% 
  summarize(n = sum(!is.na(conc)),
            conc_mean = mean(conc, na.rm = T),
            conc_SE = sd(conc)/sqrt(n))

# Create summary of P concs (means and SE) for MB 2015 and 2015 River
p_summ <-
  alldata %>% 
  filter((site == "mb" & year(date) == 2015) | location == "River") %>% 
  filter(!is.na(samp_depth_cat2) | location == "River") %>% 
  select(date, yday, location, samp_depth_cat, samp_depth_cat2, SRP:TP) %>% 
  pivot_longer(cols = SRP:TP, names_to = "var", values_to = "conc") %>% 
  group_by(date, yday, location, samp_depth_cat, samp_depth_cat2, var) %>% 
  summarize(n = sum(!is.na(conc)),
            conc_mean = mean(conc, na.rm = T),
            conc_SE = sd(conc)/sqrt(n))

# Create summary of cond & turb levels (means and SE) for MB 2015 and 2015 River
ysi_summ <-
  alldata %>% 
  filter((site == "mb" & year(date) == 2015) | location == "River") %>% 
  mutate(depth_range = cut_width(depth, width = 1, boundary = 0)) %>% 
  select(date, yday, location, depth, depth_range,  cond, turb) %>% 
  pivot_longer(cols = c(cond, turb), names_to = "var", values_to = "value") %>% 
  group_by(date, yday, location, depth_range, var) %>% 
  summarize(n = sum(!is.na(value)),
            conc_mean = mean(value, na.rm = T),
            conc_SE = sd(value, na.rm = T)/sqrt(n))


# Create plot themes
theme_bar <- 
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.background = element_rect(fill = "white"),
          axis.text.x = element_text(size = 9, angle = 90, vjust = 0.5),
          axis.title = element_text(size = 10),
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 10),
          legend.key.size = unit(0.15, "in"))

# New facet labels
facet_labels <- c("48" = "DOY 48", "69" = "DOY 69", "78" = "DOY 78", "84" = "DOY 84", "87" = "DOY 87", "96" = "DOY 96")

# Create plots comparing lake cond, turb, N, and P levels to corresponding river levels
cond_facet <- ysi_summ %>% 
  filter(var == "cond") %>% 
  filter(yday >= 48) %>% 
  mutate(depth_range = factor(depth_range, levels = c("[0,1]", "(1,2]", "(2,3]", "(3,4]", "River"), labels = c("0-1m", "1-2m", "2-3m", "3-4m", "River"))) %>% 
  mutate(depth_range = replace(depth_range, location == "River", "River")) %>% 
  ggplot(aes(x = depth_range, y = conc_mean, fill = depth_range)) +
  facet_wrap(~yday, ncol = 6, labeller=labeller(yday = facet_labels)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = conc_mean-conc_SE, ymax = conc_mean+conc_SE), 
                position = position_dodge(width = 0.9, preserve = "single"),
                width=.2) +
  scale_fill_manual(name="Depth/Source",
                    values=c("gray75", "gray60", "gray45", "gray30", "dodgerblue3")) +  
  ylab(expression(Cond.~(mu*S~cm^{-1}))) + xlab("Depth or source") +
  theme_bar +
  theme(legend.position = "none",
        axis.title.x = element_blank())
  
turb_facet <- ysi_summ %>% 
  filter(var == "turb") %>% 
  filter(yday >= 48) %>% 
  mutate(depth_range = factor(depth_range, levels = c("[0,1]", "(1,2]", "(2,3]", "(3,4]", "River"), labels = c("0-1m", "1-2m", "2-3m", "3-4m", "River"))) %>% 
  mutate(depth_range = replace(depth_range, location == "River", "River")) %>% 
  ggplot(aes(x = depth_range, y = conc_mean, fill = depth_range)) +
  facet_wrap(~yday, ncol = 6, labeller=labeller(yday = facet_labels)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = conc_mean-conc_SE, ymax = conc_mean+conc_SE), 
                position = position_dodge(width = 0.9, preserve = "single"),
                width=.2) +
  scale_fill_manual(name="Depth/Source",
                    values=c("gray75", "gray60", "gray45", "gray30", "dodgerblue3")) +  
  ylab("Turb. (NTU)") + xlab("Depth or source") +
  theme_bar +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.title.x = element_blank())

n_stack <- n_summ %>% 
  filter(yday >= 48) %>% 
  select(-conc_SE) %>% 
  pivot_wider(names_from = var, values_from = conc_mean) %>% 
  # Calculate unmeasured N concentration  
  mutate(N_other = TN - (NO3 + NH4)) %>% 
  # Wide to long format
  pivot_longer(cols = c(NO3, NH4, N_other), names_to = "var", values_to = "conc") %>% 
  mutate(samp_depth_cat2 = factor(samp_depth_cat2, 
                                  levels=c("Top", "Mid-1", "Mid-2", "Mid-3", "Bottom", "River"))) %>% 
  # Plot
  ggplot(aes(x = samp_depth_cat2, y = conc, fill = var)) +
  facet_wrap(~yday, ncol = 6, labeller=labeller(yday = facet_labels)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(name = "N species",
                    breaks = c("N_other", "NH4", "NO3"),
                    labels = c("DON + PN", "NH4", "NO3"),
                    values = c("gray80", "#1b9e77", "#7570b3")) +
  ylab(expression(paste("Conc.", " (",mu,"mol N"," l"^"-1",")"))) +
  xlab("Depth or source") +
  theme_bar +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank())

p_stack <- p_summ %>% 
  filter(yday >= 48) %>% 
  filter(var %in% c("PP", "DOP", "SRP")) %>% 
  mutate(samp_depth_cat2 = factor(samp_depth_cat2, 
                                  levels=c("Top", "Mid-1", "Mid-2", "Mid-3", "Bottom", "River"))) %>% 
  # Plot
  ggplot(aes(x = samp_depth_cat2, y = conc_mean, fill = var)) +
  facet_wrap(~yday, ncol = 6, labeller=labeller(yday = facet_labels)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(name = "P species",
                    breaks = c("PP", "DOP", "SRP"),
                    labels = c("PP", "DOP", "SRP"),
                    values = c("gray80", "#1b9e77", "#7570b3")) +
  ylab(expression(paste("Conc.", " (",mu,"mol P"," l"^"-1",")"))) +
  xlab("Depth or source") +
  theme_bar +
  theme(axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)))

# Combine the subplots into one plot using patchwork (amazing!)
fig1 <- turb_facet + cond_facet + n_stack + p_stack + plot_layout(ncol = 1) +
  plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(face = "bold", vjust = -0.5))
}  
  