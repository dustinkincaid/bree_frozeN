# Use this script to visualize and analyze the data in Kincaid et al. submitted to L&O Letters
# View outline to navigate sections
# Edited by DW Kincaid on 13 Oct 2020


# LOAD PACKAGES ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load("tidyverse", "lubridate", "dataRetrieval",
               "gridExtra", "gtable", "grid", "cowplot",
               "akima", "RColorBrewer", "directlabels",
               "broom")


# READ & TIDY DATA ----
# Data for Fig. 1 (mean temp, river stage, & ice thickness) ----
{
  # Daily air temperatures
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
    
  # Discharge data from USGS downloaded on 2020-10-13
    # 04293500 = East Berkshire
    # 04282795 = Laplatte River @ Shelburne Falls
    str <- read_csv("01_raw data/USGS_stageData_2020-10-13.csv")
  
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
    
  # Ice thickness
  ice <- read_csv("01_raw data/iceDepths.csv") %>% 
    mutate(date = mdy(date))
    
  # Read in the compiled sampling data ('alldata') to get sampling dates
  sampling_dates <- read_csv("01_raw data/alldata_2014_2015_mb_sp_compiled.csv") %>% 
    select(site, date, yday) %>% 
    distinct(site, date, yday)
 }  
  
  
# Data for Fig. 2 (contour plots of all vars); Fig. 3 (NO3 profiles); ----
# Fig. S2 (NH4 profiles); Fig S3 (TN profiles); Fig. S4 (distribution of N species for TN pool)
# Fig. S5 (compare lake vs. river NO3 concs)
  # All concentrations are in umol/L (uM)
  alldata <- read_csv("01_raw data/alldata_2014_2015_mb_sp_compiled.csv") %>% 
    # Remove outliers
    mutate(NO3 = ifelse(site == "sp" & samp_depth_cat2 == "Mid-3" & yday == 49 & NO3 > 10, NA, NO3)) %>% 
    mutate(NH4 = ifelse(site == "mb" & year(date) == 2015 & samp_depth_cat2 == "Mid-1" & yday == 48 & NH4 > 10, NA, NH4)) %>% 
    mutate(SRP = ifelse(site == "sp" & samp_depth_cat2 == "Mid-2" & yday == 78 & SRP > 0.02, NA, SRP)) %>%
    mutate(TP = ifelse(site == "sp" & samp_depth_cat2 == "Mid-2" & yday == 78 & TP > 2, NA, TP)) %>%
    mutate(DOP = ifelse(site == "sp" & samp_depth_cat2 == "Mid-2" & yday == 78 & DOP > 0.015, NA, DOP)) %>%
    mutate(PP = ifelse(site == "sp" & samp_depth_cat2 == "Mid-2" & yday == 78 & PP > 0.03, NA, PP)) %>%
    # Calculate ratios
    mutate(no3_srp = NO3/SRP,
           din_srp = (NO3 + NH4)/SRP,
           tn_tp = TN/TP)  
 
   
# Data for Fig. S5 (comparison of lake vs. river NO3 concs) ----
{
  # All under-ice N data for MB in mg N/L (converted to umol/L below)
  mb_n <- read.csv("01_raw data/WINTER 2015 MB_TN-TP.csv", header=T, stringsAsFactors = F) %>% 
    select(Date.Collected, Depth.or.BLANK, Nox.Corrected, NH4.Corrected, TN.Corrected) %>% 
    rename(date=Date.Collected, samp_depth_cat=Depth.or.BLANK, NO3=Nox.Corrected, NH4=NH4.Corrected, TN=TN.Corrected) %>% 
    mutate(date = mdy(as.character(date), tz="US/Eastern"),
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
      # Add yday
      mutate(yday = yday(date)) %>% 
      # Convert all N concs from mg N/L to umol/L
      mutate(NO3 = NO3*1000/14.007,
             NH4 = NH4*1000/14.007,
             TN = TN*1000/14.007) %>% 
      # Add location (i.e., Lake)
      mutate(Location = "Lake")
  
  # Pull Miss Riv data from Joung et al 2017 Supp Info
  mb_chem <- read.csv("01_raw data/WINTER 2015 MB_supp_data.csv", header=T, stringsAsFactors = F, na.strings = " ") %>% 
    filter(Location == "River") %>% 
    select(-(ends_with("SD"))) %>% 
    select(Cal.Date, Jul.Day, Location, Depth_grab:DOC) %>% 
    rename(date=Cal.Date, yday = Jul.Day, depth=Depth_grab) %>% 
    mutate(date = mdy(as.character(date)),
           site = "mb") %>% 
    # Convert NO3-N from mg N/L to umol/L
    mutate(NO3 = NO3*1000/14.007)
  
  # Append the df's to each other & add samp_depth_cat2
  mb_all <- 
    bind_rows(mb_n, mb_chem) %>%
    mutate(samp_depth_cat2 = ifelse(samp_depth_cat == "D1", "Top",
                                    ifelse(samp_depth_cat == "D2", "Mid-1",
                                           ifelse(samp_depth_cat == "D3", "Mid-2",
                                                  ifelse(samp_depth_cat == "D4", "Mid-3",
                                                         ifelse(samp_depth_cat == "D5", "Bottom", NA))))))  
 }  
  

# CREATE Fig. 1 (mean temp, river stage, & ice thickness) ----
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
    
{
  # Air temperature
    p1 <- airTemp %>% 
      # Make a lake_winter code
      mutate(lake_winter = paste(lake, winter, sep = "_")) %>% 
      # Exclude sp_winter2014
      filter(lake_winter != "sp_winter2014") %>% 
      # Plot
      ggplot(aes(x = yday, y = tavg_alt, group = lake_winter, color = lake_winter)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
      geom_line(size = 0.7) +
      geom_vline(xintercept = 78, linetype = "dashed", size = 0.7, color = "gray50") +
      geom_vline(xintercept = 70, linetype = "dashed", size = 0.7, color = "gray50") +
      scale_color_manual(breaks = c("mb_winter2014", "mb_winter2015", "sp_winter2015"),
                         labels = c("MB 2014", "MB 2015", "SP 2015"),
                         values = c("#56B4E9", "#0072B2", "#009E73")) +
      scale_x_continuous(limits = c(-31, 100), breaks = c(-30, 0, 30, 60, 90)) + 
      xlab("Day of year") +
      ylab(expression(Air~temperature~(degree*C))) +
      theme1 +
      theme(legend.position = "none",
            axis.text.x = element_blank(),
            axis.title.x = element_blank()) +
      labs(tag = "a")
    
  # River stage
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
      scale_x_continuous(limits = c(-31, 100), breaks = c(-30, 0, 30, 60, 90)) + 
      xlab("Day of year") +
      ylab("River stage (m)") +
      theme1 +
      theme(legend.position = "none",
            axis.text.x = element_blank(),
            axis.title.x = element_blank()) +
      labs(tag = "b")
    
  # Ice thickness
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
      scale_x_continuous(limits = c(-31, 100), breaks = c(-30, 0, 30, 60, 90)) + 
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
  
  # Save plot if desired
  # ggsave(file="03_figures/plot_airtemp_stage_ice_600dpi.pdf", plot1, width = 3.5, height = 5.5, units = "in", dpi = 600)
  


# CREATE FIG. 2 (filled contour plots) ----
# This code will create plots with countour labels; the final figure in the MS was further edited using Inkscape
  
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

  # Create plots - alternative layout
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
    # MB 2014                                                                                                 ylab(expression(paste("NH"["4"]^" +", " (",mu,"M)")))
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
  
  # Save plot if desired
  # save_plot("03_figures/plot_contour_ALT_allPlots_withColorGuides.png", grob2,
  #         base_height = 7.5, base_width = 7.67, dpi = 600)
  
  # The remaining edits (adding contour labels, etc) is done using Inkscape
  

# LINEAR REGRESSIONS for Figs. 3, S2, S3 (NO3/NH4/TN ~ DOY) ----
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
  
  
# CREATE FIG. 3 (NO3 ~ DOY) ----
# Note: regression eqs. were added to plots manually in Inkscape
  # Set a theme for the following plots
  theme3 <- theme_minimal() +
    theme(strip.background = element_blank(),
          strip.text.x = element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.ticks = element_line(),
          axis.title = element_blank(),
          # Adjust plot margin: top, right, bottom, left
          plot.margin = unit(c(0.15, 0.15, 0, 0.1), "in"),
          axis.text = element_text(size = 10),
          plot.title = element_text(face = "bold"))

  {
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
        geom_vline(xintercept=79, linetype="dashed") + 
        scale_y_continuous(limits=c(0, 65),
                           breaks = seq(0, 60, by = 30)) +
        scale_x_continuous(limits=c(10, 100),
                           breaks = seq(0, 100, by = 20)) +  
        xlab("Day of the year") + 
        ylab(expression(paste("NO"["3"]^" -", " (",mu,"mol"," l"^"-1",")"))) +
        theme3 +
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
        xlab("Day of the year") +
        ylab(expression(paste("NO"["3"]^" -", " (",mu,"mol"," l"^"-1",")"))) +
        theme3 +
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
        xlab("Day of the year") + 
        ylab(expression(paste("NO"["3"]^" -", " (",mu,"mol"," l"^"-1",")"))) +
        theme3 +
        annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf) +
        annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf) +
        ggtitle("SP 2015")
    
    # Combine these three NO3 plots into one plot
    pl_no3_all <- plot_grid(pl_mb14, pl_mb15, NULL, pl_sp15, ncol = 2, align = "hv", hjust = 0.25)
    
    # Add common x and y axis titles
    y.grob_no3 <- textGrob(expression(paste("NO"["3"]^" -", " (",mu,"mol"," l"^"-1",")")), gp = gpar(fontsize = 14), rot = 90, vjust = 0.5)
    x.grob_no3 <- textGrob("Day of year", gp = gpar(fontsize = 14))
    grob_no3 <- grid.arrange(arrangeGrob(pl_no3_all, left = y.grob_no3, bottom = x.grob_no3))
    
    }
    
    # Save plot if desired
    # save_plot("03_figures/plot_no3_decline.png", grob_no3,
    #           base_height = 6, base_width = 6, dpi = 300)
    
  # Look at NO3 equations
  # These were added to plots manually
  lm_no3 <- lm_results %>% 
    filter(analyte == "NO3") 
    
    
# CREATE FIG. S2 (NH4 ~ DOY) ----
  {  
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
      xlab("Day of the year") + 
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
      xlab("Day of the year") +
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
      xlab("Day of the year") + 
      ylab(expression(paste("NH"["4"]^" +", " (",mu,"mol"," l"^"-1",")"))) +
      theme2 +
      annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf) +
      annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf) +
      ggtitle("SP 2015")
  
  # Combine these three NH4 plots into one plot
  pl_nh4_all <- plot_grid(pl_mb14_nh4, pl_mb15_nh4, NULL, pl_sp15_nh4, ncol = 2, align = "hv", hjust = 0.25)
  
  # Add common x and y axis titles
  y.grob_nh4 <- textGrob(expression(paste("NH"["4"]^" +", " (",mu,"mol"," l"^"-1",")")), gp = gpar(fontsize = 14), rot = 90, vjust = 0.5)
  x.grob_nh4 <- textGrob("Day of year", gp = gpar(fontsize = 14))
  grob_nh4 <- grid.arrange(arrangeGrob(pl_nh4_all, left = y.grob_nh4, bottom = x.grob_nh4))
  
  }
  
  # Save plot if desired
  # save_plot("03_figures/plot_nh4_patterns.png", grob_nh4,
  #           base_height = 6, base_width = 6, dpi = 300)
  
  # Look at NH4 equations
  # These were added manually to the plots
  lm_nh4 <- lm_results %>% 
    filter(analyte == "NH4")
  
  
# CREATE FIG. S3 (TN ~ DOY) ----
  {  
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
      xlab("Day of the year") + 
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
      xlab("Day of the year") +
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
      xlab("Day of the year") + 
      ylab(expression(paste("TN", " (",mu,"mol"," l"^"-1",")"))) +
      theme2 +
      annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf) +
      annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf) +
      ggtitle("SP 2015")
  
  # Combine these three TN plots into one plot
  pl_tn_all <- plot_grid(pl_mb14_tn, pl_mb15_tn, NULL, pl_sp15_tn, ncol = 2, align = "hv", hjust = 0.25)
  
  # Add common x and y axis titles
  y.grob_tn <- textGrob(expression(paste("TN", " (",mu,"mol"," l"^"-1",")")), gp = gpar(fontsize = 14), rot = 90, vjust = 0.5)
  x.grob_tn <- textGrob("Day of year", gp = gpar(fontsize = 14))
  grob_tn <- grid.arrange(arrangeGrob(pl_tn_all, left = y.grob_tn, bottom = x.grob_tn))
  
  }  
  
  # Save plot if desired
  # save_plot("03_figures/plot_tn_patterns.png", grob_tn,
  #           base_height = 6, base_width = 6, dpi = 300)  

  # Look at TN equations
  # Equations were added manually to plots
  lm_tn <- lm_results %>% 
    filter(analyte == "TN")
  

# CREATE FIG. S4 (distribution of N species in TN pool) ----
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
  
  # Save plot if desired
  # ggsave("03_figures/plot_Nspecies_stackedBar.png", width = 7, height = 3.5, units = "in", dpi = 150)
  
  
# Create FIG. S5 (lake vs. river conc.) ----
{  
  # Create summary of N concs (means and SE)
  mb_summ <-
    mb_all %>% 
    select(date, yday, Location, samp_depth_cat, samp_depth_cat2, NO3:TN) %>% 
    pivot_longer(cols = NO3:TN, names_to = "var", values_to = "conc") %>% 
    group_by(date, yday, Location, samp_depth_cat, samp_depth_cat2, var) %>% 
    summarize(n = length(conc),
              conc_mean = mean(conc, na.rm = T),
              conc_SE = sd(conc)/sqrt(n))
  
  # Create plot comparing lake NO3 concs to river NO3 concs
  mb_summ %>% 
    filter(var == "NO3") %>% 
    filter(yday >= 84) %>% 
    mutate(samp_depth_cat2 = replace(samp_depth_cat2, Location == "River", "River")) %>% 
    mutate(samp_depth_cat2 = factor(samp_depth_cat2, 
                                    levels=c("Top", "Mid-1", "Mid-2", "Mid-3", "Bottom", "River"))) %>% 
    ggplot(aes(x = as.factor(yday), y = conc_mean, fill = samp_depth_cat2)) +
    geom_bar(position = position_dodge(width = 0.9, preserve = "single"), stat = "identity") +
    geom_errorbar(aes(ymin = conc_mean-conc_SE, ymax = conc_mean+conc_SE), 
                  position = position_dodge(width = 0.9, preserve = "single"),
                  width=.2) +
    scale_fill_manual(name="Depth/Source",
                      values=c("gray90", "gray75", "gray60", "gray45", "gray30", "dodgerblue3")) +
    ylab(expression(paste("NO"["3"]^" -", " (",mu,"mol"," l"^"-1",")"))) + xlab("Day of year") +
    theme_classic()
 }  
  
  # Save plot if desired
  # ggsave("03_figures/plot_compare_lake_river_conc.png", width = 5, height = 3, units = "in", dpi = 150)  