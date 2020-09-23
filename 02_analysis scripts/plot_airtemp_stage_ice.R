# Plot 2014 & 2015 air temperature, river stage, and ice thickness similar to Figure 2 in Joung et al. 2017


# Load packages ----
  library("tidyverse")
  library("lubridate")
  library("dataRetrieval")
  library("gridExtra")
  library("gtable")
  library("grid")
  

# Read in data ----
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
  
# Make plots ----
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
      scale_x_continuous(limits = c(-31, 100), breaks = c(-30, 0, 30, 60, 90)) + 
      xlab("Day of year") +
      ylab("River stage (m)") +
      theme1 +
      theme(legend.position = "none",
            axis.text.x = element_blank(),
            axis.title.x = element_blank()) +
      labs(tag = "b")

    # p2 <- str_daily %>% 
    #   # Provide a categorical for 2014 vs. 2015 winters
    #   mutate(winter = ifelse(date >= "2014-12-01", "winter2015", "winter2014")) %>% 
    #   # Make a river_winter code
    #   mutate(river_winter = paste(site_name, winter, sep = "_")) %>% 
    #   # Exclude laplatte_winter2014
    #   filter(river_winter != "laplatte_winter2014") %>% 
    #   # Plot
    #   ggplot(aes(x = yday, y = GH_mean_m, group = river_winter, color = river_winter)) +
    #   geom_line(size = 0.8) +
    #   geom_vline(xintercept = 78, linetype = "dashed", size = 0.7, color = "gray50") +
    #   annotate("text", x=74.5, y=2.7, label="2014 thaw", size = 2.5, angle=90, color = "gray20") +
    #   geom_vline(xintercept = 70, linetype = "dashed", size = 0.7, color = "gray50") +
    #   annotate("text", x=66.5, y=2.7, label="2015 thaw", size = 2.5, angle=90, color = "gray20") +
    #   scale_color_manual(breaks = c("eberk_winter2014", "eberk_winter2015", "laplatte_winter2015"),
    #                      labels = c("MB 2014", "MB 2015", "SP 2015"),
    #                      values = c("#56B4E9", "#0072B2", "#009E73")) +
    #   scale_x_continuous(limits = c(-31, 100), breaks = c(-30, 0, 30, 60, 90)) + 
    #   xlab("Day of the year") +
    #   ylab("River stage (m)") +
    #   theme1 +
    #   theme(legend.position = "none",
    #         axis.text.x = element_blank(),
    #         axis.title.x = element_blank()) +
    #   labs(tag = "b")
    
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
  
  # Save plot
  ggsave(file="03_figures/plot_airtemp_stage_ice.jpg", plot1, width = 3.5, height = 5.5, units = "in", dpi = 300)      
  