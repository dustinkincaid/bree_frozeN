# Load packages----
  library("tidyverse")
  library("lubridate")
  library("data.table")
  library("cowplot")
  library("akima")
  library("RColorBrewer")
  library("directlabels")
  library("grid")
  library("gridExtra")
  library("broom")

# TO DO: Do linear regressions of NO3 declines and add asterics to significant trends
#        Confirm depths for 2014 grab sample data
#        Plot DO & Chl a profiles
#        Figure out how to make filled contour plots like Ocean Data View plots in Joung paper
#        Calculate TN:TP molar ratios
#        Plot the ratios
#        Add DOC data from Joung supp. material & look at C:N & C:P ratios
#        Plot chla and oxygen data along with N species data
#          - will likely have to figure out how to no include rows with NA's if plotting at the same time as 

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
      select(date:TN, tntp, temp:BGA, samp_depth_cat2) 

  # Remove unnecessary objects
    rm(depth_cat2, depth_index_2014, depth_index_2015, keys, mb_2014.raw, mb_depths, mb_depths_df, 
       mb_sensor_2014, mb2014_depths_df, mb_n, mb_n.raw, sp_depths_df, sp_n, sp_n.raw, mb_sensor, sp_sensor)
  }


# Filled contour plots----
  # Helpful code here: https://stackoverflow.com/questions/19339296/plotting-contours-on-an-irregular-grid
  # also in my R script: code.lterketpond.heattransport.R (search for interp)
  # Day of the year for cold-thaw separation line on figures = 78 for 2014 and 70 for 2015
  
  # Set theme1 for plots
  {
  theme1 <-
    theme_classic() +
    theme(
      # Adjust plot margin: top, right, bottom, left
      # For ancillary plots use:
        # plot.margin = unit(c(0.05, 0.3, 0, 0.15), "in"),
      # For N plots use:
        plot.margin = unit(c(0.05, 0.35, 0, 0.15), "in"),
      legend.title = element_blank(),
      #legend.title = element_text(size = 9),
      legend.text = element_text(size = 9),
      legend.key.width = unit(0.1, "in"),
      legend.key.height = unit(0.18, "in"),
      legend.margin = margin(t=0, r=0, b=0, l=0, unit="in"),
      legend.position = c(1.07, 0.55),
      # axis.text = element_blank(),
      #axis.text = element_text(size = 8),
      axis.title = element_blank()
      #axis.title.x = element_text(size = 10, margin = margin(t = 10, r = 0, b = 0, l = 0)),
      #axis.title.y = element_text(size = 10, margin = margin(t = 0, r = 10, b = 0, l = 0)),
    )
  }
  
  # Create plot function
  # For help with non-standard evaluation use in functions with dplyr & ggplot: https://edwinth.github.io/blog/dplyr-recipes/
  # And here: https://tidyeval.tidyverse.org/dplyr.html AND https://dplyr.tidyverse.org/articles/programming.html
  make_contourplot <- function(df, year, site_exp, var, leg_title, leg_lim_vec, leg_break_vec, cont_break_vec, 
                               col_guide = FALSE, ax_txt_x = FALSE, ax_txt_y = FALSE) {
     # Turn necessary variables into strings
     site_exp_enq <- enquo(site_exp)
     var_enq <- enquo(var)
     # Subset data by 
     df_sub <- df %>% 
       filter(year(date) == year & !!site_exp_enq & !is.na(!!var_enq) & yday < 100) %>% 
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
      # Additional help on plot: https://stackoverflow.com/questions/38154679/r-adding-legend-and-directlabels-to-ggplot2-contour-plot
      # Method for labeling contours and keeping the color legend for the fill color gradient: https://github.com/tdhock/directlabels/issues/8
      # Help with label position options: http://directlabels.r-forge.r-project.org/docs/index.html
      #limits_exp_enq <- enquo(limits_exp)
      
      plot <- ggplot(data = df_int, aes(x = yday, y = depth, z = var)) +
        geom_tile(aes(fill = var)) +
        scale_fill_gradientn(colors = brewer.pal(n = 11, name = "Spectral"), trans = "reverse",
                             guide = if (col_guide) guide_colorbar(reverse = TRUE) else NULL,
                             # guide = guide_colorbar(reverse = TRUE),
                             limits = leg_lim_vec, # choose this range based on range of all plots (i.e., 2014 MB & 2015 SP)
                             breaks = leg_break_vec,
                             name = leg_title) +
        # Day separating cold from thaw period (78 for 2014, 70 for 2015)
        geom_vline(xintercept=ifelse(year == 2014, 78, 70), linetype="dashed", color = "black", size = 0.5) +
        geom_point(data = df_sub, aes(x = yday, y = depth), color = "white", shape = 1, size = 0.3) +
        geom_contour(color = "gray50", breaks = cont_break_vec, size = 0.25) +
        # geom_dl(aes(label = ..level..), breaks = cont_break_vec, method = list("bottom.pieces", cex = 0.5), stat="contour", color = "gray20") +
        scale_x_continuous(limits = c(10,90), breaks = c(20, 40, 60, 80)) +
        xlab("Day of year") + ylab("Depth (m)") +
        theme1 +
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
  }    
  
  
# Alt layout: ALL plots combined---- 
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
  x.grob <- textGrob("Day of the year", gp = gpar(fontsize = 10))
  grob2 <- grid.arrange(arrangeGrob(grob1, left = y.grob, bottom = x.grob))

  # Save plot
  save_plot("03_figures/plot_contour_ALT_allPlots_noColorGuides.png", grob2,
            base_height = 7.5, base_width = 7.67, dpi = 600)  
  
  
  
  # Create plots - original layout
  {
  # Temp
    # MB 2014
    p_temp_mb_2014 <- make_contourplot(df = alldata, year = 2014, site_exp = site == "mb", var = temp, leg_title = expression(Temp.~(degree*C)), 
                             leg_lim_vec = c(6,0), leg_break_vec = c(0, 2, 4, 6), cont_break_vec = c(0,1,2,3,4,5,6), ax_txt_y = TRUE)
    # MB 2015
    p_temp_mb_2015 <- make_contourplot(df = alldata, year = 2015, site_exp = site == "mb", var = temp, leg_title = expression(Temp.~(degree*C)),
                             leg_lim_vec = c(6,0), leg_break_vec = c(0, 2, 4, 6), cont_break_vec = c(0,1,2,3,4,5,6), ax_txt_y = TRUE)
    # SP 2015
    p_temp_sp_2015 <- make_contourplot(df = alldata, year = 2015, site_exp = site == "sp", var = temp, leg_title = expression(Temp.~(degree*C)),
                             leg_lim_vec = c(6,0), leg_break_vec = c(0, 2, 4, 6), cont_break_vec = c(0,1,2,3,4,5,6), ax_txt_x = TRUE, ax_txt_y = TRUE)
  # DO
    # MB 2014
    p_do_mb_2014 <- make_contourplot(df = alldata, year = 2014, site_exp = site == "mb", var = DO, leg_title = expression(DO~(mg~l^{-1})), 
                             leg_lim_vec = c(20,0), leg_break_vec = c(0,10,20), cont_break_vec = c(0,5,10,15,20))
    # MB 2015
    p_do_mb_2015 <- make_contourplot(df = alldata, year = 2015, site_exp = site == "mb", var = DO, leg_title = expression(DO~(mg~l^{-1})),  
                             leg_lim_vec = c(20,0), leg_break_vec = c(0,10,20), cont_break_vec = c(0,5,10,15,20))
    # SP 2015
    p_do_sp_2015 <- make_contourplot(df = alldata, year = 2015, site_exp = site == "sp", var = DO, leg_title = expression(DO~(mg~l^{-1})),  
                             leg_lim_vec = c(20,0), leg_break_vec = c(0,10,20), cont_break_vec = c(0,5,10,15,20), ax_txt_x = TRUE)
  # chl a
    # MB 2014
    p_chla_mb_2014 <- make_contourplot(df = alldata, year = 2014, site_exp = site == "mb", var = chla, leg_title = expression(Chl~italic(a)~(mu*g~l^{-1})), 
                             leg_lim_vec = c(50,0), leg_break_vec = c(0,25,50), cont_break_vec = c(0,10,20,30,40,50))
    # MB 2015
    p_chla_mb_2015 <- make_contourplot(df = alldata, year = 2015, site_exp = site == "mb", var = chla, leg_title = expression(Chl~italic(a)~(mu*g~l^{-1})), 
                             leg_lim_vec = c(50,0), leg_break_vec = c(0,25,50), cont_break_vec = c(0,10,20,30,40,50))
    # SP 2015
    p_chla_sp_2015 <- make_contourplot(df = alldata, year = 2015, site_exp = site == "sp", var = chla, leg_title = expression(Chl~italic(a)~(mu*g~l^{-1})),
                             leg_lim_vec = c(50,0), leg_break_vec = c(0,25,50), cont_break_vec = c(0,10,20,30,40,50), ax_txt_x = TRUE)
    
  # NH4 - umol N/L
    # MB 2014                                                                                                 ylab(expression(paste("NH"["4"]^" +", " (",mu,"M)")))
    p_nh4_mb_2014 <- make_contourplot(df = alldata, year = 2014, site_exp = site == "mb", var = NH4, leg_title = expression(paste("NH"["4"]^" +", " (",mu,"mol"," l"^"-1",")")), 
                             leg_lim_vec = c(40,0), leg_break_vec = c(0,20,40), cont_break_vec = c(0,10,20,30,40), ax_txt_y = TRUE)
    # MB 2015
    p_nh4_mb_2015 <- make_contourplot(df = alldata, year = 2015, site_exp = site == "mb", var = NH4, leg_title = expression(paste("NH"["4"]^" +", " (",mu,"mol"," l"^"-1",")")), 
                             leg_lim_vec = c(40,0), leg_break_vec = c(0,20,40), cont_break_vec = c(0,10,20,30,40), ax_txt_y = TRUE)
    # SP 2015
    p_nh4_sp_2015 <- make_contourplot(df = alldata, year = 2015, site_exp = site == "sp", var = NH4, leg_title = expression(paste("NH"["4"]^" +", " (",mu,"mol"," l"^"-1",")")),
                             leg_lim_vec = c(40,0), leg_break_vec = c(0,20,40), cont_break_vec = c(0,10,20,30,40), ax_txt_x = TRUE, ax_txt_y = TRUE)   
  # NO3 - umol N/L
    # MB 2014
    p_no3_mb_2014 <- make_contourplot(df = alldata, year = 2014, site_exp = site == "mb", var = NO3, leg_title = expression(paste("NO"["3"]^" -", " (",mu,"mol"," l"^"-1",")")), 
                             leg_lim_vec = c(60,0), leg_break_vec = c(0,20,40,60), cont_break_vec = c(0,10,20,30,40,50,60))
    # MB 2015
    p_no3_mb_2015 <- make_contourplot(df = alldata, year = 2015, site_exp = site == "mb", var = NO3, leg_title = expression(paste("NO"["3"]^" -", " (",mu,"mol"," l"^"-1",")")), 
                             leg_lim_vec = c(60,0), leg_break_vec = c(0,20,40,60), cont_break_vec = c(0,10,20,30,40,50,60))
    # SP 2015
    p_no3_sp_2015 <- make_contourplot(df = alldata, year = 2015, site_exp = site == "sp", var = NO3, leg_title = expression(paste("NO"["3"]^" -", " (",mu,"mol"," l"^"-1",")")),
                             leg_lim_vec = c(60,0), leg_break_vec = c(0,20,40,60), cont_break_vec = c(0,10,20,30,40,50,60), ax_txt_x = TRUE)
  # TN - umol N/L
    # MB 2014
    p_tn_mb_2014 <- make_contourplot(df = alldata, year = 2014, site_exp = site == "mb", var = TN, leg_title = expression(TN~(mu*mol~l^{-1})), 
                             leg_lim_vec = c(120,0), leg_break_vec = c(0,40,80,120), cont_break_vec = c(0,20,40,60,80,100,120))
    # MB 2015
    p_tn_mb_2015 <- make_contourplot(df = alldata, year = 2015, site_exp = site == "mb", var = TN, leg_title = expression(TN~(mu*mol~l^{-1})), 
                             leg_lim_vec = c(120,0), leg_break_vec = c(0,40,80,120), cont_break_vec = c(0,20,40,60,80,100,120))
    # SP 2015
    p_tn_sp_2015 <- make_contourplot(df = alldata, year = 2015, site_exp = site == "sp", var = TN, leg_title = expression(TN~(mu*mol~l^{-1})), 
                             leg_lim_vec = c(120,0), leg_break_vec = c(0,40,80,120), cont_break_vec = c(0,20,40,60,80,100,120), ax_txt_x = TRUE)
  # TP - umol P/L - need to adjust limits and breaks
    # MB 2014
    p_tp_mb_2014 <- make_contourplot(df = alldata, year = 2014, site_exp = site == "mb", var = TP, leg_title = expression(TP~(mu*mol~l^{-1})), 
                             leg_lim_vec = c(4.5,0), leg_break_vec = c(0,2,4), cont_break_vec = c(0,1,2,3,4))
    # MB 2015
    p_tp_mb_2015 <- make_contourplot(df = alldata, year = 2015, site_exp = site == "mb", var = TP, leg_title = expression(TP~(mu*mol~l^{-1})), 
                             leg_lim_vec = c(4.5,0), leg_break_vec = c(0,2,4), cont_break_vec = c(0,1,2,3,4))
    # SP 2015
    p_tp_sp_2015 <- make_contourplot(df = alldata, year = 2015, site_exp = site == "sp", var = TP, leg_title = expression(TP~(mu*mol~l^{-1})), 
                             leg_lim_vec = c(4.5,0), leg_break_vec = c(0,2,4), cont_break_vec = c(0,1,2,3,4))
  # TN:TP - umol P/L - need to adjust limits and breaks
    # MB 2014
    p_tntp_mb_2014 <- make_contourplot(df = alldata, year = 2014, site_exp = site == "mb", var = tntp, leg_title = "TN:TP", 
                             leg_lim_vec = c(400,0), leg_break_vec = c(0,200,400), cont_break_vec = c(0,100,200,300,400))
    # MB 2015
    p_tntp_mb_2015 <- make_contourplot(df = alldata, year = 2015, site_exp = site == "mb", var = tntp, leg_title = expression(TP~(mu*mol~l^{-1})), 
                             leg_lim_vec = c(400,0), leg_break_vec = c(0,200,400), cont_break_vec = c(0,100,200,300,400))
    # SP 2015
    p_tntp_sp_2015 <- make_contourplot(df = alldata, year = 2015, site_exp = site == "sp", var = tntp, leg_title = expression(TP~(mu*mol~l^{-1})), 
                             leg_lim_vec = c(400,0), leg_break_vec = c(0,200,400), cont_break_vec = c(0,100,200,300,400))
  }
  

# Compile ancillary contour plots (temp, DO, chl a)----  
  # Create titles for figures
  title_temp <- textGrob(expression(Temp.~(degree*C)), gp = gpar(fontsize = 10), vjust = 0.5)
  title_do <- textGrob(expression(D.O.~(mg~l^{-1})), gp = gpar(fontsize = 10), vjust = 0.5)
  title_chla <- textGrob(expression(Chl.~italic(a)~(mu*g~l^{-1})), gp = gpar(fontsize = 10), vjust = 0.5)
  
  # Arrange plots and add column titles in this first grob object
  grob1 <- grid.arrange(arrangeGrob(p_temp_mb_2014, p_temp_mb_2015, p_temp_sp_2015, top = title_temp, ncol = 1),
                       arrangeGrob(p_do_mb_2014, p_do_mb_2015, p_do_sp_2015, top = title_do, ncol = 1),
                       arrangeGrob(p_chla_mb_2014, p_chla_mb_2015, p_chla_sp_2015, top = title_chla, ncol = 1),
                       ncol = 3)
  
  # Add common x and y axis titles
  y.grob <- textGrob("Depth (m)", gp = gpar(fontsize = 10), rot = 90, vjust = 1.25)
  x.grob <- textGrob("Day of the year", gp = gpar(fontsize = 10))
  grob2 <- grid.arrange(arrangeGrob(grob1, left = y.grob, bottom = x.grob))

  # Save plot
  save_plot("03_figures/plot_contour_ancillary_noInfo.png", grob2,
            base_height = 4, base_width = 7.5, dpi = 300)
  

  
# Compile N contour plots (temp, DO, chl a)----      
  # Create titles for figures
  title_nh4 <- textGrob(expression(paste("NH"["4"]^" +", " (",mu,"mol"," l"^"-1",")")), gp = gpar(fontsize = 10), vjust = 0.5)
  title_no3 <- textGrob(expression(paste("NO"["3"]^" -", " (",mu,"mol"," l"^"-1",")")), gp = gpar(fontsize = 10), vjust = 0.5)
  title_tn <- textGrob(expression(TN~(mu*mol~l^{-1})), gp = gpar(fontsize = 10), vjust = 0.5)
  
  # Arrange plots and add column titles in this first grob object
  grob1_n <- grid.arrange(arrangeGrob(p_nh4_mb_2014, p_nh4_mb_2015, p_nh4_sp_2015, top = title_nh4, ncol = 1),
                       arrangeGrob(p_no3_mb_2014, p_no3_mb_2015, p_no3_sp_2015, top = title_no3, ncol = 1),
                       arrangeGrob(p_tn_mb_2014, p_tn_mb_2015, p_tn_sp_2015, top = title_tn, ncol = 1),
                       ncol = 3)
  
  # Add common x and y axis titles
  y.grob_n <- textGrob("Depth (m)", gp = gpar(fontsize = 10), rot = 90, vjust = 1.25)
  x.grob_n <- textGrob("Day of the year", gp = gpar(fontsize = 10))
  grob2_n <- grid.arrange(arrangeGrob(grob1_n, left = y.grob_n, bottom = x.grob_n))

  # Save plot
  save_plot("03_figures/plot_contour_Nconc_noInfo.png", grob2_n,
            base_height = 4, base_width = 7.5, dpi = 300)  
  

  

# Plot all N species over time at each depth category----
  # https://cran.r-project.org/web/packages/broom/vignettes/broom_and_dplyr.html
  # https://r4ds.had.co.nz/many-models.html
  
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
  # Remove an outlier for Sh Pond at "Mid-3"
  filter(!(site == "sp" &samp_depth_cat2 == "Mid-3" & yday == 49 & NO3 > 10)) %>%     
  # To simplify the df, select relevant columns
  select(site, year, date, yday, depth, samp_depth_cat, samp_depth_cat2, NO3:TN) %>% 
  # Pivot the measured variables into long format
  pivot_longer(cols = NO3:TN, names_to = "analyte", values_to = "conc") %>% 
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
  
# Set a theme for the following plots
theme2 <- theme_minimal() +
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

# NO3 plots ----
  # MB 2014
  pl_mb14 <- alldata %>% 
    # Add year column
    mutate(year = year(date)) %>% 
    # Filter site and year
    filter(site == "mb" & year == 2014) %>% 
    filter(date < "2014-05-01") %>% 
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
      xlab("Day of the year") + 
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
      xlab("Day of the year") +
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
    # Remove an outlier at "Mid-3"
    filter(!(samp_depth_cat2 == "Mid-3" & yday == 49 & NO3 > 10)) %>% 
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
      facet_wrap(~samp_depth_cat2, ncol=1) +
      xlab("Day of the year") + 
      ylab(expression(paste("NO"["3"]^" -", " (",mu,"mol"," l"^"-1",")"))) +
      theme2 +
      annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf) +
      annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf) +
      ggtitle("SP 2015")
  
  # Combine these three NO3 plots into one plot
  pl_no3_all <- plot_grid(pl_mb14, pl_mb15, NULL, pl_sp15, ncol = 2, align = "hv", hjust = 0.25)
  
  # Add common x and y axis titles
  y.grob_no3 <- textGrob(expression(paste("NO"["3"]^" -", " (",mu,"mol"," l"^"-1",")")), gp = gpar(fontsize = 14), rot = 90, vjust = 0.5)
  x.grob_no3 <- textGrob("Day of the year", gp = gpar(fontsize = 14))
  grob_no3 <- grid.arrange(arrangeGrob(pl_no3_all, left = y.grob_no3, bottom = x.grob_no3))
  
  # Save plot
  # save_plot("03_figures/plot_no3_decline.png", grob_no3,
  #           base_height = 11.5, base_width = 4, dpi = 150)
  save_plot("03_figures/plot_no3_decline.png", grob_no3,
            base_height = 6, base_width = 6, dpi = 300)

  
  

# Plots for supporting information ----
# Set a theme for the NH4 and TN  plots
theme3 <- theme_minimal() +
  theme(
        strip.text.x = element_text(size = 8),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(),
        # Adjust plot margin: top, right, bottom, left
        plot.margin = unit(c(0, 0.1, 0, 0), "in"),
        axis.text = element_text(size = 8),
        axis.title = element_blank(),
        # axis.title.x = element_text(size = 8, margin = margin(t = 5, r = 0, b = 0, l = 0)),
        # axis.title.y = element_text(size = 8, margin = margin(t = 0, r = 0, b = 0, l = 0)),
        plot.title = element_text(size = 8, face = "bold", vjust = -4))  
  
# NH4 plots ----
  # MB 2014
  pl_mb14_nh4 <- alldata %>% 
    # Add year column
    mutate(year = year(date)) %>% 
    # Filter site and year
    filter(site == "mb" & year == 2014) %>% 
    filter(date < "2014-05-01") %>% 
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
      scale_y_continuous(limits=c(0, 25),
                   breaks = seq(0, 20, by = 10)) +
      xlab("Day of the year") + 
      ylab(expression(paste("NH"["4"]^" +", " (",mu,"mol"," l"^"-1",")"))) +
      theme3 +
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
      geom_vline(xintercept=70, linetype="dashed") +
      geom_vline(xintercept=85, linetype="dashed") +
      geom_vline(xintercept=95, linetype="dashed") +
      xlab("Day of the year") +
      ylab(expression(paste("NH"["4"]^" +", " (",mu,"mol"," l"^"-1",")"))) +
      theme3 +
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
      geom_vline(xintercept=70, linetype="dashed") +
      geom_vline(xintercept=85, linetype="dashed") +
      geom_vline(xintercept=95, linetype="dashed") +
      facet_wrap(~samp_depth_cat2, ncol=1) +
      xlab("Day of the year") + 
      ylab(expression(paste("NH"["4"]^" +", " (",mu,"mol"," l"^"-1",")"))) +
      theme3 +
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
    filter(date < "2014-05-01") %>% 
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
      xlab("Day of the year") + 
      ylab(expression(paste("TN", " (",mu,"mol"," l"^"-1",")"))) +
      theme3 +
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
      xlab("Day of the year") +
      ylab(expression(paste("TN", " (",mu,"mol"," l"^"-1",")"))) +
      theme3 +
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
      xlab("Day of the year") + 
      ylab(expression(paste("TN", " (",mu,"mol"," l"^"-1",")"))) +
      theme3 +
      annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf) +
      annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf) +
      ggtitle("SP 2015")  
  
  
  # Combine these three NO3 plots into one plot
  pl_nh4_all <- plot_grid(pl_mb14_nh4, pl_mb15_nh4, pl_sp15_nh4,
                             ncol = 1, align = "hv")
  pl_tn_all <- plot_grid(pl_mb14_tn, pl_mb15_tn, pl_sp15_tn,
                             ncol = 1, align = "hv")  
  
  # Add common x and y axis titles
  y.grob_nh4 <- textGrob(expression(paste("NH"["4"]^" +", " (",mu,"mol"," l"^"-1",")")), gp = gpar(fontsize = 10), rot = 90, vjust = 0.5)
  y.grob_tn <- textGrob(expression(paste("TN", " (",mu,"mol"," l"^"-1",")")), gp = gpar(fontsize = 10), rot = 90, vjust = 0.5)
  x.grob_nh4tn <- textGrob("Day of the year", gp = gpar(fontsize = 10))
  
  # NH4 grob
  grob_nh4 <- grid.arrange(arrangeGrob(pl_nh4_all, left = y.grob_nh4)) 
  # TN grob
  grob_tn <- grid.arrange(arrangeGrob(pl_tn_all, left = y.grob_tn))  
  
  # NH4 & TN
  grob_nh4tn1 <- plot_grid(grob_nh4, grob_tn, ncol = 2, align = "hv")
  grob_nh4tn2 <- grid.arrange(arrangeGrob(grob_nh4tn1, bottom = x.grob_nh4tn))  
  
  # Save plot
  save_plot("03_figures/plot_suppInfo_nh4_tn.png", grob_nh4tn2,
            base_height = 10, base_width = 7, dpi = 150)

  
  
  
  
  

# Old plotting code ----  
  # 2014 MB - DO----
    # Subset MB 2014 DO data
    df_sub <- alldata %>% 
      filter(year(date) == 2014 & site == "mb" & !is.na(DO)) %>% 
      select(date, yday, depth, DO) %>% 
      group_by(date, yday, depth) %>% 
      # Take the mean of any DO measurements at the same time and depth
      summarize(DO = mean(DO, na.rm = T)) %>% 
      # Change the minimum depth (closest to 0) for each sampling date to 0; this is what D Joung did for figures in 2017 paper
      group_by(yday) %>% 
      mutate(depth = ifelse(depth == min(depth), 0, depth)) %>% 
      # Convert depths to negative for plotting purposes
      mutate(depth = depth*-1)

      # Interpolate data (akima::interp does gridded bivariate interpolation for irregular data)     
      int <- with(df_sub, interp(x = yday, y = depth, z = DO))
      # Create a data.frame frame from the interp() result
      df_int <- interp2xyz(int, data.frame = TRUE) %>%
        filter(!is.na(z)) %>%
        rename(yday = x, depth = y, DO = z) %>%
        arrange(yday, desc(depth))
      
      # Plot
      # Additional help on plot: https://stackoverflow.com/questions/38154679/r-adding-legend-and-directlabels-to-ggplot2-contour-plot
      # Method for labeling contours and keeping the color legend for the fill color gradient: https://github.com/tdhock/directlabels/issues/8
      # Help with label position options: http://directlabels.r-forge.r-project.org/docs/index.html
      p_2014_mb_do <- ggplot(data = df_int, aes(x = yday, y = depth, z = DO)) +
        geom_tile(aes(fill = DO)) +
        scale_fill_gradientn(colors = brewer.pal(n = 11, name = "Spectral"), trans = "reverse",
                             guide = guide_colorbar(reverse = TRUE),
                             limits = c(20,0), # choose this range based on range of all plots (i.e., 2014 MB & 2015 SP)
                             name = "DO (mg/L)") +          
        geom_contour(color = "gray30", breaks = c(5,10,15,20)) +
        geom_dl(aes(label = ..level..), breaks = c(5,10,15,20), method="bottom.pieces", stat="contour", color = "gray30") +
        geom_point(data = df_sub, aes(x = yday, y = depth), color = "white", shape = 1) +
        geom_vline(xintercept=78, linetype="dashed", color = "gray30", size = 2) + # change to 78 for 2014 plots and 70 for 2015
        xlab("Day of year") + ylab("Depth (m)") +
        theme1

      # Other ways to change color scheme in above plot
          #scale_fill_gradientn(colors = rainbow(5)) + 
          # scale_fill_distiller(palette = "Spectral") +
          # scale_fill_continuous(name = "DO (mg/l)",
          #                       low = "white", high = "blue") +   
  
  # 2015 MB - DO----
    # Subset MB 2015 DO data
    df_sub <- alldata %>% 
      filter(date > "2014-12-31" & site == "mb" & !is.na(DO)) %>% 
      select(date, yday, depth, DO) %>% 
      group_by(date, yday, depth) %>% 
      # Take the mean of any DO measurements at the same time and depth
      summarize(DO = mean(DO, na.rm = T)) %>% 
      # Change the minimum depth (closest to 0) for each sampling date to 0; this is what D Joung did for figures in 2017 paper
      group_by(yday) %>% 
      mutate(depth = ifelse(depth == min(depth), 0, depth)) %>% 
      # Convert depths to negative for plotting purposes
      mutate(depth = depth*-1)

      # Interpolate data (akima::interp does gridded bivariate interpolation for irregular data)     
      int <- with(df_sub, interp(x = yday, y = depth, z = DO))
      # Create a data.frame frame from the interp() result
      df_int <- interp2xyz(int, data.frame = TRUE) %>%
        filter(!is.na(z)) %>%
        rename(yday = x, depth = y, DO = z) %>%
        arrange(yday, desc(depth))
      
      # Plot
      # Additional help on plot: https://stackoverflow.com/questions/38154679/r-adding-legend-and-directlabels-to-ggplot2-contour-plot
      # Method for labeling contours and keeping the color legend for the fill color gradient: https://github.com/tdhock/directlabels/issues/8
      # Help with label position options: http://directlabels.r-forge.r-project.org/docs/index.html
      ggplot(data = df_int, aes(x = yday, y = depth, z = DO)) +
        geom_tile(aes(fill = DO)) +
        scale_fill_gradientn(colors = brewer.pal(n = 11, name = "Spectral"), trans = "reverse",
                             guide = guide_colorbar(reverse = TRUE),
                             limits = c(20,0), # choose this range based on range of all plots (i.e., 2014 MB & 2015 SP)
                             name = "DO (mg/L)") +          
        geom_contour(color = "gray30", breaks = c(5,10,15,20)) +
        geom_dl(aes(label = ..level..), breaks = c(5,10,15,20), method="bottom.pieces", stat="contour", color = "gray30") +
        geom_point(data = df_sub, aes(x = yday, y = depth), color = "white", shape = 1) +
        geom_vline(xintercept=70, linetype="dashed", color = "gray30", size = 2) + # change to 78 for 2014 plots and 70 for 2015
        xlab("Day of year") + ylab("Depth (m)") +
        theme1

      # Other ways to change color scheme in above plot
          #scale_fill_gradientn(colors = rainbow(5)) + 
          # scale_fill_distiller(palette = "Spectral") +
          # scale_fill_continuous(name = "DO (mg/l)",
          #                       low = "white", high = "blue") +        
          
  # 2015 MB - NO3----
    df_sub <- alldata %>% 
      filter(date > "2014-12-31" & site == "mb" & !is.na(NO3)) %>% 
      select(date, yday, depth, NO3) %>% 
      group_by(date, yday, depth) %>% 
      summarize(NO3 = mean(NO3, na.rm = T)) %>% 
      group_by(yday) %>% 
      mutate(depth = ifelse(depth == min(depth), 0, depth)) %>% 
      mutate(depth = depth*-1)

    int <- with(df_sub, interp(x = yday, y = depth, z = NO3))
    df_int <- interp2xyz(int, data.frame=TRUE) %>%
      filter(!is.na(z)) %>% 
      rename(yday = x, depth = y, NO3 = z) %>% 
      arrange(yday, desc(depth))
      
      # Find max and min for variable for all lakes and years
      #min(alldata$NO3, na.rm = T)
      #max(alldata$NO3, na.rm = T)
      
      # Plot - specify proper concentration range (limits) & appropriate breaks for the variable
        ggplot(data = df_int, aes(x = yday, y = depth, z = NO3)) +
          geom_tile(aes(fill = NO3)) +
          scale_fill_gradientn(colors = brewer.pal(n = 11, name = "Spectral"), trans = "reverse",
                               guide = guide_colorbar(reverse = TRUE),
                               limits = c(1,0), # choose this range based on range of all plots (i.e., 2014 MB & 2015 SP)
                               name = "NO3 (mg N/L)") + 
          geom_contour(color = "gray30", breaks = c(0.2,0.4,0.6,0.8)) +
          geom_dl(aes(label = ..level..), breaks = c(0.2,0.4,0.6,0.8), method="bottom.pieces", stat="contour", color = "gray30") +
          geom_point(data = df_sub, aes(x = yday, y = depth), color = "white", shape = 1) +
          geom_vline(xintercept=70, linetype="dashed", color = "gray30", size = 2) + # change to 78 for 2014 plots and 70 for 2015
          xlab("Day of year") + ylab("Depth (m)") +
          theme1
  
  
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

# -----

  
  
# ----Plot winter2015_chem_all data to look for interesting trends----
# Profiles of each N species plotted by date
  # Missisquoi Bay 2014
  # N plots
  mb_2014 %>% 
    filter(date < "2014-05-01") %>% 
    select(-TP) %>% 
    gather(key="analyte", value="conc", c(NO3:TN)) %>% 
    filter(!is.na(conc)) %>% 
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
         width=8, height=2, units="in", dpi=150)
  
  # TN:TP plots
  alldata %>% 
    filter(site == "mb" & date < "2014-05-01") %>% 
    select(date:depth, tntp) %>% 
    filter(!is.na(tntp)) %>% 
    group_by(yday, depth) %>% 
    summarize(mean.conc = mean(tntp, na.rm = T)) %>% # average the replicate samples
    ggplot(aes(x=depth*-1, y=mean.conc)) + # depth*-1 makes depths negative
      geom_line() + geom_point() +
      coord_flip() + 
      # scale_x_continuous(limits=c(-3.5, 0), 
      #                    breaks = seq(-3.5, 0, by=0.5)) +
      facet_wrap(~yday, ncol=8) +
      xlab("Depth (m)") + ylab("TN:TP") +
      theme_bw() +
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank())
    
  ggsave("/Users/dustinkincaid/ownCloud/bree_frozeN/03_figures/2014_winterN_profiles_tntp_mp.png", 
         width=8, height=2, units="in", dpi=150)  
  
  # TN & TP plots
  alldata %>% 
    filter(site == "mb" & date < "2014-05-01") %>% 
    select(date:depth, TN, TP) %>% 
    gather(key="analyte", value="conc", c(TN,TP)) %>% 
    filter(!is.na(conc)) %>% 
    group_by(yday, depth, analyte) %>% 
    summarize(mean.conc = mean(conc, na.rm = T)) %>% # average the replicate samples
    ggplot(aes(x=depth*-1, y=mean.conc, group=analyte, color=analyte)) + # depth*-1 makes depths negative
      geom_line() + geom_point() +
      coord_flip() + 
      # scale_x_continuous(limits=c(-3.5, 0), 
      #                    breaks = seq(-3.5, 0, by=0.5)) +
      facet_wrap(~yday, ncol=8) +
      xlab("Depth (m)") + ylab("Conc. (mg/L)") +
      theme_bw() +
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank())
    
  ggsave("/Users/dustinkincaid/ownCloud/bree_frozeN/03_figures/2014_winterN_profiles_bothTN_TP_mp.png", 
         width=8, height=2, units="in", dpi=150)
  
  # DO plot
  alldata %>% 
    filter(site == "mb" & date < "2014-05-01") %>% 
    select(date:depth, DO) %>% 
    filter(!is.na(DO)) %>% 
    group_by(yday, depth) %>% 
    summarize(mean.conc = mean(DO, na.rm = T)) %>% # average the replicate samples
    ggplot(aes(x=depth*-1, y=mean.conc)) + # depth*-1 makes depths negative
      geom_line() + geom_point() +
      coord_flip() + 
      # scale_x_continuous(limits=c(-3.5, 0), 
      #                    breaks = seq(-3.5, 0, by=0.5)) +
      facet_wrap(~yday, ncol=8) +
      xlab("Depth (m)") + ylab("DO (mg/L)") +
      theme_bw() +
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank())
    
  ggsave("/Users/dustinkincaid/ownCloud/bree_frozeN/03_figures/2014_winterN_profiles_DO_mp.png", 
         width=8, height=2, units="in", dpi=150)  
  
  # chla plot - filtered out high value on yday 64 at bottom of profile
  alldata %>% 
    filter(site == "mb" & date < "2014-05-01") %>% 
    select(date:depth, chla) %>% 
    filter(!is.na(chla), chla < 10) %>% 
    group_by(yday, depth) %>% 
    summarize(mean.conc = mean(chla, na.rm = T)) %>% # average the replicate samples
    ggplot(aes(x=depth*-1, y=mean.conc)) + # depth*-1 makes depths negative
      geom_line() + geom_point() +
      coord_flip() + 
      # scale_x_continuous(limits=c(-3.5, 0), 
      #                    breaks = seq(-3.5, 0, by=0.5)) +
      facet_wrap(~yday, ncol=8) +
      xlab("Depth (m)") + ylab("chl-a (ug/L)") +
      theme_bw() +
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank())
    
  ggsave("/Users/dustinkincaid/ownCloud/bree_frozeN/03_figures/2014_winterN_profiles_chla_mp.png", 
         width=8, height=2, units="in", dpi=150)  
  
  # turb plot
  alldata %>% 
    filter(site == "mb" & date < "2014-05-01") %>% 
    select(date:depth, turb) %>% 
    filter(!is.na(turb)) %>% 
    group_by(yday, depth) %>% 
    summarize(mean.conc = mean(turb, na.rm = T)) %>% # average the replicate samples
    ggplot(aes(x=depth*-1, y=mean.conc)) + # depth*-1 makes depths negative
      geom_line() + geom_point() +
      coord_flip() + 
      # scale_x_continuous(limits=c(-3.5, 0), 
      #                    breaks = seq(-3.5, 0, by=0.5)) +
      facet_wrap(~yday, ncol=8) +
      xlab("Depth (m)") + ylab("Turbidity (NTU)") +
      theme_bw() +
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank())
    
  ggsave("/Users/dustinkincaid/ownCloud/bree_frozeN/03_figures/2014_winterN_profiles_turb_mp.png", 
         width=8, height=2, units="in", dpi=150)  

  # cond plot
  alldata %>% 
    filter(site == "mb" & date < "2014-05-01") %>% 
    select(date:depth, cond) %>% 
    filter(!is.na(cond)) %>% 
    group_by(yday, depth) %>% 
    summarize(mean.conc = mean(cond, na.rm = T)) %>% # average the replicate samples
    ggplot(aes(x=depth*-1, y=mean.conc)) + # depth*-1 makes depths negative
      geom_line() + geom_point() +
      coord_flip() + 
      # scale_x_continuous(limits=c(-3.5, 0), 
      #                    breaks = seq(-3.5, 0, by=0.5)) +
      facet_wrap(~yday, ncol=8) +
      xlab("Depth (m)") + ylab("Cond. (uS/cm)") +
      theme_bw() +
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank())
    
  ggsave("/Users/dustinkincaid/ownCloud/bree_frozeN/03_figures/2014_winterN_profiles_cond_mp.png", 
         width=8, height=2, units="in", dpi=150)    
  
  
  # alldata %>% 
  #   filter(site == "mb" & date < "2014-05-01") %>% 
  #   select(date:depth, TP, TN, tntp, DO, chla) %>% 
  #   gather(key="analyte", value="conc", c(TP:chla)) %>% 
  #   group_by(yday, depth, analyte) %>% 
  #   summarize(mean.conc = mean(conc, na.rm = T)) %>% # average the replicate samples
  #   filter(!is.na(mean.conc)) %>% 
  #   ggplot(aes(x=depth*-1, y=mean.conc)) + # depth*-1 makes depths negative
  #     geom_line() + geom_point() +
  #     coord_flip() + 
  #     # scale_x_continuous(limits=c(-3.5, 0), 
  #     #                    breaks = seq(-3.5, 0, by=0.5)) +
  #     facet_grid(analyte~yday, scales = "free_x") +
  #     xlab("Depth (m)") + ylab("Conc. or ratio") +
  #     theme_bw() +
  #     theme(panel.grid.major = element_blank(), 
  #           panel.grid.minor = element_blank())
  #   
  # ggsave("/Users/dustinkincaid/ownCloud/bree_frozeN/03_figures/2014_winterN_profiles_tntpPLUS_mp.png", 
  #        width=10, height=10, units="in", dpi=150)    
  

  
# Missisquoi Bay 2015
  # Plots of N species
  winter2015_chem_all %>% 
    filter(site == "mb") %>% 
    select(-TP, -c(temp:BGA)) %>% 
    gather(key="analyte", value="conc", c(NO3:TN)) %>% 
    filter(!is.na(conc)) %>% 
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
  
  ggsave("/Users/dustinkincaid/ownCloud/bree_frozeN/03_figures/2015_winterN_profiles_mb.png", 
         width=8, height=2, units="in", dpi=150)

  # TN:TP plots
  alldata %>% 
    filter(site == "mb" & date > "2014-05-01") %>% 
    select(date:depth, tntp) %>% 
    filter(!is.na(tntp)) %>% 
    group_by(yday, depth) %>% 
    summarize(mean.conc = mean(tntp, na.rm = T)) %>% # average the replicate samples
    ggplot(aes(x=depth*-1, y=mean.conc)) + # depth*-1 makes depths negative
      geom_line() + geom_point() +
      coord_flip() + 
      # scale_x_continuous(limits=c(-3.5, 0), 
      #                    breaks = seq(-3.5, 0, by=0.5)) +
      facet_wrap(~yday, ncol=8) +
      xlab("Depth (m)") + ylab("TN:TP") +
      theme_bw() +
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank())
    
  ggsave("/Users/dustinkincaid/ownCloud/bree_frozeN/03_figures/2015_winterN_profiles_tntp_mb.png", 
         width=8, height=2, units="in", dpi=150)  
  
  # TN & TP plots
  alldata %>% 
    filter(site == "mb" & date > "2014-05-01") %>% 
    select(date:depth, TN, TP) %>% 
    gather(key="analyte", value="conc", c(TN,TP)) %>% 
    filter(!is.na(conc)) %>% 
    group_by(yday, depth, analyte) %>% 
    summarize(mean.conc = mean(conc, na.rm = T)) %>% # average the replicate samples
    ggplot(aes(x=depth*-1, y=mean.conc, group=analyte, color=analyte)) + # depth*-1 makes depths negative
      geom_line() + geom_point() +
      coord_flip() + 
      # scale_x_continuous(limits=c(-3.5, 0), 
      #                    breaks = seq(-3.5, 0, by=0.5)) +
      facet_wrap(~yday, ncol=8) +
      xlab("Depth (m)") + ylab("Conc. (mg/L)") +
      theme_bw() +
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank())
    
  ggsave("/Users/dustinkincaid/ownCloud/bree_frozeN/03_figures/2015_winterN_profiles_bothTN_TP_mb.png", 
         width=8, height=2, units="in", dpi=150)
  
  # DO plot
  alldata %>% 
    filter(site == "mb" & date > "2014-05-01") %>% 
    select(date:depth, DO) %>% 
    filter(!is.na(DO)) %>% 
    group_by(yday, depth) %>% 
    summarize(mean.conc = mean(DO, na.rm = T)) %>% # average the replicate samples
    ggplot(aes(x=depth*-1, y=mean.conc)) + # depth*-1 makes depths negative
      geom_line() + geom_point() +
      coord_flip() + 
      # scale_x_continuous(limits=c(-3.5, 0), 
      #                    breaks = seq(-3.5, 0, by=0.5)) +
      facet_wrap(~yday, ncol=8) +
      xlab("Depth (m)") + ylab("DO (mg/L)") +
      theme_bw() +
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank())
    
  ggsave("/Users/dustinkincaid/ownCloud/bree_frozeN/03_figures/2015_winterN_profiles_DO_mb.png", 
         width=8, height=2, units="in", dpi=150)  
  
  # chla plot
  alldata %>% 
    filter(site == "mb" & date > "2014-05-01") %>% 
    select(date:depth, chla) %>% 
    filter(!is.na(chla)) %>% 
    group_by(yday, depth) %>% 
    summarize(mean.conc = mean(chla, na.rm = T)) %>% # average the replicate samples
    ggplot(aes(x=depth*-1, y=mean.conc)) + # depth*-1 makes depths negative
      geom_line() + geom_point() +
      coord_flip() + 
      # scale_x_continuous(limits=c(-3.5, 0), 
      #                    breaks = seq(-3.5, 0, by=0.5)) +
      facet_wrap(~yday, ncol=8) +
      xlab("Depth (m)") + ylab("chl-a (ug/L)") +
      theme_bw() +
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank())
    
  ggsave("/Users/dustinkincaid/ownCloud/bree_frozeN/03_figures/2015_winterN_profiles_chla_mb.png", 
         width=8, height=2, units="in", dpi=150) 
  
  # turb plot
  alldata %>% 
    filter(site == "mb" & date > "2014-05-01") %>% 
    select(date:depth, turb) %>% 
    filter(!is.na(turb)) %>% 
    group_by(yday, depth) %>% 
    summarize(mean.conc = mean(turb, na.rm = T)) %>% # average the replicate samples
    ggplot(aes(x=depth*-1, y=mean.conc)) + # depth*-1 makes depths negative
      geom_line() + geom_point() +
      coord_flip() + 
      # scale_x_continuous(limits=c(-3.5, 0), 
      #                    breaks = seq(-3.5, 0, by=0.5)) +
      facet_wrap(~yday, ncol=8) +
      xlab("Depth (m)") + ylab("Turbidity (NTU)") +
      theme_bw() +
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank())
    
  ggsave("/Users/dustinkincaid/ownCloud/bree_frozeN/03_figures/2015_winterN_profiles_turb_mb.png", 
         width=8, height=2, units="in", dpi=150)

  # cond plot
  alldata %>% 
    filter(site == "mb" & date > "2014-05-01") %>% 
    select(date:depth, cond) %>% 
    filter(!is.na(cond)) %>% 
    group_by(yday, depth) %>% 
    summarize(mean.conc = mean(cond, na.rm = T)) %>% # average the replicate samples
    ggplot(aes(x=depth*-1, y=mean.conc)) + # depth*-1 makes depths negative
      geom_line() + geom_point() +
      coord_flip() + 
      # scale_x_continuous(limits=c(-3.5, 0), 
      #                    breaks = seq(-3.5, 0, by=0.5)) +
      facet_wrap(~yday, ncol=8) +
      xlab("Depth (m)") + ylab("Cond. (uS/cm)") +
      theme_bw() +
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank())
    
  ggsave("/Users/dustinkincaid/ownCloud/bree_frozeN/03_figures/2015_winterN_profiles_cond_mb.png", 
         width=8, height=2, units="in", dpi=150)   
  
  
# Shelburne Pond
  # Plots of N species
  winter2015_chem_all %>% 
    filter(site == "sp") %>% 
    select(-TP) %>% 
    gather(key="analyte", value="conc", c(NO3:TN)) %>% 
    filter(!is.na(conc)) %>% 
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
         width=8, height=2, units="in", dpi=150)

  # TN:TP plots
  alldata %>% 
    filter(site == "sp" & date > "2014-05-01") %>% 
    select(date:depth, tntp) %>% 
    filter(!is.na(tntp)) %>% 
    group_by(yday, depth) %>% 
    summarize(mean.conc = mean(tntp, na.rm = T)) %>% # average the replicate samples
    ggplot(aes(x=depth*-1, y=mean.conc)) + # depth*-1 makes depths negative
      geom_line() + geom_point() +
      coord_flip() + 
      # scale_x_continuous(limits=c(-3.5, 0), 
      #                    breaks = seq(-3.5, 0, by=0.5)) +
      facet_wrap(~yday, ncol=8) +
      xlab("Depth (m)") + ylab("TN:TP") +
      theme_bw() +
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank())
    
  ggsave("/Users/dustinkincaid/ownCloud/bree_frozeN/03_figures/2015_winterN_profiles_tntp_sp.png", 
         width=8, height=2, units="in", dpi=150)  
  
  # TN & TP plots
  alldata %>% 
    filter(site == "sp" & date > "2014-05-01") %>% 
    select(date:depth, TN, TP) %>% 
    gather(key="analyte", value="conc", c(TN,TP)) %>% 
    filter(!is.na(conc)) %>% 
    group_by(yday, depth, analyte) %>% 
    summarize(mean.conc = mean(conc, na.rm = T)) %>% # average the replicate samples
    ggplot(aes(x=depth*-1, y=mean.conc, group=analyte, color=analyte)) + # depth*-1 makes depths negative
      geom_line() + geom_point() +
      coord_flip() + 
      # scale_x_continuous(limits=c(-3.5, 0), 
      #                    breaks = seq(-3.5, 0, by=0.5)) +
      facet_wrap(~yday, ncol=8) +
      xlab("Depth (m)") + ylab("Conc. (mg/L)") +
      theme_bw() +
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank())
    
  ggsave("/Users/dustinkincaid/ownCloud/bree_frozeN/03_figures/2015_winterN_profiles_bothTN_TP_sp.png", 
         width=8, height=2, units="in", dpi=150)
  
  # DO plot
  alldata %>% 
    filter(site == "sp" & date > "2014-05-01") %>% 
    select(date:depth, DO) %>% 
    filter(!is.na(DO)) %>% 
    group_by(yday, depth) %>% 
    summarize(mean.conc = mean(DO, na.rm = T)) %>% # average the replicate samples
    ggplot(aes(x=depth*-1, y=mean.conc)) + # depth*-1 makes depths negative
      geom_line() + geom_point() +
      coord_flip() + 
      # scale_x_continuous(limits=c(-3.5, 0), 
      #                    breaks = seq(-3.5, 0, by=0.5)) +
      facet_wrap(~yday, ncol=8) +
      xlab("Depth (m)") + ylab("DO (mg/L)") +
      theme_bw() +
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank())
    
  ggsave("/Users/dustinkincaid/ownCloud/bree_frozeN/03_figures/2015_winterN_profiles_DO_sp.png", 
         width=8, height=2, units="in", dpi=150)  
  
  # chla plot
  alldata %>% 
    filter(site == "sp" & date > "2014-05-01") %>% 
    select(date:depth, chla) %>% 
    filter(!is.na(chla)) %>% 
    group_by(yday, depth) %>% 
    summarize(mean.conc = mean(chla, na.rm = T)) %>% # average the replicate samples
    ggplot(aes(x=depth*-1, y=mean.conc)) + # depth*-1 makes depths negative
      geom_line() + geom_point() +
      coord_flip() + 
      # scale_x_continuous(limits=c(-3.5, 0), 
      #                    breaks = seq(-3.5, 0, by=0.5)) +
      facet_wrap(~yday, ncol=8) +
      xlab("Depth (m)") + ylab("chl-a (ug/L)") +
      theme_bw() +
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank())
    
  ggsave("/Users/dustinkincaid/ownCloud/bree_frozeN/03_figures/2015_winterN_profiles_chla_sp.png", 
         width=8, height=2, units="in", dpi=150)   
  
  # turb plot
  alldata %>% 
    filter(site == "sp" & date > "2014-05-01") %>% 
    select(date:depth, turb) %>% 
    filter(!is.na(turb)) %>% 
    group_by(yday, depth) %>% 
    summarize(mean.conc = mean(turb, na.rm = T)) %>% # average the replicate samples
    ggplot(aes(x=depth*-1, y=mean.conc)) + # depth*-1 makes depths negative
      geom_line() + geom_point() +
      coord_flip() + 
      # scale_x_continuous(limits=c(-3.5, 0), 
      #                    breaks = seq(-3.5, 0, by=0.5)) +
      facet_wrap(~yday, ncol=8) +
      xlab("Depth (m)") + ylab("Turbidity (NTU)") +
      theme_bw() +
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank())
    
  ggsave("/Users/dustinkincaid/ownCloud/bree_frozeN/03_figures/2015_winterN_profiles_turb_sp.png", 
         width=8, height=2, units="in", dpi=150)
  
  # cond plot
  alldata %>% 
    filter(site == "sp" & date > "2014-05-01") %>% 
    select(date:depth, cond) %>% 
    filter(!is.na(cond)) %>% 
    group_by(yday, depth) %>% 
    summarize(mean.conc = mean(cond, na.rm = T)) %>% # average the replicate samples
    ggplot(aes(x=depth*-1, y=mean.conc)) + # depth*-1 makes depths negative
      geom_line() + geom_point() +
      coord_flip() + 
      # scale_x_continuous(limits=c(-3.5, 0), 
      #                    breaks = seq(-3.5, 0, by=0.5)) +
      facet_wrap(~yday, ncol=8) +
      xlab("Depth (m)") + ylab("Cond. (uS/cm)") +
      theme_bw() +
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank())
    
  ggsave("/Users/dustinkincaid/ownCloud/bree_frozeN/03_figures/2015_winterN_profiles_cond_sp.png", 
         width=8, height=2, units="in", dpi=150) 
  
  

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


# Interpolate 2015 sensor measurements & filter dataframe for rows with nutrient grab data
  # Here I use rule 2 on both left and right sides of the interval to get closest value for top and bottom depths if there was no value
  int_winter2015_chem_all <- winter2015_chem_all %>% 
    arrange(site, date, depth) %>% 
    group_by(site, date) %>% 
    mutate_at(vars(c(temp:BGA)),
              funs("i" = approx(x=depth, y=., xout=depth, rule=2, method="linear")[["y"]])) %>% 
    filter(!is.na(NO3)) %>% 
    select(date:samp_depth_cat, samp_depth_cat2, everything())