# Load packages----
  library("tidyverse")
  library("lubridate")
  # library("data.table")
  library("cowplot")
  library("akima")
  library("RColorBrewer")
  library("directlabels")
  library("grid")
  library("gridExtra")
  library("broom")
  library("patchwork")


# Read in data and tidy----
  # All concentrations are in umol/L (uM)
  alldata <- read_csv("01_raw data/alldata_2014_2015_mb_sp_compiled.csv") %>% 
    # Focus on the lake (vs river grab) data
    filter(location == "Lake") %>% 
    # Calculate ratios
    mutate(no3_srp = NO3/SRP,
           din_srp = (NO3 + NH4)/SRP,
           tn_tp = TN/TP)

# Look at P data
  # alldata %>%
  #   mutate(site_yr = paste(site, year(date), sep = "_")) %>%
  #   ggplot(aes(x = yday, y = NH4)) +
  #   facet_wrap(site_yr~samp_depth_cat2) +
  #   geom_point()
  
# Look at max ice thickness
  alldata %>% 
    group_by(site, year(date)) %>% 
    slice(which.max(ice_depth))
  
# Look at ice thickness stats
  # Here as Jason did, we first calculate the mean for eaching sampling data, then the mean for the whole season
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
  justN <- alldata %>%
    filter(!is.na(samp_depth_cat)) %>%
    group_by(site, year(date), yday, samp_depth_cat) %>%
    summarize(NH4 = mean(NH4, na.rm = T),
              NO3 = mean(NO3, na.rm = T),
              TN = mean(TN, na.rm = T)) %>%
    arrange(site, `year(date)`, samp_depth_cat, yday)
  
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
    
  

# Create Supp Info table of sampling dates and depths
# Sensor and grab together
# test <-
#   alldata %>% 
#   select(site, date, yday, depth, samp_depth_cat2, temp, NO3) %>% 
#   mutate(sensor = ifelse(!is.na(temp), "sensor", NA),
#          grab = ifelse(!is.na(NO3), "grab", NA),
#          Sample = ifelse(sensor == "sensor" & is.na(grab), "sensor",
#                          ifelse(sensor == "sensor" & grab == "grab", "grab, sensor", "grab"))) %>% 
#   # I can't get the scenario where Sample is "grab" to work for whatever reason; here's a fix
#   mutate(Sample = replace_na(Sample, "grab")) %>% 
#   select(-c(temp, NO3, sensor, grab)) %>% 
#   distinct() %>% 
#   # There are site, date, yday, depth, samp_depth_cat2 repeats where there are 2 NO3 measurements
#   # To eliminate them
#   group_by(site, date, yday, depth, samp_depth_cat2) %>% 
#   mutate(rep = row_number()) %>% 
#   filter(rep == 1) %>% 
#   select(-rep)

# Just sensor data
table_sensor <-
  alldata %>% 
  select(site, date, yday, depth, temp) %>% 
  filter(!is.na(temp)) %>% 
  select(-temp) %>% 
  rename(Site = site, Date = date, DOY = yday, "Depth (m)" = depth) %>% 
  mutate(Site = ifelse(Site == "mb", "MB", "SP")) %>% 
  mutate(Winter = year(Date)) %>% 
  select(Site, Winter, everything()) %>% 
  mutate(Date = as.character(Date))
# write_csv(table_sensor, "03_figures/table_SI_sensorDepths.csv")

# Just grab data
table_grab <-
  alldata %>% 
  select(site, date, yday, depth, samp_depth_cat2, NO3) %>% 
  filter(!is.na(NO3)) %>% 
  select(-NO3) %>% 
  distinct() %>% 
  rename(Site = site, Date = date, DOY = yday, "Depth (m)" = depth, "Depth ID" = samp_depth_cat2) %>% 
  mutate(Site = ifelse(Site == "mb", "MB", "SP")) %>% 
  mutate(Winter = year(Date)) %>% 
  select(Site, Winter, everything()) %>% 
  mutate(Date = as.character(Date))
# write_csv(table_grab, "03_figures/table_SI_grabDepths.csv")



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
      plot.margin = unit(c(0.05, 0.35, 0, 0.15), "in"),
      legend.title = element_blank(),
      # legend.title = element_text(size = 9),
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
        # This will add contour labels
        # geom_dl(aes(label = ..level..), breaks = cont_break_vec, method = list("bottom.pieces", cex = 0.5), stat="contour", color = "gray20") +
        scale_x_continuous(limits = c(10, 100), breaks = c(20, 40, 60, 80, 100)) +
        # scale_x_continuous(limits = if (extend_y) c(10, 120) else c(10, 100), 
        #                    breaks = c(20, 40, 60, 80, 100)) +
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
  # DO conc
    # MB 2014
    p_do_mb_2014 <- make_contourplot(df = alldata, year = 2014, site_exp = site == "mb", var = DO, leg_title = expression(DO~(mg~l^{-1})), 
                             leg_lim_vec = c(20,0), leg_break_vec = c(0,10,20), cont_break_vec = seq(0, 20, by = 2), ax_txt_y = TRUE)
    # MB 2015
    p_do_mb_2015 <- make_contourplot(df = alldata, year = 2015, site_exp = site == "mb", var = DO, leg_title = expression(DO~(mg~l^{-1})),  
                             leg_lim_vec = c(20,0), leg_break_vec = c(0,10,20), cont_break_vec = seq(0, 20, by = 2))
    # SP 2015
    p_do_sp_2015 <- make_contourplot(df = alldata, year = 2015, site_exp = site == "sp", var = DO, leg_title = expression(DO~(mg~l^{-1})),  
                             leg_lim_vec = c(20,0), leg_break_vec = c(0,10,20), cont_break_vec = seq(0, 20, by = 2), ax_txt_x = FALSE, col_guide = TRUE)
    
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
    
  # Cond
    # MB 2014
    p_cond_mb_2014 <- make_contourplot(df = alldata, year = 2014, site_exp = site == "mb", var = cond, leg_title = expression(Cond.~(mu*S~cm^{-1})), 
                             leg_lim_vec = c(400,0), leg_break_vec = c(0,200,400), cont_break_vec = seq(0, 400, by = 20), ax_txt_y = TRUE)
    # MB 2015
    p_cond_mb_2015 <- make_contourplot(df = alldata, year = 2015, site_exp = site == "mb", var = cond, leg_title = expression(Cond.~(mu*S~cm^{-1})), 
                             leg_lim_vec = c(400,0), leg_break_vec = c(0,200,400), cont_break_vec = seq(0, 400, by = 20))
    # SP 2015
    p_cond_sp_2015 <- make_contourplot(df = alldata, year = 2015, site_exp = site == "sp", var = cond, leg_title = expression(Cond.~(mu*S~cm^{-1})),
                             leg_lim_vec = c(400,0), leg_break_vec = c(0,200,400), cont_break_vec = seq(0, 400, by = 40), ax_txt_x = FALSE, col_guide = TRUE)
    
  # Turb
    # MB 2014
    p_turb_mb_2014 <- make_contourplot(df = alldata, year = 2014, site_exp = site == "mb", var = turb, leg_title = "Turb. (NTU)", 
                             leg_lim_vec = c(50,0), leg_break_vec = c(0,25,50), cont_break_vec = seq(0, 50, by = 10), ax_txt_y = TRUE)
    # MB 2015
    p_turb_mb_2015 <- make_contourplot(df = alldata, year = 2015, site_exp = site == "mb", var = turb, leg_title = "Turb. (NTU)", 
                             leg_lim_vec = c(50,0), leg_break_vec = c(0,25,50), cont_break_vec = seq(0, 50, by = 10))
    # SP 2015
    p_turb_sp_2015 <- make_contourplot(df = alldata, year = 2015, site_exp = site == "sp", var = turb, leg_title = "Turb. (NTU)",
                             leg_lim_vec = c(50,0), leg_break_vec = c(0,25,50), cont_break_vec = seq(0, 50, by = 10), ax_txt_x = FALSE, col_guide = TRUE)    
    
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
  

  
# Alt layout: with cond and turb included----
  # Create titles for figures
  title_2014MB <- textGrob("2014 MB", gp = gpar(fontsize = 10), vjust = 0.5)
  title_2015MB <- textGrob("2015 MB", gp = gpar(fontsize = 10), vjust = 0.5)
  title_2015SP <- textGrob("2015 SP", gp = gpar(fontsize = 10), vjust = 0.5)  
  
  # Arrange plots and add column titles in this first grob object
  grob1 <- grid.arrange(arrangeGrob(p_temp_mb_2014, p_do_mb_2014, p_chla_mb_2014, p_cond_mb_2014, p_turb_mb_2014, p_nh4_mb_2014, p_no3_mb_2014, p_tn_mb_2014, top = title_2014MB, ncol = 1),
                       arrangeGrob(p_temp_mb_2015, p_do_mb_2015, p_chla_mb_2015, p_cond_mb_2015, p_turb_mb_2015, p_nh4_mb_2015, p_no3_mb_2015, p_tn_mb_2015, top = title_2015MB, ncol = 1),
                       arrangeGrob(p_temp_sp_2015, p_do_sp_2015, p_chla_sp_2015, p_cond_sp_2015, p_turb_sp_2015, p_nh4_sp_2015, p_no3_sp_2015, p_tn_sp_2015, top = title_2015SP, ncol = 1),
                       ncol = 3)
  
  # Add common x and y axis titles
  y.grob <- textGrob("Depth (m)", gp = gpar(fontsize = 10), rot = 90, vjust = 1.25)
  x.grob <- textGrob("Day of year", gp = gpar(fontsize = 10))
  grob2 <- grid.arrange(arrangeGrob(grob1, left = y.grob, bottom = x.grob))

  # Save plot
  # No contour labels
  save_plot("03_figures/plot_contour_ALT_plusCondTurb_noColorGuides.png", grob2,
            base_height = 8, base_width = 7.67, dpi = 600)
  # Contour labels
  # save_plot("03_figures/plot_contour_ALT_allPlots_withColorGuides.png", grob2,
  #         base_height = 7.5, base_width = 7.67, dpi = 600)
  # The remaining edits (adding contour labels, etc) is done using Inkscape  
  
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
  x.grob <- textGrob("Day of year", gp = gpar(fontsize = 10))
  grob2 <- grid.arrange(arrangeGrob(grob1, left = y.grob, bottom = x.grob))

  # Save plot
  # No contour labels
  save_plot("03_figures/plot_contour_ALT_allPlots_noColorGuides.png", grob2,
            base_height = 7.5, base_width = 7.67, dpi = 600)
  # Contour labels
  # save_plot("03_figures/plot_contour_ALT_allPlots_withColorGuides.png", grob2,
  #         base_height = 7.5, base_width = 7.67, dpi = 600)
  # The remaining edits (adding contour labels, etc) is done using Inkscape
  
  
  
  # Create plots - original layout
  {
  # # Temp
  #   # MB 2014
  #   p_temp_mb_2014 <- make_contourplot(df = alldata, year = 2014, site_exp = site == "mb", var = temp, leg_title = expression(Temp.~(degree*C)), 
  #                            leg_lim_vec = c(6,0), leg_break_vec = c(0, 2, 4, 6), cont_break_vec = c(0,1,2,3,4,5,6), ax_txt_y = TRUE)
  #   # MB 2015
  #   p_temp_mb_2015 <- make_contourplot(df = alldata, year = 2015, site_exp = site == "mb", var = temp, leg_title = expression(Temp.~(degree*C)),
  #                            leg_lim_vec = c(6,0), leg_break_vec = c(0, 2, 4, 6), cont_break_vec = c(0,1,2,3,4,5,6), ax_txt_y = TRUE)
  #   # SP 2015
  #   p_temp_sp_2015 <- make_contourplot(df = alldata, year = 2015, site_exp = site == "sp", var = temp, leg_title = expression(Temp.~(degree*C)),
  #                            leg_lim_vec = c(6,0), leg_break_vec = c(0, 2, 4, 6), cont_break_vec = c(0,1,2,3,4,5,6), ax_txt_x = TRUE, ax_txt_y = TRUE)
  # # DO
  #   # MB 2014
  #   p_do_mb_2014 <- make_contourplot(df = alldata, year = 2014, site_exp = site == "mb", var = DO, leg_title = expression(DO~(mg~l^{-1})), 
  #                            leg_lim_vec = c(20,0), leg_break_vec = c(0,10,20), cont_break_vec = c(0,5,10,15,20))
  #   # MB 2015
  #   p_do_mb_2015 <- make_contourplot(df = alldata, year = 2015, site_exp = site == "mb", var = DO, leg_title = expression(DO~(mg~l^{-1})),  
  #                            leg_lim_vec = c(20,0), leg_break_vec = c(0,10,20), cont_break_vec = c(0,5,10,15,20))
  #   # SP 2015
  #   p_do_sp_2015 <- make_contourplot(df = alldata, year = 2015, site_exp = site == "sp", var = DO, leg_title = expression(DO~(mg~l^{-1})),  
  #                            leg_lim_vec = c(20,0), leg_break_vec = c(0,10,20), cont_break_vec = c(0,5,10,15,20), ax_txt_x = TRUE)
  # # chl a
  #   # MB 2014
  #   p_chla_mb_2014 <- make_contourplot(df = alldata, year = 2014, site_exp = site == "mb", var = chla, leg_title = expression(Chl~italic(a)~(mu*g~l^{-1})), 
  #                            leg_lim_vec = c(50,0), leg_break_vec = c(0,25,50), cont_break_vec = c(0,10,20,30,40,50))
  #   # MB 2015
  #   p_chla_mb_2015 <- make_contourplot(df = alldata, year = 2015, site_exp = site == "mb", var = chla, leg_title = expression(Chl~italic(a)~(mu*g~l^{-1})), 
  #                            leg_lim_vec = c(50,0), leg_break_vec = c(0,25,50), cont_break_vec = c(0,10,20,30,40,50))
  #   # SP 2015
  #   p_chla_sp_2015 <- make_contourplot(df = alldata, year = 2015, site_exp = site == "sp", var = chla, leg_title = expression(Chl~italic(a)~(mu*g~l^{-1})),
  #                            leg_lim_vec = c(50,0), leg_break_vec = c(0,25,50), cont_break_vec = c(0,10,20,30,40,50), ax_txt_x = TRUE)
  #   
  # # NH4 - umol N/L
  #   # MB 2014                                                                                                 ylab(expression(paste("NH"["4"]^" +", " (",mu,"M)")))
  #   p_nh4_mb_2014 <- make_contourplot(df = alldata, year = 2014, site_exp = site == "mb", var = NH4, leg_title = expression(paste("NH"["4"]^" +", " (",mu,"mol"," l"^"-1",")")), 
  #                            leg_lim_vec = c(40,0), leg_break_vec = c(0,20,40), cont_break_vec = c(0,10,20,30,40), ax_txt_y = TRUE)
  #   # MB 2015
  #   p_nh4_mb_2015 <- make_contourplot(df = alldata, year = 2015, site_exp = site == "mb", var = NH4, leg_title = expression(paste("NH"["4"]^" +", " (",mu,"mol"," l"^"-1",")")), 
  #                            leg_lim_vec = c(40,0), leg_break_vec = c(0,20,40), cont_break_vec = c(0,10,20,30,40), ax_txt_y = TRUE)
  #   # SP 2015
  #   p_nh4_sp_2015 <- make_contourplot(df = alldata, year = 2015, site_exp = site == "sp", var = NH4, leg_title = expression(paste("NH"["4"]^" +", " (",mu,"mol"," l"^"-1",")")),
  #                            leg_lim_vec = c(40,0), leg_break_vec = c(0,20,40), cont_break_vec = c(0,10,20,30,40), ax_txt_x = TRUE, ax_txt_y = TRUE)   
  # # NO3 - umol N/L
  #   # MB 2014
  #   p_no3_mb_2014 <- make_contourplot(df = alldata, year = 2014, site_exp = site == "mb", var = NO3, leg_title = expression(paste("NO"["3"]^" -", " (",mu,"mol"," l"^"-1",")")), 
  #                            leg_lim_vec = c(60,0), leg_break_vec = c(0,20,40,60), cont_break_vec = c(0,10,20,30,40,50,60))
  #   # MB 2015
  #   p_no3_mb_2015 <- make_contourplot(df = alldata, year = 2015, site_exp = site == "mb", var = NO3, leg_title = expression(paste("NO"["3"]^" -", " (",mu,"mol"," l"^"-1",")")), 
  #                            leg_lim_vec = c(60,0), leg_break_vec = c(0,20,40,60), cont_break_vec = c(0,10,20,30,40,50,60))
  #   # SP 2015
  #   p_no3_sp_2015 <- make_contourplot(df = alldata, year = 2015, site_exp = site == "sp", var = NO3, leg_title = expression(paste("NO"["3"]^" -", " (",mu,"mol"," l"^"-1",")")),
  #                            leg_lim_vec = c(60,0), leg_break_vec = c(0,20,40,60), cont_break_vec = c(0,10,20,30,40,50,60), ax_txt_x = TRUE)
  # # TN - umol N/L
  #   # MB 2014
  #   p_tn_mb_2014 <- make_contourplot(df = alldata, year = 2014, site_exp = site == "mb", var = TN, leg_title = expression(TN~(mu*mol~l^{-1})), 
  #                            leg_lim_vec = c(120,0), leg_break_vec = c(0,40,80,120), cont_break_vec = c(0,20,40,60,80,100,120))
  #   # MB 2015
  #   p_tn_mb_2015 <- make_contourplot(df = alldata, year = 2015, site_exp = site == "mb", var = TN, leg_title = expression(TN~(mu*mol~l^{-1})), 
  #                            leg_lim_vec = c(120,0), leg_break_vec = c(0,40,80,120), cont_break_vec = c(0,20,40,60,80,100,120))
  #   # SP 2015
  #   p_tn_sp_2015 <- make_contourplot(df = alldata, year = 2015, site_exp = site == "sp", var = TN, leg_title = expression(TN~(mu*mol~l^{-1})), 
  #                            leg_lim_vec = c(120,0), leg_break_vec = c(0,40,80,120), cont_break_vec = c(0,20,40,60,80,100,120), ax_txt_x = TRUE)
  # # TP - umol P/L - need to adjust limits and breaks
  #   # MB 2014
  #   p_tp_mb_2014 <- make_contourplot(df = alldata, year = 2014, site_exp = site == "mb", var = TP, leg_title = expression(TP~(mu*mol~l^{-1})), 
  #                            leg_lim_vec = c(4.5,0), leg_break_vec = c(0,2,4), cont_break_vec = c(0,1,2,3,4))
  #   # MB 2015
  #   p_tp_mb_2015 <- make_contourplot(df = alldata, year = 2015, site_exp = site == "mb", var = TP, leg_title = expression(TP~(mu*mol~l^{-1})), 
  #                            leg_lim_vec = c(4.5,0), leg_break_vec = c(0,2,4), cont_break_vec = c(0,1,2,3,4))
  #   # SP 2015
  #   p_tp_sp_2015 <- make_contourplot(df = alldata, year = 2015, site_exp = site == "sp", var = TP, leg_title = expression(TP~(mu*mol~l^{-1})), 
  #                            leg_lim_vec = c(4.5,0), leg_break_vec = c(0,2,4), cont_break_vec = c(0,1,2,3,4))
  # # TN:TP - umol P/L - need to adjust limits and breaks
  #   # MB 2014
  #   p_tntp_mb_2014 <- make_contourplot(df = alldata, year = 2014, site_exp = site == "mb", var = tntp, leg_title = "TN:TP", 
  #                            leg_lim_vec = c(400,0), leg_break_vec = c(0,200,400), cont_break_vec = c(0,100,200,300,400))
  #   # MB 2015
  #   p_tntp_mb_2015 <- make_contourplot(df = alldata, year = 2015, site_exp = site == "mb", var = tntp, leg_title = expression(TP~(mu*mol~l^{-1})), 
  #                            leg_lim_vec = c(400,0), leg_break_vec = c(0,200,400), cont_break_vec = c(0,100,200,300,400))
  #   # SP 2015
  #   p_tntp_sp_2015 <- make_contourplot(df = alldata, year = 2015, site_exp = site == "sp", var = tntp, leg_title = expression(TP~(mu*mol~l^{-1})), 
  #                            leg_lim_vec = c(400,0), leg_break_vec = c(0,200,400), cont_break_vec = c(0,100,200,300,400))
  }
  

# Compile ancillary contour plots (temp, DO, chl a)----  
  {
#   # Create titles for figures
#   title_temp <- textGrob(expression(Temp.~(degree*C)), gp = gpar(fontsize = 10), vjust = 0.5)
#   title_do <- textGrob(expression(D.O.~(mg~l^{-1})), gp = gpar(fontsize = 10), vjust = 0.5)
#   title_chla <- textGrob(expression(Chl.~italic(a)~(mu*g~l^{-1})), gp = gpar(fontsize = 10), vjust = 0.5)
#   
#   # Arrange plots and add column titles in this first grob object
#   grob1 <- grid.arrange(arrangeGrob(p_temp_mb_2014, p_temp_mb_2015, p_temp_sp_2015, top = title_temp, ncol = 1),
#                        arrangeGrob(p_do_mb_2014, p_do_mb_2015, p_do_sp_2015, top = title_do, ncol = 1),
#                        arrangeGrob(p_chla_mb_2014, p_chla_mb_2015, p_chla_sp_2015, top = title_chla, ncol = 1),
#                        ncol = 3)
#   
#   # Add common x and y axis titles
#   y.grob <- textGrob("Depth (m)", gp = gpar(fontsize = 10), rot = 90, vjust = 1.25)
#   x.grob <- textGrob("Day of the year", gp = gpar(fontsize = 10))
#   grob2 <- grid.arrange(arrangeGrob(grob1, left = y.grob, bottom = x.grob))
# 
#   # Save plot
#   save_plot("03_figures/plot_contour_ancillary_noInfo.png", grob2,
#             base_height = 4, base_width = 7.5, dpi = 300)
#   
# 
#   
# # Compile N contour plots (NH4, NO3, TN)----      
#   # Create titles for figures
#   title_nh4 <- textGrob(expression(paste("NH"["4"]^" +", " (",mu,"mol"," l"^"-1",")")), gp = gpar(fontsize = 10), vjust = 0.5)
#   title_no3 <- textGrob(expression(paste("NO"["3"]^" -", " (",mu,"mol"," l"^"-1",")")), gp = gpar(fontsize = 10), vjust = 0.5)
#   title_tn <- textGrob(expression(TN~(mu*mol~l^{-1})), gp = gpar(fontsize = 10), vjust = 0.5)
#   
#   # Arrange plots and add column titles in this first grob object
#   grob1_n <- grid.arrange(arrangeGrob(p_nh4_mb_2014, p_nh4_mb_2015, p_nh4_sp_2015, top = title_nh4, ncol = 1),
#                        arrangeGrob(p_no3_mb_2014, p_no3_mb_2015, p_no3_sp_2015, top = title_no3, ncol = 1),
#                        arrangeGrob(p_tn_mb_2014, p_tn_mb_2015, p_tn_sp_2015, top = title_tn, ncol = 1),
#                        ncol = 3)
#   
#   # Add common x and y axis titles
#   y.grob_n <- textGrob("Depth (m)", gp = gpar(fontsize = 10), rot = 90, vjust = 1.25)
#   x.grob_n <- textGrob("Day of the year", gp = gpar(fontsize = 10))
#   grob2_n <- grid.arrange(arrangeGrob(grob1_n, left = y.grob_n, bottom = x.grob_n))
# 
#   # Save plot
#   save_plot("03_figures/plot_contour_Nconc_noInfo.png", grob2_n,
#             base_height = 4, base_width = 7.5, dpi = 300) 
  }
  

  
# Plot all N species over time at each depth category----
  # https://cran.r-project.org/web/packages/broom/vignettes/broom_and_dplyr.html
  # https://r4ds.had.co.nz/many-models.html
  
# Linear regressions ----  
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

# Also going to log-transform nutrient concentrations to estimate first-order rate constant (k) for Supp Info
# lm_results_k <- alldata %>%
#   mutate(samp_depth_cat2 = factor(samp_depth_cat2, levels = c("Top", "Mid-1", "Mid-2", "Mid-3", "Bottom"))) %>% 
#   filter(!is.na(samp_depth_cat2)) %>% 
#   # Create a year column
#   mutate(year = year(date)) %>% 
#   # Let's filter the data to only do regressions on data before the thaw
#   filter((site == "mb" & year == 2014 & yday < 79) |
#            (site == "mb" & year == 2015 & yday < 70) |
#            (site == "sp" & year == 2015 & yday < 70)) %>% 
#   # To simplify the df, select relevant columns
#   select(site, year, date, yday, depth, samp_depth_cat, samp_depth_cat2, NH4:TN, SRP, TP) %>% 
#   # Pivot the measured variables into long format
#   pivot_longer(cols = c(NH4:TN, SRP, TP), names_to = "analyte", values_to = "conc") %>% 
#   # Group and nest the groupings - one regression for each transect date, reach section, and var
#   group_by(site, year, samp_depth_cat2, analyte) %>% 
#   nest() %>% 
#   # Here is where we regress the value of the variable on distance along reach
#   mutate(model = map(data, ~lm(log(conc) ~ yday, data = .x)),
#          tidied = map(model, tidy),
#          glanced = map(model, glance))
# 
# # Get the coefficient estimates and stats
# lm_results_k_coef <- lm_results_k %>% 
#   # Unnesting 'tidied' will give you a summary of the coefficients
#   unnest(tidied) %>%
#   select(-c(data, model, glanced)) %>% 
#   pivot_wider(names_from = term, values_from = c(estimate:p.value))
# 
# # Get R^2 and other summary stats
# lm_results_k_r2 <- lm_results_k %>% 
#   # Unnesting 'tidied' will give you a summary of the coefficients
#   unnest(glanced) %>% 
#   select(-c(data, model, tidied, sigma, statistic, p.value, df, logLik, AIC, BIC, deviance, df.residual))
# 
# # Join these together
# lm_results_k <- full_join(lm_results_k_coef, lm_results_k_r2, by = c("site", "year", "samp_depth_cat2", "analyte")) %>% 
#   # Drop unnecessary columns
#   select(-c(`std.error_(Intercept)`, `statistic_(Intercept)`, `p.value_(Intercept)`)) %>% 
#   # Round estimate, p-value, and r2
#   mutate_at(vars(`estimate_(Intercept)`:adj.r.squared),
#             ~round(., 3))
# rm(lm_results_k_coef, lm_results_k_r2)
# 
# # Look at only relationships for N species w/ p < 0.05 
# lm_results_k_sig <- 
#   lm_results_k %>% 
#   filter(analyte %in% c("NH4", "NO3", "TN")) %>% 
#   filter(p.value_yday < 0.05)
# 
# # Write to CSV
# lm_results_k_sig %>% write_csv("03_figures/table_SI_kestimates.csv")
  
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
  
  # Combine these three NO3 plots into one plot
  pl_no3_all <- plot_grid(pl_mb14, pl_mb15, NULL, pl_sp15, ncol = 2, align = "hv", hjust = 0.25)
  
  # Add common x and y axis titles
  y.grob_no3 <- textGrob(expression(paste("NO"["3"]^" -", " (",mu,"mol"," l"^"-1",")")), gp = gpar(fontsize = 14), rot = 90, vjust = 0.5)
  x.grob_no3 <- textGrob("Day of year", gp = gpar(fontsize = 14))
  grob_no3 <- grid.arrange(arrangeGrob(pl_no3_all, left = y.grob_no3, bottom = x.grob_no3))
  
  # Save plot
  # save_plot("03_figures/plot_no3_decline.png", grob_no3,
  #           base_height = 11.5, base_width = 4, dpi = 150)
  save_plot("03_figures/plot_no3_decline.png", grob_no3,
            base_height = 6, base_width = 6, dpi = 300)

  
  

# Plots for supporting information ----
# Set a theme for the NH4 and TN  plots
# theme3 <- theme_minimal() +
#   theme(
#         strip.text.x = element_text(size = 8),
#         panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(),
#         axis.ticks = element_line(),
#         # Adjust plot margin: top, right, bottom, left
#         plot.margin = unit(c(0, 0.1, 0, 0), "in"),
#         axis.text = element_text(size = 8),
#         axis.title = element_blank(),
#         # axis.title.x = element_text(size = 8, margin = margin(t = 5, r = 0, b = 0, l = 0)),
#         # axis.title.y = element_text(size = 8, margin = margin(t = 0, r = 0, b = 0, l = 0)),
#         plot.title = element_text(size = 8, face = "bold", vjust = -4))  
  
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
  
  # Combine these three NH4 plots into one plot
  pl_nh4_all <- plot_grid(pl_mb14_nh4, pl_mb15_nh4, NULL, pl_sp15_nh4, ncol = 2, align = "hv", hjust = 0.25)
  
  # Add common x and y axis titles
  y.grob_nh4 <- textGrob(expression(paste("NH"["4"]^" +", " (",mu,"mol"," l"^"-1",")")), gp = gpar(fontsize = 14), rot = 90, vjust = 0.5)
  x.grob_nh4 <- textGrob("Day of year", gp = gpar(fontsize = 14))
  grob_nh4 <- grid.arrange(arrangeGrob(pl_nh4_all, left = y.grob_nh4, bottom = x.grob_nh4))
  
  # Save plot
  save_plot("03_figures/plot_nh4_patterns.png", grob_nh4,
            base_height = 6, base_width = 6, dpi = 300)
  
  # Look at NH4 equations
  lm_nh4 <- lm_results %>% 
    filter(analyte == "NH4")
  
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
  
  # Combine these three TN plots into one plot
  pl_tn_all <- plot_grid(pl_mb14_tn, pl_mb15_tn, NULL, pl_sp15_tn, ncol = 2, align = "hv", hjust = 0.25)
  
  # Add common x and y axis titles
  y.grob_tn <- textGrob(expression(paste("TN", " (",mu,"mol"," l"^"-1",")")), gp = gpar(fontsize = 14), rot = 90, vjust = 0.5)
  x.grob_tn <- textGrob("Day of year", gp = gpar(fontsize = 14))
  grob_tn <- grid.arrange(arrangeGrob(pl_tn_all, left = y.grob_tn, bottom = x.grob_tn))
  
  # Save plot
  save_plot("03_figures/plot_tn_patterns.png", grob_tn,
            base_height = 6, base_width = 6, dpi = 300)  

  # Look at TN equations
  lm_tn <- lm_results %>% 
    filter(analyte == "TN")
  

# Alternate layout, includes NO3, NH4, TN (using library("patchwork"))
  pl_nh4_all_2 <- pl_mb14_nh4 + theme(plot.title = element_text(size = 14, hjust = 0.5, face = "plain"), axis.text.x = element_blank(), axis.title.y = element_text(size = 14), axis.text.y = element_text(size = 9)) +
                  pl_mb15_nh4 + theme(plot.title = element_text(size = 14, hjust = 0.5, face = "plain"), axis.text = element_blank()) +
                  pl_sp15_nh4 + theme(plot.title = element_text(size = 14, hjust = 0.5, face = "plain"), axis.text = element_blank())
  pl_no3_all_2 <- pl_mb14 + theme(plot.title = element_blank(), axis.text.x = element_blank(), axis.title.y = element_text(size = 14), axis.text.y = element_text(size = 9)) +
                  pl_mb15 + theme(plot.title = element_blank(), axis.text = element_blank()) +
                  pl_sp15 + theme(plot.title = element_blank(), axis.text = element_blank())
  pl_tn_all_2 <- pl_mb14_tn + theme(plot.title = element_blank(), axis.title.y = element_text(size = 14), axis.text.y = element_text(size = 9), axis.text.x = element_text(size = 9)) +
                 pl_mb15_tn + theme(plot.title = element_blank(), axis.title.x = element_text(margin = margin(t = 8), size = 15), axis.text.x = element_text(size = 9), axis.text.y = element_blank()) +
                 pl_sp15_tn + theme(plot.title = element_blank(), axis.text.x = element_text(size = 9), axis.text.y = element_blank())
  
  pl_N_comb <- pl_nh4_all_2 / pl_no3_all_2 / pl_tn_all_2
  
  ggsave("03_figures/plot_N_comb.png", plot = pl_N_comb, width = 7.5, height = 7, units = "in", dpi = 300)
  # I add equations and plot letters in Inkscape
  
  
# DO plots INCOMPLETE ----
  # Let's make the depths relative to the bottom of each lake (depth = 0 = bottom)
  depREV <-
    alldata %>% 
    arrange(site, date, depth) %>% 
    group_by(site, date) %>% 
    mutate(depth_max = max(depth),
           depth_reverse = depth_max - depth) %>% 
    mutate(depth_range = cut_width(depth_reverse, width = 0.5, boundary = 0))
  
  # Set theme
  theme_bar <- 
      theme_bw() +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            strip.background = element_rect(fill = "white"))  
  
  # Plot DO from bottom to +2m above bottom
  depREV %>% 
    filter(year(date) == 2015) %>% 
    mutate(sample_date = ifelse(yday %in% c(15), "DOY 15",
                                ifelse(yday %in% c(35, 37), "DOY 35-37",
                                       ifelse(yday %in% c(48, 49), "DOY 48-49",
                                              ifelse(yday %in% c(69), "DOY 69",
                                                     ifelse(yday %in% c(78), "DOY 78",
                                                            ifelse(yday %in% c(84), "DOY 84",
                                                                   ifelse(yday %in% c(86, 87), "DOY 86-87", "DOY 97")))))))) %>% 
    filter(depth_reverse <= 2) %>% 
    filter(!is.na(DO)) %>% 
    group_by(site, yday, sample_date, depth_range) %>% 
    summarize(DO_min = min(DO)) %>% 
    ggplot(aes(x = depth_range, y = DO_min, fill = site)) +
    geom_bar(position = position_dodge2(preserve = "single"), stat = "identity") +
    facet_wrap(~sample_date, ncol = 2) +
    scale_fill_manual(name = "Lake",
                      breaks = c("mb", "sp"),
                      labels = c("MB", "SP"),
                      values = c("black", "gray70")) +
    theme_bar
  
  
  # # Fit the linear regression models for each site, year, depth, and analyte combo
  # lm_results_DO <- alldata %>%
  #   mutate(depth_range = cut_width(depth, width = 1, boundary = 0)) %>%
  #   mutate(depth_range = factor(depth_range, levels = c("[0,1]", "(1,2]", "(2,3]", "(3,4]"), labels = c("0-1m", "1-2m", "2-3m", "3-4m"))) %>%
  #   filter(!is.na(DO)) %>%
  #   # Create a year column
  #   mutate(year = year(date)) %>%
  #   # Let's filter the data to only do regressions on data before the thaw
  #   filter((site == "mb" & year == 2014 & yday < 79) |
  #            (site == "mb" & year == 2015 & yday < 70) |
  #            (site == "sp" & year == 2015 & yday < 70)) %>%
  #   # To simplify the df, select relevant columns
  #   select(site, year, date, yday, depth, depth_range, DO) %>%
  #   # Group and nest the groupings - one regression for each transect date, reach section, and var
  #   group_by(site, year, depth_range) %>%
  #   nest() %>%
  #   # Here is where we regress the value of the variable on distance along reach
  #   mutate(model = map(data, ~lm(DO ~ yday, data = .x)),
  #          tidied = map(model, tidy),
  #          glanced = map(model, glance))
  # 
  # # Get the coefficient estimates and stats
  # lm_results_coef_DO <- lm_results_DO %>% 
  #   # Unnesting 'tidied' will give you a summary of the coefficients
  #   unnest(tidied) %>%
  #   select(-c(data, model, glanced)) %>% 
  #   pivot_wider(names_from = term, values_from = c(estimate:p.value))
  # 
  # # Get R^2 and other summary stats
  # lm_results_r2_DO <- lm_results_DO %>% 
  #   # Unnesting 'tidied' will give you a summary of the coefficients
  #   unnest(glanced) %>% 
  #   select(-c(data, model, tidied, sigma, statistic, p.value, df, logLik, AIC, BIC, deviance, df.residual))
  # 
  # # Join these together
  # lm_results_DO <- full_join(lm_results_coef_DO, lm_results_r2_DO) %>% 
  #   # Drop unnecessary columns
  #   select(-c(`std.error_(Intercept)`, `statistic_(Intercept)`, `p.value_(Intercept)`)) %>% 
  #   # Round estimate, p-value, and r2
  #   mutate_at(vars(`estimate_(Intercept)`:adj.r.squared),
  #             ~round(., 3))
  # rm(lm_results_coef_DO, lm_results_r2_DO)
  # 
  # # MB 2014
  # pl_mb14_DO <- alldata %>% 
  #   # Add year column
  #   mutate(year = year(date)) %>% 
  #   # Filter site and year
  #   filter(site == "mb" & year == 2014) %>% 
  #   filter(date < "2014-04-10") %>% 
  #   # Order the sample depth categories
  #   mutate(depth_range = cut_width(depth, width = 0.5, boundary = 0)) %>%
  #   mutate(depth_range2 = ifelse(depth_range %in% c("[0,0.5]", "(0.5,1]"), "0-1m",
  #                                ifelse(depth_range %in% c("(1,1.5]", "(1.5,2]"), "1-2m",
  #                                       ifelse(depth_range %in% c("(1,1.5]", "(1.5,2]"), "1-2m",))
  #   mutate(depth_range = factor(depth_range, levels = c("[0,1]", "(1,2]", "(2,3]", "(3,4]"), labels = c("0-1m", "1-2m", "2-3m", "3-4m"))) %>% 
  #   filter(!is.na(DO)) %>% 
  #   # Join DO~yday linear regression results
  #   left_join(lm_results_DO) %>% 
  #   ggplot(aes(x=yday, y=DO)) +
  #     facet_wrap(~depth_range, ncol=1) +
  #     geom_point() +
  #     geom_smooth(data = . %>% filter(p.value_yday < 0.05 & yday < 79), method=lm, se=FALSE, color="black") +
  #     geom_vline(xintercept=79, linetype="dashed") + #Need to figure out exactly when thaw period began see Joung et al. 2017 & Schroth et al. 2015
  #     # scale_y_continuous(limits=c(0, 65),
  #     #                    breaks = seq(0, 60, by = 30)) +
  #     scale_x_continuous(limits=c(10, 100),
  #                        breaks = seq(0, 100, by = 20)) +  
  #     xlab("Day of the year") + 
  #     ylab(expression(DO~(mg~l^{-1}))) +
  #     theme2 +
  #     annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf) +
  #     annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf) +
  #     ggtitle("MB 2014")
  # 
  # # MB 2015
  # pl_mb15_DO <-  alldata %>% 
  #   # Add year column
  #   mutate(year = year(date)) %>%   
  #   # Filter site and year
  #   filter(site == "mb" & year == 2015) %>% 
  #   # Order the sample depth categories
  #   # Order the sample depth categories
  #   mutate(depth_range = cut_width(depth, width = 1, boundary = 0)) %>%
  #   mutate(depth_range = factor(depth_range, levels = c("[0,1]", "(1,2]", "(2,3]", "(3,4]"), labels = c("0-1m", "1-2m", "2-3m", "3-4m"))) %>% 
  #   filter(!is.na(DO)) %>% 
  #   # Join DO~yday linear regression results
  #   left_join(lm_results_DO) %>% 
  #   ggplot(aes(x=yday, y=DO)) +
  #     facet_wrap(~depth_range, ncol=1) +
  #     geom_point() +
  #     geom_smooth(data = . %>% filter(p.value_yday < 0.05 & yday < 70), method=lm, se=FALSE, color="black") +
  #     geom_vline(xintercept=70, linetype="dashed") +
  #     geom_vline(xintercept=85, linetype="dashed") +
  #     geom_vline(xintercept=95, linetype="dashed") +
  #     # scale_y_continuous(limits=c(0, 65),
  #     #                    breaks = seq(0, 60, by = 30)) +
  #     scale_x_continuous(limits=c(10, 100),
  #                        breaks = seq(0, 100, by = 20)) +      
  #     xlab("Day of the year") +
  #     ylab(expression(DO~(mg~l^{-1}))) +
  #     theme2 +
  #     annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf) +
  #     annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf) +
  #     ggtitle("MB 2015")
  # 
  # # SP 2015
  # pl_sp15_DO <-  alldata %>% 
  #   # Add year column
  #   mutate(year = year(date)) %>%   
  #   # Filter site and year
  #   filter(site == "sp" & year == 2015) %>% 
  #   filter(!is.na(NO3)) %>% 
  #   # Order the sample depth categories
  #   mutate(samp_depth_cat2 = factor(samp_depth_cat2, levels = c("Top", "Mid-1", "Mid-2", "Mid-3", "Bottom"))) %>% 
  #   filter(!is.na(samp_depth_cat2)) %>% 
  #   # Join NO3~yday linear regression results
  #   left_join(lm_results %>% filter(analyte == "NO3"), by = c("site", "year", "samp_depth_cat2")) %>% 
  #   ggplot(aes(x=yday, y=NO3)) +
  #     geom_point() +
  #     geom_smooth(data = . %>% filter(p.value_yday < 0.05, yday < 70), method=lm, se=FALSE, color="black") +
  #     geom_vline(xintercept=70, linetype="dashed") +
  #     geom_vline(xintercept=85, linetype="dashed") +
  #     geom_vline(xintercept=95, linetype="dashed") +
  #     scale_y_continuous(limits=c(0, 65),
  #                        breaks = seq(0, 60, by = 30)) +
  #     scale_x_continuous(limits=c(10, 100),
  #                        breaks = seq(0, 100, by = 20)) +    
  #     facet_wrap(~samp_depth_cat2, ncol=1) +
  #     xlab("Day of the year") + 
  #     ylab(expression(paste("NO"["3"]^" -", " (",mu,"mol"," l"^"-1",")"))) +
  #     theme2 +
  #     annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf) +
  #     annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf) +
  #     ggtitle("SP 2015")
  # 
  # # Combine these three NO3 plots into one plot
  # pl_no3_all <- plot_grid(pl_mb14, pl_mb15, NULL, pl_sp15, ncol = 2, align = "hv", hjust = 0.25)
  # 
  # # Add common x and y axis titles
  # y.grob_no3 <- textGrob(expression(paste("NO"["3"]^" -", " (",mu,"mol"," l"^"-1",")")), gp = gpar(fontsize = 14), rot = 90, vjust = 0.5)
  # x.grob_no3 <- textGrob("Day of year", gp = gpar(fontsize = 14))
  # grob_no3 <- grid.arrange(arrangeGrob(pl_no3_all, left = y.grob_no3, bottom = x.grob_no3))
  # 
  # # Save plot
  # # save_plot("03_figures/plot_no3_decline.png", grob_no3,
  # #           base_height = 11.5, base_width = 4, dpi = 150)
  # save_plot("03_figures/plot_no3_decline.png", grob_no3,
  #           base_height = 6, base_width = 6, dpi = 300)  
  

# Stacked bar graph of N & P species ----
# Each figure represents the average concentration for the water column


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
    theme_bar

ggsave("03_figures/plot_Nspecies_stackedBar.png", width = 7, height = 3.5, units = "in", dpi = 150)

# P species
alldata %>% 
    # Drop columns with any missing values for P concs
    select(site, date, yday, SRP, DOP, PP) %>% 
    drop_na() %>% 
    # filter out 2014 sample date 115
    filter(yday != 115) %>% 
    # wide to long format
    pivot_longer(cols = c(SRP, DOP, PP), names_to = "analyte", values_to = "conc") %>% 
    # calculate whole water column mean
    group_by(site, year(date), yday, analyte) %>% 
    summarize(conc_mean = mean(conc, na.rm = T)) %>% 
    # plot
    ggplot(aes(x = as.factor(yday), y = conc_mean, fill = analyte)) +
    facet_wrap(site~`year(date)`, scales = "free_x", labeller = labeller(site = labels)) +
    geom_bar(position = "stack", stat = "identity") +
    # scale_fill_manual(name = "P species",
    #                   breaks = c("N_other", "NH4", "NO3"),
    #                   labels = c("Unmeasured", "NH4", "NO3"),
    #                   values = c("gray80", "#1b9e77", "#7570b3")) +
    ylab(expression(paste("Conc.", " (",mu,"mol"," l"^"-1",")"))) +
    xlab("Day of the year") +
    theme_bar
    
  
  
# RATIOS ----  
# Linear regressions - log(ratio) ~ yday
lm_results_ratios <- alldata %>%
  mutate(samp_depth_cat2 = factor(samp_depth_cat2, levels = c("Top", "Mid-1", "Mid-2", "Mid-3", "Bottom"))) %>% 
  filter(!is.na(samp_depth_cat2)) %>% 
  # Create a year column
  mutate(year = year(date)) %>% 
  # Let's filter the data to only do regressions on data before the thaw
  filter((site == "mb" & year == 2014 & yday < 79) |
           (site == "mb" & year == 2015 & yday < 70) |
           (site == "sp" & year == 2015 & yday < 70)) %>% 
  # To simplify the df, select relevant columns
  select(site, year, date, yday, depth, samp_depth_cat, samp_depth_cat2, no3_srp, din_srp, tn_tp) %>% 
  # Pivot the measured variables into long format
  pivot_longer(cols = c(no3_srp, din_srp, tn_tp), names_to = "ratio", values_to = "value") %>% 
  # Group and nest the groupings - one regression for each transect date, reach section, and var
  group_by(site, year, samp_depth_cat2, ratio) %>% 
  nest() %>% 
  # Here is where we regress the value of the variable on distance along reach
  ### CHOOSE TO DO BY LOG(RATIO) OR NOT ###
  # mutate(model = map(data, ~lm(log(value) ~ yday, data = .x)),
  mutate(model = map(data, ~lm(value ~ yday, data = .x)),
         tidied = map(model, tidy),
         glanced = map(model, glance))

# Get the coefficient estimates and stats
lm_results_coef_ratios <- lm_results_ratios %>% 
  # Unnesting 'tidied' will give you a summary of the coefficients
  unnest(tidied) %>%
  select(-c(data, model, glanced)) %>% 
  pivot_wider(names_from = term, values_from = c(estimate:p.value))

# Get R^2 and other summary stats
lm_results_r2_ratios <- lm_results_ratios %>% 
  # Unnesting 'tidied' will give you a summary of the coefficients
  unnest(glanced) %>% 
  select(-c(data, model, tidied, sigma, statistic, p.value, df, logLik, AIC, BIC, deviance, df.residual))

# Join these together
lm_results_ratios <- full_join(lm_results_coef_ratios, lm_results_r2_ratios, by = c("site", "year", "samp_depth_cat2", "ratio")) %>% 
  # Drop unnecessary columns
  select(-c(`std.error_(Intercept)`, `statistic_(Intercept)`, `p.value_(Intercept)`)) %>% 
  # Round estimate, p-value, and r2
  mutate_at(vars(`estimate_(Intercept)`:adj.r.squared),
            ~round(., 3)) %>% 
  arrange(ratio)

rm(lm_results_coef_ratios, lm_results_r2_ratios)

# NO3:SRP plots ----
  # MB 2014
  pl_rat_dis_mb14 <- alldata %>% 
    # Add year column
    mutate(year = year(date)) %>% 
    # Filter site and year
    filter(site == "mb" & year == 2014) %>% 
    filter(date < "2014-05-01") %>% 
    filter(!is.na(no3_srp)) %>% 
    # filter out 2014 sample date 115
    filter(yday != 115) %>% 
    # Order the sample depth categories
    mutate(samp_depth_cat2 = factor(samp_depth_cat2, levels = c("Top", "Mid-1", "Mid-2", "Mid-3", "Bottom"))) %>% 
    filter(!is.na(samp_depth_cat2)) %>% 
    # Calculate the 
    # Join log(no3_srp)~yday linear regression results
    left_join(lm_results_ratios %>% filter(ratio == "no3_srp"), by = c("site", "year", "samp_depth_cat2")) %>%
    # ggplot(aes(x=yday, y=log(no3_srp))) +
    ggplot(aes(x=yday, y=no3_srp)) +
      facet_wrap(~samp_depth_cat2, ncol=1) +
      geom_point() +
      geom_smooth(data = . %>% filter(p.value_yday < 0.05 & yday < 79), method=lm, se=FALSE, color="black") +
      geom_vline(xintercept=79, linetype="dashed") + #Need to figure out exactly when thaw period began see Joung et al. 2017 & Schroth et al. 2015
      # scale_y_continuous(sec.axis = sec_axis(trans = ~ 10 ^ ., breaks = c(10^-9, 10^-8, 10^-7))) +
      # scale_y_continuous(limits = c(5, 10), breaks = seq(6, 10, by = 2)) +
      scale_y_continuous(limits = c(0, 12000), breaks = seq(0, 12000, by = 4000)) +
      theme2 +
      annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf) +
      annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf) +
      ggtitle("MB 2014")
  
  # MB 2015
  pl_rat_dis_mb15 <- alldata %>% 
    # Add year column
    mutate(year = year(date)) %>%   
    # Filter site and year
    filter(site == "mb" & year == 2015) %>% 
    filter(!is.na(no3_srp)) %>% 
    # Order the sample depth categories
    mutate(samp_depth_cat2 = factor(samp_depth_cat2, levels = c("Top", "Mid-1", "Mid-2", "Mid-3", "Bottom"))) %>% 
    filter(!is.na(samp_depth_cat2)) %>% 
    # Join log(no3_srp)~yday linear regression results
    left_join(lm_results_ratios %>% filter(ratio == "no3_srp"), by = c("site", "year", "samp_depth_cat2")) %>% 
    # ggplot(aes(x=yday, y=log(no3_srp))) +
    ggplot(aes(x=yday, y=no3_srp)) +
      facet_wrap(~samp_depth_cat2, ncol=1) +
      geom_point() +
      geom_smooth(data = . %>% filter(p.value_yday < 0.05 & yday < 70), method=lm, se=FALSE, color="black") +
      geom_vline(xintercept=70, linetype="dashed") +
      geom_vline(xintercept=85, linetype="dashed") +
      geom_vline(xintercept=95, linetype="dashed") +
      # scale_y_continuous(limits = c(5, 10), breaks = seq(6, 10, by = 2)) +
      scale_y_continuous(limits = c(0, 10000), breaks = seq(0, 10000, by = 3000)) +
      theme2 +
      annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf) +
      annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf) +
      ggtitle("MB 2015")

  # SP 2015
  pl_rat_dis_sp15 <- alldata %>% 
    # Add year column
    mutate(year = year(date)) %>%   
    # Filter site and year
    filter(site == "sp" & year == 2015) %>% 
    filter(!is.na(no3_srp)) %>% 
    # Order the sample depth categories
    mutate(samp_depth_cat2 = factor(samp_depth_cat2, levels = c("Top", "Mid-1", "Mid-2", "Mid-3", "Bottom"))) %>% 
    filter(!is.na(samp_depth_cat2)) %>% 
    # Join log(no3_srp)~yday linear regression results
    left_join(lm_results_ratios %>% filter(ratio == "no3_srp"), by = c("site", "year", "samp_depth_cat2")) %>%
    # ggplot(aes(x=yday, y=log(no3_srp))) +
    ggplot(aes(x=yday, y=no3_srp)) +
      geom_point() +
      geom_smooth(data = . %>% filter(p.value_yday < 0.05, yday < 70), method=lm, se=FALSE, color="black") +
      geom_vline(xintercept=70, linetype="dashed") +
      geom_vline(xintercept=85, linetype="dashed") +
      geom_vline(xintercept=95, linetype="dashed") +
      facet_wrap(~samp_depth_cat2, ncol=1) +
      # scale_y_continuous(limits = c(5, 10), breaks = seq(6, 10, by = 2)) +
      scale_y_continuous(limits = c(0, 20000), breaks = seq(0, 20000, by = 6000)) +
      theme2 +
      annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf) +
      annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf) +
      ggtitle("SP 2015")

  
  # Combine these three no3_srp plots into one plot
  pl_no3_srp_all <- plot_grid(pl_rat_dis_mb14, pl_rat_dis_mb15, NULL, pl_rat_dis_sp15, ncol = 2, align = "hv", hjust = 0.25)
  
  # Add common x and y axis titles
  # y.grob_no3_srp <- textGrob(expression(log~"("~NO[3]^-{}~":"~SRP~")"), gp = gpar(fontsize = 14), rot = 90, vjust = 0.5)
  y.grob_no3_srp <- textGrob(expression(NO[3]^-{}~":"~SRP), gp = gpar(fontsize = 14), rot = 90, vjust = 0.5)
  x.grob_no3_srp <- textGrob("Day of year", gp = gpar(fontsize = 14))
  grob_no3_srp <- grid.arrange(arrangeGrob(pl_no3_srp_all, left = y.grob_no3_srp, bottom = x.grob_no3_srp))
  
  # Save plot
  save_plot("03_figures/plot_no3_srp_ratio_patterns.png", grob_no3_srp,
            base_height = 6, base_width = 6, dpi = 300)  

  # Look at no3_srp equations
  lm_no3_srp <- lm_results_ratios %>% 
    filter(ratio == "no3_srp")

  
# DIN:SRP plots ----
  # MB 2014
  pl_rat_din_mb14 <- alldata %>% 
    # Add year column
    mutate(year = year(date)) %>% 
    # Filter site and year
    filter(site == "mb" & year == 2014) %>% 
    filter(date < "2014-05-01") %>% 
    filter(!is.na(din_srp)) %>% 
    # filter out 2014 sample date 115
    filter(yday != 115) %>% 
    # Order the sample depth categories
    mutate(samp_depth_cat2 = factor(samp_depth_cat2, levels = c("Top", "Mid-1", "Mid-2", "Mid-3", "Bottom"))) %>% 
    filter(!is.na(samp_depth_cat2)) %>% 
    # Calculate the 
    # Join log(din_srp)~yday linear regression results
    left_join(lm_results_ratios %>% filter(ratio == "din_srp"), by = c("site", "year", "samp_depth_cat2")) %>%
    # ggplot(aes(x=yday, y=log(din_srp))) +
    ggplot(aes(x=yday, y=din_srp)) +
      facet_wrap(~samp_depth_cat2, ncol=1) +
      geom_point() +
      geom_smooth(data = . %>% filter(p.value_yday < 0.05 & yday < 79), method=lm, se=FALSE, color="black") +
      geom_vline(xintercept=79, linetype="dashed") + #Need to figure out exactly when thaw period began see Joung et al. 2017 & Schroth et al. 2015
      # scale_y_continuous(sec.axis = sec_axis(trans = ~ 10 ^ ., breaks = c(10^-9, 10^-8, 10^-7))) +
      # scale_y_continuous(limits = c(5, 10), breaks = seq(6, 10, by = 2)) +
      # scale_y_continuous(limits = c(0, 12000), breaks = seq(0, 12000, by = 4000)) +
      theme2 +
      annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf) +
      annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf) +
      ggtitle("MB 2014")
  
  # MB 2015
  pl_rat_din_mb15 <- alldata %>% 
    # Add year column
    mutate(year = year(date)) %>%   
    # Filter site and year
    filter(site == "mb" & year == 2015) %>% 
    filter(!is.na(din_srp)) %>% 
    # Order the sample depth categories
    mutate(samp_depth_cat2 = factor(samp_depth_cat2, levels = c("Top", "Mid-1", "Mid-2", "Mid-3", "Bottom"))) %>% 
    filter(!is.na(samp_depth_cat2)) %>% 
    # Join log(din_srp)~yday linear regression results
    left_join(lm_results_ratios %>% filter(ratio == "din_srp"), by = c("site", "year", "samp_depth_cat2")) %>% 
    # ggplot(aes(x=yday, y=log(din_srp))) +
    ggplot(aes(x=yday, y=din_srp)) +
      facet_wrap(~samp_depth_cat2, ncol=1) +
      geom_point() +
      geom_smooth(data = . %>% filter(p.value_yday < 0.05 & yday < 70), method=lm, se=FALSE, color="black") +
      geom_vline(xintercept=70, linetype="dashed") +
      geom_vline(xintercept=85, linetype="dashed") +
      geom_vline(xintercept=95, linetype="dashed") +
      # scale_y_continuous(limits = c(5, 10), breaks = seq(6, 10, by = 2)) +
      # scale_y_continuous(limits = c(0, 10000), breaks = seq(0, 10000, by = 3000)) +
      theme2 +
      annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf) +
      annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf) +
      ggtitle("MB 2015")

  # SP 2015
  pl_rat_din_sp15 <- alldata %>% 
    # Add year column
    mutate(year = year(date)) %>%   
    # Filter site and year
    filter(site == "sp" & year == 2015) %>% 
    filter(!is.na(din_srp)) %>% 
    # Order the sample depth categories
    mutate(samp_depth_cat2 = factor(samp_depth_cat2, levels = c("Top", "Mid-1", "Mid-2", "Mid-3", "Bottom"))) %>% 
    filter(!is.na(samp_depth_cat2)) %>% 
    # Join log(din_srp)~yday linear regression results
    left_join(lm_results_ratios %>% filter(ratio == "din_srp"), by = c("site", "year", "samp_depth_cat2")) %>%
    # ggplot(aes(x=yday, y=log(din_srp))) +
    ggplot(aes(x=yday, y=din_srp)) +
      geom_point() +
      geom_smooth(data = . %>% filter(p.value_yday < 0.05, yday < 70), method=lm, se=FALSE, color="black") +
      geom_vline(xintercept=70, linetype="dashed") +
      geom_vline(xintercept=85, linetype="dashed") +
      geom_vline(xintercept=95, linetype="dashed") +
      facet_wrap(~samp_depth_cat2, ncol=1) +
      # scale_y_continuous(limits = c(5, 10), breaks = seq(6, 10, by = 2)) +
      # scale_y_continuous(limits = c(0, 20000), breaks = seq(0, 20000, by = 6000)) +
      theme2 +
      annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf) +
      annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf) +
      ggtitle("SP 2015")

  
  # Combine these three no3_srp plots into one plot
  pl_din_srp_all <- plot_grid(pl_rat_din_mb14, pl_rat_din_mb15, NULL, pl_rat_din_sp15, ncol = 2, align = "hv", hjust = 0.25)
  
  # Add common x and y axis titles
  # y.grob_din_srp <- textGrob("DIN:SRP"), gp = gpar(fontsize = 14), rot = 90, vjust = 0.5)
  y.grob_din_srp <- textGrob("DIN:SRP", gp = gpar(fontsize = 14), rot = 90, vjust = 0.5)
  x.grob_din_srp <- textGrob("Day of year", gp = gpar(fontsize = 14))
  grob_din_srp <- grid.arrange(arrangeGrob(pl_din_srp_all, left = y.grob_din_srp, bottom = x.grob_din_srp))
  
  # Save plot
  save_plot("03_figures/plot_din_srp_ratio_patterns.png", grob_din_srp,
            base_height = 6, base_width = 6, dpi = 300)  

  # Look at din_srp equations
  lm_din_srp <- lm_results_ratios %>% 
    filter(ratio == "din_srp")  
  
# TN:TP plots ----
  # MB 2014
  pl_rat_tot_mb14 <- alldata %>% 
    # Add year column
    mutate(year = year(date)) %>% 
    # Filter site and year
    filter(site == "mb" & year == 2014) %>% 
    filter(date < "2014-05-01") %>% 
    filter(!is.na(tn_tp)) %>% 
    # Order the sample depth categories
    mutate(samp_depth_cat2 = factor(samp_depth_cat2, levels = c("Top", "Mid-1", "Mid-2", "Mid-3", "Bottom"))) %>% 
    filter(!is.na(samp_depth_cat2)) %>% 
    # Calculate the 
    # Join log(tn_tp)~yday linear regression results
    left_join(lm_results_ratios %>% filter(ratio == "tn_tp"), by = c("site", "year", "samp_depth_cat2")) %>%
    ggplot(aes(x=yday, y=log(tn_tp))) +
      facet_wrap(~samp_depth_cat2, ncol=1) +
      geom_point() +
      geom_smooth(data = . %>% filter(p.value_yday < 0.05 & yday < 79), method=lm, se=FALSE, color="black") +
      geom_vline(xintercept=79, linetype="dashed") + #Need to figure out exactly when thaw period began see Joung et al. 2017 & Schroth et al. 2015
      xlab("Day of the year") + 
      ylab(expression(log~"("~NO[3]^-{}~":"~SRP~")")) +
      # scale_y_continuous(sec.axis = sec_axis(trans = ~ 10 ^ ., breaks = c(10^-9, 10^-8, 10^-7))) +
      scale_y_continuous(limits = c(3, 6), breaks = seq(3, 6, by = 1)) +
      theme2 +
      annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf) +
      annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf) +
      ggtitle("MB 2014")
  
  # MB 2015
  pl_rat_tot_mb15 <- alldata %>% 
    # Add year column
    mutate(year = year(date)) %>%   
    # Filter site and year
    filter(site == "mb" & year == 2015) %>% 
    filter(!is.na(tn_tp)) %>% 
    # Order the sample depth categories
    mutate(samp_depth_cat2 = factor(samp_depth_cat2, levels = c("Top", "Mid-1", "Mid-2", "Mid-3", "Bottom"))) %>% 
    filter(!is.na(samp_depth_cat2)) %>% 
    # Join log(tn_tp)~yday linear regression results
    left_join(lm_results_ratios %>% filter(ratio == "tn_tp"), by = c("site", "year", "samp_depth_cat2")) %>% 
    ggplot(aes(x=yday, y=log(tn_tp))) +
      facet_wrap(~samp_depth_cat2, ncol=1) +
      geom_point() +
      geom_smooth(data = . %>% filter(p.value_yday < 0.05 & yday < 70), method=lm, se=FALSE, color="black") +
      geom_vline(xintercept=70, linetype="dashed") +
      geom_vline(xintercept=85, linetype="dashed") +
      geom_vline(xintercept=95, linetype="dashed") +
      xlab("Day of the year") +
      ylab(expression(log~"("~NO[3]^-{}~":"~SRP~")")) +
      scale_y_continuous(limits = c(3, 6), breaks = seq(3, 6, by = 1)) +
      theme2 +
      annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf) +
      annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf) +
      ggtitle("MB 2015")

  # SP 2015
  pl_rat_tot_sp15 <- alldata %>% 
    # Add year column
    mutate(year = year(date)) %>%   
    # Filter site and year
    filter(site == "sp" & year == 2015) %>% 
    filter(!is.na(tn_tp)) %>% 
    # Order the sample depth categories
    mutate(samp_depth_cat2 = factor(samp_depth_cat2, levels = c("Top", "Mid-1", "Mid-2", "Mid-3", "Bottom"))) %>% 
    filter(!is.na(samp_depth_cat2)) %>% 
    # Join log(tn_tp)~yday linear regression results
    left_join(lm_results_ratios %>% filter(ratio == "tn_tp"), by = c("site", "year", "samp_depth_cat2")) %>%
    ggplot(aes(x=yday, y=log(tn_tp))) +
      geom_point() +
      geom_smooth(data = . %>% filter(p.value_yday < 0.05, yday < 70), method=lm, se=FALSE, color="black") +
      geom_vline(xintercept=70, linetype="dashed") +
      geom_vline(xintercept=85, linetype="dashed") +
      geom_vline(xintercept=95, linetype="dashed") +
      facet_wrap(~samp_depth_cat2, ncol=1) +
      xlab("Day of the year") + 
      ylab(expression(log~"("~NO[3]^-{}~":"~SRP~")")) +
      scale_y_continuous(limits = c(3, 6), breaks = seq(3, 6, by = 1)) +
      theme2 +
      annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf) +
      annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf) +
      ggtitle("SP 2015")    
  
  # Combine these three tn_tp plots into one plot
  pl_tn_tp_all <- plot_grid(pl_rat_tot_mb14, pl_rat_tot_mb15, NULL, pl_rat_tot_sp15, ncol = 2, align = "hv", hjust = 0.25)
  
  # Add common x and y axis titles
  y.grob_tn_tp <- textGrob("log(TN:TP)", gp = gpar(fontsize = 14), rot = 90, vjust = 0.5)
  x.grob_tn_tp <- textGrob("Day of the year", gp = gpar(fontsize = 14))
  grob_tn_tp <- grid.arrange(arrangeGrob(pl_tn_tp_all, left = y.grob_tn_tp, bottom = x.grob_tn_tp))
  
  # Save plot
  save_plot("03_figures/plot_tn_tp_ratio_patterns.png", grob_tn_tp,
            base_height = 6, base_width = 6, dpi = 300)  

  # Look at tn_tp equations
  lm_tn_tp <- lm_results_ratios %>% 
    filter(ratio == "tn_tp")  
  
  
  
  
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