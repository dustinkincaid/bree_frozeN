# Example of use
make_counterplot(df = alldata, year = 2014, site = "mb" or "sp", var = "DO" or "NO3" etc, 
                 legname = "DO mg/L", leglimtits = c(20, 0), breaks = c(5,10,15,20))

make_contourplot <- function(df, year, site, var, legname, leglimits, breaks) {
  # Subset MB 2015 DO data
  df_sub <- df %>% 
    filter(year(date) == year & site == site & !is.na(var)) %>% 
    select(date, yday, depth, var) %>% 
    group_by(date, yday, depth) %>% 
    # Take the mean of any variable measurements at the same time and depth
    summarize(var = mean(var, na.rm = T)) %>% 
    # Change the minimum depth (closest to 0) for each sampling date to 0; this is what D Joung did for figures in 2017 paper
    group_by(yday) %>% 
    mutate(depth = ifelse(depth == min(depth), 0, depth)) %>% 
    # Convert depths to negative for plotting purposes
    mutate(depth = depth*-1)
  
  # Interpolate data (akima::interp does gridded bivariate interpolation for irregular data)     
  int <- with(df_sub, interp(x = yday, y = depth, z = var))
  # Create a data.frame frame from the interp() result
  df_int <- interp2xyz(int, data.frame = TRUE) %>%
    filter(!is.na(z)) %>%
    rename(yday = x, depth = y, var = z) %>%
    arrange(yday, desc(depth))
  
  # Plot
  # Additional help on plot: https://stackoverflow.com/questions/38154679/r-adding-legend-and-directlabels-to-ggplot2-contour-plot
  # Method for labeling contours and keeping the color legend for the fill color gradient: https://github.com/tdhock/directlabels/issues/8
  # Help with label position options: http://directlabels.r-forge.r-project.org/docs/index.html
  plot <- ggplot(data = df_int, aes(x = yday, y = depth, z = var)) +
    geom_tile(aes(fill = var)) +
    scale_fill_gradientn(colors = brewer.pal(n = 11, name = "Spectral"), trans = "reverse",
                         guide = guide_colorbar(reverse = TRUE),
                         limits = leglimtits, # choose this range based on range of all plots (i.e., 2014 MB & 2015 SP)
                         name = "DO (mg/L)") +          
    geom_contour(color = "gray30", breaks = c(5,10,15,20)) +
    geom_dl(aes(label = ..level..), breaks = c(5,10,15,20), method="bottom.pieces", stat="contour", color = "gray30") +
    geom_point(data = df_sub, aes(x = yday, y = depth), color = "white", shape = 1) +
    geom_vline(xintercept=70, linetype="dashed", color = "gray30", size = 2) + # change to 78 for 2014 plots and 70 for 2015
    xlab("Day of year") + ylab("Depth (m)") +
    theme1
  
  return(plot)
}

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
  
tidy_scanspectra <- function(df, type) {
  # basic input validation is extremely useful
  stopifnot(is.data.frame(df))
  stopifnot(is.character(type))

  colnames(df) <- df[1,]                            
  df <- df %>% 
  slice(-1) %>%                                                               
  rename(Date.Time = "Date/Time") %>% 
  mutate(timestamp = parse_date_time(Date.Time, "%Y.%m.%d %H:%M:%S")) %>%     
  select(timestamp, Date.Time, everything()) %>% select(-Date.Time) %>%       
  select(-c(starts_with("Status"):"217.5", "735":"750"))
  # pass the character type
  colnames(df)[-c(1,2)] <- paste(type, colnames(df)[-c(1,2)], sep = "_")
  return(df)
}