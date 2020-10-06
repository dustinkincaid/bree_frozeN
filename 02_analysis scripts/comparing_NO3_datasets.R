library("tidyverse")
library("lubridate")

# MISSISQUOI BAY ----
# Read in data
# All under-ice N data for MB in mg N/L
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
    # Add location (i.e., Lake)
    mutate(Location = "Lake") %>% 
    # Add data source
    mutate(source = "TN_TP_Excel") %>% 
    mutate(samp_depth_cat2 = ifelse(samp_depth_cat == "D1", "Top",
                                ifelse(samp_depth_cat == "D2", "Mid-1",
                                       ifelse(samp_depth_cat == "D3", "Mid-2",
                                              ifelse(samp_depth_cat == "D4", "Mid-3",
                                                     ifelse(samp_depth_cat == "D5", "Bottom", NA))))))

# Pull Miss Riv data from Joung et al 2017 Supp Info
mb_chem <- read.csv("01_raw data/WINTER 2015 MB_supp_data.csv", header=T, stringsAsFactors = F, na.strings = " ") %>% 
  select(-(ends_with("SD"))) %>% 
  select(Cal.Date, Jul.Day, Location, Depth_grab:DOC) %>% 
  rename(date=Cal.Date, yday = Jul.Day, depth=Depth_grab) %>% 
  mutate(date = mdy(as.character(date))) %>% 
  # Trying to figure out if NO3 is expressed as mg NO3/L or mg N/L
  # Here I assume conc is mg NO3/L and convert to mg N/L
  # mutate(NO3 = NO3*14.007/62.005) %>% 
  # It is clear that the Supp Info concs are in mg N/L, not mg NO3/L, so no need to convert
  filter(!is.na(NO3)) %>% 
  group_by(yday, Location) %>% 
  mutate(row = row_number()) %>% 
  mutate(samp_depth_cat2 = ifelse(row == 1, "Top",
                                  ifelse(row == 2, "Mid-1",
                                         ifelse(row == 3, "Mid-2",
                                                ifelse(row == 4, "Mid-3",
                                                       ifelse(row == 5, "Bottom", NA)))))) %>% 
  mutate(samp_depth_cat2 = replace(samp_depth_cat2, Location == "River", "River")) %>% 
  mutate(source = "SuppInfo")

# Append the df's to each other & add samp_depth_cat2
mb_all <- 
  bind_rows(mb_n, mb_chem)
        
# Create summary of N concs (means and SE)
mb_summ <-
  mb_all %>% 
  select(source, date, yday, Location, samp_depth_cat2, NO3:TN) %>% 
  pivot_longer(cols = NO3:TN, names_to = "var", values_to = "conc") %>% 
  group_by(source, date, yday, Location, samp_depth_cat2, var) %>% 
  summarize(n = length(conc),
            conc_mean = mean(conc, na.rm = T),
            conc_SE = sd(conc)/sqrt(n))

# Compare NO3 concs in TN_TP_Excel spreadsheet to those in Supp Info
mb_summ %>% 
  filter(var == "NO3") %>% 
  mutate(samp_depth_cat2 = factor(samp_depth_cat2, levels = c("Top", "Mid-1", "Mid-2", "Mid-3", "Bottom", "River"))) %>% 
  ggplot(aes(x = as.factor(yday), y = conc_mean, fill = source)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_errorbar(aes(ymin = conc_mean-conc_SE, ymax = conc_mean+conc_SE), width=.2, position=position_dodge(.9)) +
  facet_wrap(~samp_depth_cat2, ncol = 1) +
  ylab("NO3 conc. (mg N/L)") + xlab("Day of year")

ggsave("03_figures/plot_compare_NO3conc_sources_MB.png", dpi = 150)



# Shelburne Pond
# Read in data
# All under-ice N data for SP in mg N/L
sp_n <- read.csv("01_raw data/WINTER 2015 SP_TN-TP.csv", header=T, stringsAsFactors = F) %>% 
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

# Add depths to mb_n & join 2015 P data
  sp_n <- full_join(sp_n, sp_depths_df, by = c("date", "samp_depth_cat")) %>% 
    # Add rep no.
    group_by(date, samp_depth_cat) %>% 
    mutate(rep = row_number()) %>% 
    # Add yday
    mutate(yday = yday(date)) %>% 
    # Add location (i.e., Lake)
    mutate(Location = "Lake") %>% 
    # Add data source
    mutate(source = "TN_TP_Excel") %>% 
    mutate(samp_depth_cat2 = ifelse(samp_depth_cat == "D1", "Top",
                                ifelse(samp_depth_cat == "D2", "Mid-1",
                                       ifelse(samp_depth_cat == "D3", "Mid-2",
                                              ifelse(samp_depth_cat == "D4", "Mid-3",
                                                     ifelse(samp_depth_cat == "D5", "Bottom", NA))))))

# Pull SP data from Joung et al 2017 Supp Info
sp_chem <- read.csv("01_raw data/WINTER 2015 SP_supp_data.csv", header=T, stringsAsFactors = F, na.strings = " ") %>% 
  select(-(ends_with("SD"))) %>% 
  select(Cal.Date, Jul.Day, Location, Depth_grab:DOC) %>% 
  rename(date=Cal.Date, yday = Jul.Day, depth=Depth_grab) %>% 
  mutate(date = mdy(as.character(date))) %>% 
  # Trying to figure out if NO3 is expressed as mg NO3/L or mg N/L
  # Here I assume conc is mg NO3/L and convert to mg N/L
  # mutate(NO3 = NO3*14.007/62.005) %>% 
  # It is clear that the Supp Info concs are in mg N/L, not mg NO3/L, so no need to convert
  filter(!is.na(NO3)) %>% 
  group_by(yday, Location) %>% 
  mutate(row = row_number()) %>% 
  mutate(samp_depth_cat2 = ifelse(row == 1, "Top",
                                  ifelse(row == 2, "Mid-1",
                                         ifelse(row == 3, "Mid-2",
                                                ifelse(row == 4, "Mid-3",
                                                       ifelse(row == 5, "Bottom", NA)))))) %>% 
  mutate(samp_depth_cat2 = replace(samp_depth_cat2, Location == "River", "River")) %>% 
  mutate(source = "SuppInfo")

# Append the df's to each other & add samp_depth_cat2
sp_all <- 
  bind_rows(sp_n, sp_chem)
        
# Create summary of N concs (means and SE)
sp_summ <-
  sp_all %>% 
  select(source, date, yday, Location, samp_depth_cat2, NO3:TN) %>% 
  pivot_longer(cols = NO3:TN, names_to = "var", values_to = "conc") %>% 
  group_by(source, date, yday, Location, samp_depth_cat2, var) %>% 
  summarize(n = length(conc),
            conc_mean = mean(conc, na.rm = T),
            conc_SE = sd(conc)/sqrt(n))

# Compare NO3 concs in TN_TP_Excel spreadsheet to those in Supp Info
sp_summ %>% 
  filter(var == "NO3") %>% 
  mutate(samp_depth_cat2 = factor(samp_depth_cat2, levels = c("Top", "Mid-1", "Mid-2", "Mid-3", "Bottom", "River"))) %>% 
  ggplot(aes(x = as.factor(yday), y = conc_mean, fill = source)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_errorbar(aes(ymin = conc_mean-conc_SE, ymax = conc_mean+conc_SE), width=.2, position=position_dodge(.9)) +
  facet_wrap(~samp_depth_cat2, ncol = 1) +
  ylab("NO3 conc. (mg N/L)") + xlab("Day of year")

ggsave("03_figures/plot_compare_NO3conc_sources_SP.png", dpi = 150)
