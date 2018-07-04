# Load libraries
library(tidyverse)
library(lubridate)


# ----Read in 2014 grab sample chemistry data for Missisquoi Bay----
mb_2014.raw <- read.csv("/Users/dustinkincaid/ownCloud/bree_frozeN/01_raw data/Copy of Lake Grab 2014_2-6-15_sb.csv", 
                          header=T, stringsAsFactors = F, na.strings = c("", " ", "#N/A"))

# Tidy these up
mb_2014 <- mb_2014.raw %>% 
  select(Depth.or.BLANK, Date.Collected, TP.Corrected, Nox.Corrected, NH4.Corrected, TN.Corrected) %>% 
  filter(Depth.or.BLANK != "blank", Depth.or.BLANK != "blank ", Depth.or.BLANK != "Blank", !is.na(Depth.or.BLANK)) %>% 
  rename(date=Date.Collected, samp_depth_cat=Depth.or.BLANK, TP=TP.Corrected, NO3=Nox.Corrected, NH4=NH4.Corrected, TN=TN.Corrected) %>% 
  mutate(date = mdy(as.character(date), tz="US/Eastern"),
         site = "mb")  

# Update Julian day
mb_2014 <- mb_2014 %>% mutate(yday = yday(date))

# Add a numerical depth column
# NOTE: these are temporary estimates; need to get bottom depth for each sampling date and reference
mb_2014$depth <- NA
mb_2014$depth[mb_2014$samp_depth_cat %in% c("below ice", "surface", "surface ", "Surface", "suface")] <- 0.5
mb_2014$depth[mb_2014$samp_depth_cat %in% c("1 m", "1 m ")] <- 1
mb_2014$depth[mb_2014$samp_depth_cat %in% c("1 m from bottom", "B +1")] <- 2.5
mb_2014$depth[mb_2014$samp_depth_cat %in% c(".5 m from bottom")] <- 3
mb_2014$depth[mb_2014$samp_depth_cat %in% c("bottom", "bottom ", "Bottom")] <- 3.5

# ------------------------------------------------------------------


# ----Read in 2015 under ice water chemistry data for Missisquoi Bay & Shelburne Pond----
# Missisquoi Bay
mb_n.raw <- read.csv("/Users/dustinkincaid/ownCloud/bree_frozeN/01_raw data/WINTER 2015 MB_TN-TP.csv", 
                     header=T, stringsAsFactors = F)
# Shelburne Pond
sp_n.raw <- read.csv("/Users/dustinkincaid/ownCloud/bree_frozeN/01_raw data/WINTER 2015 SP_TN-TP.csv", 
                     header=T, stringsAsFactors = F)

# Tidy up these data and combine into one
mb_n <- mb_n.raw %>% 
  select(Date.Collected, Depth.or.BLANK, TP.Corrected, Nox.Corrected, NH4.Corrected, TN.Corrected) %>% 
  rename(date=Date.Collected, samp_depth_cat=Depth.or.BLANK, TP=TP.Corrected, NO3=Nox.Corrected, NH4=NH4.Corrected, TN=TN.Corrected) %>% 
  mutate(date = mdy(as.character(date), tz="US/Eastern"),
         yday = yday(date),
         site = "mb")
# Correct one sampling date
mb_n$date[mb_n$date == "2015-03-27"] <- "2015-03-28"
# Update Julian day
mb_n <- mb_n %>% mutate(yday = yday(date))

# Add measured depths as column
# Currently using the depths as reported in 2015 Winter -All sensor data.xlsx Sheet4 column AD
mb_depths = c(0.5, 2.5, 3.0, 3.3, 3.5, #1/15/2015
              0.5, 2.8, 3.0, 3.2, 3.5, #2/4/2015
              0.7, 2.3, 2.8, 3.15, 3.3, #2/17/2015
              0.65, 2.1, 2.6, 2.75, 3.05, #3/10/2015 
              0.7, 2.1, 2.6, 2.85, 3.1, #3/19/2015 
              0.7, 2.1, 2.6, 2.85, 3.05, #3/25/2015
              0.7, 2.1, 2.6, 2.85, 3.12, #3/28/2015
              0.6, 2.38, 2.88, 3.13, 3.38 #4/6/2015
              )
# Create a df with values to match and merge with mb_n
mb_depths_df <- data.frame(date = rep(unique(mb_n$date), each=5), samp_depth_cat = rep(unique(mb_n$samp_depth_cat), 8), depth = mb_depths)
# Join to add depths to mb_n
mb_n <- full_join(mb_n, mb_depths_df, by = c("date", "samp_depth_cat"))

# Repeat for Shelburne Pond data
sp_n <- sp_n.raw %>% 
  select(Date.Collected, Depth.or.BLANK, TP.Corrected, Nox.Corrected, NH4.Corrected, TN.Corrected) %>% 
  rename(date=Date.Collected, samp_depth_cat=Depth.or.BLANK, TP=TP.Corrected, NO3=Nox.Corrected, NH4=NH4.Corrected, TN=TN.Corrected) %>% 
  mutate(date = mdy(as.character(date), tz="US/Eastern"),
         site = "sp")
# Correct sampling dates
sp_n$date[sp_n$date == "2015-02-04"] <- "2015-02-06"
sp_n$date[sp_n$date == "2015-02-17"] <- "2015-02-18"
# Update Julian day
sp_n <- sp_n %>% mutate(yday = yday(date))

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

winter2015_chem_all <- bind_rows(mb_n, sp_n)
# ---------------------------------------------------------------------------------------


# ----Plot winter2015_chem_all data to look for interesting trends----
# Profiles of each N species plotted by date
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
    xlab("Depth (cm)") + ylab("Conc. (mg N/L)") +
    theme_bw() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank())

ggsave("/Users/dustinkincaid/ownCloud/bree_frozeN/03_figures/2014_winterN_profiles_mp.png", width=10, height=3, units="in", dpi=150)

# Missisquoi Bay 2015
winter2015_chem_all %>% 
  filter(site == "mb") %>% 
  select(-TP) %>% 
  gather(key="analyte", value="conc", c(NO3:TN)) %>% 
  group_by(yday, depth, analyte) %>% 
  summarize(mean.conc = mean(conc, na.rm = T)) %>% #average the replicate samples
  ggplot(aes(x=depth, y=mean.conc, group=analyte, color=analyte)) + 
    geom_line() + geom_point() +
    coord_flip() + scale_x_reverse() +
    facet_wrap(~yday, ncol=8) +
    xlab("Depth (cm)") + ylab("Conc. (mg N/L)") +
    theme_bw() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank())

ggsave("/Users/dustinkincaid/ownCloud/bree_frozeN/03_figures/2015_winterN_profiles_mp.png", width=10, height=3, units="in", dpi=150)

# Shelburne Pond
winter2015_chem_all %>% 
  filter(site == "sp") %>% 
  select(-TP) %>% 
  gather(key="analyte", value="conc", c(NO3:TN)) %>% 
  group_by(yday, depth, analyte) %>% 
  summarize(mean.conc = mean(conc, na.rm = T)) %>% #average the replicate samples
  ggplot(aes(x=depth, y=mean.conc, group=analyte, color=analyte)) + 
    geom_line() + geom_point() +
    coord_flip() + scale_x_reverse() +
    facet_wrap(~yday, ncol=8) +
    xlab("Depth (cm)") + ylab("Conc. (mg N/L)") +
    theme_bw() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank())

ggsave("/Users/dustinkincaid/ownCloud/bree_frozeN/03_figures/2015_winterN_profiles_sp.png", width=10, height=3, units="in", dpi=150)

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

ggsave("/Users/dustinkincaid/ownCloud/bree_frozeN/03_figures/2014_winterN_timeseries_mp.png", width=5, height=6, units="in", dpi=150)

# Missisquoi Bay - 2015
winter2015_chem_all %>% 
  filter(site == "mb") %>% 
  select(-TP) %>% 
  gather(key="analyte", value="conc", c(NO3:TN)) %>% 
  group_by(yday, samp_depth_cat, analyte) %>% 
  summarize(mean.conc = mean(conc, na.rm = T)) %>% 
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

ggsave("/Users/dustinkincaid/ownCloud/bree_frozeN/03_figures/2015_winterN_timeseries_mp.png", width=5, height=6, units="in", dpi=150)

# Shelburne Pond
winter2015_chem_all %>% 
  filter(site == "sp") %>% 
  select(-TP) %>% 
  gather(key="analyte", value="conc", c(NO3:TN)) %>% 
  group_by(yday, samp_depth_cat, analyte) %>% 
  summarize(mean.conc = mean(conc, na.rm = T)) %>% 
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
    
ggsave("/Users/dustinkincaid/ownCloud/bree_frozeN/03_figures/2015_winterN_timeseries_sp.png", width=5, height=6, units="in", dpi=150)





# --------------------------------------------------------------------