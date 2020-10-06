library("tidyverse")
library("lubridate")

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
    # Convert all N concs from mg N/L to umol/L
    mutate(NO3 = NO3*1000/14.007,
           NH4 = NH4*1000/14.007,
           TN = TN*1000/14.007) %>% 
    # Add location (i.e., Lake)
    mutate(Location = "Lake")

# Pull Miss Riv data from Joung et al 2017 Supp Info
mb_chem <- read.csv("01_raw data/WINTER 2015 MB_supp_data.csv", header=T, stringsAsFactors = F, na.strings = " ") %>% 
  filter(Location == "River") %>% 
  # select(-c(ends_with("SD"), ends_with("02"), ends_with("45"), ends_with("coll")))
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

ggsave("03_figures/plot_compare_lake_river_conc.png", width = 5, height = 3, units = "in", dpi = 150)
