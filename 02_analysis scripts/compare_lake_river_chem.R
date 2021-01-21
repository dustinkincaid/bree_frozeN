library("tidyverse")
library("lubridate")
library("cowplot")

# All lake data compiled using compile_data.R; concentrations are in umol/L (uM)
alldata <- read_csv("01_raw data/alldata_2014_2015_mb_sp_compiled.csv")
  
# Pull Miss Riv data from Joung et al 2017 Supp Info
# mb_chem <- read.csv("01_raw data/WINTER 2015 MB_supp_data.csv", header=T, stringsAsFactors = F, na.strings = " ") %>% 
#   filter(location == "River") %>% 
#   # select(-c(ends_with("SD"), ends_with("02"), ends_with("45"), ends_with("coll")))
#   select(-(ends_with("SD"))) %>% 
#   select(Cal.Date, Jul.Day, location, `Temp.`:DOC) %>% 
#   rename(date=Cal.Date, yday = Jul.Day, depth=Depth_grab, temp = `Temp.`, cond = `Cond.`, turb = `Turb.`, chla = `Chl.a`) %>% 
#   mutate(date = mdy(as.character(date)),
#          site = "mb") %>% 
#   # Convert NO3-N from mg N/L to umol/L
#   mutate(NO3 = NO3*1000/14.007)
# 
# # Append both
# all_chem <-
#   bind_rows(alldata, mb_chem)

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


# Create plot theme
theme1 <-
  theme_classic() +
  theme(legend.title = element_text(size = 8),
        legend.text = element_text(size = 8),
        legend.key.size = unit(0.15, "in"))

# Create plot comparing lake NO3 concs to river NO3 concs
no3 <- n_summ %>% 
  filter(var == "NO3") %>% 
  filter(yday >= 84) %>% 
  # filter(yday >= 84 & yday <= 115) %>% 
  mutate(samp_depth_cat2 = replace(samp_depth_cat2, location == "River", "River")) %>% 
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
  theme1

# ggsave("03_figures/plot_compare_lake_river_conc.png", width = 5, height = 3, units = "in", dpi = 150)

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

# Create plot comparing lake cond levels to river cond levels
cond <- ysi_summ %>% 
  filter(var == "cond") %>% 
  filter(yday >= 84) %>% 
  mutate(depth_range = factor(depth_range, levels = c("[0,1]", "(1,2]", "(2,3]", "(3,4]", "River"), labels = c("0-1 m", "1-2 m", "2-3 m", "3-4 m", "River"))) %>% 
  mutate(depth_range = replace(depth_range, location == "River", "River")) %>% 
  ggplot(aes(x = as.factor(yday), y = conc_mean, fill = depth_range)) +
  geom_bar(position = position_dodge(width = 0.9, preserve = "single"), stat = "identity") +
  geom_errorbar(aes(ymin = conc_mean-conc_SE, ymax = conc_mean+conc_SE), 
                position = position_dodge(width = 0.9, preserve = "single"),
                width=.2) +
  scale_fill_manual(name="Depth/Source",
                    values=c("gray75", "gray60", "gray45", "gray30", "dodgerblue3")) +
  ylab(expression(Cond.~(mu*S~cm^{-1}))) + xlab("Day of year") +
  theme1

# Create plot comparing lake turb levels to river turb levels
turb <- ysi_summ %>% 
  filter(var == "turb") %>% 
  filter(yday >= 84) %>% 
  mutate(depth_range = factor(depth_range, levels = c("[0,1]", "(1,2]", "(2,3]", "(3,4]", "River"), labels = c("0-1 m", "1-2 m", "2-3 m", "3-4 m", "River"))) %>% 
  mutate(depth_range = replace(depth_range, location == "River", "River")) %>% 
  ggplot(aes(x = as.factor(yday), y = conc_mean, fill = depth_range)) +
  geom_bar(position = position_dodge(width = 0.9, preserve = "single"), stat = "identity") +
  geom_errorbar(aes(ymin = conc_mean-conc_SE, ymax = conc_mean+conc_SE), 
                position = position_dodge(width = 0.9, preserve = "single"),
                width=.2) +
  scale_fill_manual(name="Depth/Source",
                    values=c("gray75", "gray60", "gray45", "gray30", "dodgerblue3")) +
  ylab("Turb. (NTU)") + xlab("Day of year") +
  theme1

# Create plot comparing lake NO3 concs to river NO3 concs
# Stacked bar graph
no3 <- n_summ %>% 
  filter(var == "NO3") %>% 
  filter(yday >= 84) %>% 
  # filter(yday >= 84 & yday <= 115) %>% 
  mutate(samp_depth_cat2 = replace(samp_depth_cat2, location == "River", "River")) %>% 
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
  theme1

# Combine into one plot
fig1 <- plot_grid(cond, turb, no3, align = "hv", ncol = 1, labels = "auto")
save_plot("03_figures/suppInfo_melt_lake_river_comparison.png", fig1, dpi = 150,
          units = "in", base_height = 7, base_width = 5)
