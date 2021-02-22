# Compare conductivity and turibity levels and N and P concentrations in lake vs. in river during melt period

# Load packages
library("tidyverse")
library("lubridate")
library("patchwork")

# All lake data compiled using compile_data.R; concentrations are in umol/L (uM)
alldata <- read_csv("01_raw data/alldata_2014_2015_mb_sp_compiled.csv")
  

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
ggsave("03_figures/suppInfo_melt_lake_river_comparison.png", plot = fig1, width = 7, height = 7, units = "in", dpi = 150)



# Older plots
# Create plot themes
# theme1 <-
#   theme_classic() +
#   theme(legend.title = element_text(size = 8),
#         legend.text = element_text(size = 8),
#         legend.key.size = unit(0.15, "in"))

# cond <- ysi_summ %>% 
#   filter(var == "cond") %>% 
#   filter(yday >= 84) %>% 
#   mutate(depth_range = factor(depth_range, levels = c("[0,1]", "(1,2]", "(2,3]", "(3,4]", "River"), labels = c("0-1m", "1-2m", "2-3m", "3-4m", "River"))) %>% 
#   mutate(depth_range = replace(depth_range, location == "River", "River")) %>% 
#   ggplot(aes(x = as.factor(yday), y = conc_mean, fill = depth_range)) +
#   geom_bar(position = position_dodge(width = 0.9, preserve = "single"), stat = "identity") +
#   geom_errorbar(aes(ymin = conc_mean-conc_SE, ymax = conc_mean+conc_SE), 
#                 position = position_dodge(width = 0.9, preserve = "single"),
#                 width=.2) +
#   scale_fill_manual(name="Depth/Source",
#                     values=c("gray75", "gray60", "gray45", "gray30", "dodgerblue3")) +
#   ylab(expression(Cond.~(mu*S~cm^{-1}))) + xlab("Day of year") +
#   theme1
# 
# turb <- ysi_summ %>% 
#   filter(var == "turb") %>% 
#   filter(yday >= 84) %>% 
#   mutate(depth_range = factor(depth_range, levels = c("[0,1]", "(1,2]", "(2,3]", "(3,4]", "River"), labels = c("0-1m", "1-2m", "2-3m", "3-4m", "River"))) %>% 
#   mutate(depth_range = replace(depth_range, location == "River", "River")) %>% 
#   ggplot(aes(x = as.factor(yday), y = conc_mean, fill = depth_range)) +
#   geom_bar(position = position_dodge(width = 0.9, preserve = "single"), stat = "identity") +
#   geom_errorbar(aes(ymin = conc_mean-conc_SE, ymax = conc_mean+conc_SE), 
#                 position = position_dodge(width = 0.9, preserve = "single"),
#                 width=.2) +
#   scale_fill_manual(name="Depth/Source",
#                     values=c("gray75", "gray60", "gray45", "gray30", "dodgerblue3")) +
#   ylab("Turb. (NTU)") + xlab("Day of year") +
#   theme1
# 
# no3 <- n_summ %>% 
#   filter(var == "NO3") %>% 
#   filter(yday >= 84) %>% 
#   # filter(yday >= 84 & yday <= 115) %>% 
#   mutate(samp_depth_cat2 = factor(samp_depth_cat2, 
#                                   levels=c("Top", "Mid-1", "Mid-2", "Mid-3", "Bottom", "River"))) %>% 
#   ggplot(aes(x = as.factor(yday), y = conc_mean, fill = samp_depth_cat2)) +
#   geom_bar(position = position_dodge(width = 0.9, preserve = "single"), stat = "identity") +
#   geom_errorbar(aes(ymin = conc_mean-conc_SE, ymax = conc_mean+conc_SE), 
#                 position = position_dodge(width = 0.9, preserve = "single"),
#                 width=.2) +
#   scale_fill_manual(name="Depth/Source",
#                     values=c("gray90", "gray75", "gray60", "gray45", "gray30", "dodgerblue3")) +
#   ylab(expression(paste("NO"["3"]^" -", " (",mu,"mol"," l"^"-1",")"))) + xlab("Day of year") +
#   theme1