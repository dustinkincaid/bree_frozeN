library("tidyverse")
library("lubridate")

mb_chem <- read.csv("01_raw data/WINTER 2015 MB_supp_data.csv", header=T, stringsAsFactors = F, na.strings = " ") %>% 
  # select(-c(ends_with("SD"), ends_with("02"), ends_with("45"), ends_with("coll")))
  select(-(ends_with("SD"))) %>% 
  select(Cal.Date, Jul.Day, Location, Depth_grab:DOC) %>% 
  rename(date=Cal.Date, depth=Depth_grab) %>% 
  mutate(date = mdy(as.character(date)),
         site = "mb")


mb_chem %>% 
  filter(Jul.Day >= 84) %>% 
  mutate(depth = replace(depth, Location == "River", 1)) %>% 
  mutate(loc = paste(Location, depth, sep = "")) %>% 
  filter(!is.na(depth)) %>% 
  pivot_longer(cols = SRP:DOC, names_to = "var", values_to = "conc") %>% 
  ggplot(aes(x = as.factor(Jul.Day), y = conc, fill = loc)) +
  facet_wrap(~var, scales = "free_y", ncol = 1) +
  geom_bar(position = "dodge", stat = "identity")
