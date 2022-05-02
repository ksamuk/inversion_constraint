
################################################################################
# Create plot of simulation results, Figures S1-S3 in the manuscript
# Marius Roesti, Kieran Samuk, Kim Gilbert
# April 2022
################################################################################

library("tidyverse")
library("patchwork")
options(scipen = 999)

haplo_df <- read.table("data/slim_combined_haplo_df.txt.gz", h = T)

# read in fitness data, censor out gen 1 (pops are initialized in gen 1)
fitness_df <- read.table("data/slim_combined_fitness_df.txt.gz", h = T) %>%
  filter(gen > 1) %>%
  mutate(inv_active = ifelse(grepl("absent", inv_active), "Absent", "Present")) 

################################################################################
# compute moving averages for visualization
################################################################################

mv_avg_optimal_adaptation <- fitness_df %>%
  group_by(sim_type, inv_active, mig_rate, total_sel_str, n_loci, pop, gen) %>%
  summarise(optimal_adaptation = mean(optimal_adaptation)) %>%
  ungroup %>%
  mutate(seed = 1)

################################################################################
# Figure S1: novel environment with parameter combinations
################################################################################

mv_avg_absent_basic <- mv_avg_optimal_adaptation %>%
  filter(sim_type == "novel_environment") %>%
  filter(inv_active == "Absent")

mv_avg_basic_inv_present <- mv_avg_optimal_adaptation %>%
  filter(sim_type == "novel_environment") %>%
  filter(inv_active == "Present")

figureS1 <- fitness_df %>%
  filter(sim_type == "novel_environment") %>%
  ggplot(aes(x = gen, y = optimal_adaptation,  color = pop, linetype = inv_active,
             group = interaction(pop, seed)))+
  geom_line(alpha = 0.1, col = "grey",linetype = "solid")+
  geom_line(data = mv_avg_absent_basic, size = 1)+
  geom_line(data = mv_avg_basic_inv_present, size = 1)+
  facet_grid(mig_rate~total_sel_str)+
  labs(x = "Generation", y = "Scaled Fitness", 
       col = "Population", linetype = "Inversion Presence")+
  theme_bw()+
  theme(strip.background = element_blank())+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Migration rate", 
                                         breaks = NULL, labels = NULL)) +
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Total strength of selection", 
                                         breaks = NULL, labels = NULL))+
  scale_color_brewer(palette = "Set1")+
  scale_linetype_manual(values = c("11", "solid"))

ggsave(figureS1, filename = "figures/FigureS1.png",  
       height = 7, width = 9)

ggsave(figureS1, filename = "figures/FigureS1.pdf", device = "pdf",
       height = 7, width = 9)

################################################################################
# Figure S2: climate change with parameter combinations
################################################################################

mv_avg_absent_basic <- mv_avg_optimal_adaptation %>%
  filter(sim_type == "clim_change") %>%
  filter(inv_active == "Absent")

mv_avg_basic_inv_present <- mv_avg_optimal_adaptation %>%
  filter(sim_type == "clim_change") %>%
  filter(inv_active == "Present")

figureS2 <- fitness_df %>%
  filter(sim_type == "clim_change") %>%
  ggplot(aes(x = gen, y = optimal_adaptation,  color = pop, linetype = inv_active,
             group = interaction(pop, seed)))+
  geom_line(alpha = 0.1, col = "grey",linetype = "solid")+
  geom_line(data = mv_avg_absent_basic, size = 1)+
  geom_line(data = mv_avg_basic_inv_present, size = 1)+
  facet_grid(mig_rate~total_sel_str)+
  labs(x = "Generation", y = "Scaled Fitness", 
       col = "Population", linetype = "Inversion Presence")+
  theme_bw()+
  theme(strip.background = element_blank())+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Migration rate", 
                                         breaks = NULL, labels = NULL)) +
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Total strength of selection", 
                                         breaks = NULL, labels = NULL))+
  scale_color_brewer(palette = "Set1")+
  scale_linetype_manual(values = c("11", "solid"))

ggsave(figureS2, filename = "figures/FigureS2.png",  
       height = 7, width = 9)

ggsave(figureS2, filename = "figures/FigureS2.pdf", device = "pdf",
       height = 7, width = 9)

################################################################################
# Figure S3: polygenic (loci = 100) with parameter combinations
################################################################################

mv_avg_absent_basic <- mv_avg_optimal_adaptation %>%
  filter(sim_type == "polygenic") %>%
  filter(n_loci == 100) %>%
  filter(inv_active == "Absent")

mv_avg_basic_inv_present <- mv_avg_optimal_adaptation %>%
  filter(sim_type == "polygenic") %>%
  filter(n_loci == 100) %>%
  filter(inv_active == "Present")

figureS3 <- fitness_df %>%
  filter(sim_type == "polygenic") %>%
  filter(n_loci == 100) %>%
  ggplot(aes(x = gen, y = optimal_adaptation,  color = pop, linetype = inv_active,
             group = interaction(pop, seed)))+
  geom_line(alpha = 0.1, col = "grey",linetype = "solid")+
  geom_line(data = mv_avg_absent_basic, size = 1)+
  geom_line(data = mv_avg_basic_inv_present, size = 1)+
  facet_grid(mig_rate~total_sel_str)+
  labs(x = "Generation", y = "Scaled Fitness", 
       col = "Population", linetype = "Inversion Presence")+
  theme_bw()+
  theme(strip.background = element_blank())+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Migration rate", 
                                         breaks = NULL, labels = NULL)) +
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Total strength of selection", 
                                         breaks = NULL, labels = NULL))+
  scale_color_brewer(palette = "Set1")+
  scale_linetype_manual(values = c("11", "solid"))

ggsave(figureS3, filename = "figures/FigureS3.png",  
       height = 7, width = 9)

ggsave(figureS3, filename = "figures/FigureS3.pdf", device = "pdf",
       height = 7, width = 9)
  