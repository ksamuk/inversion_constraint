
################################################################################
# Create plot of simulation results, Figure 1 in the manuscript
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
# Figure 1B: basic scenario
################################################################################

mv_avg_absent_basic <- mv_avg_optimal_adaptation %>%
  filter(mig_rate == 0.01, total_sel_str == 0.1) %>%
  filter(sim_type == "novel_environment") %>%
  filter(inv_active == "Absent")

mv_avg_basic_inv_present <- mv_avg_optimal_adaptation %>%
  filter(mig_rate == 0.01, total_sel_str == 0.1) %>%
  filter(sim_type == "novel_environment") %>%
  filter(inv_active == "Present")

inv_absent_basic <- fitness_df %>%
  filter(mig_rate == 0.01, total_sel_str == 0.1) %>%
  filter(sim_type == "novel_environment") %>%
  #filter(inv_active == "inversion.absent") %>%
  ggplot(aes(x = gen, y = optimal_adaptation,  color = pop, linetype = inv_active,
             group = interaction(pop, seed)))+
  geom_line(alpha = 0.1, col = "grey",linetype = "solid")+
  geom_line(data = mv_avg_absent_basic, size = 1)+
  geom_line(data = mv_avg_basic_inv_present, size = 1)+
  #facet_grid(mig_rate~total_sel_str)+
  labs(x = "Generation", y = "Scaled Fitness", col = "Population", linetype = "Inversion Presence")+
  theme_bw()+
  scale_color_brewer(palette = "Set1")+
  scale_linetype_manual(values = c("11", "solid"))

ggsave(inv_absent_basic, filename = "figures/Figure1B.png", type = "cairo", 
       height = 3, width = 6)

