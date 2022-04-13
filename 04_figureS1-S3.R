
################################################################################
# Plot results of inversion constraint simulations
# Marius Roesti, Kieran Samuk, Kim Gilbert
# April 2022
################################################################################

library("tidyverse")
library("patchwork")
options(scipen = 999)

haplo_df <- read.table("data/slim_combined_haplo_df.txt.gz", h = T)

# read in fitness data, censor out gen 1 (pops are initialized in gen 1)
fitness_df <- read.table("data/slim_combined_fitness_df.txt.gz", h = T) %>%
  filter(gen > 1)

################################################################################
# compute moving averages for visualization
################################################################################

mv_avg_optimal_adaptation <- fitness_df %>%
  group_by(sim_type, inv_active, mig_rate, total_sel_str, pop, gen) %>%
  summarise(optimal_adaptation = mean(optimal_adaptation)) %>%
  ungroup %>%
  mutate(seed = 1)

################################################################################
# Figure 1: basic scenario
# this is the 'original' style plot
################################################################################

mv_avg_absent_basic <- mv_avg_optimal_adaptation %>%
  filter(sim_type == "novel_environment") %>%
  filter(inv_active == "inversion.absent")

inv_absent_basic <- fitness_df %>%
  filter(sim_type == "novel_environment") %>%
  filter(inv_active == "inversion.absent") %>%
  ggplot(aes(x = gen, y = optimal_adaptation,  color = pop, 
             group = interaction(pop, seed)))+
  geom_line(alpha = 0.1, color = "grey", size = 1)+
  geom_line(data = mv_avg_absent_basic, size = 1)+
  facet_grid(mig_rate~total_sel_str)+
  xlab("Generation")+
  ylab("Scaled Fitness")+ 
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Migration rate", breaks = NULL, labels = NULL)) +
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Total strength of selection", breaks = NULL, labels = NULL))+
  theme_bw()+
  scale_color_brewer(palette = "Set1")

mv_avg_present_basic <- mv_avg_optimal_adaptation %>%
  filter(sim_type == "novel_environment") %>%
  filter(inv_active == "inversion.present")

inv_present_basic <- fitness_df %>%
  filter(sim_type == "novel_environment") %>%
  filter(inv_active == "inversion.present") %>%
  ggplot(aes(x = gen, y = optimal_adaptation, color = pop, 
             group = interaction(seed, pop)))+
  geom_line(alpha = 0.1, color = "grey", size = 1)+
  geom_line(data = mv_avg_present_basic, size = 1)+
  facet_grid(mig_rate~total_sel_str)+
  xlab("Generation")+
  ylab("Scaled Fitness")+ 
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Migration rate", breaks = NULL, labels = NULL)) +
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Total strength of selection", breaks = NULL, labels = NULL))+
  theme_bw()+
  scale_color_brewer(palette = "Set1")


figure1 <- inv_absent_basic / inv_present_basic
figure1 + plot_layout(guides = "collect")


################################################################################
# Figure 1: basic scenario
# this is the 'alternative' style plot, focusing on the inversion contrast
################################################################################

mv_avg_basic_p3 <- mv_avg_optimal_adaptation %>%
  filter(sim_type == "novel_environment") %>%
  filter(pop == "pop3") 

inv_compare_basic_p3 <- fitness_df %>%
  filter(sim_type == "novel_environment") %>%
  filter(pop == "pop3") %>%
  ggplot(aes(x = gen, y = optimal_adaptation,  color = inv_active, 
             group = interaction(inv_active, seed)))+
  geom_line(alpha = 0.1, color = "grey", size = 1)+
  geom_line(data = mv_avg_basic_p3, size = 1)+
  facet_grid(mig_rate~total_sel_str)+
  xlab("Generation")+
  ylab("Scaled Fitness")+ 
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Migration rate", breaks = NULL, labels = NULL)) +
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Total strength of selection", breaks = NULL, labels = NULL))+
  theme_bw()+
  scale_color_brewer(palette = "Set1")

mv_avg_basic_p1p2 <- mv_avg_optimal_adaptation %>%
  filter(sim_type == "novel_environment") %>%
  filter(pop == "pop1" | pop == "pop2")

inv_compare_basic_p1p2 <- fitness_df %>%
  filter(sim_type == "novel_environment") %>%
  filter(pop == "pop1" | pop == "pop2") %>%
  ggplot(aes(x = gen, y = optimal_adaptation,  color = inv_active, 
             group = interaction(inv_active, seed)))+
  geom_line(alpha = 0.1, color = "grey", size = 1)+
  geom_line(data = mv_avg_basic_p1p2, size = 1)+
  facet_grid(mig_rate~total_sel_str)+
  xlab("Generation")+
  ylab("Scaled Fitness")+ 
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Migration rate", breaks = NULL, labels = NULL)) +
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Total strength of selection", breaks = NULL, labels = NULL))+
  theme_bw()+
  scale_color_brewer(palette = "Set1")

figure1_alt <- inv_compare_basic_p3 / inv_compare_basic_p1p2
figure1_alt + plot_layout(guides = "collect")

################################################################################
# Figure 1: basic scenario
# this is the plot suggested by kim, combining the two above
################################################################################

mv_avg_absent_basic <- mv_avg_optimal_adaptation %>%
  filter(sim_type == "novel_environment") %>%
  filter(inv_active == "inversion.absent")

mv_avg_basic_inv_present <- mv_avg_optimal_adaptation %>%
  filter(sim_type == "novel_environment") %>%
  filter(inv_active == "inversion.present")

inv_absent_basic <- fitness_df %>%
  filter(sim_type == "novel_environment") %>%
  filter(inv_active == "inversion.absent") %>%
  ggplot(aes(x = gen, y = optimal_adaptation,  color = pop, 
             group = interaction(pop, seed)))+
  geom_line(alpha = 0.1, color = "grey", size = 1)+
  geom_line(data = mv_avg_absent_basic, size = 1)+
  geom_line(data = mv_avg_basic_inv_present, size = 1, linetype = 2)+
  facet_grid(mig_rate~total_sel_str)+
  xlab("Generation")+
  ylab("Scaled Fitness")+ 
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Migration rate", breaks = NULL, labels = NULL)) +
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Total strength of selection", breaks = NULL, labels = NULL))+
  theme_bw()+
  scale_color_brewer(palette = "Set1")

################################################################################
# Figure S1: polygenic scenario
# this is the 'original' style plot
################################################################################

mv_avg_absent_pg <- mv_avg_optimal_adaptation %>%
  filter(sim_type == "polygenic") %>%
  filter(inv_active == "inversion.absent")

inv_absent_pg <- fitness_df %>%
  filter(sim_type == "polygenic") %>%
  filter(inv_active == "inversion.absent") %>%
  ggplot(aes(x = gen, y = optimal_adaptation,  color = pop, 
             group = interaction(pop, seed)))+
  geom_line(alpha = 0.1, color = "grey")+
  geom_line(data = mv_avg_absent_pg)+
  facet_grid(mig_rate~total_sel_str)+
  xlab("Generation")+
  ylab("Scaled Fitness")+ 
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Migration rate", breaks = NULL, labels = NULL)) +
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Total strength of selection", breaks = NULL, labels = NULL))+
  theme_bw()

mv_avg_present_pg <- mv_avg_optimal_adaptation %>%
  filter(sim_type == "polygenic") %>%
  filter(inv_active == "inversion.present")

inv_present_pg <- fitness_df %>%
  filter(sim_type == "polygenic") %>%
  filter(inv_active == "inversion.present") %>%
  ggplot(aes(x = gen, y = optimal_adaptation, color = pop, 
             group = interaction(seed, pop)))+
  geom_line(alpha = 0.1, color = "grey")+
  geom_line(data = mv_avg_present_pg)+
  facet_grid(mig_rate~total_sel_str)+
  xlab("Generation")+
  ylab("Scaled Fitness")+ 
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Migration rate", breaks = NULL, labels = NULL)) +
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Total strength of selection", breaks = NULL, labels = NULL))+
  theme_bw()


figureS1 <- inv_absent_pg / inv_present_pg

################################################################################
# Figure S1: polygenic scenario
# this is the 'alternative' style plot
################################################################################

mv_avg_pg_p3 <- mv_avg_optimal_adaptation %>%
  filter(sim_type == "polygenic") %>%
  filter(pop == "pop3")

inv_compare_pg_p3 <- fitness_df %>%
  filter(sim_type == "polygenic") %>%
  filter(pop == "pop3") %>%
  ggplot(aes(x = gen, y = optimal_adaptation,  color = inv_active, 
             group = interaction(inv_active, seed, pop)))+
  geom_line(alpha = 0.1, color = "grey", size = 1)+
  geom_line(data = mv_avg_pg_p3, size = 1)+
  facet_grid(mig_rate~total_sel_str)+
  xlab("Generation")+
  ylab("Scaled Fitness")+ 
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Migration rate", breaks = NULL, labels = NULL)) +
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Total strength of selection", breaks = NULL, labels = NULL))+
  theme_bw()+
  scale_color_brewer(palette = "Set1")

mv_avg_pg_p1p2 <- mv_avg_optimal_adaptation %>%
  filter(sim_type == "polygenic") %>%
  filter(pop == "pop1" | pop == "pop2")

inv_compare_pg_p1p2 <- fitness_df %>%
  filter(sim_type == "polygenic") %>%
  filter(pop == "pop1" | pop == "pop2") %>%
  ggplot(aes(x = gen, y = optimal_adaptation,  color = inv_active, 
             group = interaction(inv_active, seed, pop)))+
  geom_line(alpha = 0.1, color = "grey", size = 1)+
  geom_line(data = mv_avg_pg_p1p2, size = 1)+
  facet_grid(mig_rate~total_sel_str)+
  xlab("Generation")+
  ylab("Scaled Fitness")+ 
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Migration rate", breaks = NULL, labels = NULL)) +
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Total strength of selection", breaks = NULL, labels = NULL))+
  theme_bw()+
  scale_color_brewer(palette = "Set1")

figureS1_alt <- inv_compare_pg_p3 / inv_compare_pg_p1p2
figureS1_alt + plot_layout(guides = "collect")
  