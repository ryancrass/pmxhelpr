#####Load Packages#####
library(hexSticker)
library(tidyverse)
library(Hmisc)
library(conflicted)
library(vpc)
conflict_prefer("filter", "dplyr")


#####Read Model and Run Simulation#####
model <- model_mread_load(model = "model")
simout <- data_sad %>%
  filter(NTIME <= 72) %>%
  filter(DOSE %in% c(10, 50, 100, 200)) %>%
  df_mrgsim_replicate(model = model, replicates = 100,
                              output_vars = c(DV = "ODV"),
                              num_vars = c("CMT", "EVID", "MDV", "NTIME", "LLOQ", "WTBL", "FOOD"))


#####Plot VPC#####
vpc_theme <-  new_vpc_theme(list(
  sim_pi_fill = "green4", sim_pi_alpha = 0.5,
  sim_median_fill = "white", sim_median_alpha = 0.5,
  bin_separators_color = NA))

vpc_plot <- plot_vpc_exactbins(
  sim = simout,
  pcvpc = TRUE,
  pi = c(0.05, 0.95),
  ci = c(0.05, 0.95),
  log_y = TRUE,
  vpc_theme = vpc_theme)+
  theme_classic()+
  theme_transparent()+
  theme(axis.title = element_blank(),
        axis.text = element_blank())

vpc_plot

sticker(vpc_plot, package="pmxhelpr",
        p_size=20, p_color="white",
        s_x=1, s_y=.75, s_width=1.3, s_height=1,
        filename="inst/figures/pmxhelpr_logo.png")
