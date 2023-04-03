library(tidyverse)
library(tictoc)
library(bayesplot)
library(corrmorant)
library(modelr)
library(magrittr)
library(purrr)
library(forcats)
library(ggdist)
library(tidybayes)
library(ggplot2)
library(cowplot)
library(rstan)
library(brms)

rm(list=ls())

output_directory="C:/Users/40274182/OneDrive - Queen's University Belfast/PhD/Chapters/Bleaching/Mangroves/Working_file"
Bleaching_data_directory="C:/Users/40274182/OneDrive - Queen's University Belfast/PhD/Chapters/Bleaching/Mangroves/Data files"

m1 <- readRDS(file = file.path(output_directory, 'mangrove_distance_model_negbinomial_brms.rds'))

summary(m1)

png(file=file.path(output_directory,'trace_plots.png'),height=2500,width=3000,res=350)
mcmc_plot(m1, type = "trace", variable = c("b_SSTA_DHW", "b_distance", "b_SSTA_DHW:distance", "b_kd490_value", "b_Depth"),  facet_args = list(nrow = 3))
dev.off()


mcmc_plot(m1, variable = c("b_SSTA_DHW", "b_distance", "b_SSTA_DHW:distance", "b_kd490_value", "b_Depth"))

model_df <- m1$data

y <- round(model_df$Count)
yrep <- posterior_predict(m1, draws=100)
ppc_dens_overlay(y, yrep[1:50,]) + xlim(0,20)
ppc_hist(y, yrep[1:5,])
prop_zero <- function(x) mean(x == 0)
prop_zero(y) # check proportion of zeros in y

png(file=file.path(output_directory,'ppc_brms_negbi.png'),height=2500,width=3000,res=350)
ppc_stat(y, yrep, stat="prop_zero", binwidth = 0.001)
dev.off()

# coefficient plot 
variables(m1)

coeffs <- posterior_summary(m1, digits = 0.6, probs=c(0.95, 0.05,0.8, 0.2,0.5))
coeffs <- as.data.frame(coeffs[2:6,])
coeffs <- rownames_to_column(coeffs)

p_coeff <- ggplot(coeffs, aes(Estimate, reorder(rowname, Estimate))) +
  geom_vline(aes(xintercept = 0), size = .25, linetype = "dashed") +         
  geom_errorbarh(aes(xmax = Q95, xmin =Q5), size = 1.3, height = 0, color = "light grey", position = position_dodge(width = 0.5)) +
  geom_errorbarh(aes(xmax = Q80, xmin =Q20), size = 2, height = 0, color = "Dark grey", position = position_dodge(width = 0.8)) +
  geom_point(size = 3, position = position_dodge(width=0.5)) +
  scale_y_discrete(labels=c("Depth", "Turbidity", "Distance", "DHW*Distance", "DHW")) +
  theme_classic() +
  theme(axis.title.x = element_text(size = 22, color = "black"), axis.title.y = element_text(size = 22, color = "black"), text=element_text(size=16)) +
  theme(axis.text.y = element_text(size=18, color="black"), axis.text.x = element_text(size=18, color="black")) +
  labs(y= "Predictor", x="Model coefficient") 

p_coeff

png(file=file.path(output_directory, 'brms_coeffs.png'),height=2500,width=3000,res=350)
p_coeff
dev.off()

##############################################################################################################
######################################### conditional effects ################################################


summary(m1)

library(fishualize)

int_conditions <- list(SSTA_DHW = setNames(c(-1,0,1), c("-1", "0", "1")))

p2 <- conditional_effects(x=m1, effects = "distance:SSTA_DHW", int_conditions = int_conditions, prob=0.8)

fac_labs <- c("DHW -1SD", "DHW mean", "DHW +1SD")
names(fac_labs) <- c("-1","0","1")

png(file=file.path(output_directory, 'Conditional_effects.png'),height=3000,width=4000,res=400)
plot(p2,
     points = F,
     plot=T)[[1]] +
  scale_color_fish_d(option = "Gramma_loreto", direction=1) +
  scale_fill_fish_d(option = "Gramma_loreto", direction=1) +
  theme_classic() +
  #theme(legend.position = c(0.3,0.8)) +
  #theme(legend.background = element_rect(size=0.5, linetype="solid",colour ="black")) +
  theme(legend.position = "none") +
  scale_x_continuous(breaks=c(0,9)) +
  facet_wrap(~SSTA_DHW, labeller = labeller(SSTA_DHW=fac_labs)) +
  theme(axis.title.x = element_text(size = 22, color = "black"), axis.title.y = element_text(size = 22, color = "black"), text=element_text(size=16)) +
  theme(axis.text.y = element_text(size=18, color="black"), axis.text.x = element_text(size=18, color="black")) +
  labs(y= "Bleaching (%)", x="Distance from nearest mangrove (log)") 
dev.off()

####################################################################################################################
####################################### group by the standard deviation of distance ################################


int_conditions_dist <- list(distance = setNames(c(-1,0,1), c("-1", "0", "1")))

p3 <- conditional_effects(x=m1, effects = "SSTA_DHW:distance", int_conditions = int_conditions_dist, prob=0.8)


fac_labs_2 <- c("Close Proximity", "Moderate Proximity", "Distant Proximity") #immediate, moderate, distant 
names(fac_labs_2) <- c("-1","0","1")

png(file=file.path(output_directory, 'Conditional_effects_mangroves_group.png'),height=3000,width=4000,res=400)
plot(p3,
     points = F,
     plot=T)[[1]] +
  scale_color_fish_d(option = "Gramma_loreto", direction=1) +
  scale_fill_fish_d(option = "Gramma_loreto", direction=1) +
  theme_classic() +
  #theme(legend.position = c(0.3,0.8)) +
  #theme(legend.background = element_rect(size=0.5, linetype="solid",colour ="black")) +
  theme(legend.position = "none") +
  #scale_x_continuous(breaks=c(0,10)) +
  ylim(0,100) + # estimate from the model don't exist beyond this range (also convieneint with visualisation given bleaching is a percentge)
  xlim(-1,8.3) + # estimates from the model also don't exist out of this range
  #scale_y_continuous(breaks=c(0,100,200,300) , labels=c(0,25,50,75))+ # divide bleaching% by 4
  facet_wrap(~distance, labeller = labeller(distance=fac_labs_2)) +
  theme(axis.title.x = element_text(size = 22, color = "black"), axis.title.y = element_text(size = 22, color = "black"), text=element_text(size=16)) +
  theme(axis.text.y = element_text(size=18, color="black"), axis.text.x = element_text(size=18, color="black")) +
  labs(y= "Bleaching (%)", x="DHW (standardised)") 
dev.off()
