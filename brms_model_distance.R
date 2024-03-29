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
setwd(Bleaching_data_directory)

Bleaching_Data <- read.csv("RC_ERG_CoRTAD_GMW_distances.csv")

Bleaching_Data$gmw1km[is.na(Bleaching_Data$gmw1km)] <- 0
summary(Bleaching_Data$gmw1km)

Bleaching_Data <- Bleaching_Data[!is.na(Bleaching_Data$SSTA_DHW),]
Bleaching_Data <- Bleaching_Data[!is.na(Bleaching_Data$Site),]
Bleaching_Data <- Bleaching_Data[!is.na(Bleaching_Data$Ecoregion),]
Bleaching_Data <- Bleaching_Data[!is.na(Bleaching_Data$Year),]
#Bleaching_Data <- Bleaching_Data[!is.na(Bleaching_Data$area4km),]
#Bleaching_Data <- Bleaching_Data[!is.na(Bleaching_Data$area2km),]
#Bleaching_Data <- Bleaching_Data[!is.na(Bleaching_Data$area1km),]
Bleaching_Data <- Bleaching_Data[!is.na(Bleaching_Data$Count),]
Bleaching_Data <- Bleaching_Data[!is.na(Bleaching_Data$kd490_value),]
Bleaching_Data <- Bleaching_Data[!is.na(Bleaching_Data$distance),]


Bleaching_Data$Ecoregion <- as.factor(Bleaching_Data$Ecoregion)

myStd <- function(x) { (x-mean(x))/sd(x)}



#   Resid. Df Resid. Dev Df Deviance  Pr(>Chi)    
# 1       810     7427.3                          
# 2       808     7292.9  2   134.38 < 2.2e-16 ***



# use the 1km distance - check for collinearity

collinearity_df <- dplyr::select(Bleaching_Data, c(distance, SSTA_DHW, Depth, kd490_value))

corfig <- ggcorrm(collinearity_df, 
                  mapping = aes(col = .corr, fill = .corr),
                  bg_dia = "grey20", 
                  rescale = "by_sd", 
                  corr_method = "pearson") +
  #lotri(geom_smooth(method = "glm", size = .3)) +
  lotri(geom_point(alpha = 0.5)) +
  #lotri_heatcircle(alpha = 1, col = 1) +
  utri_corrtext(nrow = 2, squeeze = 0.4) +
  dia_names(y_pos = 0.15, size = 3, color = "white") +
  dia_histogram(lower = 0.3, color = "grey80", fill = "grey60", size = .3) +
  scale_color_corr(aesthetics = c("fill", "color")) +
  theme(text = element_text(size = 14)) 
corfig


png(file=file.path(output_directory, 'collinieraity_matrix_distance.png'),height=2500,width=2500,res=350)
corfig
dev.off()

# Resid. Df Resid. Dev Df Deviance  Pr(>Chi)    
# 1      2782      27837                          
# 2      2780      27577  2   260.49 < 2.2e-16 ***
# stan model


str(Bleaching_Data$Ecoregion)
str(Bleaching_Data$MPA)
str(Bleaching_Data$Site)
Bleaching_Data$MPA <- as.factor(Bleaching_Data$MPA)
Bleaching_Data$Site <- as.factor(Bleaching_Data$Site)

Bleaching_Data <- Bleaching_Data[Bleaching_Data$kd490_value<=1,]

model_df <- Bleaching_Data
model_df$Count <- model_df$Count/4
hist(model_df$Count)
summary(model_df$Count)


# standardise predictiors


model_df$SSTA_DHW <- myStd(model_df$SSTA_DHW)
model_df$distance <- myStd(model_df$distance)
model_df$kd490_value <- myStd(model_df$kd490_value)
model_df$Depth <- myStd(model_df$Depth)

hist(model_df$SSTA_DHW)
hist(model_df$distance)
hist(model_df$kd490_value)
hist(model_df$Depth)

# work in parallel

options(mc.cores=parallel::detectCores())


######################################################################################################################
############################################### brms negative binomial ###############################################

m2_prior <- get_prior(round(Count) ~ (SSTA_DHW*distance) + kd490_value + Depth + (1|Ecoregion/Site),
                      data = model_df, family = negbinomial())


tic()
m1 <- brm(round(Count) ~ (SSTA_DHW*distance) + kd490_value + Depth + (1|Ecoregion/Site),
          data = model_df,
          chains = 3, 
          cores = 3, 
          iter = 5000,
          warmup = 2500,
          prior = m2_prior,
          family = negbinomial())
toc()

saveRDS(file = file.path(output_directory, 'mangrove_distance_model_negbinomial_brms.rds'), m1)

summary(m1)
mcmc_plot(m1, type = "trace", variable = c("b_SSTA_DHW", "b_distance", "b_SSTA_DHW:distance", "b_kd490_value", "b_Depth"))
mcmc_plot(m1, variable = c("b_SSTA_DHW", "b_distance", "b_SSTA_DHW:distance", "b_kd490_value", "b_Depth"))


y <- round(model_df$Count)
yrep <- posterior_predict(m1, draws=100)
ppc_dens_overlay(y, yrep[1:50,]) + xlim(0,20)
ppc_hist(y, yrep[1:5,])
prop_zero <- function(x) mean(x == 0)
prop_zero(y) # check proportion of zeros in y

ppc_stat(y, yrep, stat="prop_zero", binwidth = 0.001)
