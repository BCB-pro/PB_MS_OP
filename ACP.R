
# Library -----------------------------------------------------------------


library(tidyr)
library(FactoMineR)
library("xlsx")
library(factoextra)
library(MASS)
library(tidyverse)
library(corrplot)

library(ggResidpanel) # To check hypothesis
library(kableExtra) # To represent table

library(rstatix) # Statistical analysis ANOVA
library(ggpubr) # Plot results of an ANOVA

library(lmerTest) # Tests on lmer objects
library(lme4) # Linear, generalized linear, & nonlinear mixed models

library(texreg) # Convert Regression Output to LaTeX or HTML Tables
library(sjPlot) # ICC calculations

data <- read.csv("/home/baptiste.criniere/Documents/Oceane/data_long_mean.csv")
data$time_ord <- factor(data$time, levels = c("Before-Surgery", "1 Week", "1 Month", "2 Months", "3 Months"))
data$group <- relevel(data$group, ref = "NG")
data1 <- read.xlsx("/home/baptiste.criniere/Documents/PB_MS_OP/inst/extdata/comportement_tests.xlsx", 1, startRow = 3, header=TRUE, colClasses=NA)
data1 <- data1[,c("X3.Months.13")]
data$speed <- data1
`%ni%` <- Negate(`%in%`)


# ACP ---------------------------------------------------------------------


# Data management, organisation des données pour la AFM
# On transforme la variable nb of errors en sqrt car variable de comptage
data2 <- data %>% mutate(sqrt_nb_of_errors = sqrt(number_of_errors))
# On transforme la variable time to cross en log
data2 <- data2 %>% mutate(log_time_to_cross = log(time_to_cross))

data2 <- data2 %>% dplyr::select(-time_ord, -donnor_id, -group, -num_time, -number_of_errors, -time_to_cross, -speed)
d <- data2 %>% 
  gather(key, value, -time, -mouse) %>%  
  unite(new.col, c(key, time)) %>%   
  spread(new.col, value) 

d <- merge(d, unique(data[,c("mouse", "speed", "group", "donnor_id")]), by.x = "mouse", by.y = "mouse")
row.names(d) <- d$mouse
d <- d %>% dplyr::select(-mouse)
d <- d[, c(1,6,11,16,2,7,12,17,3,8,13,18,4,9,14,19,21,5,10,15,20,22,23)]
names(d)


df <- d %>% 
  dplyr::select(speed, "log_time_to_cross_3 Months", "sqrt_nb_of_errors_3 Months", "front_limb_3 Months")
names(df) <- c("Speed", "Time to cross", "Nb of errors", "Front limb")
df0 <- na.omit(df)
res.pca <- PCA(df0, graph = FALSE)
fviz_pca_var(res.pca, col.var = "black")

df0 %>% 
  cor(method = "pearson",
      use = "complete.obs") %>% 
  corrplot::corrplot.mixed(upper = 'circle', 
                     lower = "number", number.cex = 1.8)



# AFM ---------------------------------------------------------------------



data <- read.csv("/home/baptiste.criniere/Documents/Oceane/data_long_mean.csv")
data$time_ord <- factor(data$time, levels = c("Before-Surgery", "1 Week", "1 Month", "2 Months", "3 Months"))
data$group <- relevel(data$group, ref = "NG")
data1 <- read.xlsx("/home/baptiste.criniere/Documents/PB_MS_OP/inst/extdata/comportement_tests.xlsx", 1, startRow = 3, header=TRUE, colClasses=NA)
data1 <- data1[,c("X3.Months.13")]
#data$speed <- data1
`%ni%` <- Negate(`%in%`)

# Data management, organisation des données pour la AFM
# On transforme la variable nb of errors en sqrt car variable de comptage
data2 <- data %>% mutate(sqrt_nb_of_errors = sqrt(number_of_errors))
# On transforme la variable time to cross en log
data2 <- data2 %>% mutate(log_time_to_cross = log(time_to_cross))

data2 <- data2 %>% dplyr::select(-time_ord, -donnor_id, -group, -num_time, -number_of_errors, -time_to_cross)
d <- data2 %>% 
  gather(key, value, -time, -mouse) %>%  
  unite(new.col, c(key, time)) %>%   
  spread(new.col, value) 

d <- merge(d, unique(data[,c("mouse", "group", "donnor_id")]), by.x = "mouse", by.y = "mouse")
row.names(d) <- d$mouse
d <- d %>% dplyr::select(-mouse)

# Ajout test
d$speed <- data1
d <- d[, c(1,6,11,16,2,7,12,17,3,8,13,18,4,9,14,19,23,5,10,15,20,21,22)]
d <- d %>% select(-donnor_id)


names(d)

# On lance l'AFM
res.mfa <- MFA(d, 
               group = c(4, 4, 4, 5, 4, 1), 
               type = c("s", "s", "s", "s", "s", "n"),
               name.group = c("1 month", "1 week", "2 months", "3 months", "before-surgery", "group"), num.group.sup = c(6),
               graph = FALSE)
ind <- get_mfa_ind(res.mfa)

fviz_mfa_ind(res.mfa, habillage = "group", geom = c("point"), shape.ind = "group",
             addEllipses = TRUE, ellipse.type = "confidence", 
             repel = TRUE , legend.partial.title = NULL
) + scale_fill_manual(breaks = c("NG", "HD", "MS"), values = c("#077e97", "#0b9a64", "#d1146e"))+
  scale_shape_manual(breaks = c("NG", "HD","MS"), values = c(16, 15, 17)) +
  scale_color_manual(breaks = c("NG", "HD", "MS"), values = c("#077e97", "#0b9a64", "#d1146e"))
