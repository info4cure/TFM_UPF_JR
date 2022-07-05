library(tidyverse) 
library(ggplot2)
library(ggpubr)
update.packages(tibble)
install.packages("ggpubr")
install.packages("tibble")

names(model_reg)
 [1] "guia"           "gpc_source"     "date_GPC"      
 [4] "estudio"        "economics"      "date_economics"
 [7] "dist"           "quality_tool"   "quality_study" 
[10] "FI"             "Funding"        "Citas"         
[13] "Journal"        "open_access"    "design"

p <- ggboxplot(model_reg, x = "group", y = "time",
                color = "group", palette =c("#00AFBB", "#E7B800", "#FC4E07"),
                add = "jitter", shape = "group")

my_comparisons <- list( c("CIR", "EFyC"), c("CIR", "MI"), c("EFyC", "MI"))
p + stat_compare_means(label.y=35) + stat_compare_means(comparisons = my_comparisons)


install.packages("sjPlot")
installed.packages("sjmisc")
installed.packages("sjlabelled")
library(sjPlot)
library(sjmisc)
library(sjlabelled)

set.seed(123)
model_1<-lm(log(quality_study,2) ~ gpc_source+log(dist,2) +Funding+log(FI,2)+log(Citas,2)+open_access+design, data= model_reg)
summary(model_1)
anova(model_1)

model_reg$log_quality<-log(model_reg$quality_study,2)
model_reg$log_citas<-log(model_reg$Citas,2)
model_reg$log_FI<log(model_reg$FI,2)
model_reg$log_dist<-log(model_reg$dist,2)

# SOURCE
p <- ggboxplot(model_reg, x = "gpc_source", y = "log_quality",
                color = "gpc_source", palette =c("#00AFBB", "#E7B800", "#FC4E07", "#FC4E07"),
                add = "jitter", shape = "gpc_source")
p + stat_compare_means(label.y=4) 

# FUNDING
p <- ggboxplot(model_reg, x = "Funding", y = "log_quality",
                color = "Funding", palette =c("#00AFBB", "#E7B800", "#FC4E07", "#00FF00"),
                add = "jitter", shape = "Funding")
my_comparisons <- list( c("Privada", "No"), c("Privada", "Mixta"), c("Privada", "Académica"), c("No", "Mixta"), c("No", "Académica"), c("Mixta", "Académica"))
p + stat_compare_means(label.y=7) + stat_compare_means(comparisons = my_comparisons)

# DESIGN

p <- ggboxplot(model_reg, x = "design", y = "log_quality",
                color = "design", palette =c("#00AFBB", "#E7B800", "#FC4E07", "#00FF00"),
                add = "jitter", shape = "design")
my_comparisons <- list( c("observacional", "modelo"), c("observacional", "RCT"), c("observacional", "RS"), c("modelo", "RCT"), c("modelo", "RS"), c("RCT", "RS"))
p + stat_compare_means(label.y=7) + stat_compare_means(comparisons = my_comparisons)

# dist
p <- ggboxplot(model_reg, x = "design", y = "log_dist",
                color = "design", palette =c("#00AFBB", "#E7B800", "#FC4E07", "#FC4E07"),
                add = "jitter", shape = "design")
my_comparisons <- list( c("observacional", "modelo"), c("observacional", "RCT"), c("observacional", "RS"), c("modelo", "RCT"), c("modelo", "RS"), c("RCT", "RS"))
p + stat_compare_means(label.y=7) + stat_compare_means(comparisons = my_comparisons)


# corr plots
ggplot(data = model_reg, mapping = aes(x = log_dist, y = log_quality, color = design)) + 
  geom_point()+
  geom_smooth()

ggplot(data = model_reg) + 
  geom_point(mapping = aes(x = log_citas, y = log_quality))

