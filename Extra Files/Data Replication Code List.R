library(tidyverse)
library(usethis)
library(lmodel2)
library(skimr)
library(latticeExtra)
library(broom)
library(sjPlot)
library(mosaic)
library(manipulate)
library(patchwork)
library(infer)
f <- "https://raw.githubusercontent.com/TheCoolestPotato/Data-Replication-Assignment/refs/heads/main/41467_2018_5280_MOESM4_ESM.csv"
d <- read.csv(f)
d <- d |> select(Taxonomic.Order, Family, Insularity, N, Brain..mm3., Body..g., Brain.Residual)
skim(d)
f2 <- "https://raw.githubusercontent.com/TheCoolestPotato/Data-Replication-Assignment/refs/heads/main/41467_2018_5280_MOESM5_ESM.csv"
d2 <- read.csv(f2)
dist_plot <- ggplot(data = d, aes(x = Brain..mm3., y = Insularity)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.05)
plot(dist_plot)
##This is the plot that shows brain size should be log transformed##
d <- d |>  mutate(logbrain = log(Brain..mm3.), logbody = log(Body..g.))
d <- (drop_na(d, logbrain))
log_dist_plot <- ggplot(data = d, aes(x = logbrain, y = Insularity)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.05)
plot(log_dist_plot)
##this one shows body size should be log transformed##
brainsize_island_plot <- ggplot(data = d, aes(x = Insularity, y = Brain.Residual, fill = Insularity)) +
  geom_boxplot(notch = TRUE, outliers = FALSE) +
  theme(legend.position = "none") +
  ylab("Relative Brain Size") +
  xlab("")
plot(brainsize_island_plot)
##This is the fancy plot that matches Fig. 1 in the paper##
bodysize_comparison_plot <- ggplot(data = d, aes(x = Insularity, y = logbody)) +
  geom_boxplot() +
  theme(legend.position = "none") +
  ylab("Body Size Change?") +
  xlab("")
plot(bodysize_comparison_plot)

dfamily <- d |>
  select(N, Family, Species, Brain..mm3., Insularity, Migratory.Behavior, logbrain, Brain.Residual, logbody, Body..g., Brain.Residual) |>
  group_by(Family) |>
  summarize(meanrelative = mean(Brain.Residual), )

dContinent <- d[d$Insularity == "continental",]
dIsland <- d[d$Insularity == "insular",]
dIslandBrain <- dIsland |>
  group_by(Family) |>
  summarize(meanbrain = mean(Brain.Residual))
dContinentBrain <- dContinent |>
  group_by(Family) |>
  summarize(meanbrain = mean(Brain.Residual))
dgroup <- d |>
  group_by(Family) |>
  summarize(meanresidual = mean(Brain.Residual))
comm_fams <- intersect(dIslandBrain$Family, dContinentBrain$Family)
dIslandBrain <- filter(dIslandBrain, Family %in% comm_col)
dContinentBrain <- filter(dContinentBrain, Family %in% comm_col)
merge <- merge(dIslandBrain, dContinentBrain, by=comm_fams, all.x=TRUE)
dIslandBrain$difference <- (dIslandBrain$meanbrain - dContinentBrain$meanbrain)
list <- d %>% distinct(Family, .keep_all = TRUE)
list <- filter(list, Family %in% comm_col)
list <- list |> select(Family, Taxonomic.Order)
dIslandBrain <- merge(dIslandBrain, list)
taxonomic_diff <- dIslandBrain |>
  group_by(Taxonomic.Order) |>
  summarize(meandiff = mean(difference))
Tax_Diff_Plot <- ggplot(data = taxonomic_diff, aes(x=meandiff, y=Taxonomic.Order)) +
  geom_col() +
  xlab("Mean Difference in Brain Size") +
  ylab("Taxonomic Order")
plot(Tax_Diff_Plot)
lm1 <- lm(Brain.Residual ~ Insularity, data = d)
lm2 <- lm(Brain.Residual ~ Family, data = d)
aov1 <- aov(Brain.Residual ~ Insularity, data = d)
summary(lm1)
summary(aov1)
summary(lm2)
lm3 <- lm(Brain.Residual ~ Taxonomic.Order, data = d)
lm4 <- lm(Brain.Residual ~ Body..g., data = d)
summary(lm3)
summary(lm4)
aov2 <- aov(data = d, Brain.Residual ~ Insularity + Family)
aov3 <- aov(data = d, Brain.Residual ~ Insularity + Taxonomic.Order)
aov4 <- aov(data = d, Brain.Residual ~ Insularity + Taxonomic.Order + Body..g.)
summary(aov2)
summary(aov3)
summary(aov4)
posthoc1 <- TukeyHSD(aov1, which = "Insularity", conf.level = 0.95)
posthoc2 <- TukeyHSD(aov4, "Insularity", conf.level = 0.95)
plot(posthoc1)
plot(posthoc2)
