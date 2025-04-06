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
skim(d)
head(d)
f2 <- "https://raw.githubusercontent.com/TheCoolestPotato/Data-Replication-Assignment/refs/heads/main/41467_2018_5280_MOESM5_ESM.csv"
d2 <- read.csv(f2)
dist_plot <- ggplot(data = d, aes(x = Brain..mm3., y = Insularity)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.05)
plot(dist_plot)
d <- d |>  mutate(logbrain = log(Brain..mm3.), logbody = log(Body..g.))
d <- (drop_na(d, logbrain))
log_dist_plot <- ggplot(data = d, aes(x = logbrain, y = Insularity)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.05)
plot(log_dist_plot)

brainsize_island_plot <- ggplot(data = d, aes(x = Insularity, y = Brain.Residual, fill = Insularity)) +
  geom_boxplot(notch = TRUE, outliers = FALSE) +
  theme(legend.position = "none") +
  ylab("Relative Brain Size") +
  xlab("")
plot(brainsize_island_plot)

dgroup <- d |>
  select(N, Family, Species, Brain..mm3., Insularity, Migratory.Behavior, logbrain, Brain.Residual, logbody, Body..g., Brain.Residual) |>
  group_by(Family) |>
  summarize(relativebrainsize = mean(Brain.Residual), N = sum(N), meanbrain = mean(Brain..mm3.), meanbody = mean(Body..g.), meanlogbody = mean(logbody), meanlogbrain = mean(logbrain))

d |>
  group_by(Family) |>
  summarize(meanresidual = mean(Brain.Residual))
