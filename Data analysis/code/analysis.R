# ==============================================================================
# Statistical analysis for manuscript:
# A.L. Silva, A.L. Maravilha, J.A. Ramirez, J.-Y. Potvin, F.Campelo
# One-Dimensional Loading Capacitated Vehicle Routing Problem 
# with Multiple Stacks
# 
# Analysis by F. Campelo (fcampelo@ufmg.br, fcampelo@gmail.com)
# Belo Horizonte, Brazil - April 29, 2015
# ==============================================================================

# Load data
data <- read.table("../data/algo.txt",
                   header = TRUE)

# Set instance number to start with 1 (instead of 0):
data$inst <- data$inst + 1

#=====
# Part I: comparison of performance - full ALNS vs. pruned versions

# Aggregate data (algorithm means by instances)
aggdata <- with(data,
                aggregate(x   = resultado,
                          by  = list(alg, inst),
                          FUN = mean))

# Add subgroup ID variable
aggdata<-data.frame(Algorithm        = as.factor(aggdata[, 1]),
                    Instance         = as.factor(aggdata[, 2]),
                    Group            = rep(as.factor(1:36), 
                                           each = 35),
                    Mean_Performance = aggdata[, 3])


# Final aggregation (algorithm means by instance subgroup)
aggdata2 <- with(aggdata,
                 aggregate(x    = Mean_Performance,
                           by   = list(Algorithm, 
                                       Group),
                           FUN  = mean))

# Rename columns
names(aggdata2) <- c("Algorithm",
                     "Instance_Group",
                     "Mean_Performance")

# First model
model <- aov(Mean_Performance~Algorithm+Instance_Group,
             data = aggdata2)

# Graphical test of assumptions
par(mfrow = c(2, 2))
plot(model, pch = 20)

# Try log-transformed data
model2 <- aov(log(Mean_Performance)~Algorithm+Instance_Group,
              data = aggdata2)

# Graphical test of assumptions
plot(model2, pch = 20)
library(car)
par(mfrow = c(1, 1))
qqPlot(model2$residuals,
       pch = 20)

# Excellent! Print model summary
summary(model2)

# Get model r2 (quantify proportion of explained variance)
summary.lm(model2)$r.squared

# Perform multiple comparisons against full ALNS
library(multcomp)
duntest <- glht(model2,
                linfct = mcp(Algorithm = "Dunnett"))

duntestCI <- confint(duntest)
plot(duntestCI,
     xlab = "Mean difference (log scale)")

# preprocess for plotting using ggplot2 (paper quality)
dtCI <- as.data.frame(duntestCI$confint)
dtCI <- cbind(dtCI, 
              names = c("- RR",
                        "- RWDC",
                        "- RRDHWC",
                        "- RRR",
                        "- IG",
                        "- IR"))

#=====
library(ggplot2)
pdf("../figures/Dunnett.pdf",
    width = 16,
    height = 8)
dunPlot <- ggplot(dtCI, 
                  aes(y = Estimate, 
                      x = names))
dunPlot +
  geom_point(size = 5) +
  geom_errorbar(mapping = aes(ymin = lwr,
                              ymax = upr),
                width   = .1, 
                size    = 1, 
                colour  = "black") + 
  geom_hline(mapping  = aes(yintercept = 0),
             linetype = "dashed", 
             size     = 1) + 
  coord_flip() + 
  theme(axis.text   = element_text(size = 20),
        axis.title  = element_text(size = 20),
        plot.title  = element_text(size = 20)) + 
  ylab("Estimated difference (%)") + 
  xlab("Heuristic removed") + 
  ggtitle("Comparisons against standard (full) ALNS")

dev.off()

#=====
# Effects (exploratory)
# library(effects)
# effs <- allEffects(model2,
#                    transformation = list(link     = log,
#                                          inverse  = exp))
# plot(effs,
#      "Algorithm",
#      type   = "response",
#      lty    = 3,
#      lwd    = 1,
#      grid   = TRUE,
#      cex    = 2,
#      ylab   = "Mean Cost")
#=====

# Part II: performance of the "best" version vs. optimal values of selected 
# instances

# Isolate the "best" configuration (ALNS - RR)
data2 <- subset(data,
                alg == 2)[, -(1:2)]
names(data2) <- c("Instance",
                  "Result")
data2$Instance <- as.factor(data2$Instance)

# Boxplots (exploratory)
# with(data2,
#      boxplot(Result~Instance,
#              pch    = 16,
#              cex    = 0.25,
#              col    = "#777777",
#              las    = 1,
#              box    = "n",
#              tck    = 1,
#              xlab   = "Instance",
#              ylab   = "Cost"))
# grid(NA, NULL, lwd = 2)

#=====
# Read and preprocess optimal values
optvals <- read.csv("../data/optimal_values.csv",
                    header  = T,
                    sep     = ",")[, -1]
optvals <- optvals[complete.cases(optvals), ]
optvals$Instance <- as.factor(optvals$Instance)
optvals[, 3] <- optvals[, 3] / 1000
names(optvals)[3] <- "Time_s"
optvals <- cbind(optvals,
                 Posx = as.numeric(rownames(optvals)))

#=====
# Subset results from ALNS (-RR) for the relevant instances
data3 <- subset(data2,
                Instance %in% optvals$Instance)
data3$Instance <- factor(data3$Instance)
Group <- c(rep("01", 5),
           rep("04", 5),
           rep("12", 4))
data3 <- cbind(data3, 
               Group = rep(Group,
                           times = 30))

#=====
# Generate plot (paper quality)
pdf("../figures/ALNSvsOPT.pdf",
    width   = 16,
    height  = 8)
mybox <- ggplot(data    = data3, 
                mapping = aes(x = Instance, 
                              y = Result))
mybox + 
  geom_boxplot() + 
  geom_vline(mapping  = aes(xintercept = 5.5),
             linetype = 2, 
             size     = 1,
             alpha    = 0.5) + 
  geom_vline(mapping  = aes(xintercept = 10.5),
             linetype = 2, 
             size     = 1,
             alpha    = 0.5) + 
  geom_segment(data     = optvals, 
               mapping  = aes(x   = Posx - 0.35,
                             xend = Posx + 0.35,
                             y    = Value,
                             yend = Value), 
               linetype = 2,
               size     = 1,
               colour   = 2) + 
  theme(axis.text   = element_text(size = 20),
        axis.title  = element_text(size = 20),
        plot.title  = element_text(size = 20)) + 
  ylab("Result") + 
  xlab("Instance") + 
  ggtitle("ALNS (-RR) versus exact solution")
dev.off()

#=====
# Extract best ALNS(-RR) solutions for each instance
aggbest <- with(data2,
                aggregate(x   = Result,
                          by  = list(Instance),
                          FUN = min))

names(aggbest) <- c("Instance",
                    "Best_ALNS")

# Best ALNS values vs optimal values 
res <- cbind(aggbest[as.numeric(as.character(optvals[, 1])),],
             Optimal_value = optvals[, 2],
             Gap = round(100 * (aggbest[as.numeric(as.character(optvals[, 1])), 2] 
                                - optvals[, 2]) / optvals[, 2], 2))
