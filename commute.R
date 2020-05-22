library(dplyr)
library(data.table)
library(ggplot2)

setwd("~/Dropbox/research/commute/replicate")
# replace to your dir



################################
# Fig. 1
################################
dt <- read.table("data/city-commute/d1_city.csv", sep=",", header = T)

# pearson, mean, and sd. of commuting distance
# Fig. 1B
cor.test(dt$Popu2010, dt$comm_mean, method = "pearson")
# mean commuting distance (in meter)
mean(dt$comm_mean)
#s.e. of commuting distance
sd(dt$comm_mean)/sqrt(length(dt$comm_mean))
# regression between population and average commuting distance
summary(lm(comm_mean ~ Popu2010, data = dt))

# pearson commuting distance (with subway system)
dt1 <- dt[dt$subway == 1, ]
cor.test(dt1$Popu2010, dt1$comm_mean, method = "pearson")


# Fig. 1C
# regression between built-up area and average commuting distance
summary(lm(comm_mean ~ SUM_bup10, data = dt))

# Fig. 1D
# pearson, mean, and sd. of locations
cor.test(dt$Popu2010, dt$location_mean, method = "pearson")
# regression between population and # of location
summary(lm(location_mean ~ Popu2010, data = dt))

# Fig. 1E
# regression between built-up area and # of location
summary(lm(location_mean ~ SUM_bup10, data = dt))


# Fig. 1B
ggplot(aes(x = log10(Popu2010*10000), y = comm_mean/1000), data = dt) + 
  geom_point(size = 2, alpha = 0.4) + 
  geom_point(aes(x = log10(Popu2010*10000), y = comm_mean/1000), 
             color = "red", size = 2, alpha = 0.6, data = dt1) + 
  geom_hline(yintercept = mean(dt$comm_mean)/1000, size = 1, color = "red") +
  xlab("log10(Population)") + 
  ylab("Average commute dist. (km)") + ylim(c(0, 15)) +
  theme_classic(base_size = 14)
#ggsave("fig/commutingdistance.pdf", width = 3.5, height = 3)


# Fig. 1D
ggplot(aes(x = log10(Popu2010*10000), y = location_mean), data = dt) + 
  geom_point(size = 2, alpha = 0.4) + 
  geom_hline(yintercept = mean(dt$location_mean), size = 1, color = "red") +
  xlab("log10(Population)") + 
  ylab("Average number of locations") + 
  ylim(c(0, 10)) +
  theme_classic(base_size = 14)
#ggsave("fig/numlocations.pdf", width = 3.5, height = 3)


# Fig. 1C commuting distance vs. built-up area
ggplot(aes(x = log10(SUM_bup10), y = comm_mean/1000), data = dt) + 
  geom_point(size = 2, alpha = 0.4) + 
  geom_hline(yintercept = mean(dt$comm_mean)/1000, size = 1, color = "red") +
  xlab("log10(Built-up area)") + 
  ylab("Average commute dist. (km)") + ylim(c(0, 15)) +
  theme_classic(base_size = 14)
#ggsave("fig/commutingdistancebuiltup.pdf", width = 3.5, height = 3)


# Fig. 1E locations vs. built-up area 
ggplot(aes(x = log10(SUM_bup10), y = location_mean), data = dt) + 
  geom_point(size = 2, alpha = 0.4) + 
  geom_hline(yintercept = mean(dt$location_mean), size = 1, color = "red") +
  xlab("log10(Built-up area)") + 
  ylab("Average number of locations") + 
  ylim(c(0, 10)) +
  theme_classic(base_size = 14)
#ggsave("fig/builtuparea.pdf", width = 3.5, height = 3)


################################
# Fig. 2 & SFig. 5, 6
################################
library("npregfast")

npreg <- function(model){
  x <- model$x
  y <- as.data.frame(as.data.frame(model$p)[,c(1)])
  colnames(y) <- "pred"
  y$lower <- NA
  y$upper <- NA
  
  repboot <- as.data.frame(model$repboot)
  for (i in 1:nrow(repboot)){
    tmp <- repboot[i, seq(1,1500, 3)]
    rst <- quantile(tmp, probs = c(0.05, 0.95))
    y$lower[i] <- rst[[1]]
    y$upper[i] <- rst[[2]]
  }
  return(as.data.frame(cbind(x, y)))
}

# replace the name of city
city <- read.table("data/city-commute/chengdu_commute_v2.csv", sep=",", header = T)
#city <- city[seq(1,nrow(city),2),]
city$driving_dura_mean <- city$driving_dura_mean + 60*10 # add 10min fixed parking time
city$driving_dura_median <- city$driving_dura_median + 60*10 # add 10min fixed parking time

# no-para fitting
transit.dist.model <- frfast(transit_dist_median ~ dist, data = city, p = 1, seed = 1234)
transit.dura.model <- frfast(transit_dura_median ~ dist, data = city, p = 1, seed = 1234)
driving.dist.model <- frfast(driving_dist_median ~ dist, data = city, p = 1, seed = 1234)
driving.dura.model <- frfast(driving_dura_median ~ dist, data = city, p = 1, seed = 1234)
transit.dist.fit <- npreg(transit.dist.model)
transit.dura.fit <- npreg(transit.dura.model)
driving.dist.fit <- npreg(driving.dist.model)
driving.dura.fit <- npreg(driving.dura.model)

transit.dist.model_mean <- frfast(transit_dist_mean ~ dist, data = city, p = 1, seed = 1234)
transit.dura.model_mean <- frfast(transit_dura_mean ~ dist, data = city, p = 1, seed = 1234)
driving.dist.model_mean <- frfast(driving_dist_mean ~ dist, data = city, p = 1, seed = 1234)
driving.dura.model_mean <- frfast(driving_dura_mean ~ dist, data = city, p = 1, seed = 1234)
transit.dist.fit_mean <- npreg(transit.dist.model_mean)
transit.dura.fit_mean <- npreg(transit.dura.model_mean)
driving.dist.fit_mean <- npreg(driving.dist.model_mean)
driving.dura.fit_mean <- npreg(driving.dura.model_mean)


# Fig. 2B/E
ggplot() + 
  geom_point(aes(x = dist, y = transit_dist_median/1000), data = city,
             size = 1.5, alpha = 0.6, color = "red") + 
  geom_point(aes(x = dist, y = driving_dist_median/1000), data = city,
             size = 1.5, alpha = 0.6) + 
  geom_line(aes(x = x, y = pred/1000), data = transit.dist.fit,
            size = 0.8, alpha = 0.8, color = "red") + 
  geom_line(aes(x = x, y = pred/1000), data = driving.dist.fit,
            size = 0.8, alpha = 0.8) + 
  geom_ribbon(aes(x = x, ymin = lower/1000, ymax = upper/1000), data = transit.dist.fit, 
              alpha = 0.2, fill = "red") + 
  geom_ribbon(aes(x = x, ymin = lower/1000, ymax = upper/1000), data = driving.dist.fit, 
              alpha = 0.2) + 
  geom_point(aes(x = dist, y = transit_dist_mean/1000), data = city,
             shape = 17, size = 1.5, alpha = 0.6, color = "red") + 
  geom_point(aes(x = dist, y = driving_dist_mean/1000), data = city,
             shape = 17, size = 1.5, alpha = 0.6) + 
  geom_line(aes(x = x, y = pred/1000), data = transit.dist.fit_mean,
            size = 0.8, alpha = 0.8, linetype = "dashed", color = "red") + 
  geom_line(aes(x = x, y = pred/1000), data = driving.dist.fit_mean,
            size = 0.8, alpha = 0.8, linetype = "dashed") + 
  geom_ribbon(aes(x = x, ymin = lower/1000, ymax = upper/1000), data = transit.dist.fit_mean, 
              alpha = 0.2, fill = "red") + 
  geom_ribbon(aes(x = x, ymin = lower/1000, ymax = upper/1000), data = driving.dist.fit_mean, 
              alpha = 0.2) + 
  xlab("Distance from center (km)") + 
  ylab("Average commute dist. (km)") + 
  xlim(1,21) +
  ylim(4,10) +
  theme_classic(base_size = 14)
#ggsave("fig/chengdu_dist_v1.pdf", width = 3.5, height = 3)


# Fig. 2C/F
ggplot() + 
  geom_point(aes(x = dist, y = transit_dura_median/60), data = city,
             size = 1.5, alpha = 0.6, color = "red") + 
  geom_point(aes(x = dist, y = driving_dura_median/60), data = city,
             size = 1.5, alpha = 0.6) + 
  geom_line(aes(x = x, y = pred/60), data = transit.dura.fit,
            size = 0.8, alpha = 0.8, color = "red") + 
  geom_line(aes(x = x, y = pred/60), data = driving.dura.fit,
            size = 0.8, alpha = 0.8) + 
  geom_ribbon(aes(x = x, ymin = lower/60, ymax = upper/60), data = transit.dura.fit, 
              alpha = 0.2, fill = "red") + 
  geom_ribbon(aes(x = x, ymin = lower/60, ymax = upper/60), data = driving.dura.fit, 
              alpha = 0.2) + 
  geom_point(aes(x = dist, y = transit_dura_mean/60), data = city,
             shape = 17, size = 1.5, alpha = 0.6, color = "red") + 
  geom_point(aes(x = dist, y = driving_dura_mean/60), data = city,
             shape = 17, size = 1.5, alpha = 0.6) + 
  geom_line(aes(x = x, y = pred/60), data = transit.dura.fit_mean,
            size = 0.8, alpha = 0.8, linetype = "dashed", color = "red") + 
  geom_line(aes(x = x, y = pred/60), data = driving.dura.fit_mean,
            size = 0.8, alpha = 0.8, linetype = "dashed") + 
  geom_ribbon(aes(x = x, ymin = lower/60, ymax = upper/60), data = transit.dura.fit_mean, 
              alpha = 0.2, fill = "red") + 
  geom_ribbon(aes(x = x, ymin = lower/60, ymax = upper/60), data = driving.dura.fit_mean, 
              alpha = 0.2) + 
  xlab("Distance from center (km)") + 
  ylab("Average commute time (min)") + 
  xlim(1,21) +
  ylim(20,55) +
  theme_classic(base_size = 14)
#ggsave("fig/chengdu_dura_v1.pdf", width = 3.5, height = 3)



################################
# Fig. 4
################################

# relationship between city population and wage per capita
wage <- read.table("data/city-commute/city_wage.csv", sep=",", header = T)
summary(lm(log10(Wage) ~ log10(Population), data = wage))

ggplot(aes(x = log10(Population), y = log10(Wage)), data = wage) + 
  geom_point(size = 2, alpha = 0.6) + 
  geom_smooth(method = "lm", se = FALSE, color = "red", alpha = 0.8) + 
  xlab("log10(Population)") + 
  ylab("log10(Wage) [RMB]") + 
  xlim(6,7.5) +
  ylim(4.2, 4.8) +
  theme_classic(base_size = 14)
#ggsave("fig/wage_v2.pdf", width = 3.5, height = 3)


### Fig. 4
setwd("~/Downloads/commute/simulation-v3")

BETA <- c("0.01", "0.025", "0.05", "0.075", 
          "0.1", "0.1125", "0.125", "0.1325", "0.15", "0.1625", "0.175", "0.1875",
          "0.2", "0.2125", "0.225", "0.2325", "0.25", "0.2625", "0.275", "0.2875",
          "0.3", "0.325", "0.35", "0.375", 
          "0.4", "0.425", "0.45", "0.475",
          "0.5", "0.55", "0.6")
MU <- c("2.0")

# for Fig. 4D
N <- c("4000")
C <- c("2.0", "10.0")


# for Fig. 4E
N <- c("500", "1000", "2000", "4000", "8000")
C <- c("2.0")

rst <- data.frame()
for (b in BETA){
  for (n in N){
    for (c in C){
      for (mu in MU){
        filename <- paste0("network-", n, "-", b, "-", mu, "-", c, ".csv")
        dt <- fread(filename, sep = ",", header = TRUE)
        dt1 <- dt %>%
          group_by(round) %>%
          summarise(first = max(interaction)/(sum(interaction)/2),
                    second = sort(interaction)[length(interaction)-1]/(sum(interaction)/2),
                    commute = mean(commudist))
        
        tmp <- data.frame("beta" = b, "N" = n, "c" = c, "mu" = mu,
                          "first" = mean(dt1$first), 
                          "second" = mean(dt1$second),
                          "commute" = mean(dt1$commute))
        rst <- rbind(rst, tmp)
      }
    }
  }
}
rst$beta <- as.numeric(as.character(rst$beta))


# Fig. 4D beta and the largest cluster
ggplot() + 
  geom_point(aes(x = beta, y = first), data = rst[rst$c == "2.0" & rst$mu == "2.0" & rst$N == "4000",],
             size = 1.5, alpha = 0.6) + 
  geom_line(aes(x = beta, y = first), data = rst[rst$c == "2.0" & rst$mu == "2.0" & rst$N == "4000",],
             size = 0.5, alpha = 0.6) + 
  geom_point(aes(x = beta, y = first), data = rst[rst$c == "10.0" & rst$mu == "2.0" & rst$N == "4000",],
            size = 1.5, alpha = 0.6, color = "red") + 
  geom_line(aes(x = beta, y = first), data = rst[rst$c == "10.0" & rst$mu == "2.0" & rst$N == "4000",],
             size = 0.5, alpha = 0.6, color = "red") +
  xlab("beta^*") + 
  ylab("Largest center (%)") + 
  theme_classic(base_size = 14)
#ggsave("fig/simu-firstcenter-v3.pdf", width = 3.3, height = 3)


# Fig. 4D insert beta and the second largest cluster
ggplot() + 
  geom_point(aes(x = beta, y = second), data = rst[rst$c == "2.0" & rst$N == "4000",],
             size = 1.5, alpha = 0.6) + 
  geom_line(aes(x = beta, y = second), data = rst[rst$c == "2.0" & rst$N == "4000",],
            size = 0.5, alpha = 0.6) + 
  geom_point(aes(x = beta, y = second), data = rst[rst$c == "10.0" & rst$N == "4000",],
             size = 1.5, alpha = 0.6, color = "red") + 
  geom_line(aes(x = beta, y = second), data = rst[rst$c == "10.0" & rst$N == "4000",],
            size = 0.5, alpha = 0.6, color = "red") +
  xlab("beta^*") + 
  ylab("Largest center (%)") + 
  theme_classic(base_size = 14)
#ggsave("fig/simu-secondcenter-v3.pdf", width = 2.5, height = 2)



# Fig. 4E beta and commuting distance
ggplot() + 
  geom_point(aes(x = beta, y = commute), data =rst[rst$c == "2.0" & rst$N == "500",],
            size = 1.5, alpha = 0.8, color = "red") + 
  geom_line(aes(x = beta, y = commute), data =rst[rst$c == "2.0" & rst$N == "500",],
             size = 0.5, alpha = 0.8, color = "red") + 
  geom_point(aes(x = beta, y = commute), data =rst[rst$c == "2.0" & rst$N == "1000",],
             size = 1.5, alpha = 0.4, color = "red") + 
  geom_line(aes(x = beta, y = commute), data =rst[rst$c == "2.0" & rst$N == "1000",],
            size = 0.5, alpha = 0.4, color = "red") + 
  geom_point(aes(x = beta, y = commute), data =rst[rst$c == "2.0" & rst$N == "2000",],
             size = 1.5, alpha = 0.8, color = "blue4") + 
  geom_line(aes(x = beta, y = commute), data =rst[rst$c == "2.0" & rst$N == "2000",],
            size = 0.5, alpha = 0.8, color = "blue4") + 
  geom_point(aes(x = beta, y = commute), data =rst[rst$c == "2.0" & rst$N == "4000",],
             size = 1.5, alpha = 0.4) + 
  geom_line(aes(x = beta, y = commute), data =rst[rst$c == "2.0" & rst$N == "4000",],
            size = 0.5, alpha = 0.4) + 
  geom_point(aes(x = beta, y = commute), data =rst[rst$c == "2.0" & rst$N == "8000",],
             size = 1.5, alpha = 0.8) + 
  geom_line(aes(x = beta, y = commute), data =rst[rst$c == "2.0" & rst$N == "8000",],
            size = 0.5, alpha = 0.8) + 
  xlab("beta^*") + 
  ylab("Commte dist.") + 
  theme_classic(base_size = 14) +
  theme(legend.position = c(0.8, 0.6))
#ggsave("fig/simu-commute-v3.pdf", width = 3.3, height = 3)


# Fig. 4E insert
ggplot() + 
  geom_point(aes(x = as.numeric(as.character(N)), y = commute), data =rst[rst$beta == 0.2 & rst$c == "2.0",],
             size = 1.5, alpha = 0.6) + 
  xlab("N") + 
  ylab("Commte dist.") + 
  ylim(0, 4) +
  theme_classic(base_size = 14) 
#ggsave("fig/simu-commute-insert-v3.pdf", width = 2.5, height = 2)



# Fig. 4F
poly <- fread("network-8000-0.2-2.0-2.0.csv", sep = ",", header = TRUE)
mono <- fread("network-8000-0.05-2.0-2.0.csv", sep = ",", header = TRUE)
dece <- fread("network-8000-0.5-2.0-2.0.csv", sep = ",", header = TRUE)
poly1 <- fread("network-8000-0.2-2.0-4.0.csv", sep = ",", header = TRUE)

aggre <- function(dt){
  dt$dist <- as.integer(((dt$x **2) + (dt$y **2))**0.5 * 4)
  dt1 <- dt %>%
    group_by(dist) %>%
    summarise(commute = mean(commudist))
  return (dt1)
}

poly.agg <- aggre(poly)
poly1.agg <- aggre(poly1)
mono.agg <- aggre(mono)
dece.agg <- aggre(dece)

ggplot() + 
  geom_point(aes(x = dist/4, y = commute), data = poly.agg, 
             size = 1.5, alpha = 0.6, color = "red") + 
  geom_line(aes(x = dist/4, y = commute), data = poly.agg, 
             size = 0.5, alpha = 0.6, color = "red") + 
  geom_point(aes(x = dist/4, y = commute), data = mono.agg, 
             size = 1.5, alpha = 0.6) + 
  geom_line(aes(x = dist/4, y = commute), data = mono.agg, 
             size = 0.5, alpha = 0.6) + 
  geom_point(aes(x = dist/4, y = commute), data = dece.agg, 
             size = 1.5, alpha = 0.6, "color" = "blue4") + 
  geom_line(aes(x = dist/4, y = commute), data = dece.agg, 
             size = 0.5, alpha = 0.6, "color" = "blue4") + 
  geom_point(aes(x = dist/4, y = commute), data = poly1.agg, 
             size = 1.5, alpha = 0.6, "color" = "green4") + 
  geom_line(aes(x = dist/4, y = commute), data = poly1.agg, 
             size = 0.5, alpha = 0.6, "color" = "green4") + 
  xlim(0, 7) +
  ylim(0, 6)+
  xlab("Distance from city center") + 
  ylab("Commute distance") + 
  theme_classic(base_size = 14)
#ggsave("fig/simu-commute2-v3.pdf", width = 3.3, height = 3)
