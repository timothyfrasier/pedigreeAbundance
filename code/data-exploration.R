#####################################
# CODE FOR DATA EXPLORATION WITH    #
# RESPECT TO DATA CHARACTERISTICS   #
# THAT COULD LEAD TO OVER- OR UNDER-#
# ESTIMATION OF ABUNDANCE.          #
#                                   #
# Last updated: 12-AUG-2025         #
# Tim Frasier                       #
#####################################


#----------------------------#
# Load appropriate libraries #
#----------------------------#
library(ggplot2)


#-------------------------------#
#   Read In & Organize the Data #
#-------------------------------#
data <- read.table("../data/paternity_summary.csv", sep = ",", header = TRUE)


#-----------------------------------------------#
# Plot # of genotyped mother-calf pairs by year #
#-----------------------------------------------#
ggplot(data) +
  theme_bw() +
  geom_point(aes(x = year, y = nGenotypedCalves), size = 3) +
  geom_line(aes(x = year, y = nGenotypedCalves)) +
  labs(
    y = "Number of genotyped mother-calf pairs",
    x = "Year"
  )

png("../results/mother_calf_pairs.png", width = 7, height = 4, units = "in", res = 300)
ggplot(data) +
  theme_bw() +
  geom_point(aes(x = year, y = nGenotypedCalves), size = 3) +
  geom_line(aes(x = year, y = nGenotypedCalves)) +
  labs(
    y = "Number of genotyped mother-calf pairs",
    x = "Year"
  )
dev.off()


#-----------------------------------------------#
# Plot # of paternities assigned by year        #
#-----------------------------------------------#
ggplot(data) +
  theme_bw() +
  geom_point(aes(x = year, y = nPaternities), size = 3) +
  geom_line(aes(x = year, y = nPaternities)) +
  labs(
    y = "Number of paternities assigned",
    x = "Year"
  )

png("../results/nPaternities.png", width = 7, height = 4, units = "in", res = 300)
ggplot(data) +
  theme_bw() +
  geom_point(aes(x = year, y = nPaternities), size = 3) +
  geom_line(aes(x = year, y = nPaternities)) +
  labs(
    y = "Number of paternities assigned",
    x = "Year"
  )
dev.off()


#-----------------------------------------------#
# Plot % of calves with fathers assigned        #
#-----------------------------------------------#
data$percent <- data$nPaternities / data$nGenotypedCalves
ggplot(data) +
  theme_bw() +
  geom_point(aes(x = year, y = percent), size = 3) +
  geom_line(aes(x = year, y = percent)) +
  labs(
    y = "% of genotyped m-c pairs with paternity assigned",
    x = "Year"
  )

png("../results/percent_paternities.png", width = 7, height = 4, units = "in", res = 300)
ggplot(data) +
  theme_bw() +
  geom_point(aes(x = year, y = percent), size = 3) +
  geom_line(aes(x = year, y = percent)) +
  labs(
    y = "% of genotyped m-c pairs with paternity assigned",
    x = "Year"
  )
dev.off()


#-----------------------------------------------#
# Plot # of candidates by year                  #
#-----------------------------------------------#
ggplot(data) +
  theme_bw() +
  geom_point(aes(x = year, y = nCandidates), size = 3) +
  geom_line(aes(x = year, y = nCandidates)) +
  labs(
    y = "# of genotyped candidates",
    x = "Year"
  )

png("../results/candidates.png", width = 7, height = 4, units = "in", res = 300)
ggplot(data) +
  theme_bw() +
  geom_point(aes(x = year, y = nCandidates), size = 3) +
  geom_line(aes(x = year, y = nCandidates)) +
  labs(
    y = "# of genotyped candidates",
    x = "Year"
  )
dev.off()
