library(reshape)
library(dplyr)
library(readxl)


load("nwhi_ramp_nspc_0618.rda")
area <- read_excel("area_nwhi.xlsx")

df <- nwhi_ramp_nspc
df <- df %>% mutate(DENSITY = COUNT / 176.625, 
                    BIOMASS_G_M2 = (LW_a * (SIZE * TLtoLen_Conv) ^ LW_b) / 176.625)

nwhi_fish <- df[, c("ISLAND", "REEF_ZONE", "DEPTH_BIN", "SITEVISITID", "REP", 
                    "REPLICATEID", "Taxon", "Trophic", "COUNT", 
                    "DENSITY", "BIOMASS_G_M2")]
nwhi_fish$STRAT <- paste(substring(as.character(nwhi_fish$REEF_ZONE), 1, 1), 
                         substring(as.character(nwhi_fish$DEPTH_BIN), 1, 1), sep = "_")
nwhi_fish$STRAT <- factor(nwhi_fish$STRAT)

dat <- cast(nwhi_fish, ISLAND + STRAT + SITEVISITID + REP + REPLICATEID ~ Trophic, 
            value = "COUNT", fun.aggregate = "sum")
dat$TotF <- rowSums(dat[, c("Corallivore", "Herbivore", "Invertivore", "Omnivore", 
                            "Piscivore", "Planktivore")])

trophic <- c("Piscivore", "Planktivore", "Herbivore", "Invertivore", "TotF")
sd_table <- data.frame(ISLAND = character(0), 
                       STRAT = character(0), 
                       STRAT_G = numeric(0),
                       Area = numeric(0), 
                       Atot = numeric(0), 
                       Area_wt = numeric(0))

for (i in 1 : length(trophic)) {
  val <- trophic[i]
  fish_site <- with(dat, aggregate(dat[, val], by = list(ISLAND, STRAT, SITEVISITID), mean))
  colnames(fish_site) <- c("ISLAND", "STRAT", "SITEVISITID", "SiteAve")

  fish_var <- with(fish_site, aggregate(SiteAve, by = list(ISLAND, STRAT), var))
  colnames(fish_var) <- c("ISLAND", "STRAT", "Var")
  fish_var$StDev <- sqrt(fish_var$Var)
  
  # Merge area with fish_strat
  fish_strat <- inner_join(fish_var, area, by = c("ISLAND", "STRAT"))
  fish_strat <- fish_strat[, c("ISLAND", "STRAT", "STRAT_G", "Area",
                               "Atot", "Area_wt", "StDev")]
  colnames(fish_strat)[7] <- val
  sd_table <- left_join(fish_strat, sd_table, 
                        by = c("ISLAND", "STRAT", "STRAT_G", "Area", "Atot", "Area_wt"))
}

sd_table <- droplevels(sd_table)

prop_table <- sd_table
for (j in 1 : length(trophic)){
  col = trophic[j]
  for (i in 1 : nrow(sd_table)) {
    prop_table[i, col] <- sd_table[i, col] / max(sd_table[sd_table$ISLAND == sd_table$ISLAND[i] & sd_table$STRAT_G == sd_table$STRAT_G[i], col])
  }
}
prop_table$BY_SD <- apply(prop_table[, trophic], 1, mean)
prop_table$BY_AREA <- 0
for (i in 1 : nrow(prop_table)) {
  prop_table$BY_AREA[i] <- prop_table$Area[i] / max(prop_table$Area[prop_table$ISLAND == prop_table$ISLAND[i] & prop_table$STRAT_G == prop_table$STRAT_G[i]])
}
prop_table$COMBINED <- round(prop_table$BY_SD * prop_table$BY_AREA, 2)
prop_table$WEIGHT <- 0
for (i in 1 : nrow(prop_table)) {
  prop_table$WEIGHT[i] <- prop_table$COMBINED[i] / sum(prop_table$COMBINED[prop_table$ISLAND == prop_table$ISLAND[i] & prop_table$STRAT_G == prop_table$STRAT_G[i]])
}
prop_table$WEIGHT <- round(prop_table$WEIGHT, 2)
prop_table <- prop_table %>%
  arrange(ISLAND, STRAT_G, STRAT)

write.csv(prop_table, "RAMP_sample_allocation.csv", row.names = FALSE)