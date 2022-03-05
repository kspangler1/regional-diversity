# Load the original soil diversity calculations and the bootstrap results
library(soildiv)
library(tidyverse)

div_final2 <- readRDS(file = "data/diversity_calcs_010921.RDS")

# Load the bootstrap sensitivity results
load("data/bootstrap_results_011121.RData")

# Recompute the GEOID with leading zeros to facilitate join.
div_final2 <- div_final2 %>%
  dplyr::mutate(GEOID = dplyr::if_else(nchar(GEOID) < 5,
                                       paste0("0", GEOID), as.character(GEOID)))
tdf_boot_final <- tdf_boot_final %>%
  dplyr::mutate(GEOID = dplyr::if_else(nchar(GEOID) < 5,
                                       paste0("0", GEOID), as.character(GEOID)))
tdf_boot_final_reclass <- tdf_boot_final_reclass %>%
  dplyr::mutate(GEOID = dplyr::if_else(nchar(GEOID) < 5,
                                       paste0("0", GEOID), as.character(GEOID)))
tdf_boot_final_reclass2 <- tdf_boot_final_reclass2 %>%
  dplyr::mutate(GEOID = dplyr::if_else(nchar(GEOID) < 5,
                                       paste0("0", GEOID), as.character(GEOID)))

# Reclassify updated FIPS ID due to county name change. This eliminates a current
# missing value in the dataset.
# - https://www.ddorn.net/data/FIPS_County_Code_Changes.pdf
div_final2$GEOID[div_final2$GEOID == "46113"] <- "46102"
tdf_boot_final$GEOID[tdf_boot_final$GEOID == "46113"] <- "46102"
tdf_boot_final_reclass$GEOID[tdf_boot_final_reclass$GEOID == "46113"] <- "46102"
tdf_boot_final_reclass2$GEOID[tdf_boot_final_reclass2$GEOID == "46113"] <- "46102"


# Ensure match on the 2017 and 2012 values.
#=============================================================================
match <- readRDS("data/divers_classif_data.RDS")

checkmatch <- dplyr::left_join(match, div_final2, by = c("GEOID", "YEAR")) %>%
  dplyr::select(GEOID, YEAR, SDI_CDL_AG, shannon, SIDI_CDL_AG,
                simpson, RICH_CDL_AG, richness, count) %>%
  dplyr::mutate(ratio1 = abs(SDI_CDL_AG - shannon)/SDI_CDL_AG,
                ratio2 = abs(SIDI_CDL_AG - simpson)/SIDI_CDL_AG,)

plot(checkmatch$SDI_CDL_AG, checkmatch$shannon)
plot(checkmatch$SIDI_CDL_AG, checkmatch$simpson)
plot(checkmatch$RICH_CDL_AG, checkmatch$richness)

#differences explained by different shapefiles we used from NASS that changes the diversity metric
#further justification for weighting or cutoff of diversity metrics because any difference in resolution will change this
#join by this script but exclude SDI_CDL_AG, etc. and use "count" column to filter counties below threshold 
#=============================================================================

# Check out 2017 - no reclassification
#=============================================================================
div_2017 <- div_final2 %>%
  dplyr::filter(YEAR == 2017) %>%
  dplyr::select(GEOID, shannon, simpson, richness, count) %>%
  dplyr::inner_join(., tdf_boot_final, by = "GEOID")

plot(div_2017$count, div_2017$shannon_sd, xlim = c(0, 2e03),
     main = "shannon", xlab = "pixel count",
     ylab = "bootstrap standard deviation")
abline(v = 250)
 
# plot(div_2017$count, div_2017$richness_sd, xlim = c(0, 1e04)) # no apparent pattern
#=============================================================================

# Check out 2017 - reclassification #1
#=============================================================================
div_2017_reclass <- div_final2 %>%
  dplyr::filter(YEAR == 2017) %>%
  dplyr::select(GEOID, shannon_new, simpson_new, richness_new, count) %>%
  dplyr::inner_join(., tdf_boot_final_reclass, by = "GEOID")

plot(div_2017_reclass$count, div_2017_reclass$shannon_sd, xlim = c(0, 2e03),
     main = "Reclass - shannon", xlab = "pixel count",
     ylab = "bootstrap standard deviation")
abline(v = 250)
#abline(h = 0.05)
plot(div_2017_reclass$count, div_2017_reclass$simpson_sd, xlim = c(0, 2e03),
     main = "Reclass - simpson", xlab = "pixel count",
     ylab = "bootstrap standard deviation")
abline(v = 250)
# plot(div_2017$count, div_2017$richness_sd, xlim = c(0, 1e04)) # no apparent pattern
#=============================================================================

# Check out 2017 - reclassification #2
#=============================================================================
div_2017_reclass2 <- div_final2 %>%
  dplyr::filter(YEAR == 2017) %>%
  dplyr::select(GEOID, shannon_double, simpson_double, richness_double, count) %>%
  dplyr::inner_join(., tdf_boot_final_reclass2, by = "GEOID")

plot(div_2017_reclass2$count, div_2017_reclass2$shannon_sd, xlim = c(0, 1e04),
     main = "Reclass (double count) - shannon", xlab = "pixel count",
     ylab = "bootstrap standard deviation")
plot(div_2017_reclass2$count, div_2017_reclass2$simpson_sd, xlim = c(0, 1e04),
     main = "Reclass (double count) - simpson", xlab = "pixel count",
     ylab = "bootstrap standard deviation")
# plot(div_2017$count, div_2017$richness_sd, xlim = c(0, 1e04)) # no apparent pattern
#=============================================================================

