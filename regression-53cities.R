# reference: https://towardsdatascience.com/7-steps-to-run-a-linear-regression-analysis-using-r-d5897a66b835/

# install.packages("tidyverse")
library(tidyverse)

df <- read.csv("c40cities-co2-em-conc.csv") # outcome of https://github.com/heytian/d2d-oco3-tools/blob/main/20260404_03_merge-co2-em-conc.py
df <- df |> drop_na(population)

# normalize independent variables using min-max / interquartile range 

# Option 1: min-max; stretch full range of each var to [0,1]
minmax_norm <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

# Option 2: interquartile; values below q1 negative, values above q3 go above 1
iqr_norm <- function(x) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  (x - q1) / (q3 - q1)
}


norm_df <- df |> 
  mutate(
    # co2_conc_mm   = minmax_norm(xco2_ppm),
    # co2_em_mm     = minmax_norm(annual_co2_em_mtco2),
    # sif_mm        = minmax_norm(sif_757nm),
    # pop_mm = minmax_norm(population),
    # numsample_mm  = minmax_norm(number_of_sams),
    # gdp_mm        = minmax_norm(gdp_bil_usd),
    # impact_mm     = co2_conc_mm - co2_em_mm,
    co2_conc_iqr   = iqr_norm(xco2_ppm),
    co2_em_iqr     = iqr_norm(annual_co2_em_mtco2),
    sif_iqr        = iqr_norm(sif_757nm),
    pop_iqr = iqr_norm(population),
    xco2numsams_iqr  = iqr_norm(xco2_numsams),
    sifnumsams_iqr  = iqr_norm(sif_numsams),
    gdp_iqr        = iqr_norm(gdp_bil_usd),
    impact_iqr     = co2_conc_iqr - co2_em_iqr
  )
    
# lm_mm_co2_conc <- lm(co2_conc_mm ~ sif_mm + pop_mm + co2_em_mm + numsample_mm + gdp_mm, 
#                      data = norm_df)
# summary(lm_mm_co2_conc)
# 
# lm_iqr_co2_conc<- lm(co2_conc_iqr ~ sif_iqr + pop_iqr + co2_em_iqr + numsample_iqr + gdp_iqr, 
#                    data = norm_df)
# summary(lm_iqr_co2_conc)
#   
# lm_mm_impact<- lm(impact_mm ~ sif_mm + pop_mm + numsample_mm + gdp_mm, 
#                   data = norm_df)
# summary(lm_mm_impact)

lm_iqr_impact<- lm(impact_iqr ~ sif_iqr + pop_iqr + xco2numsams_iqr + sifnumsams_iqr + gdp_iqr, 
                  data = norm_df)
summary(lm_iqr_impact)





  
