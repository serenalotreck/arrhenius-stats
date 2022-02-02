#' Test suite for arrhenius_stats.R
source("Shiu_lab/for-others/seth_temp_sensitivity/arrhenius_stats.R", chdir = TRUE)
library(testthat)

test_that('No NA or missing measurement, 5 days', {
  
  # Build the input matrix
  SITE <- c('id1', 'id1', 'id1', 'id1', 'id1')
  DATE <- c('1/7/22', '1/8/22', '1/15/22', '1/17/22', '1/20/22')
  STANDTEMP <- c(-0.34, -0.70, -0.24, -0.42, 0.48)
  InvGPP <- c(0.61, 0.48, 0.69, 0.57, 1.28)
  InvER <- c(1.27, 0.67, 0.55, 0.37, 0.24)
  input <- data.frame(SITE, DATE, STANDTEMP, InvGPP, InvER)
  
  # Build the output matrix
  # NOTE: Because of missing data, the outputs for rows 1-2 and 3-5 are the same,
  # So I'm just going to reassign the same vectors for those rows
  row1_lm_GPP <- lm(InvGPP~STANDTEMP, data = input[1:2,])
  row1_lm_ER <- lm(InvER~STANDTEMP, data = input[1:2,])
  row1_stats <- c(summary(row1_lm_GPP)$coefficients["STANDTEMP", "Estimate"],
                  summary(row1_lm_GPP)$coefficients["(Intercept)", "Estimate"],
                  summary(row1_lm_GPP)$fstatistic["value"],
                  summary(row1_lm_GPP)$fstatistic["numdf"],
                  anova(row1_lm_GPP)$'Pr(>F)'[1],
                  summary(row1_lm_ER)$coefficients["STANDTEMP", "Estimate"],
                  summary(row1_lm_ER)$coefficients["(Intercept)", "Estimate"],
                  summary(row1_lm_ER)$fstatistic["value"],
                  summary(row1_lm_ER)$fstatistic["numdf"],
                  anova(row1_lm_ER)$'Pr(>F)'[1]
                  )
  row2_stats <- row1_stats
  row3_lm_GPP <- lm(InvGPP~STANDTEMP, data = input[3:5,])
  row3_lm_ER <- lm(InvER~STANDTEMP, data = input[3:5,])
  row3_stats <- c(summary(row3_lm_GPP)$coefficients["STANDTEMP", "Estimate"],
                  summary(row3_lm_GPP)$coefficients["(Intercept)", "Estimate"],
                  summary(row3_lm_GPP)$fstatistic["value"],
                  summary(row3_lm_GPP)$fstatistic["numdf"],
                  anova(row3_lm_GPP)$'Pr(>F)'[1],
                  summary(row3_lm_ER)$coefficients["STANDTEMP", "Estimate"],
                  summary(row3_lm_ER)$coefficients["(Intercept)", "Estimate"],
                  summary(row3_lm_ER)$fstatistic["value"],
                  summary(row3_lm_ER)$fstatistic["numdf"],
                  anova(row3_lm_ER)$'Pr(>F)'[1]
                  )
  row4_stats <- row3_stats
  row5_stats <- row3_stats
  
  output <- do.call(rbind.data.frame, c(row1_stats, row2_stats, row3_stats, row4_stats, row5_stats))
  
  # Test
  expect_equal(calculate_stats(input, 5), output)
  
})

test_that('Output should contain NA rows, 5 days', {
  
  # Build the input matrix
  SITE <- c('id1', 'id1', 'id1', 'id1', 'id1')
  DATE <- c('1/2/22', '1/8/22', '1/15/22', '1/17/22', '1/20/22')
  STANDTEMP <- c(-0.34, -0.70, -0.24, -0.42, 0.48)
  InvGPP <- c(0.61, 0.48, 0.69, 0.57, 1.28)
  InvER <- c(1.27, 0.67, 0.55, 0.37, 0.24)
  input <- data.frame(SITE, DATE, STANDTEMP, InvGPP, InvER)
  
  # Build the output matrix
  row1_stats <- rep(NA,10)
  row2_stats <- rep(NA,10)
  row3_lm_GPP <- lm(InvGPP~STANDTEMP, data = input[3:5,])
  row3_lm_ER <- lm(InvER~STANDTEMP, data = input[3:5,])
  row3_stats <- c(summary(row3_lm_GPP)$coefficients["STANDTEMP", "Estimate"],
                  summary(row3_lm_GPP)$coefficients["(Intercept)", "Estimate"],
                  summary(row3_lm_GPP)$fstatistic["value"],
                  summary(row3_lm_GPP)$fstatistic["numdf"],
                  anova(row3_lm_GPP)$'Pr(>F)'[1],
                  summary(row3_lm_ER)$coefficients["STANDTEMP", "Estimate"],
                  summary(row3_lm_ER)$coefficients["(Intercept)", "Estimate"],
                  summary(row3_lm_ER)$fstatistic["value"],
                  summary(row3_lm_ER)$fstatistic["numdf"],
                  anova(row3_lm_ER)$'Pr(>F)'[1]
  )
  row4_stats <- row3_stats
  row5_stats <- row3_stats
  
  output <- do.call(rbind.data.frame, c(row1_stats, row2_stats, row3_stats, row4_stats, row5_stats))
  
  # Test
  expect_equal(calculate_stats(input, 5), output)
  
})

test_that('Missing InvGPP or ER', {
  
  # Build the input matrix
  SITE <- c('id1', 'id1', 'id1', 'id1', 'id1')
  DATE <- c('1/7/22', '1/8/22', '1/15/22', '1/17/22', '1/20/22')
  STANDTEMP <- c(-0.34, -0.70, -0.24, -0.42, 0.48)
  InvGPP <- c(0.61, 0.48, 0.69, 0.57, NA)
  InvER <- c(1.27, 0.67, NA, 0.37, 0.24)
  input <- data.frame(SITE, DATE, STANDTEMP, InvGPP, InvER)
  
  # Build the output matrix
  row1_lm_GPP <- lm(InvGPP~STANDTEMP, data = input[1:2,])
  row1_lm_ER <- lm(InvER~STANDTEMP, data = input[1:2,])
  row1_stats <- c(summary(row1_lm_GPP)$coefficients["STANDTEMP", "Estimate"],
                  summary(row1_lm_GPP)$coefficients["(Intercept)", "Estimate"],
                  summary(row1_lm_GPP)$fstatistic["value"],
                  summary(row1_lm_GPP)$fstatistic["numdf"],
                  anova(row1_lm_GPP)$'Pr(>F)'[1],
                  summary(row1_lm_ER)$coefficients["STANDTEMP", "Estimate"],
                  summary(row1_lm_ER)$coefficients["(Intercept)", "Estimate"],
                  summary(row1_lm_ER)$fstatistic["value"],
                  summary(row1_lm_ER)$fstatistic["numdf"],
                  anova(row1_lm_ER)$'Pr(>F)'[1]
  )
  row2_stats <- row1_stats
  row3_lm_GPP(InvGPP~STANDTEMP, data = input[3:4,])
  row3_stats <- c(summary(row1_lm_GPP)$coefficients["STANDTEMP", "Estimate"],
                  summary(row1_lm_GPP)$coefficients["(Intercept)", "Estimate"],
                  summary(row1_lm_GPP)$fstatistic["value"],
                  summary(row1_lm_GPP)$fstatistic["numdf"],
                  anova(row1_lm_GPP)$'Pr(>F)'[1],
                  NA, NA, NA, NA, NA)
  row4_lm_GPP <- lm(InvGPP~STANDTEMP, data = input[3:4,])
  row4_lm_ER <- lm(InvER~STANDTEMP, data = input[4:5,])
  row4_stats <- c(summary(row4_lm_GPP)$coefficients["STANDTEMP", "Estimate"],
                  summary(row4_lm_GPP)$coefficients["(Intercept)", "Estimate"],
                  summary(row4_lm_GPP)$fstatistic["value"],
                  summary(row4_lm_GPP)$fstatistic["numdf"],
                  anova(row4_lm_GPP)$'Pr(>F)'[1],
                  summary(row4_lm_ER)$coefficients["STANDTEMP", "Estimate"],
                  summary(row4_lm_ER)$coefficients["(Intercept)", "Estimate"],
                  summary(row4_lm_ER)$fstatistic["value"],
                  summary(row4_lm_ER)$fstatistic["numdf"],
                  anova(row4_lm_ER)$'Pr(>F)'[1]
  )
  row5_lm_ER <- lm(InvER~STANDTEMP, data = input[4:5,])
  row5_stats <- c(NA, NA, NA, NA, NA,
                  summary(row5_lm_ER)$coefficients["STANDTEMP", "Estimate"],
                  summary(row5_lm_ER)$coefficients["(Intercept)", "Estimate"],
                  summary(row5_lm_ER)$fstatistic["value"],
                  summary(row5_lm_ER)$fstatistic["numdf"],
                  anova(row5_lm_ER)$'Pr(>F)'[1])
  
  output <- do.call(rbind.data.frame, c(row1_stats, row2_stats, row3_stats, row4_stats, row5_stats))
  
  # Test
  expect_equal(calculate_stats(input, 5), output)
  
})

test_that('Site IDs are different', {
  
  # Build the input matrix
  SITE <- c('id1', 'id2', 'id1', 'id2', 'id1')
  DATE <- c('1/7/22', '1/8/22', '1/15/22', '1/17/22', '1/20/22')
  STANDTEMP <- c(-0.34, -0.70, -0.24, -0.42, 0.48)
  InvGPP <- c(0.61, 0.48, 0.69, 0.57, 0.78)
  InvER <- c(1.27, 0.67, 0.45, 0.37, 0.24)
  input <- data.frame(SITE, DATE, STANDTEMP, InvGPP, InvER)
  
  # Build the output matrix
  row1_stats <- rep(NA, 10)
  row2_stats <- rep(NA, 10)
  
  row3_lm_GPP <- lm(InvGPP~STANDTEMP, data = input[c(3,5),])
  row3_lm_ER <- lm(InvER~STANDTEMP, data = input[c(3,5),])
  row3_stats <- c(summary(row3_lm_GPP)$coefficients["STANDTEMP", "Estimate"],
                  summary(row3_lm_GPP)$coefficients["(Intercept)", "Estimate"],
                  summary(row3_lm_GPP)$fstatistic["value"],
                  summary(row3_lm_GPP)$fstatistic["numdf"],
                  anova(row3_lm_GPP)$'Pr(>F)'[1],
                  summary(row3_lm_ER)$coefficients["STANDTEMP", "Estimate"],
                  summary(row3_lm_ER)$coefficients["(Intercept)", "Estimate"],
                  summary(row3_lm_ER)$fstatistic["value"],
                  summary(row3_lm_ER)$fstatistic["numdf"],
                  anova(row3_lm_ER)$'Pr(>F)'[1]
  )
  
  row4_stats <- rep(NA, 10)
  
  row5_lm_GPP <- lm(InvGPP~STANDTEMP, data = input[c(3,5),])
  row5_lm_ER <- lm(InvER~STANDTEMP, data = input[c(3,5),])
  row5_stats <- c(summary(row5_lm_GPP)$coefficients["STANDTEMP", "Estimate"],
                  summary(row5_lm_GPP)$coefficients["(Intercept)", "Estimate"],
                  summary(row5_lm_GPP)$fstatistic["value"],
                  summary(row5_lm_GPP)$fstatistic["numdf"],
                  anova(row5_lm_GPP)$'Pr(>F)'[1],
                  summary(row5_lm_ER)$coefficients["STANDTEMP", "Estimate"],
                  summary(row5_lm_ER)$coefficients["(Intercept)", "Estimate"],
                  summary(row5_lm_ER)$fstatistic["value"],
                  summary(row5_lm_ER)$fstatistic["numdf"],
                  anova(row5_lm_ER)$'Pr(>F)'[1]
  )
  
  output <- do.call(rbind.data.frame, c(row1_stats, row2_stats, row3_stats, row4_stats, row5_stats))
  
  # Test
  expect_equal(calculate_stats(input, 5), output)
})