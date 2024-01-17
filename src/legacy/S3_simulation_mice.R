library(dplyr)
library(mice)
library(data.table)

m_imputations <- 5

set.seed(42)

# provide dataset id
args <- commandArgs(trailingOnly=TRUE)
data_file <- args[[1]]
df <- read.csv(paste0("../sim_data/",data_file,".csv"))

results <- data.frame(matrix(vector(), 0, 8, dimnames=list(c(), c('est','lo95','hi95','fmi','id','missing_rate','mech','data_id'))),
                stringsAsFactors=F)


pool_r_squared <- function(r_squared, sample_size) {

  # Set up array r2 to store R2 values, Fisher z-transformations of R2
  # values and its variance.
  m <- length(r_squared)
  r2 <- matrix(NA,
               nrow = m, ncol = 3, dimnames = list(seq_len(m),
               c("R^2", "Fisher trans F^2", "se()")))

  # Fill arrays
  for (i in seq_len(m)) {
    r2[i, 1] <- sqrt(r_squared[i])
    r2[i, 2] <- 0.5 * log((r2[i, 1] + 1) / (1 - r2[i, 1]))
    r2[i, 3] <- 1 / (sample_size - 3) # replaced nobs by sample_size
  }

  # Compute within, between and total variances following Rubin's rules
  # with function pool.scalar().
  fit <- mice::pool.scalar(r2[, 2], r2[, 3])

  # Make table with results.
  qbar <- fit$qbar
  table <- array(((exp(2 * qbar) - 1) / (1 + exp(2 * qbar)))^2,
    dim = c(1, 4)
  )

  dimnames(table) <- list("R^2", c("est", "lo95", "hi95", "fmi"))


  table[, 2] <- ((exp(2 * (qbar - 1.96 * sqrt(fit$t))) - 1) /
                 (1 + exp(2 * (qbar - 1.96 * sqrt(fit$t)))))^2
  table[, 3] <- ((exp(2 * (qbar + 1.96 * sqrt(fit$t))) - 1) /
                (1 + exp(2 * (qbar + 1.96 * sqrt(fit$t)))))^2
  table[, 4] <- fit$f

  return(table)
}

for (data_id in 1:max(df$id)) {
       print(data_id)
       
       df_m <- df %>% filter(id == data_id) %>% select(y, x1, x2)

       imp_mice <- mice(df_m, 
                        m= m_imputations, 
                        maxiter=20, 
                        seed=42, 
                        verbose=FALSE)
              
       r_mice <- c(1:m_imputations)

       for (i in 1:m_imputations) {

              df_mice <- mice::complete(imp_mice, i)
              lm_s_mice <- lm(y ~ x1 + x2, data=df_mice) %>% summary
              r_mice[i] <- lm_s_mice$r.squared
       
       }

       temp <- pool_r_squared(r_mice, sample_size= nrow(df_m)) %>% as.data.frame

       temp$id <- c(1)
       temp$missing_rate <- c(df$prop[1])
       temp$mech <- c(df$mech[1])
       temp$data_id <- c(data_file)

       results <- rbind(results, temp)
}

write.csv(results, paste0("r2_mice_", data_file,".csv"),  row.names=FALSE)