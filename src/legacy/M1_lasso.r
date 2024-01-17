# Lasso model with full list of variables


library(tidymodels)
library(dplyr)

path <- setPath("p1402-mauricim")

bs_id <- 1

imp <- readRDS(paste0(path, "data/imputed_bs_sim", bs_id, ".rds"))

all_imp <- completeData(imp)

df <- all_imp[[1]]

model <- lm(hn4_dv_c30_ghs ~ hn1_na8_cb_sex + hn1_dv_age_cons + hn3_dv_c30_ghs, data =df )

cor(df$hn1_dv_age_cons, df$hn4_dv_c30_ghs)

summary(model)

orig_imp <- readRDS(paste0(path, "data/imputed_bs_sim0.rds"))
orig_all_imp <- completeData(orig_imp)
orig <- orig_all_imp$Dataset_1

orig_miss <- read.csv(paste0(path, "data/BD4QoL_150523_encoded.csv"), stringsAsFactors = TRUE)
orig_miss <- orig_miss %>% filter(!is.na(hn4_dv_c30_ghs))
orig_miss <- orig_miss %>% mutate(miss_y = ifelse(is.na(hn4_dv_c30_ghs), 1, 0))
orig_miss <- orig_miss %>% filter(hn4_dv_status == "1 - Alive")

orig$miss_y <- orig_miss$miss_y

val <- data.frame(estimate = predict(model, newdata = orig), truth = orig$hn4_dv_c30_ghs)

rsq(val, truth, estimate)
rmse(val, truth, estimate)
orig_imp_y <- orig %>% filter(miss_y == 1)
orig_meas_y <- orig %>% filter(miss_y == 0)

val_imp_y <- data.frame(estimate = predict(model, newdata = orig_imp_y), truth = orig_imp_y$hn4_dv_c30_ghs)
rsq(val_imp_y, truth, estimate)
rmse(val_imp_y, truth, estimate)

val_meas_y <- data.frame(estimate = predict(model, newdata = orig_meas_y), truth = orig_meas_y$hn4_dv_c30_ghs)
rsq(val_meas_y, truth, estimate)
rmse(val_meas_y, truth, estimate)

bs1_only_miss_y <- df %>% filter(Studyid_hn057 %in% orig_imp_y$Studyid_hn057)

# model dev. only missing
model_miss_y <- lm(hn4_dv_c30_ghs ~ hn1_na8_cb_sex + hn1_dv_age_cons + hn3_dv_c30_ghs, data = bs1_only_miss_y )

val_imp_imp_y <- data.frame(estimate = predict(model_miss_y, newdata = orig_imp_y), truth = orig_imp_y$hn4_dv_c30_ghs)
rsq(val_imp_imp_y, truth, estimate)
rmse(val_imp_imp_y, truth, estimate)

plot(val_imp_y$truth, val_imp_y$estimate)

summary(val_imp_y$truth)
summary(val_meas_y$truth)

par(mfrow=c(1,2))
hist(val_meas_y$truth)
hist(val_imp_y$truth)


bs1_only_meas_y <-  df #%>% filter(!(Studyid_hn057 %in% orig_imp_y$Studyid_hn057)) 
              
# model dev. only meas. y
model_meas_y <- lm(hn4_dv_c30_ghs ~ hn1_na8_cb_sex + hn1_dv_age_cons + hn3_dv_c30_ghs, data = bs1_only_meas_y )

threshold <- 100
val_y <- data.frame(estimate = predict(model_meas_y, newdata = orig_meas_y %>% filter(hn4_dv_c30_ghs<=threshold)), truth = orig_meas_y %>% filter(hn4_dv_c30_ghs<=threshold)  %>% pull(hn4_dv_c30_ghs))
rsq(val_y, truth, estimate)
rmse(val_y, truth, estimate)




#bs1 <- bootstrap_samples %>% filter(bs_id==1)

#OOB_ids <- df_imp %>% filter(!(Studyid_hn057 %in% bs1$Studyid_hn057)) %>% pull(Studyid_hn057)

#df_imp %>% filter(Studyid_hn057 %in% bootstr)

