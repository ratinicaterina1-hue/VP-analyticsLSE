# 12_train_models.R
# Purpose: train predictive models (OLS, LASSO, Random Forest, XGBoost)
# on the features dataset produced by 11_build_features.R.
# Produces: saved model objects and a CSV with model performance metrics.
# NOTE: Robust to dplyr masking, zero-variance predictors and Date handling.

# -------------------- Setup --------------------
library(tidyverse)
library(lubridate)

# modeling packages (install if missing)
req_pkgs <- c("caret", "glmnet", "ranger", "xgboost", "recipes", "rsample", "yardstick")
for (p in req_pkgs) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)

library(caret)
library(glmnet)
library(ranger)
library(xgboost)
library(recipes)
library(rsample)
library(yardstick)

# paths
if (exists("paths")) {
  features_path <- file.path(paths$outputs, "features", "events_features.csv")
  models_outdir <- file.path(paths$outputs, "models")
} else {
  features_path <- "data/features/events_features.csv"
  models_outdir <- "outputs/models"
}

dir.create(models_outdir, recursive = TRUE, showWarnings = FALSE)

message("Loading features from: ", features_path)
features <- readr::read_csv(features_path, show_col_types = FALSE)

# -------------------- Data prep --------------------
# We'll predict 1-day market-adjusted return (label_ret1)
# Filter rows with non-missing label_ret1 and reasonable feature completeness
model_df <- features %>%
  dplyr::select(Ticker, Earnings_Date, pre1, pre3, pre5, vol10, vol_ratio, EPS_Surprise, Surprise_Pct, VIX, CPI, FedFunds, label_ret1, label_ret5) %>%
  mutate(Earnings_Date = as.Date(Earnings_Date)) %>%
  filter(!is.na(label_ret1))

# quick NA imputation for features (median) - operate columnwise safely
impute_median <- function(x) ifelse(is.na(x), median(x, na.rm = TRUE), x)
model_df <- model_df %>%
  mutate(across(c(pre1, pre3, pre5, vol10, vol_ratio, EPS_Surprise, Surprise_Pct, VIX, CPI, FedFunds), ~ impute_median(.)))

# Feature log transforms where useful
model_df <- model_df %>%
  mutate(vol_ratio = ifelse(is.na(vol_ratio), 0, vol_ratio),
         log_vol_ratio = log1p(vol_ratio))

# remove infinite / NA
model_df <- model_df %>% mutate(across(where(is.numeric), ~ ifelse(is.infinite(.), NA_real_, .)))
model_df <- model_df %>% drop_na()

# -------------------- Time-based split --------------------
# Train on events before a cutoff, test after. Use 80/20 time split by Earnings_Date
n <- nrow(model_df)
cut_ix <- max(1, floor(0.8 * n))   # ensure at least 1
cutoff_date <- model_df %>%
  dplyr::arrange(Earnings_Date) %>%
  dplyr::slice(cut_ix) %>%
  dplyr::pull(Earnings_Date) %>%
  dplyr::first()

train_df <- model_df %>% filter(Earnings_Date <= cutoff_date)
test_df  <- model_df %>% filter(Earnings_Date >  cutoff_date)

message("Cutoff date used for time split: ", cutoff_date)
message("Training events: ", nrow(train_df), " | Test events: ", nrow(test_df))

# -------------------- Recipe --------------------
# Build a recipe for preprocessing: remove zero-variance, then center/scale
rec <- recipe(label_ret1 ~ pre1 + pre3 + pre5 + vol10 + log_vol_ratio +
                EPS_Surprise + Surprise_Pct + VIX + CPI + FedFunds,
              data = train_df) %>%
  step_zv(all_predictors()) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors())

prep_rec <- prep(rec, training = train_df, verbose = TRUE)
train_x <- bake(prep_rec, new_data = train_df)
test_x  <- bake(prep_rec, new_data = test_df)

message("Columns after baking: ", paste(colnames(train_x), collapse = ", "))

# -------------------- Extract matrices for xgboost --------------------
# The baked data contains predictors and label_ret1. Remove only the label.
if (!"label_ret1" %in% colnames(train_x)) stop("label_ret1 not found in train_x after bake()")
train_mat <- as.matrix(train_x %>% dplyr::select(-label_ret1))
train_y   <- train_x$label_ret1

test_mat <- as.matrix(test_x %>% dplyr::select(-label_ret1))
test_y   <- test_x$label_ret1

# -------------------- 1) Baseline OLS (fixed dynamic formula) --------------------

# Build formula dynamically from baked predictor names
predictor_cols <- setdiff(colnames(train_x), "label_ret1")
ols_formula <- as.formula(
  paste("label_ret1 ~", paste(predictor_cols, collapse = " + "))
)

ols_mod <- lm(ols_formula, data = train_x)
ols_pred <- predict(ols_mod, newdata = test_x)

ols_metrics <- tibble(
  model = "OLS",
  rmse = rmse_vec(test_y, ols_pred),
  mae  = mae_vec(test_y, ols_pred),
  rsq  = ifelse(is.na(cor(test_y, ols_pred)), NA_real_, rsq_vec(test_y, ols_pred))
)

saveRDS(ols_mod, file.path(models_outdir, "model_OLS.rds"))


# -------------------- 2) LASSO (glmnet) (robust to missing metadata) --------------------

# Safely remove metadata columns if present (won't error if they are not)
x_train <- model.matrix(label_ret1 ~ . - 1,
                        data = train_x %>% dplyr::select(-dplyr::any_of(c("Ticker", "Earnings_Date"))))
y_train <- train_x$label_ret1

x_test  <- model.matrix(label_ret1 ~ . - 1,
                        data = test_x  %>% dplyr::select(-dplyr::any_of(c("Ticker", "Earnings_Date"))))
y_test  <- test_x$label_ret1

# Align columns: keep only the intersection (common predictors), in the same order
common_cols <- intersect(colnames(x_train), colnames(x_test))
if (length(common_cols) == 0) stop("No common predictors between train and test after preprocessing!")

x_train_aligned <- x_train[, common_cols, drop = FALSE]
x_test_aligned  <- x_test[,  common_cols, drop = FALSE]

# Fit LASSO (cv) on aligned matrices
cv_lasso  <- glmnet::cv.glmnet(x_train_aligned, y_train, alpha = 1)
lasso_mod <- glmnet::glmnet(x_train_aligned, y_train, alpha = 1, lambda = cv_lasso$lambda.min)

# Predict (coerce to numeric)
lasso_pred <- as.numeric(predict(lasso_mod, newx = x_test_aligned, s = cv_lasso$lambda.min))

# Metrics
lasso_metrics <- tibble(
  model = "LASSO",
  rmse  = rmse_vec(test_y, lasso_pred),
  mae   = mae_vec(test_y, lasso_pred),
  rsq   = ifelse(is.na(cor(test_y, lasso_pred)), NA_real_, rsq_vec(test_y, lasso_pred))
)
saveRDS(lasso_mod, file.path(models_outdir, "model_LASSO.rds"))


# -------------------- 3) Random Forest (ranger via caret) --------------------
set.seed(42)

rf_ctrl <- trainControl(method = "cv", number = 5)

rf_train <- train(
  label_ret1 ~ .,
  data = train_x %>% dplyr::select(-dplyr::any_of(c("Ticker", "Earnings_Date"))),
  method = "ranger",
  trControl = rf_ctrl,
  importance = "impurity"
)

rf_pred <- predict(rf_train, newdata = test_x)

rf_metrics <- tibble(
  model = "RANGER_RF",
  rmse  = rmse_vec(test_y, rf_pred),
  mae   = mae_vec(test_y, rf_pred),
  rsq   = ifelse(is.na(cor(test_y, rf_pred)),
                 NA_real_,
                 rsq_vec(test_y, rf_pred))
)

saveRDS(rf_train, file.path(models_outdir, "model_RANGER.rds"))


# -------------------- 4) XGBoost --------------------
# Align test/train matrices for xgboost
train_mat_xgb <- as.matrix(train_mat)
test_mat_xgb  <- as.matrix(test_mat)

# ensure column count matches
if (ncol(train_mat_xgb) != ncol(test_mat_xgb)) {
  common <- intersect(colnames(train_mat_xgb), colnames(test_mat_xgb))
  train_mat_xgb <- train_mat_xgb[, common, drop = FALSE]
  test_mat_xgb  <- test_mat_xgb[, common, drop = FALSE]
}

dtrain <- xgboost::xgb.DMatrix(data = train_mat_xgb, label = train_y)
dtest  <- xgboost::xgb.DMatrix(data = test_mat_xgb, label = test_y)
params <- list(objective = "reg:squarederror", eval_metric = "rmse")
set.seed(42)
xgb_mod <- xgboost::xgb.train(params = params, data = dtrain, nrounds = 100, verbose = 0)
xgb_pred <- predict(xgb_mod, dtest)

xgb_metrics <- tibble(
  model = "XGBoost",
  rmse = rmse_vec(test_y, xgb_pred),
  mae  = mae_vec(test_y, xgb_pred),
  rsq  = ifelse(is.na(cor(test_y, xgb_pred)), NA_real_, rsq_vec(test_y, xgb_pred))
)
saveRDS(xgb_mod, file.path(models_outdir, "model_XGBoost.rds"))

# -------------------- Assemble metrics --------------------
metrics_all <- bind_rows(ols_metrics, lasso_metrics, rf_metrics, xgb_metrics) %>%
  mutate(created = Sys.time())

readr::write_csv(metrics_all, file.path(models_outdir, "model_metrics.csv"))
message("✅ Models trained and saved to: ", models_outdir)
message("Metrics:")
print(metrics_all)

# Also save the recipe for later preprocessing
saveRDS(prep_rec, file.path(models_outdir, "preprocessing_recipe.rds"))

# End of 12_train_models.R
