library(tidyverse)
library(olsrr)

#Load
train <- read_csv("train.csv")
test  <- read_csv("test.csv")

#Adds X infront of Variables that start with number (olsrr compatibility)
train <- train %>% 
  rename_with(~ make.names(.x, unique = TRUE)) %>%
  mutate(across(where(is.character), as.factor))
test  <- test  %>% 
  rename_with(~ make.names(.x, unique = TRUE)) %>% 
  mutate(across(where(is.character), as.factor))

#Analysis 1
homes <- train %>% 
  filter(Neighborhood %in% c("NAmes", "Edwards", "BrkSide")) %>% 
  mutate(GrLivArea100 = GrLivArea / 100)

model_neigh <- lm(SalePrice ~ GrLivArea100 * Neighborhood, data = homes)

summary(model_neigh)
confint(model_neigh)

#Diagnostics
par(mfrow = c(2, 2))
plot(model_neigh)

#Price for 1500 sq ft in each neighborhood
predict(model_neigh, newdata = tibble(GrLivArea100 = 15, Neighborhood = c("NAmes", "Edwards", "BrkSide")), interval = "confidence")


#Analysis 2
#Make factors
train <- train %>% mutate(across(where(is.character), as.factor))
test  <- test  %>% mutate(across(where(is.character), as.factor))

#Simple model
mod_simple <- lm(SalePrice ~ OverallQual, data = train)
summary(mod_simple)

#Diagnostics
par(mfrow = c(2, 2))                    
plot(mod_simple) 

#Fixed multiple model
mod_fixed  <- lm(SalePrice ~ GrLivArea + FullBath, data = train)
summary(mod_fixed)

#Diagnostics
par(mfrow = c(2, 2))                    
plot(mod_fixed) 

#Add variables for selection
num_vars <- train %>% select(where(is.numeric), -Id, -SalePrice) %>% names()
cat_vars <- c("OverallQual", "Neighborhood", "ExterQual", "SaleCondition", "Foundation")
cat_vars <- cat_vars[cat_vars %in% names(train)]
cat_vars <- train %>% select(all_of(cat_vars)) %>% keep(~ nlevels(.x) > 1) %>% names()

sel_vars <- c(num_vars, cat_vars)

comp <- train %>% select(SalePrice, all_of(sel_vars)) %>% drop_na()

full_fit <- lm(SalePrice ~ . , data = comp)

#Variable selection
fwd       <- ols_step_forward_p (full_fit, penter  = 0.05, details = FALSE)
bwd       <- ols_step_backward_p(full_fit, premove = 0.05, details = FALSE)
stepwise  <- ols_step_both_p    (full_fit, penter = 0.05, premove = 0.05, details = FALSE)

summary(fwd$model)
summary(bwd$model)
summary(stepwise$model)

#Diagnostics
par(mfrow = c(2, 2))                    
plot(stepwise$model) 

print(ols_press(mod_simple))
print(ols_press(mod_fixed))

press_stat <- function(m){e <- resid(m); h <- hatvalues(m)
  ok <- h < 1 - .Machine$double.eps
  sum((e[ok]/(1 - h[ok]))^2)}

press_stepwise <- press_stat(stepwise$model)

#Predictions
simple_sub <- data.frame(Id = test$Id, SalePrice = predict(mod_simple, newdata = test))
write_csv(simple_sub, "simple_model.csv")

fixed_sub <- data.frame(Id = test$Id, SalePrice = predict(mod_fixed, newdata = test))
write_csv(fixed_sub, "fixed_model.csv")

stepwise_sub<- data.frame(Id = test$Id, SalePrice = predict(stepwise$model, newdata = test))
write_csv(stepwise_sub, "stepwise_model.csv")


