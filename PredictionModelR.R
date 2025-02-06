library(tidyverse)
library(pscl)

#converting boolean to 1s and 0s

mba_decision_dataset$decision_to_pursue <- ifelse(
  mba_decision_dataset$decision_to_pursue == "Yes",1,0
)

#MODEL FOR PREDICTING DECISION TO PURSUE

#Logistic Regression

logit_model <- glm(decision_to_pursue ~ age + gender + undergrad_major +
                     undergrad_gpa + years_work_experience + current_jobtitle +
                     annual_pre_mba_salary + has_management_experience + gre_gmat_score +
                     entrepreneurial_interest + networking_importance + mba_funding_source +
                     desired_post_mba_source + expected_post_mba_salary +
                     post_mba_location_preference + reason_for_mba + 
                     online_vs_oncampus_mba_preference,
                   data = mba_decision_dataset,
                   family = binomial)
summary(logit_model)

#odds ratio
exp(coef(logit_model))

#model prediction
mba_decision_dataset$predicted_prob <- predict(logit_model, type = 'response')

View(mba_decision_dataset)

#model evaluation

pR2(logit_model)
