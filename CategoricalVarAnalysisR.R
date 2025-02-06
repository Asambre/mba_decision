library(tidyverse)

colnames(mba_decision_dataset)

#GENDER

#chi-square test

contigency <- table(mba_decision_dataset$gender, 
                    mba_decision_dataset$decision_to_pursue)
result <- chisq.test(contigency)
print(result)

#ViZ

ggplot(data = mba_decision_dataset)+
  geom_bar(mapping = aes(x = gender, fill = decision_to_pursue))+
  labs(title = 'Impact of Gender on Desision to Pursue MBA',
       x = 'Gender',
       y= 'Number of Participants',
       fill = "Decided to Pursue MBA?")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw()

#UNDERGRADUATE MAJOR

#chi-square-test

contigency <- table(mba_decision_dataset$undergrad_major, 
                    mba_decision_dataset$decision_to_pursue)
result <- chisq.test(contigency)
print(result)

#viz

ggplot(data = mba_decision_dataset)+
  geom_bar(width = 0.6, mapping = aes(x = undergrad_major, fill = decision_to_pursue))+
  labs(title = 'Impact of Undergraduate Major on Desision to Pursue MBA',
       x = 'Undergraduate major',
       y= 'Number of Participants',
       fill = "Decided to Pursue MBA?")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw()+
  scale_x_discrete(expand = expansion(mult = 0.1))

#CURRENT JOB TITLE

#chi-square test

contigency <- table(mba_decision_dataset$current_jobtitle, 
                    mba_decision_dataset$decision_to_pursue)
result <- chisq.test(contigency)
print(result)

#viz

ggplot(data = mba_decision_dataset)+
  geom_bar(width = 0.6, mapping = aes(x = current_jobtitle, fill = decision_to_pursue))+
  labs(title = 'Impact of Current Job Title on Desision to Pursue MBA',
       x = 'Current Job Title',
       y= 'Number of Participants',
       fill = "Decided to Pursue MBA?")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw()+
  scale_x_discrete(expand = expansion(mult = 0.1))

#MANAGEMENT EXPERIENCE

#chi-square test

contigency <- table(mba_decision_dataset$has_management_experience, 
                    mba_decision_dataset$decision_to_pursue)
result <- chisq.test(contigency)
print(result)

#viz

ggplot(data = mba_decision_dataset)+
  geom_bar(width = 0.6, mapping = aes(x = has_management_experience, fill = decision_to_pursue))+
  labs(title = 'Impact of Having Management Experience on Desision to Pursue MBA',
       x = 'Has Management Experience?',
       y= 'Number of Participants',
       fill = "Decided to Pursue MBA?")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw()

#MBA FUNDING SOURCE

#chi-square test

contigency <- table(mba_decision_dataset$mba_funding_source, 
                    mba_decision_dataset$decision_to_pursue)
result <- chisq.test(contigency)
print(result)

#viz

ggplot(data = mba_decision_dataset)+
  geom_bar(width = 0.6, mapping = aes(x = mba_funding_source, fill = decision_to_pursue))+
  labs(title = 'Impact of the MBA Funding Source on Desision to Pursue MBA',
       x = 'MBA Funding Source',
       y= 'Number of Participants',
       fill = "Decided to Pursue MBA?")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw()

#DESIRED POST-MBA ROLE

#chi-square test

contigency <- table(mba_decision_dataset$desired_post_mba_source, 
                    mba_decision_dataset$decision_to_pursue)
result <- chisq.test(contigency)
print(result)

#viz

ggplot(data = mba_decision_dataset)+
  geom_bar(width = 0.6, mapping = aes(x = desired_post_mba_source, fill = decision_to_pursue))+
  labs(title = 'Impact of the Desired Post-MBA Role on Desision to Pursue MBA',
       x = 'Desired Post-MBA Role',
       y= 'Number of Participants',
       fill = "Decided to Pursue MBA?")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#LOCATION PREFERENCE POSTMBA

#chi-square test

contigency <- table(mba_decision_dataset$post_mba_location_preference, 
                    mba_decision_dataset$decision_to_pursue)
result <- chisq.test(contigency)
print(result)

#viz

ggplot(data = mba_decision_dataset)+
  geom_bar(width = 0.6, mapping = aes(x = post_mba_location_preference, fill = decision_to_pursue))+
  labs(title = 'Impact of the Desired Post-MBA Location',
       x = 'Desired Post-MBA Location',
       y= 'Number of Participants',
       fill = "Decided to Pursue MBA?")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw()

#REASON FOR MBA

contigency <- table(mba_decision_dataset$reason_for_mba, 
                    mba_decision_dataset$decision_to_pursue)
result <- chisq.test(contigency)
print(result)

#viz

ggplot(data = mba_decision_dataset)+
  geom_bar(width = 0.6, mapping = aes(x = reason_for_mba, fill = decision_to_pursue))+
  labs(title = 'Impact of the Reason for MBA on the Decision to Pursue an MBA',
       x = 'Reason for MBA',
       y= 'Number of Participants',
       fill = "Decided to Pursue MBA?")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#ONLINE VERSUS ONCAMPUS PREF

#chi-square test

contigency <- table(mba_decision_dataset$online_vs_oncampus_mba_preference, 
                    mba_decision_dataset$decision_to_pursue)
result <- chisq.test(contigency)
print(result)

#viz

ggplot(data = mba_decision_dataset)+
  geom_bar(width = 0.6, mapping = aes(x = online_vs_oncampus_mba_preference, fill = decision_to_pursue))+
  labs(title = 'Impact of Learning Preference (Online or ONcampus)',
       x = 'Learning Preference',
       y= 'Number of Participants',
       fill = "Decided to Pursue MBA?")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw()