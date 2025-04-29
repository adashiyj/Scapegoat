## Packages
install.packages("krippendorffsalpha")


library(tidyverse)
library(krippendorffsalpha)


## Which posts have the same number of attributions? 
set.seed(123)

headers <- read.csv("reliability2_step2.csv", nrows=1, header= F) # Create column names for data from reliability step 2

dataset <-read.csv("reliability2_step2.csv", skip=3, header = F) # load data from reliability step 2

colnames(dataset) <- headers

dataset<- dataset |> 
  select(Coder_ID:FutureTense_1)

dataset <- dataset |> 
  filter(Post_ID != 3426)

dataset |> 
  count(Post_ID, Coder_ID) |> 
  ungroup() |> 
  count(Post_ID, n) |> 
  count(Post_ID) |> 
  filter(n == 1)
  

dataset |> 
  count(Post_ID, Coder_ID) |> 
  ungroup() |> 
  count(Post_ID, n) |> 
  count(Post_ID) |> 
  filter(n > 1)

view(dataset)

## Filter out any attributions which could not be matched between coders

dataset |> 
  count()

dataset <- dataset |> 
  filter(!Post_ID %in% c(1,5,11,14,18,23,28)) |> 
  filter(!(Post_ID %in% c(8,9) & Coder_ID %in% c("Francesco", "Mare"))) |> 
  filter(!(Post_ID %in% c(11,12,22,24) & Coder_ID == "Mare")) |> 
  filter(!(Post_ID == 25 & Coder_ID %in% c("Ada", "Mare"))) |> 
  filter(!(Post_ID == 21 & Coder_ID == "Francesco"))

dataset |> 
  count()

## Create a match key for row number and Attribution


match_set <- dataset |> 
  select(Coder_ID, Attribution_Number, Post_ID, SenderAccountMatch_1) |> 
  group_by(Post_ID, Attribution_Number) |> 
  arrange(Post_ID) |>
  ungroup() |> 
  pivot_wider(
    names_from = Coder_ID, 
    values_from = SenderAccountMatch_1
  ) |> 
  mutate(key = row_number()) |> 
  select(key,Post_ID, Attribution_Number)

view(match_set)

## Turning all variables into numerical variables for later matrix transformation
dataset <- dataset |> 
  mutate(Attribution_Presence = as.factor(Attribution_Presence)) |> 
  mutate(Attribution_Presence = as.numeric(fct_recode(Attribution_Presence, 
                                           "1" = "Yes", 
                                           "2" = "No"))) |> 
  mutate(Attribution_Number = as.factor(Attribution_Number)) |> 
  mutate(Attribution_Number = as.numeric(fct_recode(Attribution_Number, 
                                           "1" = "First", 
                                           "2" = "Second",
                                         "3" = "Third"))) |> 
  mutate(AttributionType_1 = as.factor(AttributionType_1)) |> 
  mutate(AttributionType_1 = as.numeric(fct_recode(AttributionType_1, 
                                                    "1" = "Affirmed causal responsibility", 
                                                    "2" = "Negated causal responsibility"))) |> 
  mutate(SenderAccountMatch_1 = as.factor(SenderAccountMatch_1)) |> 
  mutate(SenderAccountMatch_1 = as.numeric(fct_recode(SenderAccountMatch_1, 
                                                      "1" = "Yes", 
                                                      "2" = "No"))) |> 
  mutate(Evaluation_1 = as.factor(Evaluation_1)) |> 
  mutate(Evaluation_1 = as.numeric(fct_recode(Evaluation_1, 
                                                    "1" = "Positively", 
                                                    "2" = "Negatively",
                                                    "3" = "Mixed", 
                                                    "4" = "Neutrally"))) |> 
  mutate(PresenceIndividual_1 = if_else(PresenceIndividual_1 == "", NA, PresenceIndividual_1)) |> 
  mutate(PresenceIndividual_1 = as.factor(PresenceIndividual_1)) |>
  mutate(PresenceIndividual_1 = as.numeric(fct_recode(PresenceIndividual_1,
                                              "1" = "Yes",
                                              "2" = "No"))) |> 
  mutate(SpecificIndividual_1 = if_else(SpecificIndividual_1 == "", NA, SpecificIndividual_1)) |> 
  mutate(SpecificIndividual_1 = as.factor(SpecificIndividual_1)) |>
  mutate(SpecificIndividual_1 = as.numeric(fct_recode(SpecificIndividual_1,
                                                      "1" = "Donald Trump",
                                                      "2" = "Media Individuals (e.g. Jordan Peterson)", 
                                                      "3" = "Other Individual Democratic Politicians", 
                                                      "4" = "Other Individual Republican Politicians", 
                                                      "5" = "Other Individuals"))) |> 
  mutate(PresenceCollective_1 = if_else(PresenceCollective_1 == "", NA, PresenceCollective_1)) |> 
  mutate(PresenceCollective_1 = as.factor(PresenceCollective_1)) |>
  mutate(PresenceCollective_1 = as.numeric(fct_recode(PresenceCollective_1,
                                                      "1" = "Yes",
                                                      "2" = "No"))) |> 
  mutate(SpecificCollective_1 = if_else(SpecificCollective_1 == "", NA, SpecificCollective_1)) |> 
  mutate(SpecificCollective_1 = as.factor(SpecificCollective_1)) |>
  mutate(SpecificCollective_1 = as.numeric(fct_recode(SpecificCollective_1,
                                                      "1" = "China",
                                                      "2" = "Conservative: Republicans/Right-wing ideologists", 
                                                      "3" = "EU", 
                                                      "4" = "Media", 
                                                      "5" = "Other Collectives",
                                                      "6" = "Other Countries/Country Unions", 
                                                      "7" = "Other Non-Governing/Non-Political Organizations (private/public enterprise, non-profits)", 
                                                      "8" = "Other Political/Ideological Groups",
                                                      "9"="Progressive: Democrats/Liberals/Left-wing ideologists",
                                                      "10"="The Trump-Administration", 
                                                      "11"="USA"
                                                      ))) |> 
  mutate(PresenceSystems_1 = if_else(PresenceSystems_1 == "", NA, PresenceSystems_1)) |> 
  mutate(PresenceSystems_1 = as.factor(PresenceSystems_1)) |>
  mutate(PresenceSystems_1 = as.numeric(fct_recode(PresenceSystems_1,
                                                      "1" = "Yes",
                                                      "2" = "No"))) |> 
  mutate(PresenceNetwork_1 = if_else(PresenceNetwork_1 == "", NA, PresenceNetwork_1)) |> 
  mutate(PresenceNetwork_1 = as.factor(PresenceNetwork_1)) |>
  mutate(PresenceNetwork_1 = as.numeric(fct_recode(PresenceNetwork_1,
                                                   "1" = "Yes",
                                                   "2" = "No")))|> 
  mutate(Economy_1 = if_else(Economy_1 == "", NA, Economy_1)) |> 
  mutate(Economy_1 = as.factor(Economy_1)) |>
  mutate(Economy_1 = as.numeric(fct_recode(Economy_1,
                                                   "1" = "Yes",
                                                   "2" = "No")))|> 
  mutate(SocialPolicies_1 = if_else(SocialPolicies_1 == "", NA, SocialPolicies_1)) |> 
  mutate(SocialPolicies_1 = as.factor(SocialPolicies_1)) |>
  mutate(SocialPolicies_1 = as.numeric(fct_recode(SocialPolicies_1,
                                           "1" = "Yes",
                                           "2" = "No"))) |> 
  mutate(PoliticsHumanRight_1 = if_else(PoliticsHumanRight_1 == "", NA, PoliticsHumanRight_1)) |> 
  mutate(PoliticsHumanRight_1 = as.factor(PoliticsHumanRight_1)) |>
  mutate(PoliticsHumanRight_1 = as.numeric(fct_recode(PoliticsHumanRight_1,
                                                  "1" = "Yes",
                                                  "2" = "No")))|> 
  mutate(ExternalRelations_1 = if_else(ExternalRelations_1 == "", NA, ExternalRelations_1)) |> 
  mutate(ExternalRelations_1 = as.factor(ExternalRelations_1)) |>
  mutate(ExternalRelations_1 = as.numeric(fct_recode(ExternalRelations_1,
                                                      "1" = "Yes",
                                                      "2" = "No")))|> 
  mutate(PastTense_1 = if_else(PastTense_1 == "", NA, PastTense_1)) |> 
  mutate(PastTense_1 = as.factor(PastTense_1)) |>
  mutate(PastTense_1 = as.numeric(fct_recode(PastTense_1,
                                                     "1" = "Yes",
                                                     "2" = "No")))|> 
  mutate(PresentTense_1 = if_else(PresentTense_1 == "", NA, PresentTense_1)) |> 
  mutate(PresentTense_1 = as.factor(PresentTense_1)) |>
  mutate(PresentTense_1 = as.numeric(fct_recode(PresentTense_1,
                                             "1" = "Yes",
                                             "2" = "No"))) |> 
  mutate(FutureTense_1 = if_else(FutureTense_1 == "", NA, FutureTense_1)) |> 
  mutate(FutureTense_1 = as.factor(FutureTense_1)) |>
  mutate(FutureTense_1 = as.numeric(fct_recode(FutureTense_1,
                                                "1" = "Yes",
                                                "2" = "No"))) |> 
  mutate(SpecificNetworks_1 = if_else(SpecificNetworks_1 == "", NA, SpecificNetworks_1)) |> 
  mutate(SpecificNetworks_1 = as.factor(SpecificNetworks_1)) |>
  mutate(SpecificNetworks_1 = as.numeric(fct_recode(SpecificNetworks_1,
                                               "1" = "Trade War"))) |> 
  mutate(SpecificSystem_1 = if_else(SpecificSystem_1 == "", NA, SpecificSystem_1)) |> 
  mutate(SpecificSystem_1 = as.factor(SpecificSystem_1)) |>
  mutate(SpecificSystem_1 = as.numeric(fct_recode(SpecificSystem_1,
                                                    "1" = "Laws/Treaties/Agreements/Policies (e.g., NAFTA, NATO, Medicare, Medicaid)
", 
                                                  "2" = "Other Systems, Ideologies, and Belief Systems")))
  


## Create a match key for row number and Attribution
match_set <- dataset |> 
  select(Coder_ID, Attribution_Number, Post_ID, SenderAccountMatch_1) |> 
  group_by(Post_ID, Attribution_Number) |> 
  arrange(Post_ID) |>
  ungroup() |> 
  pivot_wider(
    names_from = Coder_ID, 
    values_from = SenderAccountMatch_1
  ) |> 
  mutate(key = row_number()) |> 
  select(key,Post_ID, Attribution_Number)
view(match_set)

## K-Alpha analysis

### Attribution Type
pivot_type <- dataset |> 
  select(Coder_ID, Attribution_Number, Post_ID, AttributionType_1) |> 
  group_by(Post_ID, Attribution_Number) |> 
  arrange(Post_ID) |>
  ungroup() |> 
  pivot_wider(
    names_from = Coder_ID, 
    values_from = AttributionType_1
  ) |> 
  select(Luke, Francesco, Ada, Mare) |> 
  as.matrix()

alpha_type <- krippendorffs.alpha(pivot_type, level = "nominal", control = list(parallel = FALSE), verbose = TRUE)

summary(alpha_type)




improvement_type <- influence(alpha_type, units = seq(1:38))

bind_cols(match_set, improvement_type) |> 
  filter(dfbeta.units < 0)|> 
  mutate(dfbeta.units = round(dfbeta.units, digits = 3)) |> 
  filter(dfbeta.units < 0) 

improvement_type2 <- influence(alpha_type, coders = c(1:4))


### Sender Account Match
pivot_match <- dataset |> 
  select(Coder_ID, Attribution_Number, Post_ID, SenderAccountMatch_1) |> 
  group_by(Post_ID, Attribution_Number) |> 
  arrange(Post_ID) |>
  ungroup() |> 
  pivot_wider(
    names_from = Coder_ID, 
    values_from = SenderAccountMatch_1
  ) |> 
  select(Luke, Francesco, Ada, Mare) |> 
  as.matrix()

alpha_match <- krippendorffs.alpha(pivot_match, level = "nominal", control = list(parallel = FALSE), verbose = TRUE)

summary(alpha_match)

improvement_match <- influence(alpha_match, units = seq(1:38))

bind_cols(match_set, improvement_match) |> 
  filter(dfbeta.units < 0)|> 
  mutate(dfbeta.units = round(dfbeta.units, digits = 3)) |> 
  filter(dfbeta.units < 0) 

improvement_match2 <- influence(alpha_match, coders = c(1:4))


### evaluation
pivot_evaluation <- dataset |> 
  select(Coder_ID, Attribution_Number, Post_ID, Evaluation_1) |> 
  group_by(Post_ID, Attribution_Number) |> 
  arrange(Post_ID) |>
  ungroup() |> 
  pivot_wider(
    names_from = Coder_ID, 
    values_from = Evaluation_1
  ) |> 
  select(Luke, Francesco, Ada, Mare) |> 
  as.matrix()

alpha_evaluation <- krippendorffs.alpha(pivot_evaluation, level = "nominal", control = list(parallel = FALSE), verbose = TRUE)

summary(alpha_evaluation)

improvement_evaluation <- influence(alpha_evaluation, units = seq(1:38))

bind_cols(match_set, improvement_evaluation) |> 
  filter(dfbeta.units < 0)|> 
  mutate(dfbeta.units = round(dfbeta.units, digits = 3)) |> 
  filter(dfbeta.units < 0) 

improvement_evaluation2 <- influence(alpha_evaluation, coders = c(1:4))


### Presence individual
pivot_presenceindividual <- dataset |> 
  select(Coder_ID, Attribution_Number, Post_ID, PresenceIndividual_1) |> 
  group_by(Post_ID, Attribution_Number) |> 
  arrange(Post_ID) |>
  ungroup() |> 
  pivot_wider(
    names_from = Coder_ID, 
    values_from = PresenceIndividual_1
  ) |> 
  select(Luke, Francesco, Ada, Mare) |> 
  as.matrix()

alpha_presenceindividual <- krippendorffs.alpha(pivot_presenceindividual, level = "nominal", control = list(parallel = FALSE), verbose = TRUE)

summary(alpha_presenceindividual)

improvement_presenceindividual <- influence(alpha_presenceindividual, units = seq(1:38))

bind_cols(match_set, improvement_presenceindividual) |> 
  filter(dfbeta.units < 0)|> 
  mutate(dfbeta.units = round(dfbeta.units, digits = 3)) |> 
  filter(dfbeta.units < 0) 

improvement_presenceindividual2 <- influence(alpha_presenceindividual, coders = c(1:4))


### Specific individual
pivot_specificindividual <- dataset |> 
  select(Coder_ID, Attribution_Number, Post_ID, SpecificIndividual_1) |> 
  group_by(Post_ID, Attribution_Number) |> 
  arrange(Post_ID) |>
  ungroup() |> 
  pivot_wider(
    names_from = Coder_ID, 
    values_from = SpecificIndividual_1
  ) |> 
  select(Luke, Francesco, Ada, Mare) |> 
  as.matrix()

alpha_specificindividual <- krippendorffs.alpha(pivot_specificindividual, level = "nominal", control = list(parallel = FALSE), verbose = TRUE)

summary(alpha_specificindividual)

improvement_specificindividual <- influence(alpha_specificindividual, units = seq(1:38))

bind_cols(match_set, improvement_specificindividual) |> 
  filter(dfbeta.units < 0)|> 
  mutate(dfbeta.units = round(dfbeta.units, digits = 3)) |> 
  filter(dfbeta.units < 0) 

improvement_specificindividual2 <- influence(alpha_specificindividual, coders = c(1:4))

### Presence Collective
pivot_collective <- dataset |> 
  select(Coder_ID, Attribution_Number, Post_ID, PresenceCollective_1) |> 
  group_by(Post_ID, Attribution_Number) |> 
  arrange(Post_ID) |>
  ungroup() |> 
  pivot_wider(
    names_from = Coder_ID, 
    values_from = PresenceCollective_1
  ) |> 
  select(Luke, Francesco, Ada, Mare) |> 
  as.matrix()

alpha_collective <- krippendorffs.alpha(pivot_collective, level = "nominal", control = list(parallel = FALSE), verbose = TRUE)

summary(alpha_collective)

improvement_collective <- influence(alpha_collective, units = seq(1:38))

bind_cols(match_set, improvement_collective) |> 
  filter(dfbeta.units < 0) |> 
  mutate(dfbeta.units = round(dfbeta.units, digits = 3)) |> 
  filter(dfbeta.units < 0) 

improvement_collective2 <- influence(alpha_collective, coders = c(1:4))


### Specific Collective
pivot_specificcollective <- dataset |> 
  select(Coder_ID, Attribution_Number, Post_ID, SpecificCollective_1) |> 
  group_by(Post_ID, Attribution_Number) |> 
  arrange(Post_ID) |>
  ungroup() |> 
  pivot_wider(
    names_from = Coder_ID, 
    values_from = SpecificCollective_1
  ) |> 
  select(Luke, Francesco, Ada, Mare) |> 
  as.matrix()

alpha_specificcollective <- krippendorffs.alpha(pivot_specificcollective, level = "nominal", control = list(parallel = FALSE), verbose = TRUE)

summary(alpha_specificcollective)

improvement_specificcollective <- influence(alpha_specificcollective, units = seq(1:38))

bind_cols(match_set, improvement_specificcollective) |> 
  filter(dfbeta.units < 0) |> 
  mutate(dfbeta.units = round(dfbeta.units, digits = 3)) |> 
  filter(dfbeta.units < 0) 

improvement_specificcollective2 <- influence(alpha_specificcollective, coders = c(1:4))


### Presence Systems
pivot_systems <- dataset |> 
  select(Coder_ID, Attribution_Number, Post_ID, PresenceSystems_1) |> 
  group_by(Post_ID, Attribution_Number) |> 
  arrange(Post_ID) |>
  ungroup() |> 
  pivot_wider(
    names_from = Coder_ID, 
    values_from = PresenceSystems_1
  ) |> 
  select(Luke, Francesco, Ada, Mare) |> 
  as.matrix()

alpha_systems <- krippendorffs.alpha(pivot_systems, level = "nominal", control = list(parallel = FALSE), verbose = TRUE)

summary(alpha_systems)

improvement_systems <- influence(alpha_systems, units = seq(1:38))

bind_cols(match_set, improvement_systems) |> 
  filter(dfbeta.units < 0) |> 
  mutate(dfbeta.units = round(dfbeta.units, digits = 3)) |> 
  filter(dfbeta.units < 0) 

improvement_systems2 <- influence(alpha_systems, coders = c(1:4))

### Specific Systems
pivot_specificsystems <- dataset |> 
  select(Coder_ID, Attribution_Number, Post_ID, SpecificSystem_1) |> 
  group_by(Post_ID, Attribution_Number) |> 
  arrange(Post_ID) |>
  ungroup() |> 
  pivot_wider(
    names_from = Coder_ID, 
    values_from = SpecificSystem_1
  ) |> 
  select(Luke, Francesco, Ada, Mare) |> 
  as.matrix()

alpha_specificsystems <- krippendorffs.alpha(pivot_specificsystems, level = "nominal", control = list(parallel = FALSE), verbose = TRUE)

summary(alpha_specificsystems)

improvement_specificsystems <- influence(alpha_specificsystems, units = seq(1:38))

bind_cols(match_set, improvement_specificsystems) |> 
  filter(dfbeta.units < 0) |> 
  mutate(dfbeta.units = round(dfbeta.units, digits = 3)) |> 
  filter(dfbeta.units < 0) 

improvement_specificsystems2 <- influence(alpha_specificsystems, coders = c(1:4))

## Presence network

pivot_network <- dataset |> 
  select(Coder_ID, Attribution_Number, Post_ID, PresenceNetwork_1) |> 
  group_by(Post_ID, Attribution_Number) |> 
  arrange(Post_ID) |>
  ungroup() |> 
  pivot_wider(
    names_from = Coder_ID, 
    values_from = PresenceNetwork_1
  ) |> 
  select(Luke, Francesco, Ada, Mare) |> 
  as.matrix()

alpha_network <- krippendorffs.alpha(pivot_network, level = "nominal", control = list(parallel = FALSE), verbose = TRUE)

summary(alpha_network)

improvement_network <- influence(alpha_network, units = seq(1:38))

bind_cols(match_set, improvement_network) |> 
  filter(dfbeta.units < 0) |> 
  mutate(dfbeta.units = round(dfbeta.units, digits = 3)) |> 
  filter(dfbeta.units < 0) 

improvement_network2 <- influence(alpha_network, coders = c(1:4))


## Specific network
pivot_specificnetwork <- dataset |> 
  select(Coder_ID, Attribution_Number, Post_ID, SpecificNetworks_1) |> 
  group_by(Post_ID, Attribution_Number) |> 
  arrange(Post_ID) |>
  ungroup() |> 
  pivot_wider(
    names_from = Coder_ID, 
    values_from = SpecificNetworks_1
  ) |> 
  select(Luke, Francesco, Ada, Mare) |> 
  as.matrix()

alpha_specificnetwork <- krippendorffs.alpha(pivot_specificnetwork, level = "nominal", control = list(parallel = FALSE), verbose = TRUE)

summary(alpha_specificnetwork)

improvement_specificnetwork <- influence(alpha_specificnetwork, units = seq(1:38))

bind_cols(match_set, improvement_specificnetwork) |> 
  filter(dfbeta.units < 0) |> 
  mutate(dfbeta.units = round(dfbeta.units, digits = 3)) |> 
  filter(dfbeta.units < 0) 

improvement_specificnetwork2 <- influence(alpha_specificnetwork, coders = c(1:4))


## Economy
pivot_economy <- dataset |> 
  select(Coder_ID, Attribution_Number, Post_ID, Economy_1) |> 
  group_by(Post_ID, Attribution_Number) |> 
  arrange(Post_ID) |>
  ungroup() |> 
  pivot_wider(
    names_from = Coder_ID, 
    values_from = Economy_1
  ) |> 
  select(Luke, Francesco, Ada, Mare) |> 
  as.matrix()

alpha_economy <- krippendorffs.alpha(pivot_economy, level = "nominal", control = list(parallel = FALSE), verbose = TRUE)

summary(alpha_economy)

improvement_economy <- influence(alpha_economy, units = seq(1:38))

bind_cols(match_set, improvement_economy) |> 
  filter(dfbeta.units < 0)|> 
  mutate(dfbeta.units = round(dfbeta.units, digits = 3)) |> 
  filter(dfbeta.units < 0) 

improvement_economy2 <- influence(alpha_economy, coders = c(1:4))


## politics and human rights
pivot_pol <- dataset |> 
  select(Coder_ID, Attribution_Number, Post_ID, PoliticsHumanRight_1) |> 
  group_by(Post_ID, Attribution_Number) |> 
  arrange(Post_ID) |>
  ungroup() |> 
  pivot_wider(
    names_from = Coder_ID, 
    values_from = PoliticsHumanRight_1
  ) |> 
  select(Luke, Francesco, Ada, Mare) |> 
  as.matrix()

alpha_pol <- krippendorffs.alpha(pivot_pol, level = "nominal", control = list(parallel = FALSE), verbose = TRUE)

summary(alpha_pol)

improvement_pol <- influence(alpha_pol, units = seq(1:38))

bind_cols(match_set, improvement_pol) |> 
  filter(dfbeta.units < 0)|> 
  mutate(dfbeta.units = round(dfbeta.units, digits = 3)) |> 
  filter(dfbeta.units < 0) 

improvement_pol2 <- influence(alpha_pol, coders = c(1:4))


### External relations
pivot_external <- dataset |> 
  select(Coder_ID, Attribution_Number, Post_ID, ExternalRelations_1) |> 
  group_by(Post_ID, Attribution_Number) |> 
  arrange(Post_ID) |>
  ungroup() |> 
  pivot_wider(
    names_from = Coder_ID, 
    values_from = ExternalRelations_1
  ) |> 
  select(Luke, Francesco, Ada, Mare) |> 
  as.matrix()

alpha_external <- krippendorffs.alpha(pivot_external, level = "nominal", control = list(parallel = FALSE), verbose = TRUE)

summary(alpha_external)

improvement_external <- influence(alpha_external, units = seq(1:38))

bind_cols(match_set, improvement_external) |> 
  filter(dfbeta.units < 0)|> 
  mutate(dfbeta.units = round(dfbeta.units, digits = 3)) |> 
  filter(dfbeta.units < 0) 


### Social
pivot_social <- dataset |> 
  select(Coder_ID, Attribution_Number, Post_ID, SocialPolicies_1) |> 
  group_by(Post_ID, Attribution_Number) |> 
  arrange(Post_ID) |>
  ungroup() |> 
  pivot_wider(
    names_from = Coder_ID, 
    values_from = SocialPolicies_1
  ) |> 
  select(Luke, Francesco, Ada, Mare) |> 
  as.matrix()

alpha_social <- krippendorffs.alpha(pivot_social, level = "nominal", control = list(parallel = FALSE), verbose = TRUE)

summary(alpha_social)

improvement_social <- influence(alpha_social, units = seq(1:38))

bind_cols(match_set, improvement_social) |> 
  filter(dfbeta.units < 0)|> 
  mutate(dfbeta.units = round(dfbeta.units, digits = 3)) |> 
  filter(dfbeta.units < 0) 

improvement_social2 <- influence(alpha_social, coders = c(1:4))


## Time 

### Past
pivot_past <- dataset |> 
  select(Coder_ID, Attribution_Number, Post_ID, PastTense_1) |> 
  group_by(Post_ID, Attribution_Number) |> 
  arrange(Post_ID) |>
  ungroup() |> 
  pivot_wider(
    names_from = Coder_ID, 
    values_from = PastTense_1
  ) |> 
  select(Luke, Francesco, Ada, Mare) |> 
  as.matrix()

alpha_past <- krippendorffs.alpha(pivot_past, level = "nominal", control = list(parallel = FALSE), verbose = TRUE)

summary(alpha_past)

improvement_past <- influence(alpha_past, units = seq(1:38))

bind_cols(match_set, improvement_past) |> 
  filter(dfbeta.units < 0)|> 
  mutate(dfbeta.units = round(dfbeta.units, digits = 3)) |> 
  filter(dfbeta.units < 0) 

improvement_past2 <- influence(alpha_past, coders = c(1:4))


### Present
pivot_present <- dataset |> 
  select(Coder_ID, Attribution_Number, Post_ID, PresentTense_1) |> 
  group_by(Post_ID, Attribution_Number) |> 
  arrange(Post_ID) |>
  ungroup() |> 
  pivot_wider(
    names_from = Coder_ID, 
    values_from = PresentTense_1
  ) |> 
  select(Luke, Francesco, Ada, Mare) |> 
  as.matrix()

alpha_present <- krippendorffs.alpha(pivot_present, level = "nominal", control = list(parallel = FALSE), verbose = TRUE)

summary(alpha_present)

improvement_present <- influence(alpha_present, units = seq(1:38))

bind_cols(match_set, improvement_present) |> 
  filter(dfbeta.units < 0)|> 
  mutate(dfbeta.units = round(dfbeta.units, digits = 3)) |> 
  filter(dfbeta.units < 0) 

improvement_present2 <- influence(alpha_present, coders = c(1:4))

### Future
pivot_future <- dataset |> 
  select(Coder_ID, Attribution_Number, Post_ID, FutureTense_1) |> 
  group_by(Post_ID, Attribution_Number) |> 
  arrange(Post_ID) |>
  ungroup() |> 
  pivot_wider(
    names_from = Coder_ID, 
    values_from = FutureTense_1
  ) |> 
  select(Luke, Francesco, Ada, Mare) |> 
  as.matrix()

alpha_future <- krippendorffs.alpha(pivot_future, level = "nominal", control = list(parallel = FALSE), verbose = TRUE)

summary(alpha_future)

improvement_future <- influence(alpha_future, units = seq(1:38))

bind_cols(match_set, improvement_future) |> 
  filter(dfbeta.units < 0)|> 
  mutate(dfbeta.units = round(dfbeta.units, digits = 3)) |> 
  filter(dfbeta.units < 0) 

improvement_future2 <- influence(alpha_future, coders = c(1:4)) 




# Reliability step 1

set.seed(123)

headers2 <- read.csv("reliability_step1.csv", nrows=1, header= F) # Create column names for data from reliability step 2

dataset2 <-read.csv("reliability_step1.csv", skip=3, header = F) # load data from reliability step 2

colnames(dataset2) <- headers2
view(dataset2)

dataset2 <- dataset2|> 
  select(c(`Coder ID`, `Post ID`,`Attribution Presence`)) |> 
  rename("Coder_ID" = `Coder ID`, "Post_ID" = `Post ID`, "Attribution_Presence" = `Attribution Presence`)


dataset2 <- dataset2 |> 
  mutate(Attribution_Presence = as.factor(Attribution_Presence)) |> 
  mutate(Attribution_Presence = as.numeric(fct_recode(Attribution_Presence, 
                                                      "1" = "Yes", 
                                                      "2" = "No")))

pivot_presence <- dataset2 |> 
  group_by(Post_ID, Attribution_Presence) |> 
  arrange(Post_ID) |>
  ungroup() |> 
  pivot_wider(
    names_from = Coder_ID, 
    values_from = Attribution_Presence
  ) |> 
  select(Luke, Francesco, Ada, Mare) |> 
  as.matrix()

alpha_presence <- krippendorffs.alpha(pivot_presence, level = "nominal", control = list(parallel = FALSE), verbose = TRUE)

summary(alpha_presence)


improvement_presence <- influence(alpha_presence, units = seq(1:38))

bind_cols(match_set, improvement_presence) |> 
  filter(dfbeta.units < 0)|> 
  mutate(dfbeta.units = round(dfbeta.units, digits = 3)) |> 
  filter(dfbeta.units < 0) 

improvement_presence2 <- influence(alpha_presence, coders = c(1:4))




