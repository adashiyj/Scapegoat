library(tidyverse)
library(ggplot2)
library(DescTools)
library(broom)
library(knitr)
library(gtExtras)
library(REAT)

resat <- read_csv("clean_data.csv")

resat <- resat |> 
  mutate(Time_Stamp = as.factor(Time_Stamp), 
         Attribution_Presence = as.factor(Attribution_Presence), 
         Evaluation = as.factor(Evaluation), 
         Topic = as.factor(Topic), 
         Target = as.factor(Target)) |> 
  mutate(Time_Stamp = fct_recode(Time_Stamp, 
                                 "Before" = "0", 
                                 "After" = "1"), 
         Topic = fct_recode(Topic, 
                            "Economy" = "1", 
                            "Social Politices & Welfare" = "2", 
                            "Politics & Human Rights" = "3", 
                            "External Relations" = "4"), 
         Attribution_Presence = fct_recode(Attribution_Presence, 
                                           "Posts without attribution" = "0",
                                           "Posts with attribution" = "1" 
                                           ), 
         Evaluation = fct_recode(Evaluation, 
                                           "Negative" = "0",
                                           "Positive" = "1" 
         ), 
         Target = fct_recode(Target, 
                             "Individual" = "1", 
                             "Collective" = "2", 
                             "System" = "3", 
                             "Network" = "4")
         )

resat |> 
  count(Time_Stamp)

## Setting overall theme of plots
theme_set(theme_minimal() + 
            theme(text = element_text(family = "Times New Roman"), 
                  legend.position = "bottom"))


### RQ1a
resat1 <- resat |> 
  mutate(Attribution_Presence = 
           if_else(Sender_Relevance != 1 | is.na(Evaluation) | is.na(Topic), "Posts without attribution", Attribution_Presence))  |> 
  group_by(Post_ID) |> 
  mutate(n = row_number(Attribution_Presence)) |> 
  ungroup() |> 
  filter(n == 1)

resat1 |> 
  group_by(Time_Stamp) |> 
  count(Attribution_Presence) |> 
  ungroup() |> 
  pivot_wider(
    names_from = Attribution_Presence, 
    values_from = n
  ) |> 
  gt() |> 
  gtsave("RQ1a.png", path = '/Users/lukefischer/Desktop')


resat1 |> 
  group_by(Time_Stamp) |> 
  count(Attribution_Presence) |>
  mutate(Proportion = n/sum(n))


RQ1aplot <- resat1 |> 
  ggplot(aes(x = Time_Stamp, fill = Attribution_Presence)) + 
  geom_bar(position= "fill",  width = 0.4) + 
  coord_flip() + 
  labs(
    title = "RQ1a",
    x = "Inauguration", 
    y = "Proportion", 
    fill = "Post Type"
  )+
  scale_x_discrete(labels = c("Before", "After")) +
  scale_fill_manual( 
                    values = c("royalblue1", "violet")) +
  annotate(
    geom = "label", 
    x = "After", 
    y = 0.9225, 
    label = "15,5%", 
    hjust = "center", 
    size = 3
  ) + 
  annotate(
    geom = "label", 
    x = "After", 
    y = 0.4225,
    label = "84,5%", 
    hjust = "center", 
    size = 3
  )+ 
  annotate(
    geom = "label", 
    x = "Before", 
    y = 0.9215,
    label = "15,7%", 
    hjust = "center", 
    size = 3
  )+ 
  annotate(
    geom = "label", 
    x = "Before", 
    y = 0.4215,
    label = "84,3%", 
    hjust = "center", 
    size = 3
  ) +
  theme(plot.title.position = "plot",           
        legend.key.width = unit(1, 'cm'), 
        legend.key.height = unit(0.2, 'cm'))

ggsave("RQ1aplot.png", plot = RQ1aplot, path = '/Users/lukefischer/Desktop', width = 8, height= 3, units = "in")


### Creating a dataset with only valid responsibility attributions
resat2 <- resat |> 
  filter(Sender_Relevance == 1 & !is.na(Evaluation) & !is.na(Topic)) 

resat2 |> 
  count(Attribution_Presence)

resat2 |> 
  group_by(Time_Stamp) |> 
  count(Evaluation) |> 
  ungroup() |> 
  pivot_wider(
    names_from = Evaluation, 
    values_from = n
  ) |> 
  gt() |> 
  gtsave("RQ1b.png", path = '/Users/lukefischer/Desktop')



RQ1bplot <- resat2 |> 
  ggplot(aes(x = Time_Stamp, fill = Evaluation)) + 
  geom_bar(position= "fill",  width = 0.4) + 
  coord_flip() + 
  labs(
    title = "RQ1b",
    x = "Inauguration", 
    y = "Proportion" 
  ) +
  scale_x_discrete(labels = c("Before", "After")) +
  scale_fill_manual(labels = c("Negative", "Positive"), values = c("#EB442C", "#29AF34")) +
  annotate(
    geom = "label", 
    x = "After", 
    y = 0.3045, 
    label = "60,9%", 
    hjust = "center", 
    size = 3
  ) + 
  annotate(
    geom = "label", 
    x = "After", 
    y = 0.8045,
    label = "39,1%", 
    hjust = "center", 
    size = 3
)+ 
  annotate(
    geom = "label", 
    x = "Before", 
    y = 0.2635,
    label = "52.7%", 
    hjust = "center", 
    size = 3
  )+ 
  annotate(
    geom = "label", 
    x = "Before", 
    y = 0.7635,
    label = "47,3%", 
    hjust = "center", 
    size = 3
  ) +
  theme(plot.title.position = "plot", 
        legend.key.width = unit(1, 'cm'), 
        legend.key.height = unit(0.2, 'cm'))

ggsave("RQ1bplot.png", plot = RQ1bplot, path = '/Users/lukefischer/Desktop', width = 8, height= 3, units = "in")



## RQ2b
###pos before inauguration

resat2 |> 
  filter(Evaluation == "Positive" & Target == "Individual") |> 
  group_by(Time_Stamp) |> 
  count(Specific_Individual) |> 
  ungroup() |> 
  pivot_wider(
    names_from = Specific_Individual, 
    values_from = n
  ) |> 
  gt() |> 
  gtsave("RQ2b.png", path = '/Users/lukefischer/Desktop')


pos_before <- resat2 |> 
  filter(Evaluation == 1 & Target == 1) |> 
  group_by(Time_Stamp) |> 
  filter(Time_Stamp == "Before") |> 
  count(Specific_Individual) |> 
  mutate(proportion = n/sum(n)) |> 
  pull(n)

w_pos_before <- c(1, 1, 1, 1, 0.25)

Gini(pos_before, weights = w_pos_before, conf.level = 0.95, R = 10000)

### pos after inauguration
pos_after <- resat2 |> 
  filter(Evaluation == 1 & Target == 1) |> 
  group_by(Time_Stamp) |> 
  filter(Time_Stamp == "After") |> 
  count(Specific_Individual) |> 
  mutate(proportion = n/sum(n))|> 
  pull(n)

w_pos_after <- c(1, 1, 1, 0.25, 1, 1)

Gini(pos_after, weights = w_pos_after,conf.level = 0.95, R = 10000)


####viz
RQ2bplot <- resat2 |> 
  filter(Evaluation == "Positive" & Target == 1) |> 
  ggplot(aes(x = Time_Stamp, fill = Specific_Individual)) + 
  geom_bar(position= "dodge",  width = 0.4) +
  scale_fill_discrete(labels = 
                        c("Donald Trump", "EU/European Politicians", "Hillary Clinton", "Mike Pence", "Rep Politicians", "Other", "Putin", "Xi")) +
  labs(
    title = "RQ2b", 
    x = "Inauguration", 
    fill = "Individuals"
  )+
  theme(plot.title.position = "plot", 
        legend.key.width = unit(1, 'cm'), 
        legend.key.height = unit(0.2, 'cm'))

ggsave("RQ2bplot.png", plot = RQ2bplot, path = '/Users/lukefischer/Desktop', width = 8, height= 3, units = "in")



## RQ2a
### neg before inauguration


resat2 |> 
  filter(Evaluation == "Negative" & Target == 1) |> 
  group_by(Time_Stamp) |> 
  count(Specific_Individual) |> 
  ungroup() |> 
  pivot_wider(
    names_from = Specific_Individual, 
    values_from = n
  ) |> 
  gt() |> 
  gtsave("RQ2a.png", path = '/Users/lukefischer/Desktop')



neg_before <- resat2 |> 
  filter(Evaluation == 0 & Target == 1) |> 
  group_by(Time_Stamp) |> 
  filter(Time_Stamp == "Before") |> 
  count(Specific_Individual) |> 
  mutate(proportion = n/sum(n))|> 
  pull(n)

w_neg_before <- c(1, 1, 1, 1, 0.25)

Gini(neg_before, weights = w_neg_before, conf.level = 0.95, R = 10000)



### neg after inauguration
neg_after <- resat2 |> 
  filter(Evaluation == "Negative" & Target == 1) |> 
  group_by(Time_Stamp) |> 
  filter(Time_Stamp == "After") |> 
  count(Specific_Individual) |> 
  mutate(proportion = n/sum(n))|> 
  pull(n)

w_neg_after <- c(1, 1, 0.25)

Gini(neg_after, weights = w_neg_after, conf.level = 0.95, R = 10000)


#### viz
RQ2aplot <- resat2 |> 
  filter(Evaluation == "Negative" & Target == 1) |> 
  ggplot(aes(x = Time_Stamp, fill = Specific_Individual)) + 
  geom_bar(position= "dodge",  width = 0.4) +
  scale_fill_discrete(labels = 
                        c("Barack Obama", "Hillary Clinton", "Other Dems", "Other Reps", "Other")) +
  labs(
    title = "RQ2a", 
    x = "Inauguration", 
    fill = "Individuals"
  )+
  theme(plot.title.position = "plot", 
        legend.key.width = unit(1, 'cm'), 
        legend.key.height = unit(0.2, 'cm'))

ggsave("RQ2aplot.png", plot = RQ2aplot, path = '/Users/lukefischer/Desktop', width = 8, height= 3, units = "in")



## RQ3b ----

resat2 |> 
  filter(Evaluation == "Positive" & Target == 2) |> 
  group_by(Time_Stamp) |> 
  count(Specific_Collective) |> 
  ungroup() |> 
  pivot_wider(
    names_from = Specific_Collective, 
    values_from = n
  ) |> 
  gt() |> 
  gtsave("RQ3b.png", path = '/Users/lukefischer/Desktop')


pos_before_col <- resat2 |> 
  filter(Evaluation == 1 & Target == 2) |> 
  group_by(Time_Stamp) |> 
  filter(Time_Stamp == "Before") |> 
  count(Specific_Collective) |> 
  mutate(proportion = n/sum(n)) |> 
  pull(n)

w_pos_before_col <- c(1, 0.25, 1)

Gini(pos_before_col, weights = w_pos_before_col, conf.level = 0.95, R = 10000)

pos_after_col<- resat2 |> 
  filter(Evaluation == 1 & Target == 2) |> 
  group_by(Time_Stamp) |> 
  filter(Time_Stamp == "After") |> 
  count(Specific_Collective) |> 
  mutate(proportion = n/sum(n)) |> 
  pull(n)

w_pos_after_col <- c(1, 1, 0.25, 1, 1)

Gini(pos_after_col, weights = w_pos_after_col, conf.level = 0.95, R = 10000)


### viz
RQ3bplot <- resat2 |> 
  filter(Evaluation == "Positive" & Target == 2) |> 
  ggplot(aes(x = Time_Stamp, fill = Specific_Collective)) + 
  geom_bar(position= "dodge",  width = 0.4) +
  scale_fill_discrete(labels = 
                        c("Conservatives", "National Government Agencies", "Other", "The Trump Administration", "USA")) +
  labs(
    title = "RQ3b", 
    x = "Inauguration", 
    fill = "Collectives"
  )+
  theme(plot.title.position = "plot", 
        legend.key.width = unit(1, 'cm'), 
        legend.key.height = unit(0.2, 'cm'))

ggsave("RQ3bplot.png", plot = RQ3bplot, path = '/Users/lukefischer/Desktop', width = 8, height= 3, units = "in")



## RQ3a ----
resat2 |> 
  filter(Evaluation == "Negative" & Target == 2) |> 
  group_by(Time_Stamp) |> 
  count(Specific_Collective) |> 
  ungroup() |> 
  pivot_wider(
    names_from = Specific_Collective, 
    values_from = n
  ) |> 
  gt() |> 
  gtsave("RQ3a.png", path = '/Users/lukefischer/Desktop')

neg_before_col <- resat2 |> 
  filter(Evaluation == 0 & Target == 2) |> 
  group_by(Time_Stamp) |> 
  filter(Time_Stamp == "Before") |> 
  count(Specific_Collective) |> 
  mutate(proportion = n/sum(n)) |> 
  pull(n)

w_neg_before_col <- c(1, 1, 0.25, 1, 1)

Gini(neg_before_col, weights = w_neg_before_col, conf.level = 0.95, R = 10000)



neg_after_col <- resat2 |> 
  filter(Evaluation == "Negative" & Target == 2) |> 
  group_by(Time_Stamp) |> 
  filter(Time_Stamp == "After") |> 
  count(Specific_Collective) |> 
  mutate(proportion = n/sum(n)) |> 
  pull(n)

w_neg_after_col <- c(1, 1, 1, 0.25, 1, 1, 1)

Gini(neg_after_col, weights = w_neg_after_col, conf.level = 0.95, R = 10000)


RQ3aplot<- resat2 |> 
  filter(Evaluation == "Negative" & Target == 2) |> 
  ggplot(aes(x = Time_Stamp, fill = Specific_Collective)) + 
  geom_bar(position= "dodge",  width = 0.4)+
  scale_fill_discrete(labels = 
                        c("Conservatives", "Media", "Minorities", "National Government Agencies","Other", "Progressives", "The Obama Administration", "USA")) +
  labs(
    title = "RQ3a", 
    x = "Inauguration", 
    fill = "Collectives"
  )+
  theme(plot.title.position = "plot", 
        legend.key.width = unit(1, 'cm'), 
        legend.key.height = unit(0.2, 'cm'))

ggsave("RQ3aplot.png", plot = RQ3aplot, path = '/Users/lukefischer/Desktop', width = 8, height= 3, units = "in")


## RQ4a ----

resat2 |> 
  filter(Evaluation == "Positive") |> 
  group_by(Time_Stamp) |> 
  count(Target) |>
  mutate(Proportion = n/sum(n))

resat2 |> 
  filter(Evaluation == "Positive") |> 
  group_by(Time_Stamp) |> 
  count(Target) |> 
  ungroup() |> 
  pivot_wider(
    names_from = Target, 
    values_from = n
  )|> 
  gt() |> 
  gtsave("RQ4a.png", path = '/Users/lukefischer/Desktop')

RQ4aplot<- resat2 |> 
  filter(Evaluation == "Positive") |> 
  ggplot(aes(x = Time_Stamp, fill = Target)) + 
  geom_bar(position= "fill",  width = 0.4) + 
  coord_flip()+ 
  labs(
    title = "RQ4a",
    x = "Inauguration", 
    y = "Proportion" 
  ) +
  scale_fill_brewer(palette = "Set2")+
  theme(plot.title.position = "plot",           
        legend.key.width = unit(1, 'cm'), 
        legend.key.height = unit(0.2, 'cm'))

ggsave("RQ4aplot.png", plot = RQ4aplot, path = '/Users/lukefischer/Desktop', width = 8, height= 3, units = "in")


## RQ4b ----
resat2 |> 
  filter(Evaluation == "Negative") |> 
  group_by(Time_Stamp) |> 
  count(Target) |>
  mutate(Proportion = n/sum(n))

resat2 |> 
  filter(Evaluation == "Negative") |> 
  group_by(Time_Stamp) |> 
  count(Target) |> 
  ungroup() |> 
  pivot_wider(
    names_from = Target, 
    values_from = n
  )|> 
  gt() |> 
  gtsave("RQ4b.png", path = '/Users/lukefischer/Desktop')

RQ4bplot<- resat2 |> 
  filter(Evaluation == "Negative") |> 
  ggplot(aes(x = Time_Stamp, fill = Target)) + 
  geom_bar(position= "fill",  width = 0.4) + 
  coord_flip()+ 
  labs(
    title = "RQ4b",
    x = "Inauguration", 
    y = "Proportion" 
  ) +
  scale_fill_brewer(palette = "Set2")+
  theme(plot.title.position = "plot",           
        legend.key.width = unit(1, 'cm'), 
        legend.key.height = unit(0.2, 'cm'))

ggsave("RQ4bplot.png", plot = RQ4bplot, path = '/Users/lukefischer/Desktop', width = 8, height= 3, units = "in")


## RQ5a ----

resat2 |> 
  filter(Evaluation == 1) |> 
  group_by(Time_Stamp) |> 
  count(Topic) |> 
  ungroup() |> 
  pivot_wider(
    names_from = Topic, 
    values_from = n
  ) |> 
  gt() |> 
  gtsave("RQ5a.png", path = '/Users/lukefischer/Desktop')

resat2 |> 
  filter(Evaluation == "Positive") |> 
  group_by(Time_Stamp) |> 
  count(Topic) |>
  mutate(Proportion = n/sum(n)) |> 
  filter(Time_Stamp == "After")


RQ5aplot <- resat2 |> 
  filter(Evaluation == "Positive") |> 
  ggplot(aes(x = Time_Stamp, fill = Topic)) + 
  geom_bar(position= "fill",  width = 0.4) + 
  coord_flip()+ 
  labs(
    title = "RQ5a",
    x = "Inauguration", 
    y = "Proportion" 
  ) +
  scale_fill_brewer(palette = "Set2") +
  annotate(
    geom = "label", 
    x = "After", 
    y = 0.0835, 
    label = "16,7%", 
    hjust = "center", 
    size = 3
  ) + 
  annotate(
    geom = "label", 
    x = "After", 
    y = 0.3455,
    label = "35,7%", 
    hjust = "center", 
    size = 3
  )+ 
  annotate(
    geom = "label", 
    x = "After", 
    y = 0.631,
    label = "21,4%", 
    hjust = "center", 
    size = 3
  )+ 
  annotate(
    geom = "label", 
    x = "After", 
    y = 0.869,
    label = "26,2%", 
    hjust = "center", 
    size = 3
  )+ 
  annotate(
    geom = "label", 
    x = "Before", 
    y = 0.03675,
    label = "7,35%", 
    hjust = "center", 
    size = 3
  )+ 
  annotate(
    geom = "label", 
    x = "Before", 
    y = 0.434,
    label = "72,1%", 
    hjust = "center", 
    size = 3
  ) +
  annotate(
    geom = "label", 
    x = "Before", 
    y = 0.81655,
    label = "4,41%", 
    hjust = "center", 
    size = 3
  )+ 
  annotate(
    geom = "label", 
    x = "Before", 
    y = 0.9196,
    label = "16,2%", 
    hjust = "center", 
    size = 3
  ) +
  theme(plot.title.position = "plot",           
        legend.key.width = unit(1, 'cm'), 
        legend.key.height = unit(0.2, 'cm'))


ggsave("RQ5aplot.png", plot = RQ5aplot, path = '/Users/lukefischer/Desktop', width = 8, height= 3, units = "in")


top_after_pos <- resat2 |> 
  filter(Evaluation == "Positive") |> 
  group_by(Time_Stamp) |> 
  filter(Time_Stamp == "After") |> 
  count(Topic) |>
  pull(n)

Gini(top_after_pos, conf.level = 0.95, R = 10000, type = "bca")

top_before_pos <- resat2 |> 
  filter(Evaluation == "Positive") |> 
  group_by(Time_Stamp) |> 
  filter(Time_Stamp == "Before") |> 
  count(Topic) |>
  ungroup() |> 
  arrange(n) |> 
  pull(n)

Gini(top_before_pos, conf.level = 0.95, R = 10000, type = "bca")

####Plotting Gini
lorenz_before_pos <- resat2 |> 
  filter(Evaluation == "Positive") |> 
  group_by(Time_Stamp) |> 
  filter(Time_Stamp == "Before") |> 
  count(Topic) |>
  ungroup() |> 
  select(n) |> 
  arrange(n) 

lorenz_after_pos <- resat2 |> 
  filter(Evaluation == "Positive") |> 
  group_by(Time_Stamp) |> 
  filter(Time_Stamp == "After") |> 
  count(Topic) |>
  ungroup() |> 
  select(n) |> 
  arrange(n) 

lorenz(lorenz_before_pos, lc.col= "violet")

lorenz(lorenz_after_pos, add.lc = TRUE)




## RQ5b ----
resat2 |> 
  filter(Evaluation == 0) |> 
  group_by(Time_Stamp) |> 
  count(Topic) |> 
  mutate(proportion = n/sum(n)) |> 
  gt()


resat2 |> 
  filter(Evaluation == "Negative") |> 
  group_by(Time_Stamp) |> 
  count(Topic) |> 
  ungroup() |> 
  pivot_wider(
    names_from = Topic, 
    values_from = n
  ) |> 
  gt() |> 
  gtsave("RQ5b.png", path = '/Users/lukefischer/Desktop')


resat2 |> 
  filter(Evaluation == "Negative") |> 
  group_by(Time_Stamp) |> 
  count(Topic) |>
  mutate(Proportion = n/sum(n))


RQ5bplot <- resat2 |> 
  filter(Evaluation == "Negative") |> 
  ggplot(aes(x = Time_Stamp, fill = Topic)) + 
  geom_bar(position= "fill",  width = 0.4) + 
  coord_flip()+ 
  labs(
    title = "RQ5b",
    x = "Inauguration", 
    y = "Proportion" 
  ) +
  scale_fill_brewer(palette = "Set2") +
  annotate(
    geom = "label", 
    x = "After", 
    y = 0.1295, 
    label = "25,9%", 
    hjust = "center", 
    size = 3
  ) + 
  annotate(
    geom = "label", 
    x = "After", 
    y = 0.5555,
    label = "59,3%", 
    hjust = "center", 
    size = 3
  )+ 
  annotate(
    geom = "label", 
    x = "After", 
    y = 0.9075,
    label = "11,1%", 
    hjust = "center", 
    size = 3
  )+ 
  annotate(
    geom = "label", 
    x = "After", 
    y = 0.9815,
    label = "3,7%", 
    hjust = "center", 
    size = 3
  )+ 
  annotate(
    geom = "label", 
    x = "Before", 
    y = 0.09,
    label = "18%", 
    hjust = "center", 
    size = 3
  )+ 
  annotate(
    geom = "label", 
    x = "Before", 
    y = 0.4835,
    label = "60,7%", 
    hjust = "center", 
    size = 3
  ) +
  annotate(
    geom = "label", 
    x = "Before", 
    y = 0.81965,
    label = "6,56%", 
    hjust = "center", 
    size = 3
  )+ 
  annotate(
    geom = "label", 
    x = "Before", 
    y = 0.9266,
    label = "14,8%", 
    hjust = "center", 
    size = 3
  ) +
  theme(plot.title.position = "plot",           
        legend.key.width = unit(1, 'cm'), 
        legend.key.height = unit(0.2, 'cm'))


ggsave("RQ5bplot.png", plot = RQ5bplot, path = '/Users/lukefischer/Desktop', width = 8, height= 3, units = "in")




lorenz_before_neg <- resat2 |> 
  filter(Evaluation == "Negative") |> 
  group_by(Time_Stamp) |> 
  count(Topic) |> 
  ungroup() |> 
  filter(Time_Stamp=="Before") |> 
  select(n) |> 
  arrange(n)
  

lorenz_after_neg <- resat2 |> 
  filter(Evaluation == "Negative") |> 
  group_by(Time_Stamp) |> 
  count(Topic) |> 
  ungroup() |> 
  filter(Time_Stamp=="After") |> 
  select(n) |> 
  arrange(n)

Gini_before_neg <- lorenz_before_neg |> 
  pull(n)

Gini_after_neg <- lorenz_after_neg |> 
  pull(n)

Gini(Gini_before_neg, conf.level = 0.95, R = 10000, type = "bca")

Gini(Gini_after_neg, conf.level = 0.95, R = 10000, type = "bca")

lorenz(lorenz_before_neg,  lc.col= "violet")

lorenz(lorenz_after_neg, add.lc = TRUE)


