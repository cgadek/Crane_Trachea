### Decision tree ###
#This script runs a training set (NMDGF morphology) decision tree to predict the varibales with trcahea measuremnts.
pacman::p_load(rpart.plot,
               rpart, 
               tidyverse)

#First divide up by sexes then training(NMDGF) and prediction (MSB) and clean datasets

#Males
morpho.m.train <- morpho_massive %>%
  filter(
    sex == "M",
    data.origin == "NMDGF",
    tail < 250 & tail > 150,
    mass > 1000,
    wing.chord < 5000 & wing.chord > 300,
    tarsus < 400 & tarsus > 100,
    anterior.culmen < 250,
    anterior.culmen > 1,
    posterior.culmen < 250,
    posterior.culmen > 1,
    sex%in%c("M", "F")
  ) %>%
  dplyr::mutate(
    taxon_name = factor(taxon_name),
         sex = factor(sex)
         )%>%
  dplyr::select(4:12)%>%
  group_by(taxon_name)%>%
  slice_sample(., n=700)%>%
  na.omit()
#sampled from larger datasst to get equivalent numbers of each taxon and a training set of ~80% of combined data

#Annoyingly no sex in MSB mropho dataframe need to get that in here


morpho.m.test <- dat.s %>%
  filter(
    sex=="M"
  ) %>%
  dplyr::mutate(taxon_name = factor(taxon_name),
                sex = factor(sex))%>%
  dplyr::select(taxon_name, mass, sex, tail, wing.chord, anterior.culmen, posterior.culmen, tarsus)%>%
  na.omit()
  
# Construct and run mass model

fit <- rpart(taxon_name~mass, data = morpho.m.train, method = 'class')
rpart.plot(fit, extra = 106)

predict_unseen <-predict(fit, morpho.m.test, type = 'class')

morpho.m.test <- morpho.m.test%>%
  cbind(., predict_unseen)%>%
  mutate(correct.pred = if_else(taxon_name == predict_unseen, "correct", "incorrect"))
#posterior culmen <85 is best separtor of males by subspecies, followed by mass <4255
# Mass < 4255 lesser #other characters are less useful at parsing

table_mat <- table(morpho.m.test$taxon_name, predict_unseen)
table_mat

accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
accuracy_Test

# Females
morpho.f.train <- morpho_massive %>%
  filter(
    sex == "F",
    data.origin == "NMDGF",
    tail < 250 & tail > 150,
    mass > 1000,
    wing.chord < 5000 & wing.chord > 300,
    tarsus < 400 & tarsus > 100,
    anterior.culmen < 250,
    anterior.culmen > 1,
    posterior.culmen < 250,
    posterior.culmen > 1,
    sex%in%c("M", "F")
  ) %>%
  dplyr::mutate(
    taxon_name = factor(taxon_name),
    sex = factor(sex)
  )%>%
  dplyr::select(4:11)%>%
  group_by(taxon_name)%>%
  slice_sample(., n=700)%>%
  na.omit()
#sampled from larger datasst to get equivalent numbers of each taxon and a training set of ~80% of combined data


morpho.f.test <- dat.s %>%
  filter(
    sex=="F"
  ) %>%
  dplyr::mutate(taxon_name = factor(taxon_name),
                sex = factor(sex))%>%
  dplyr::select(taxon_name, mass, sex, tail, wing.chord, anterior.culmen, posterior.culmen, tarsus)%>%
  na.omit()

# Construct and run mass model

fit <- rpart(taxon_name~mass+ anterior.culmen + wing.chord, data = morpho.f.train, method = 'class')
rpart.plot(fit, extra = 106)

predict_unseen <-predict(fit, morpho.f.test, type = 'class')

morpho.f.test <- morpho.f.test%>%
  cbind(., predict_unseen)%>%
  mutate(correct.pred = if_else(taxon_name == predict_unseen, "correct", "incorrect"))
#Again culmen <85 is good seperator and mass <3717