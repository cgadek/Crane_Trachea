### Run models functions

### Run models functions

run_my_mtl_models <- function(data, name, seed) {
  require(tidyverse)
  require(brms)
  
  if (is.null(seed))
    seed <- NA
  
  f1 <- brm(
    formula = bf(log_mtl ~ 1),
    data = data,
    family = student(link="identity"),
    cores = 4,
    chains = 4,
    thin = 10,
    #chop out transitions?
    warmup = 5000,
    #half of iterations
    iter = 10000,
    prior = c(prior(student_t(3, 2.8, 2.5), "Intercept"),
              prior(student_t(3, 0, 2.5), "sigma")),
    #prior = priors,
    save_pars = save_pars(all = TRUE),
    #need this for loo comparison
    control = list(adapt_delta = 0.97, max_treedepth = 19),
    #adapt_delta the target average proposal acceptance probability during Stan's adaptation period
    seed = seed
  )
  f2 <- brm(
    formula = bf(log_mtl ~ 1 + log_mass.z),
    data = data,
    family = student(link="identity"),
    cores = 4,
    chains = 4,
    thin = 10,
    #chop out transitions?
    warmup = 5000,
    #half of iterations
    iter = 10000,
    #prior = c(
    #     prior(normal(0, 10), "Intercept"),
    #     prior(cauchy(0, 2.5), "sigma")),
    prior = c(
      prior(student_t(3, 2.8, 2.5), "Intercept"),
      prior(student_t(3, 0, 2.5), "sigma"),
      prior(normal(0, 5), class = "b")
    ),
    save_pars = save_pars(all = TRUE),
    #need this for loo comparison
    control = list(adapt_delta = 0.97, max_treedepth = 19),
    #adapt_delta the target average proposal acceptance probability during Stan's adaptation period
    seed = seed
  )
  
  f3 <- brm(
    formula = bf(log_mtl ~ 1 + sex, sigma ~ sex),
    data = data,
    family = student(link="identity"),
    cores = 4,
    chains = 4,
    thin = 10,
    #chop out transitions?
    warmup = 5000,
    #half of iterations
    iter = 10000,
    #prior = c(
    #     prior(normal(0, 10), "Intercept"),
    #     prior(cauchy(0, 2.5), "sigma")),
    prior = c(
      prior(student_t(3, 2.8, 2.5), "Intercept"),
      prior(normal(0, 5), class = "b")
    ),
    save_pars = save_pars(all = TRUE),
    #need this for loo comparison
    control = list(adapt_delta = 0.97, max_treedepth = 19),
    #adapt_delta the target average proposal acceptance probability during Stan's adaptation period
    seed = seed
  )
  f4 <- brm(
    formula = bf(log_mtl ~ 1 + log_mass.z + sex, sigma ~ sex),
    data = data,
    family = student(link="identity"),
    cores = 4,
    chains = 4,
    thin = 10,
    #chop out transitions?
    warmup = 5000,
    #half of iterations
    iter = 10000,
    prior = c(
      prior(student_t(3, 2.8, 2.5), "Intercept"),
      prior(normal(0, 5), class = "b")
    ),
    #prior = priors,
    save_pars = save_pars(all = TRUE),
    #need this for loo comparison
    control = list(adapt_delta = 0.97, max_treedepth = 19),
    #adapt_delta the target average proposal acceptance probability during Stan's adaptation period
    seed = seed
  )
  f5 <- brm(
    formula = bf(log_mtl ~ 1 + log_mass.z + sex + log_mass.z * sex, sigma ~ sex),
    data = data,
    family = student(link="identity"),
    cores = 4,
    chains = 4,
    thin = 10,
    #chop out transitions?
    warmup = 5000,
    #half of iterations
    iter = 10000,
    #prior = c(
    #     prior(normal(0, 10), "Intercept"),
    #     prior(cauchy(0, 2.5), "sigma")),
    prior = c(
      prior(student_t(3, 2.8, 2.5), "Intercept"),
      prior(normal(0, 5), class = "b")
    ),
    save_pars = save_pars(all = TRUE),
    #need this for loo comparison
    control = list(adapt_delta = 0.97, max_treedepth = 19),
    #adapt_delta the target average proposal acceptance probability during Stan's adaptation period
    seed = seed
  )
  
  #compare model with LOOIC
  m.comp<-LOO(f1,f2,f3,f4,f5, moment_match=T)
  
  #Get Bayes R^2
  br1<-bayes_R2(f1)
  br2<-bayes_R2(f2)
  br3<-bayes_R2(f3)
  br4<-bayes_R2(f4)
  br5<-bayes_R2(f5)
  br2L <- list(br1, br2, br3, br4, br5)
  
  t <- tibble(dataset = c(rep(name,times=5),"LOO_comp", "BayesR2"),
              model_set = c(1:5, "model_compare", "BayesR2"),
              m = list(f1, f2, f3, f4, f5, m.comp, br2L))
  
  assign(name, t, envir=globalenv())
} 


run_my_mass_models <- function(data, name, seed) {
  require(tidyverse)
  require(brms)
  
  if (is.null(seed))
    seed <- NA
  
  f1 <- brm(
    formula = bf(log_mass ~ 1),
    data = data,
    family = student(link="identity"),
    cores = 4,
    chains = 4,
    thin = 10, #chop out transitions?
    warmup = 10000, #half of iterations
    iter = 20000,
    prior = c(
      prior(student_t(3, 2.8, 2.5), "Intercept"),
      prior(student_t(3, 0, 2.5), "sigma")
    ),
    #prior = priors,
    save_pars = save_pars(all = TRUE), #need this for loo comparison
    control = list(adapt_delta = 0.97, max_treedepth = 19), #adapt_delta the target average proposal acceptance probability during Stan's adaptation period
    seed = seed
  )
  f2 <- brm(
    formula = bf(log_mass ~ 1 + sex, sigma ~ sex),
    data = data,
    family = student(link="identity"),
    cores = 4,
    chains = 4,
    thin = 10, #chop out transitions?
    warmup = 10000, #half of iterations
    iter = 20000,
    prior = c(
      prior(student_t(3, 2.8, 2.5), "Intercept"),
      prior(normal(0, 5), class = "b")
    ),
    save_pars = save_pars(all = TRUE), #need this for loo comparison
    control = list(adapt_delta = 0.97, max_treedepth = 19), #adapt_delta the target average proposal acceptance probability during Stan's adaptation period
    seed = seed
  )
  
  f3 <-brm(
    formula = bf(log_mass ~ 1 + sex, sigma ~ sex),
    data = data,
    family = student(link="identity"),
    cores = 4,
    chains = 4,
    thin = 10, #chop out transitions?
    warmup = 10000, #half of iterations
    iter = 20000,
    prior = c(
      prior(student_t(3, 2.8, 2.5), "Intercept"),
      prior(normal(0, 5), class = "b")
    ),
    save_pars = save_pars(all = TRUE), #need this for loo comparison
    control = list(adapt_delta = 0.97, max_treedepth = 19), #adapt_delta the target average proposal acceptance probability during Stan's adaptation period
    seed = seed
  )
  
  #compare wil LOOIC
  m.comp<-LOO(f1, f2, f3, moment_match=T)
  
  #Get Bayes R^2
  br1<-bayes_R2(f1)
  br2<-bayes_R2(f2)
  br3<-bayes_R2(f3)
  br2L <- list(br1, br2, br3)
  
  t <- tibble(dataset = c(rep(name,times=3),"LOO_comp", "BayesR2"),
              model_set = c(1:3, "model_compare", "BayesR2"),
              m = list(f1, f2, f3, m.comp, br2L))
  
  assign(name, t, envir=globalenv())
}


run_my_PC_models <- function(data, PC, taxon, name, seed) {
  require(tidyverse)
  require(brms)
  
  if (is.null(seed))
    seed <- NA
  if(taxon =="Antigone canadensis canadensis"){
    data <- data%>%
      filter(species == "Antigone canadensis canadensis")
  }else if(taxon =="Antigone canadensis tabida"){
    data <- data%>%
      filter(species == "Antigone canadensis tabida")
  }
  
  f1 <- brm(
    formula = bf(paste(PC, "~ 1")),
    data = data,
    family = student(link="identity"),
    cores = 4,
    chains = 4,
    thin = 10, #chop out transitions?
    warmup = 10000, #half of iterations
    iter = 20000,
    prior = c(
      prior(student_t(3, 0, 2.5), "Intercept"),
      prior(student_t(3, 0, 2.5), "sigma")
    ),
    #prior = priors,
    save_pars = save_pars(all = TRUE), #need this for loo comparison
    control = list(adapt_delta = 0.97, max_treedepth = 19), #adapt_delta the target average proposal acceptance probability during Stan's adaptation period
    seed = seed
  )
  f2 <- brm(
    formula = bf(paste(PC,  "~ 1 + log_mass.z")),
    data = data,
    family = student(link="identity"),
    cores = 4,
    chains = 4,
    thin = 10, #chop out transitions?
    warmup = 10000, #half of iterations
    iter = 20000,
    prior = c(
      prior(student_t(3, 0, 2.5), "Intercept"),
      prior(student_t(3, 0, 2.5), "sigma"),
      prior(normal(0, 5), class = "b")
    ),
    save_pars = save_pars(all = TRUE), #need this for loo comparison
    control = list(adapt_delta = 0.97, max_treedepth = 19), #adapt_delta the target average proposal acceptance probability during Stan's adaptation period
    seed = seed
  )
  
  f3 <-brm(
    formula = bf(paste(PC, "~ 1 + sex")),
    data = data,
    family = student(link="identity"),
    cores = 4,
    chains = 4,
    thin = 10, #chop out transitions?
    warmup = 10000, #half of iterations
    iter = 20000,
    prior = c(
      prior(student_t(3, 0, 2.5), "Intercept"),
      prior(normal(0, 5), class = "b")
    ),
    save_pars = save_pars(all = TRUE), #need this for loo comparison
    control = list(adapt_delta = 0.97, max_treedepth = 19), #adapt_delta the target average proposal acceptance probability during Stan's adaptation period
    seed = seed
  )
  
  f4 <-brm(
    formula = bf(paste(PC, "~ 1 + log_mass.z + sex")),
    data = data,
    family = student(link="identity"),
    cores = 4,
    chains = 4,
    thin = 10, #chop out transitions?
    warmup = 10000, #half of iterations
    iter = 20000,
    prior = c(
      prior(student_t(3, 0, 2.5), "Intercept"),
      prior(normal(0, 5), class = "b")
    ),
    save_pars = save_pars(all = TRUE), #need this for loo comparison
    control = list(adapt_delta = 0.97, max_treedepth = 19), #adapt_delta the target average proposal acceptance probability during Stan's adaptation period
    seed = seed
  )
  
  f5 <-brm(
    formula = bf(paste(PC, "~ 1 + log_mass.z + sex + log_mass.z * sex")),
    data = data,
    family = student(link="identity"),
    cores = 4,
    chains = 4,
    thin = 10, #chop out transitions?
    warmup = 10000, #half of iterations
    iter = 20000,
    prior = c(
      prior(student_t(3, 0, 2.5), "Intercept"),
      prior(normal(0, 5), class = "b")
    ),
    save_pars = save_pars(all = TRUE), #need this for loo comparison
    control = list(adapt_delta = 0.97, max_treedepth = 19), #adapt_delta the target average proposal acceptance probability during Stan's adaptation period
    seed = seed
  )
  
  #compare wil LOOIC
  m.comp<-LOO(f1, f2, f3, f4, f5, moment_match=T)
  
  #Get Bayes R^2
  br1<-bayes_R2(f1)
  br2<-bayes_R2(f2)
  br3<-bayes_R2(f3)
  br4<-bayes_R2(f4)
  br5<-bayes_R2(f5)
  br2L <- list(br1, br2, br3, br4, br5)
  
  t <- tibble(dataset = c(rep(name,times=5),"LOO_comp", "BayesR2"),
              model_set = c(1:5, "model_compare", "BayesR2"),
              m = list(f1, f2, f3, f4, f5, m.comp, br2L))
  
  assign(name, t, envir=globalenv())
}

run_my_residual_models <- function(data, name, seed) {
  require(tidyverse)
  require(brms)
  
  if (is.null(seed))
    seed <- NA
  
  data <- data%>%
    dplyr::select(segment_width.z, segment_count.z, r, sex)%>%
    na.omit()
  
  f1 <- brm(
    formula = bf(r ~ 1),
    data = data,
    family = student(),
    cores = 4,
    chains = 4,
    thin = 10, #chop out transitions?
    warmup = 10000, #half of iterations
    iter = 20000,
    prior = c(
      prior(student_t(3, 0, 2.5), "Intercept"),
      prior(student_t(3, 0, 2.5), "sigma")
    ),
    #prior = priors,
    save_pars = save_pars(all = TRUE), #need this for loo comparison
    control = list(adapt_delta = 0.97, max_treedepth = 19), #adapt_delta the target average proposal acceptance probability during Stan's adaptation period
    seed = seed
  )
  f2 <- brm(
    formula = bf(r ~ 1 + segment_width.z),
    data = data,
    family = student(),
    cores = 4,
    chains = 4,
    thin = 10, #chop out transitions?
    warmup = 10000, #half of iterations
    iter = 20000,
    prior = c(
      prior(student_t(3, 0, 2.5), "Intercept"),
      prior(student_t(3, 0, 2.5), "sigma"),
      prior(normal(0, 5), class = "b")
    ),
    save_pars = save_pars(all = TRUE), #need this for loo comparison
    control = list(adapt_delta = 0.97, max_treedepth = 19), #adapt_delta the target average proposal acceptance probability during Stan's adaptation period
    seed = seed
  )
  
  f3 <- brm(
    formula = bf(r ~ 1 + segment_count.z),
    data = data,
    family = student(),
    cores = 4,
    chains = 4,
    thin = 10, #chop out transitions?
    warmup = 10000, #half of iterations
    iter = 20000,
    prior = c(
      prior(student_t(3, 0, 2.5), "Intercept"),
      prior(student_t(3, 0, 2.5), "sigma"),
      prior(normal(0, 5), class = "b")
    ),
    save_pars = save_pars(all = TRUE), #need this for loo comparison
    control = list(adapt_delta = 0.97, max_treedepth = 19), #adapt_delta the target average proposal acceptance probability during Stan's adaptation period
    seed = seed
  )
  
  f4 <-brm(
    formula = bf(r ~ 1 + sex, sigma ~ sex),
    data = data,
    family = student(),
    cores = 4,
    chains = 4,
    thin = 10, #chop out transitions?
    warmup = 10000, #half of iterations
    iter = 20000,
    prior = c(
      prior(student_t(3, 0, 2.5), "Intercept"),
      prior(normal(0, 5), class = "b")
    ),
    save_pars = save_pars(all = TRUE), #need this for loo comparison
    control = list(adapt_delta = 0.97, max_treedepth = 19), #adapt_delta the target average proposal acceptance probability during Stan's adaptation period
    seed = seed
  )
  
  f5 <-brm(
    formula = bf(r ~ 1 + segment_width.z + sex),
    data = data,
    family = student(),
    cores = 4,
    chains = 4,
    thin = 10, #chop out transitions?
    warmup = 10000, #half of iterations
    iter = 20000,
    prior = c(
      prior(student_t(3, 0, 2.5), "Intercept"),
      prior(normal(0, 5), class = "b")
    ),
    save_pars = save_pars(all = TRUE), #need this for loo comparison
    control = list(adapt_delta = 0.97, max_treedepth = 19), #adapt_delta the target average proposal acceptance probability during Stan's adaptation period
    seed = seed
  )
  
  f6 <-brm(
    formula = bf(r ~ 1 + segment_count.z + sex),
    data = data,
    family = student(),
    cores = 4,
    chains = 4,
    thin = 10, #chop out transitions?
    warmup = 10000, #half of iterations
    iter = 20000,
    prior = c(
      prior(student_t(3, 0, 2.5), "Intercept"),
      prior(normal(0, 5), class = "b")
    ),
    save_pars = save_pars(all = TRUE), #need this for loo comparison
    control = list(adapt_delta = 0.97, max_treedepth = 19), #adapt_delta the target average proposal acceptance probability during Stan's adaptation period
    seed = seed
  )
  
  f7 <-brm(
    formula = bf(r ~ 1 + segment_width.z + sex + segment_width.z * sex),
    data = data,
    family = student(),
    cores = 4,
    chains = 4,
    thin = 10, #chop out transitions?
    warmup = 5000, #half of iterations
    iter = 10000,
    prior = c(
      prior(student_t(3, 0, 2.5), "Intercept"),
      prior(normal(0, 5), class = "b")
    ),
    save_pars = save_pars(all = TRUE), #need this for loo comparison
    control = list(adapt_delta = 0.97, max_treedepth = 19), #adapt_delta the target average proposal acceptance probability during Stan's adaptation period
    seed = seed
  )
  
  f8 <-brm(
    formula = bf(r ~ 1 + segment_count.z + sex + segment_count.z * sex),
    data = data,
    family = student(),
    cores = 4,
    chains = 4,
    thin = 10, #chop out transitions?
    warmup = 5000, #half of iterations
    iter = 10000,
    prior = c(
      prior(student_t(3, 0, 2.5), "Intercept"),
      prior(normal(0, 5), class = "b")
    ),
    save_pars = save_pars(all = TRUE), #need this for loo comparison
    control = list(adapt_delta = 0.97, max_treedepth = 19), #adapt_delta the target average proposal acceptance probability during Stan's adaptation period
    seed = seed
  )
  
  f9 <-brm(
    formula = bf(r ~ 1 + segment_count.z + segment_width.z + sex + segment_count.z * sex + segment_width.z*sex),
    data = data,
    family = student(),
    cores = 4,
    chains = 4,
    thin = 10, #chop out transitions?
    warmup = 5000, #half of iterations
    iter = 10000,
    prior = c(
      prior(student_t(3, 0, 2.5), "Intercept"),
      prior(normal(0, 5), class = "b")
    ),
    save_pars = save_pars(all = TRUE), #need this for loo comparison
    control = list(adapt_delta = 0.97, max_treedepth = 19), #adapt_delta the target average proposal acceptance probability during Stan's adaptation period
    seed = seed
  )
  
  
  
  #compare wil LOOIC
  m.comp<-LOO(f1, f2, f3, f4, f5,f6, f7, f8, f9, moment_match=T)
  
  #Get Bayes R^2
  br1<-bayes_R2(f1)
  br2<-bayes_R2(f2)
  br3<-bayes_R2(f3)
  br4<-bayes_R2(f4)
  br5<-bayes_R2(f5)
  br6<-bayes_R2(f6)
  br7<-bayes_R2(f7)
  br8<-bayes_R2(f8)
  br9<-bayes_R2(f9)
  br2L <- list(br1, br2, br3, br4, br5, br6, br7, br8, br9)
  
  t <- tibble(dataset = c(rep(name,times=9),"LOO_comp", "BayesR2"),
              model_set = c(1:9, "model_compare", "BayesR2"),
              m = list(f1, f2, f3, f4, f5,f6, f7, f8, f9, m.comp, br2L))
  
  assign(name, t, envir=globalenv())
}
