stan_files <- list.files(path = "models", pattern = "stan")
for (stan_file in stan_files){
  f <- tools::file_path_sans_ext(stan_file)
  fRDS <- paste0("models/", f, ".rds")
  fstan <- paste0("models/", stan_file)
  saveRDS(rstan::stan_model(file =fstan), fRDS)
}

stan_files <- list.files(path = "models/p_scale_priors/", pattern = "stan")
for (stan_file in stan_files){
  f <- tools::file_path_sans_ext(stan_file)
  fRDS <- paste0("models/p_scale_priors/", f, ".rds")
  fstan <- paste0("models/p_scale_priors/", stan_file)
  saveRDS(rstan::stan_model(file =fstan), fRDS)
}
