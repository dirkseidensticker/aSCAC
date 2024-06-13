# Bayesian ceramic phase modelling

library(ggplot2)
library(tidyr)

library(rcarbon)
library(nimbleCarbon)

c14 <- data.table::fread(
  "https://raw.githubusercontent.com/dirkseidensticker/aDRAC/master/aDRAC.csv", 
  encoding = "UTF-8") %>%
  dplyr::mutate(C14AGE = as.numeric(C14AGE),
                C14STD = as.numeric(C14STD))

pottery <- data.table::fread("https://raw.githubusercontent.com/dirkseidensticker/aSCAC/master/potterygroups.csv", 
                             encoding = "UTF-8") %>%
  dplyr::select(-DESCRIPTION)

# manual check for styles being present in aSCAC pottery description list:
#c14 %>% dplyr::distinct(POTTERY) %>% dplyr::filter(!(POTTERY %in% c(pottery %>% dplyr::distinct(POTTERY) %>% dplyr::pull(POTTERY))))


id <- pottery$POTTERY
res.lst <- list()

for (i in 1:length(id)) {
  c14.sel <- c14 %>% 
    dplyr::filter(C14AGE  > 70) %>%
    dplyr::filter(grepl(id[i], POTTERY)) %>% # filter for dates related to style
    dplyr::filter(!grepl(paste0("\\(" , id[i], "\\)"), POTTERY)) # remove cases in parantheses
  
  n = nrow(c14.sel)
  
  if(n >= 2){
    print(paste0(i, "/", length(id), " - ", id[i], " (", n, " c14-dates)"))
    
    # Define NIMBLE Model
    phasemodel <- nimbleCode({
      for (i in 1:N){
        #  Likelihood
        theta[i] ~ dunif(alpha[1],alpha[2]);
        # Calibration
        mu[i] <- interpLin(z=theta[i], x=calBP[], y=C14BP[]);
        sigmaCurve[i] <- interpLin(z=theta[i], x=calBP[], y=C14err[]);
        sd[i] <- (sigma[i]^2+sigmaCurve[i]^2)^(1/2);
        X[i] ~ dnorm(mean=mu[i],sd=sd[i]);
      }
      # Prior
      alpha[1] ~ dunif(0,50000);
      alpha[2] ~ T(dunif(0,50000),alpha[1],50000)
    })
    
    #define constant, data, and inits:
    data("intcal20") 
    constants <- list(N = n,
                      calBP = intcal20$CalBP,
                      C14BP = intcal20$C14Age,
                      C14err = intcal20$C14Age.sigma)
    
    data <- list(X = c14.sel$C14AGE, 
                 sigma = c14.sel$C14STD)

    m.dates = rcarbon::medCal(rcarbon::calibrate(
      c14.sel$C14AGE,
      c14.sel$C14STD,
      verbose = FALSE))
    
    # important to keep enough 'space' before and after
    start <- floor(min(c14.sel$C14AGE)/1000) * 1000 - 1000
    if (start < 0) { start <- 0 } # avoid negative starts
    end <- ceiling(max(c14.sel$C14AGE)/1000) * 1000 + 1000
    
    inits <- list(alpha = c(start,
                            end),
                  theta = m.dates)
    
    #Run MCMC
    mcmc.samples <- nimbleMCMC(
      code = phasemodel,
      constants = constants,
      data = data,
      niter = 20000, 
      nchains = 1, 
      thin = 1, 
      nburnin = 5000, 
      progressBar = FALSE, 
      monitors = c('alpha','theta'), 
      inits = inits, 
      samplesAsCodaMCMC = TRUE, 
      set.seed(123), 
      summary = TRUE)
    
    start.dens <- density(matrix(mcmc.samples$samples[,'alpha[2]']))
    start.dens <- data.frame(bp = start.dens$x, 
                             prob = start.dens$y)
    
    start.dens$median <- data.frame(mcmc.samples$summary)[2, "Median"]
    start.dens$POTTERY <- id[i]
    start.dens$alpha <- "start"
    
    end.dens <- density(matrix(mcmc.samples$samples[,'alpha[1]']))
    end.dens <- data.frame(bp = end.dens$x, 
                           prob = end.dens$y)
    end.dens$median <- data.frame(mcmc.samples$summary)[1, "Median"]
    end.dens$POTTERY <- id[i]
    end.dens$alpha <- "end"
    
    res <- rbind(start.dens,
                 end.dens)
    
    res.lst[[i]] <- res
  }
}

res.bayes <- do.call(rbind, res.lst)

res.bayes.median <- res.bayes %>% 
  dplyr::distinct(median, POTTERY, alpha) %>%
  dplyr::mutate(bp = median, 
                prob = 0) %>%
  dplyr::select(names(res.bayes))

res.bayes

#dplyr::arrange(c14age) %>% 
#  dplyr::mutate_at(vars(LABNR), dplyr::funs(factor(., levels=unique(.)))) %>%

#arrange(persistent, score) %>% mutate(id = factor(id, levels=unique(id))) 

# define order of plotting (oldest start median)
order <- res.bayes.median %>%
  dplyr::filter(alpha == "start") %>%
  dplyr::arrange(dplyr::desc(median)) %>%
  dplyr::pull(POTTERY)

# plotting:
ggplot(res.bayes) + 
  ggridges::geom_ridgeline(
    aes(x = -bp + 1950, 
        #y = stats::reorder(POTTERY, 
        #                   median, 
        #                   decreasing = TRUE), 
        y = factor(POTTERY, levels = order),
        height = prob, 
        fill = alpha), 
    scale = 100, 
    color = NA) + 
  scale_x_continuous("cal BCE/CE", limits = c(-1500, 2000)) + 
  theme(axis.title.y = element_blank())
ggsave("BayesPhases.jpg", width = 6, height = 8)

# export medians
res.bayes %>% 
  dplyr::distinct(median, POTTERY, alpha) %>%
  dplyr::mutate(median = round(-median + 1950)) %>%
  reshape2::dcast(POTTERY ~ alpha, value.var = "median") %>%
  dplyr::select(POTTERY, start, end) %>%
  write.csv("BayesPhases.csv", row.names = F)
