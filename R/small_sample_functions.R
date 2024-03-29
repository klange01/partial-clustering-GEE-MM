# Modified version of GEE.var.md() from the 'geesmv' package (Wang 2015, https://cran.r-project.org/web/packages/geesmv/index.html)

# The call to gee::gee() has been replaced with a call to geepack::geeglm().
# The references to the fitted values (beta_est), correlation parameter (alpha) and id (data$id) have been modified accordingly.
# This avoids the problem with gee::gee() causing R to crash if a dataset includes exactly 2 pairs,
# and is consistent with the use of geepack::geeglm() in the main simulation study.

GEE.var.md2 = function(formula, id, family = gaussian, data, corstr = "independence") {
  if (is.null(data$id)) {
    index <- which(names(data) == id)
    data$id <- data[, index]
  }
  init <- model.frame(formula, data)
  init$num <- 1:length(init[, 1])
  if (any(is.na(init))) {
    index <- na.omit(init)$num
    data <- data[index, ]
    m <- model.frame(formula, data)
    mt <- attr(m, "terms")
    data$response <- model.response(m, "numeric")
    mat <- as.data.frame(model.matrix(formula, m))
  }
  else {
    m <- model.frame(formula, data)
    mt <- attr(m, "terms")
    data$response <- model.response(m, "numeric")
    mat <- as.data.frame(model.matrix(formula, m))
  }
  gee.fit <- geepack::geeglm(formula, data = data, id = id, family = family, 
                 corstr = corstr)
  beta_est <- gee.fit$coefficients
  alpha <- summary(gee.fit)$corr[1, 1]
  len <- length(beta_est)
  len_vec <- len^2
  data$id <- as.numeric(levels(gee.fit$id))[gee.fit$id]
  cluster <- cluster.size(data$id)
  ncluster <- max(cluster$n)
  size <- cluster$m
  mat$subj <- rep(unique(data$id), cluster$n)
  if (is.character(corstr)) {
    var <- switch(corstr, independence = cormax.ind(ncluster), 
                  exchangeable = cormax.exch(ncluster, alpha) 
    )
  }
  else {
    print(corstr)
    stop("'working correlation structure' not recognized")
  }
  if (is.character(family)) {
    family <- switch(family, gaussian = "gaussian", binomial = "binomial", 
                     poisson = "poisson")
  }
  else {
    if (is.function(family)) {
      family <- family()[[1]]
    }
    else {
      print(family)
      stop("'family' not recognized")
    }
  }
  cov.beta <- unstr <- matrix(0, nrow = len, ncol = len)
  step11 <- matrix(0, nrow = len, ncol = len)
  for (i in 1:size) {
    y <- as.matrix(data$response[data$id == unique(data$id)[i]])
    covariate <- as.matrix(subset(mat[, -length(mat[1, ])], 
                                  mat$subj == unique(data$id)[i]))
    var_i = var[1:cluster$n[i], 1:cluster$n[i]]
    if (family == "gaussian") {
      xx <- t(covariate) %*% solve(var_i) %*% covariate
      step11 <- step11 + xx
    }
    else if (family == "poisson") {
      D <- mat.prod(covariate, exp(covariate %*% beta_est))
      Vi <- diag(sqrt(c(exp(covariate %*% beta_est))), 
                 cluster$n[i]) %*% var_i %*% diag(sqrt(c(exp(covariate %*% 
                                                               beta_est))), cluster$n[i])
      xx <- t(D) %*% solve(Vi) %*% D
      step11 <- step11 + xx
    }
    else if (family == "binomial") {
      D <- mat.prod(covariate, exp(covariate %*% beta_est)/((1 + 
                                                               exp(covariate %*% beta_est))^2))
      Vi <- diag(sqrt(c(exp(covariate %*% beta_est)/(1 + 
                                                       exp(covariate %*% beta_est))^2)), cluster$n[i]) %*% 
        var_i %*% diag(sqrt(c(exp(covariate %*% beta_est)/(1 + 
                                                             exp(covariate %*% beta_est))^2)), cluster$n[i])
      xx <- t(D) %*% solve(Vi) %*% D
      step11 <- step11 + xx
    }
  }
  step12 <- matrix(0, nrow = len, ncol = len)
  step13 <- matrix(0, nrow = len_vec, ncol = 1)
  step14 <- matrix(0, nrow = len_vec, ncol = len_vec)
  p <- matrix(0, nrow = len_vec, ncol = size)
  for (i in 1:size) {
    y <- as.matrix(data$response[data$id == unique(data$id)[i]])
    covariate <- as.matrix(subset(mat[, -length(mat[1, ])], 
                                  mat$subj == unique(data$id)[i]))
    var_i = var[1:cluster$n[i], 1:cluster$n[i]]
    if (family == "gaussian") {
      xy <- t(covariate) %*% solve(var_i) %*% solve(cormax.ind(cluster$n[i]) - 
                                                      covariate %*% solve(step11) %*% t(covariate) %*% 
                                                      solve(var_i)) %*% (y - covariate %*% beta_est)
      step12 <- step12 + xy %*% t(xy)
      step13 <- step13 + matrixcalc::vec(xy %*% t(xy))
      p[, i] <- matrixcalc::vec(xy %*% t(xy))
    }
    else if (family == "poisson") {
      D <- mat.prod(covariate, exp(covariate %*% beta_est))
      Vi <- diag(sqrt(c(exp(covariate %*% beta_est))), 
                 cluster$n[i]) %*% var_i %*% diag(sqrt(c(exp(covariate %*% 
                                                               beta_est))), cluster$n[i])
      xy <- t(D) %*% solve(Vi) %*% solve(cormax.ind(cluster$n[i]) - 
                                           D %*% solve(step11) %*% t(D) %*% solve(Vi)) %*% 
        (y - exp(covariate %*% beta_est))
      step12 <- step12 + xy %*% t(xy)
      step13 <- step13 + matrixcalc::vec(xy %*% t(xy))
      p[, i] <- matrixcalc::vec(xy %*% t(xy))
    }
    else if (family == "binomial") {
      D <- mat.prod(covariate, exp(covariate %*% beta_est)/((1 + 
                                                               exp(covariate %*% beta_est))^2))
      Vi <- diag(sqrt(c(exp(covariate %*% beta_est)/(1 + 
                                                       exp(covariate %*% beta_est))^2)), cluster$n[i]) %*% 
        var_i %*% diag(sqrt(c(exp(covariate %*% beta_est)/(1 + 
                                                             exp(covariate %*% beta_est))^2)), cluster$n[i])
      xy <- t(D) %*% solve(Vi) %*% solve(cormax.ind(cluster$n[i]) - 
                                           D %*% solve(step11) %*% t(D) %*% solve(Vi)) %*% 
        (y - exp(covariate %*% beta_est)/(1 + exp(covariate %*% 
                                                    beta_est)))
      step12 <- step12 + xy %*% t(xy)
      step13 <- step13 + matrixcalc::vec(xy %*% t(xy))
      p[, i] <- matrixcalc::vec(xy %*% t(xy))
    }
  }
  for (i in 1:size) {
    dif <- (p[, i] - step13/size) %*% t(p[, i] - step13/size)
    step14 <- step14 + dif
  }
  cov.beta <- solve(step11) %*% (step12) %*% solve(step11)
  cov.var <- size/(size - 1) * kronecker(solve(step11), solve(step11)) %*% 
    step14 %*% kronecker(solve(step11), solve(step11))
  return(list(cov.beta = diag(cov.beta), cov.var = cov.var))
}
