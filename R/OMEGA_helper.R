# Flexible omega function (e.g. to use with loadings obtained by MacOrtho)------
.OMEGA_FLEX <- function(model = NULL, type = c("EFAtools", "psych"),
                        factor_corres = NULL,
                        var_names = NULL, fac_names = NULL, g_load = NULL,
                        s_load = NULL, u2 = NULL, cormat = NULL, pattern = NULL,
                        Phi = NULL, variance = c("correlaton", "sums_load")){

  if(inherits(model, "schmid")){

    pattern <- model$oblique
    Phi <- model$phi

    model <-  model$sl

    var_names <- rownames(model)
    g_load <- model[, 1]
    s_load <- model[, 2:(ncol(model) - 3)]
    factor_names <- c("g", 1:ncol(s_load))
    u2 <- model[, "u2"]

 } else if(inherits(model, "SL")){

    cormat <- model$orig_R

    model <-  model$sl
    var_names <- rownames(model)
    g_load <- model[, 1]
    s_load <- model[, 2:(ncol(model) - 2)]
    factor_names <- c("g", 1:ncol(s_load))
    u2 <- model[, "u2"]

  } else {

    factor_names <- c("g", 1:ncol(s_load))

  }

    if(variance == "correlation"){

      if(is.null(cormat)){

        if(is.null(Phi) | is.null(pattern)) {
          stop(crayon::red$bold(cli::symbol$circle_cross), crayon::red(" Either specify the cormat argument or the Phi and pattern arguments, or set variance to 'sums_load'\n"))

        } else {

          # Create the correlation matrix from the pattern coefficients and factor
          # intercorrelations
          cormat <- psych::factor.model(f = pattern, Phi = Phi, U2 = FALSE)

        }

      } else {

        # Check if it is a correlation matrix
        if(!.is_cormat(cormat)) {

          stop(crayon::red$bold(cli::symbol$circle_cross), crayon::red(" 'x' was not a correlation matrix. Check the cormat input, specify the Phi and pattern arguments instead, or set variance to 'sums_load'\n"))

        }

      }

    }

  # Check if input to factor_corres is correct
  checkmate::assert_numeric(factor_corres, null.ok = TRUE, len = nrow(g_load))

  # Create an input dataframe
  input <- data.frame(g_load, s_load)
  colnames(input) <- factor_names
  rownames(input) <- var_names

  if(type == "EFAtools" & is.null(factor_corres)){

    stop(crayon::red$bold(cli::symbol$circle_cross), crayon::red(" Either specify the factor_corres argument or set type = 'psych' to find variable-to-factor correspondences using the highest group factor loading per variable.\n"))

  }

  if(type == "psych"){

    if(variance != "correlation"){

      warning(crayon::yellow$bold("!"), crayon::yellow(" Variance is specified. Variance is used with value '", variance, "'. Results may differ from the specified type\n"))
      }

    if(is.null(factor_corres)){
      factor_corres <- apply(input, 1,
                             function(x) which.max(abs(x[2:(ncol(s_load) + 1)])))

    } else {

      warning(crayon::yellow$bold("!"), crayon::yellow(" Argument factor_corres is specified. Specified variable-to-factor correspondences are taken. To compute factor correspondences as done in psych, leave factor_corres = NULL.\n"))
    }
  }

  input$u2 <- u2
  input <- cbind(factor_corres, input)
  names(input)[1] <- "factor"

  # Create all sums of factor loadings for each factor

  # Sums of all group factor loadings for each group factor
  sums_s <- colSums(input[, 3:(ncol(s_load) + 2)])

  # Sum of all g loadings
  sum_g <- sum(input$g)

  # Sum of all error variances
  sum_e <- sum(input$u2)

  # Compute sums of error variances and g-loadings for group factors
  sums_e_s <- NULL
  sums_g_s <- NULL
  sums_s_s <- NULL

  for (i in 1:ncol(s_load)){
    sums_e_s[i] <- sum(input[input$factor == i, "u2"])
    sums_g_s[i] <- sum(input[input$factor == i, "g"])
    sums_s_s[i] <- sum(input[input$factor == i, i + 2])
  }

  if(variance == "correlation"){

    # Compute omega total, hierarchical, and subscale for g-factor
    omega_tot_g <- (sum(cormat) - sum_e) / sum(cormat)
    omega_h_g <- sum_g^2 / sum(cormat)
    omega_sub_g <- sum(sums_s_s^2) / sum(cormat)

    # Compute omega total, hierarchical, and subscale for group factors
    omega_tot_sub <- NULL
    omega_h_sub <- NULL
    omega_sub_sub <- NULL

    for (i in 1:ncol(s_load)) {
      subf <- which(factor_corres == i)
      Vgr <- sum(cormat[subf, subf])
      omega_sub_sub[i] <- sums_s_s[i]^2 / Vgr
      omega_h_sub[i] <- sums_g_s[i]^2 / Vgr
      omega_tot_sub[i] <- (sums_s_s[i]^2 + sums_g_s[i]^2) / Vgr
    }

  } else if(variance == "sums_load") {

    # Compute omega total, hierarchical, and subscale for g-factor
    omega_tot_g <- (sum_g^2 + sum(sums_s_s^2)) / (sum_g^2 + sum(sums_s_s^2) +
                                                    sum_e)
    omega_h_g <- sum_g^2 / (sum_g^2 + sum(sums_s^2) + sum_e)
    omega_sub_g <- sum(sums_s_s^2) / (sum_g^2 + sum(sums_s^2) + sum_e)

    # Compute omega total, hierarchical, and subscale for group factors
    omega_tot_sub <- (sums_g_s^2 + sums_s_s^2) / (sums_g_s^2 + sums_s_s^2 +
                                                         sums_e_s)
    omega_h_sub <- sums_g_s^2 / (sums_g_s^2 + sums_s_s^2 + sums_e_s)
    omega_sub_sub <- sums_s_s^2 / (sums_g_s^2 + sums_s_s^2 + sums_e_s)
  }

  # Combine and display results in a table
  omega_tot <- c(omega_tot_g, omega_tot_sub)
  omega_h <- c(omega_h_g, omega_h_sub)
  omega_sub <- c(omega_sub_g, omega_sub_sub)

  omegas <- cbind(omega_tot, omega_h, omega_sub)
  colnames(omegas) <- c("tot", "hier", "sub")

  if(!is.null(fac_names)){

    rownames(omegas) <- c("g", fac_names)

  } else {

    if(is.null(model)){

      rownames(omegas) <- c("g", 1:ncol(s_load))

    } else {

      rownames(omegas) <- c("g", colnames(model)[2:(ncol(s_load) + 1)])

    }
  }

  class(omegas) <- "OMEGA"

  return(omegas)

}


# Omega function to use with lavaan bifactor output as input-------

.OMEGA_LAVAAN <- function(model = NULL, g_name = "g", group_names = NULL){

  if(lavaan::lavInspect(model, what = "converged") == FALSE){
    stop(crayon::red$bold(cli::symbol$circle_cross), crayon::red(" Model did not converge. No omegas are computed.\n"))
  }

  std_sol <- lavaan::lavInspect(model, what = "std",
                                drop.list.single.group = FALSE)

  ## Create empty list objects for further processing
  g_load <- list()
  e_load <- list()
  sums_all <- list()
  sum_g <- list()
  sum_e <- list()
  sums_s <- list()
  sums_e_s <- list()
  sums_g_s <- list()
  omega_tot_g <- list()
  omega_h_g <- list()
  omega_sub_g <- list()
  omega_tot_sub <- list()
  omega_h_sub <- list()
  omega_sub_sub <- list()
  omega_tot <- list()
  omega_h <- list()
  omega_sub <- list()
  omegas <- list()

  if(is.null(group_names)){

    group_names <- names(std_sol)

  }

    for(i in 1:length(std_sol)){

    if(any(is.na(std_sol[[i]][["lambda"]]))){
      stop(crayon::red$bold(cli::symbol$circle_cross), crayon::red(" Some loadings are NA or NaN. No omegas are computed.\n"))
    }

    if(any(std_sol[[i]][["lambda"]] >= 1)){
      stop(crayon::red$bold(cli::symbol$circle_cross), crayon::red(" A Heywood case was detected (loading equal to or larger than 1). No omegas are computed.\n"))
    }

    if(ncol(std_sol[[i]][["lambda"]]) == 1){

      if(i == 1){

        message(cli::col_cyan(cli::symbol$info, " Model contained a single factor. Only omega total is returned.\n"))

      }

      fac_names <- colnames(std_sol[[1]][["lambda"]])
      var_names <- rownames(std_sol[[1]][["lambda"]])

      # Extract sum of factor loadings and error variances
      sum_g[[i]] <- sum(std_sol[[i]][["lambda"]][, fac_names])
      sum_e[[i]] <- sum(diag(std_sol[[i]][["theta"]]))

      # Compute omega
      omegas[[i]] <- (sum_g[[i]]^2) / (sum_g[[i]]^2 + sum_e[[i]])

    } else {

      if(i == 1){

      bi_check <- std_sol[[1]][["lambda"]]
      bi_check[abs(bi_check) > 0 + .Machine$double.eps * 100] <- 1

      if(all(rowSums(bi_check) < 2)){

        stop(crayon::red$bold(cli::symbol$circle_cross), crayon::red(" You did not fit a bifactor model. Omegas cannot be computed. Either provide a bifactor model or a model with a single factor.\n"))

      }

      if(!all(rowSums(bi_check) > 1)){

        message(cli::col_cyan(cli::symbol$info, " Some variables have less than two loadings. Did you really enter a bifactor model? Either provide a bifactor model or a model with a single factor.\n"))

      }

      }

      # Create list with factor and corresponding subtest names
      col_names <- colnames(std_sol[[1]][["lambda"]])
      col_names <- col_names[!col_names %in% g_name]
      fac_names <- c(g_name, col_names)

      var_names <- list()
      for(j in 1:(length(fac_names)-1)){
        temp0 <- abs(std_sol[[1]][["lambda"]][, j]) > 0 + .Machine$double.eps * 100
        var_names[[j]] <- names(temp0)[temp0]
      }

      # Create all sums of factor loadings for each factor
      sums_all[[i]] <- vector("double", length(fac_names))
      for (j in 1:length(fac_names)){
        sums_all[[i]][j] <- sum(std_sol[[i]][["lambda"]][, fac_names[j]])
      }

      # Extract g-loadings and error variances, sum of g-loadings for g and sum of
      # respective loadings for every factor
      g_load[[i]] <- std_sol[[i]][["lambda"]][, g_name]
      e_load[[i]] <- diag(std_sol[[i]][["theta"]])
      sum_e[[i]] <- sum(e_load[[i]])
      sum_g[[i]] <- sums_all[[i]][1]
      sums_s[[i]] <- sums_all[[i]][2:length(fac_names)]

      # Compute sums of error variances and g-loadings for group factors
      sums_e_s[[i]] <- vector("double", length(var_names))
      sums_g_s[[i]] <- vector("double", length(var_names))

      for (j in 1:length(var_names)){
        sums_e_s[[i]][j] <- sum(e_load[[i]][var_names[[j]]])
        sums_g_s[[i]][j] <- sum(g_load[[i]][var_names[[j]]])
      }

      # Compute omega total, hierarchical, and subscale for g-factor
      omega_tot_g[[i]] <- (sum_g[[i]]^2 + sum(sums_s[[i]]^2)) /
        (sum_g[[i]]^2 + sum(sums_s[[i]]^2) + sum_e[[i]])
      omega_h_g[[i]] <- sum_g[[i]]^2 / (sum_g[[i]]^2 + sum(sums_s[[i]]^2) +
                                          sum_e[[i]])
      omega_sub_g[[i]] <- sum(sums_s[[i]]^2) / (sum_g[[i]]^2 +
                                                  sum(sums_s[[i]]^2) + sum_e[[i]])

      # Compute omega total, hierarchical, and subscale for group factors
      omega_tot_sub[[i]] <- (sums_s[[i]]^2 + sums_g_s[[i]]^2) /
        (sums_g_s[[i]]^2 + sums_s[[i]]^2 + sums_e_s[[i]])
      omega_h_sub[[i]] <- sums_g_s[[i]]^2 / (sums_g_s[[i]]^2 + sums_s[[i]]^2 +
                                               sums_e_s[[i]])
      omega_sub_sub[[i]] <- sums_s[[i]]^2 / (sums_g_s[[i]]^2 + sums_s[[i]]^2 +
                                               sums_e_s[[i]])

      # Combine and display results in a table
      omega_tot[[i]] <- c(omega_tot_g[[i]], omega_tot_sub[[i]])
      omega_h[[i]] <- c(omega_h_g[[i]], omega_h_sub[[i]])
      omega_sub[[i]] <- c(omega_sub_g[[i]], omega_sub_sub[[i]])

      omegas[[i]] <- cbind(omega_tot[[i]], omega_h[[i]], omega_sub[[i]])
      colnames(omegas[[i]]) <- c("tot", "hier", "sub")

      rownames(omegas[[i]]) <- fac_names

  }

  }

  if(length(std_sol) > 1){

    names(omegas) <- group_names

  } else {

    omegas <- omegas[[1]]

  }

  class(omegas) <- "OMEGA"

  return(omegas)

}