SROC_rjags <-function (X, n, model = "Bivariate", dataset = NULL,
          ref_std = NULL, title = "Summary plot", xlab = "Specificity",
          ylab = "Sensitivity", Se.range=c(0,1), Sp.range=c(0,1),
	  SROC_curve = FALSE, lwd.curve=3, col.curve="black", lty.curve="solid",
          cred_region = TRUE, predict_region = TRUE, region_level = 0.95, lty.cred.region = "solid",
          lty.predict.region = "dotted", trunc_low = 0.025, trunc_up = 0.025,
          col.predict.region = "black", col.cred.region = "red",
          lwd.cred.region =2.5, lwd.predict.region=2.5,
          study_col1 = "blue", study_col2 = rgb(0, 0, 1, 0.15), study_symbol="circles",
          summary.point=TRUE, pch.summary.point=19, cex.summary.point=3, col.summary.point=1)
{
  mcmcChain = as.matrix(X)
  min_t = 0
  max_t = 2 * pi
  dTt = 5e-05
  range_t = seq(min_t + dTt/2, max_t - dTt/2, dTt)
  if (model == "Bivariate") {
    Mean_C.Sample = mcmcChain[, "Mean_Sp"]
    Mean_S.Sample = mcmcChain[, "Mean_Se"]
    tau_S.Sample = mcmcChain[, "tau[1]"]
    tau_C.Sample = mcmcChain[, "tau[2]"]
    rho.Sample = mcmcChain[, "rho"]
    mu_S.Sample = mcmcChain[, "mu[1]"]
    mu_C.Sample = mcmcChain[, "mu[2]"]
    cov.Sample = rho.Sample * tau_S.Sample * tau_C.Sample
    S.Sample = C.Sample = numeric()
    for (i in 1:n) {
      s = mcmcChain[, paste("se[", i, "]",
                            sep = "")]
      S.Sample = cbind(S.Sample, s)
      c = mcmcChain[, paste("sp[", i, "]",
                            sep = "")]
      C.Sample = cbind(C.Sample, c)
    }
    hat_mu_A = median(mu_S.Sample)
    s_A = sd(mu_S.Sample)
    hat_sigma_A = median(tau_S.Sample)
    cos_fun_A = cos(range_t)
    hat_mu_B = median(mu_C.Sample)
    s_B = sd(mu_C.Sample)
    hat_sigma_B = median(tau_C.Sample)
    r = cor(mu_S.Sample, mu_C.Sample)
    hat_sigma_AB = median(rho.Sample)
    cos_fun_B = cos(range_t + acos(r))
    THETA.Sample = 0.5 * (sqrt(tau_C.Sample/tau_S.Sample) *
                            mu_S.Sample - sqrt(tau_S.Sample/tau_C.Sample) * (mu_C.Sample))
    LAMBDA.Sample = sqrt(tau_C.Sample/tau_S.Sample) * mu_S.Sample +
      sqrt(tau_S.Sample/tau_C.Sample) * (mu_C.Sample)
    sig.sq.theta = 0.5 * (tau_S.Sample * tau_C.Sample - cov.Sample)
    sig.sq.alpha = 2 * (tau_S.Sample * tau_C.Sample + cov.Sample)
    beta.Sample = log(tau_C.Sample/tau_S.Sample)
  }
  else {
    THETA.Sample = mcmcChain[, "THETA"]
    LAMBDA.Sample = mcmcChain[, "LAMBDA"]
    beta.Sample = mcmcChain[, "beta"]
    sigma_t.Sample = mcmcChain[, "tau[2]"]
    sigma_a.Sample = mcmcChain[, "tau[1]"]
    S.Sample = C.Sample = numeric()
    for (i in 1:n) {
      s = mcmcChain[, paste("se[", i, "]",
                            sep = "")]
      S.Sample = cbind(S.Sample, s)
      c = mcmcChain[, paste("sp[", i, "]",
                            sep = "")]
      C.Sample = cbind(C.Sample, c)
    }
    THETA_est = median(THETA.Sample)
    LAMBDA_est = median(LAMBDA.Sample)
    beta_est = median(beta.Sample)
    sigma_t_est = median(sigma_t.Sample)
    sigma_a_est = median(sigma_a.Sample)
    hat_mu_A = median(exp(-beta.Sample/2) * (THETA.Sample +
                                               LAMBDA.Sample/2))
    s_A = sd(exp(-beta.Sample/2) * (THETA.Sample + LAMBDA.Sample/2))
    hat_sigma_A = sqrt((exp(-beta_est) * (median((sigma_t.Sample)^2) +
                                            0.25 * median((sigma_a.Sample)^2))))
    cos_fun_A = cos(range_t)
    hat_mu_B = median(-exp(beta.Sample/2) * (THETA.Sample -
                                               LAMBDA.Sample/2))
    s_B = sd(-exp(beta.Sample/2) * (THETA.Sample - LAMBDA.Sample/2))
    hat_sigma_B = sqrt((exp(beta_est) * (median((sigma_t.Sample)^2) +
                                           0.25 * median((sigma_a.Sample)^2))))
    r = cor(exp(-beta.Sample/2) * (THETA.Sample + LAMBDA.Sample/2),
            -exp(beta.Sample/2) * (THETA.Sample - LAMBDA.Sample/2))
    hat_sigma_AB = -median((sigma_t.Sample)^2 - 0.25 * (sigma_a.Sample)^2)
    cos_fun_B = cos(range_t + acos(r))
  }
  main_title = "Plot"
  x.pos <- Sp.range[2]
  x.neg <- Sp.range[1]
  y.pos <- Se.range[2]
  y.neg <- Se.range[1]
  ext.x = 0.01
  ext.y = 0.01
  default.x = (range(x.neg - ext.x, x.pos + ext.x))
  default.y = range(y.neg, y.pos + ext.y)
  plot(x = default.x, y = default.y, type = "n", xlim = rev(range(default.x)),
       ylim = range(default.y), xlab = "", ylab = "",
       xaxs = "i", yaxs = "i", axes = FALSE, frame.plot = FALSE)
  title(xlab = "Specificity", ylab = "Sensitivity",
        cex.lab = 1.5)
  axis(1)
  axis(2)
  lines(x = c(x.pos + ext.x, x.pos + ext.x), y = c(y.neg +
                                                     ext.y, y.pos + ext.y))
  lines(x = c(x.pos + ext.x, x.neg - ext.x), y = c(y.neg, y.neg))
  lines(x = c(x.pos + ext.x, x.neg - ext.x), y = c(y.pos +
                                                     ext.y, y.pos + ext.y))
  lines(x = c(x.neg - ext.x, x.neg - ext.x), y = c(y.neg, y.pos +
                                                     ext.y))
  Scale_factor = 15
  if (is.null(dataset) == FALSE) {
    if (ref_std == TRUE) {
      TP <- dataset[, 1]
      FP <- dataset[, 2]
      FN <- dataset[, 3]
      TN <- dataset[, 4]
      pos <- TP + FN
      neg <- TN + FP
      crude_S = TP/pos
      crude_C = TN/neg
      total_sampsize = rowSums(cbind(pos, neg))
      if(study_symbol == "circles")
      {
        symbols(crude_C, crude_S, circles = rowSums(dataset),
                inches = 0.1 * Scale_factor/7, add = T, fg = study_col1,
                lwd = 2, bg = study_col2)
      }
      else
        {
          if(study_symbol == "squares")
          {
            symbols(crude_C, crude_S, squares = rowSums(dataset),
                    inches = 0.1 * Scale_factor/7, add = T, fg = study_col1,
                    lwd = 2, bg = study_col2)
          }
        }
    }
    else {
      Sens1 <- apply(S.Sample, 2, median)
      Spec1 <- apply(C.Sample, 2, median)
      if(study_symbol == "circles")
      {
        symbols(Spec1, Sens1, circles = rowSums(dataset),
                inches = 0.1 * Scale_factor/7, add = T, fg = study_col1,
                lwd = 2, bg = study_col2)
      }
      else
        {
          if(study_symbol == "squares")
          {
            symbols(Spec1, Sens1, squares = rowSums(dataset),
                    inches = 0.1 * Scale_factor/7, add = T, fg = study_col1,
                    lwd = 2, bg = study_col2)
          }
        }
    }
  }


  if(summary.point==TRUE) {

	if(model=="Bivariate") {
		Ov_S = median(Mean_S.Sample)
		Ov_C = median(Mean_C.Sample)
		points((Ov_C), Ov_S, pch = pch.summary.point, cex = cex.summary.point, col = col.summary.point)
	}
	else{
 		 Ov_S = median(1/(1 + exp((-(THETA.Sample) - 0.5 * (LAMBDA.Sample))/exp((beta.Sample)/2))))
 		 Ov_C = median(1/(1 + exp(((THETA.Sample) - 0.5 * (LAMBDA.Sample)) *exp((beta.Sample)/2))))
	}

  }


  if (SROC_curve == TRUE) {
    theta = log(S.Sample[, 1:n]/(1 - S.Sample[, 1:n])) *
      exp(beta.Sample/2) - LAMBDA.Sample/2
    min_TH = quantile(theta, trunc_low)
    max_TH = quantile(theta, 1 - trunc_up)
    dTH = 5e-05
    TH_range = seq(min_TH + dTH/2, max_TH - dTH/2, dTH)
    S_sroc = 1/(1 + exp((-TH_range - 0.5 * median(LAMBDA.Sample))/exp(median(beta.Sample)/2)))
    C_sroc = 1/(1 + exp((TH_range - 0.5 * median(LAMBDA.Sample)) *
                          exp(median(beta.Sample)/2)))
    lines((C_sroc), S_sroc, lwd = lwd.curve, col = col.curve, lty = lty.curve)
  }
  bound_cte = sqrt(qchisq(1 - region_level, 2, lower.tail = FALSE))
  if (predict_region == TRUE) {
    acos_value = (hat_sigma_AB+r*s_A*s_B)/(sqrt(s_A^2 + hat_sigma_A^2)+sqrt(s_B^2 + hat_sigma_B^2))
    logit_S_prediction = hat_mu_A + (sqrt(s_A^2 + hat_sigma_A^2)) *
      bound_cte * cos_fun_A
    logit_C_prediction = hat_mu_B + (sqrt(s_B^2 + hat_sigma_B^2)) *
      bound_cte * cos(range_t + acos(acos_value))
    S_prediction = 1/(1 + exp(-logit_S_prediction))
    C_prediction = 1/(1 + exp(-logit_C_prediction))
    lines(C_prediction, S_prediction, lwd = lwd.predict.region, lty = lty.predict.region,
          col = col.predict.region)
  }
  if (cred_region == TRUE) {
    logit_S_confidence = hat_mu_A + s_A * bound_cte * cos_fun_A
    logit_C_confidence = hat_mu_B + s_B * bound_cte * cos_fun_B
    S_confidence = 1/(1 + exp(-logit_S_confidence))
    C_confidence = 1/(1 + exp(-logit_C_confidence))
    lines(C_confidence, S_confidence, lwd = lwd.cred.region, lty = lty.cred.region,
          col = col.cred.region)
  }
}
