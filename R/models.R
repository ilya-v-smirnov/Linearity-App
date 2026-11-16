#' Calibration Models
#'
#' * `LRM`: linear model y = b*x + c;
#' * `QRM`: quadratic model y = a*x^2 + b*x + c;
#'   both on log10 scales (x = log10 conc, y = log10 OD) unless `linear = TRUE`;
#' * `4PL`: four-parameter logistic y = d + (a - d) / (1 + (x/c)^b);
#' * `5PL`: five-parameter logistic y = d + (a - d) / (1 + (x/c)^b)^g;
#'   both on the original OD scale.
#'
#' In the logistic models `a` is the response at zero concentration, `d` at
#' infinite concentration, `c` the inflection point (EC50), `b` the slope and
#' `g` the asymmetry.
#'
#' @return Character vector of model codes.
#' @export
calibration_models <- function() c('LRM', 'QRM', '4PL', '5PL')


#' Weighting Schemes
#'
#' * `none`: unweighted least squares;
#' * `1/y`, `1/y2`: weights 1/OD and 1/OD^2 (mean OD of the level);
#' * `1/x`, `1/x2`: weights 1/conc and 1/conc^2;
#' * `power`: 1/OD^theta, where theta is estimated from the variances of
#'   replicates (variance function var = k * mean^theta).
#'
#' Weights are normalised to a mean of 1.
#'
#' @return Character vector of weighting codes.
#' @export
weighting_schemes <- function() c('none', '1/y', '1/y2', '1/x', '1/x2', 'power')


#' Number of Parameters of a Model
#' @keywords internal
n_params <- function(model) c(LRM = 2, QRM = 3, '4PL' = 4, '5PL' = 5)[[model]]


#' Weight Function for a Calibration
#'
#' @param weights Weighting code (see [weighting_schemes()]).
#' @param x,y Concentrations and ODs of the standards used in the fit.
#'
#' @return A list with `fun(x, y)` returning unnormalised weights, `theta`
#'   (for `power`) and `notes`.
#' @keywords internal
weight_function <- function(weights, x, y) {
    if (!weights %in% weighting_schemes()) lin_abort('error.weights_unknown', weights = weights)
    notes <- list()
    level_mean <- stats::ave(y, x)
    nonzero <- abs(level_mean[!is.na(level_mean) & level_mean != 0])
    floor_y <- if (length(nonzero) > 0) min(nonzero) else 1
    pos <- function(v) pmax(abs(v), floor_y)
    theta <- NA_real_

    if (weights == 'power') {
        groups <- split(y, x)
        stats <- do.call(rbind, lapply(groups, function(g) {
            if (length(g) < 2) return(NULL)
            data.frame(m = mean(g), v = stats::var(g))
        }))
        ok <- !is.null(stats) && sum(stats$m > 0 & stats$v > 0) >= 3
        if (ok) {
            stats <- stats[stats$m > 0 & stats$v > 0, ]
            theta <- unname(coef(lm(log(stats$v) ~ log(stats$m)))[2])
            theta <- min(max(theta, 0), 4)
        } else {
            theta <- 0
            notes <- list(lin_message('note.power_fallback'))
        }
    }

    fun <- switch(weights,
                  'none' = function(x, y) rep(1, length(x)),
                  '1/y' = function(x, y) 1 / pos(y),
                  '1/y2' = function(x, y) 1 / pos(y)^2,
                  '1/x' = function(x, y) 1 / x,
                  '1/x2' = function(x, y) 1 / x^2,
                  'power' = function(x, y) 1 / pos(y)^theta)
    list(fun = fun, theta = theta, notes = notes, level_mean = level_mean)
}


#' Response Function of a Fitted Model
#'
#' @param fit A `lin_fit` object.
#' @param x Concentrations.
#' @param par Internal parameter vector (defaults to the estimates).
#'
#' @return Predicted OD on the original scale.
#' @keywords internal
curve_fun <- function(fit, x, par = fit$par) {
    switch(fit$model,
           'LRM' = ,
           'QRM' = {
               u <- if (fit$log_scale) log10(x) else x
               a <- if (fit$model == 'QRM') par[['a']] else 0
               v <- a * u^2 + par[['b']] * u + par[['c']]
               if (fit$log_scale) 10^v else v
           },
           '4PL' = par[['d']] + (par[['a']] - par[['d']]) /
               (1 + exp(par[['b']] * (log(x) - par[['lc']]))),
           '5PL' = par[['d']] + (par[['a']] - par[['d']]) /
               (1 + exp(par[['b']] * (log(x) - par[['lc']])))^par[['g']])
}


#' Concentration for a Given OD (Inverse of the Curve)
#'
#' For the quadratic model the root on the branch of the parabola that
#' contains the standards is returned. ODs outside the range of the logistic
#' asymptotes have no solution.
#'
#' @param fit A `lin_fit` object.
#' @param od Background-subtracted OD.
#' @param par Internal parameter vector.
#'
#' @return Concentrations (`NA` where there is no solution or OD <= 0 on log scales).
#' @keywords internal
inverse_fun <- function(fit, od, par = fit$par) {
    out <- rep(NA_real_, length(od))
    if (fit$model %in% c('LRM', 'QRM')) {
        ok <- !is.na(od) & (!fit$log_scale | od > 0)
        y <- if (fit$log_scale) log10(od[ok]) else od[ok]
        a <- if (fit$model == 'QRM') par[['a']] else 0
        if (a == 0) {
            u <- (y - par[['c']]) / par[['b']]
        } else {
            x_lim <- if (fit$log_scale) log10(fit$conc_range) else fit$conc_range
            disc <- par[['b']]^2 - 4 * a * (par[['c']] - y)
            vertex <- -par[['b']] / (2 * a)
            side <- if (mean(x_lim) >= vertex) 1 else -1
            u <- ifelse(disc >= 0, vertex + side * sqrt(pmax(disc, 0)) / (2 * abs(a)), NA_real_)
        }
        out[ok] <- if (fit$log_scale) 10^u else u
        # A flat line (zero slope) has no inverse
        out[!is.finite(out)] <- NA_real_
        return(out)
    }

    ok <- !is.na(od)
    ratio <- (par[['a']] - par[['d']]) / (od[ok] - par[['d']])
    if (fit$model == '5PL') ratio <- ratio^(1 / par[['g']])
    inner <- ratio - 1
    val <- rep(NA_real_, length(inner))
    good <- !is.na(inner) & is.finite(inner) & inner > 0
    val[good] <- exp(par[['lc']] + log(inner[good]) / par[['b']])
    out[ok] <- val
    out
}


#' Starting Values for the Logistic Models
#'
#' @param x,y Concentrations and ODs.
#' @param extend How far beyond the observed range the asymptotes start,
#'   as a fraction of the OD span.
#'
#' @return Named vector `a`, `b`, `lc`, `d`.
#' @keywords internal
start_4pl <- function(x, y, extend = 0.1) {
    means <- stats::aggregate(y, list(lx = log(x)), mean)
    means <- means[order(means$lx), ]
    y_lo <- means$x[1]
    y_hi <- means$x[nrow(means)]
    span <- y_hi - y_lo
    a0 <- y_lo - extend * span
    d0 <- y_hi + extend * span
    z <- suppressWarnings(log((a0 - means$x) / (means$x - d0)))
    ok <- is.finite(z)
    if (sum(ok) >= 2) {
        cf <- coef(lm(z[ok] ~ means$lx[ok]))
        b0 <- unname(cf[2])
        lc0 <- if (abs(b0) > 1e-8) unname(-cf[1] / b0) else mean(means$lx)
    } else {
        b0 <- sign(span)
        lc0 <- mean(means$lx)
    }
    if (!is.finite(b0) || abs(b0) < 1e-3) b0 <- sign(span) + (span == 0)
    c(a = a0, b = b0, lc = lc0, d = d0)
}


#' Fit a Logistic Model with Several Starting Points
#'
#' Minimises the weighted residual sum of squares with the Levenberg-Marquardt
#' algorithm (`minpack.lm::nls.lm`) from several starting points and returns
#' the best solution. `nls.lm` is used directly (not `nlsLM`), because
#' building an `nls` object fails when the parameters are not identifiable
#' (e.g. the upper asymptote is not reached), although the curve itself is
#' well determined.
#'
#' @param model `"4PL"` or `"5PL"`.
#' @param x,y,w Concentrations, ODs and weights.
#'
#' @return A list with `par` (named internal parameters) and `rss` (weighted
#'   residual sum of squares), or `NULL` if no start converged.
#' @keywords internal
fit_logistic <- function(model, x, y, w) {
    shape <- list(model = model, log_scale = FALSE)
    ctrl <- minpack.lm::nls.lm.control(maxiter = 500)

    try_fit <- function(start, lower = NULL) {
        res <- tryCatch(
            minpack.lm::nls.lm(par = start, lower = lower, control = ctrl,
                               fn = function(p) sqrt(w) * (y - curve_fun(shape, x, p))),
            error = function(e) NULL)
        if (is.null(res)) return(NULL)
        par <- unlist(res$par)
        names(par) <- names(start)
        rss <- sum(res$fvec^2)
        if (!all(is.finite(par)) || !is.finite(rss)) return(NULL)
        list(par = par, rss = rss)
    }
    best_of <- function(fits) {
        fits <- Filter(Negate(is.null), fits)
        if (length(fits) == 0) return(NULL)
        fits[[which.min(vapply(fits, function(f) f$rss, 0))]]
    }

    fits4 <- lapply(c(0.1, 0.5, 1, 3), function(e) try_fit(start_4pl(x, y, e)))
    best4 <- best_of(fits4)
    if (model == '4PL') return(best4)

    lower <- c(a = -Inf, b = -Inf, lc = -Inf, d = -Inf, g = 0.05)
    starts <- list(c(start_4pl(x, y, 0.1), g = 1))
    if (!is.null(best4)) starts <- c(list(c(best4$par, g = 1)), starts)
    best_of(lapply(starts, function(s) try_fit(s, lower)))
}


#' Covariance Matrix of Logistic Parameters
#'
#' sigma^2 (J' W J)^-1 with the Jacobian J of the curve computed numerically.
#' Parameters are considered not identifiable from the standards when the
#' condition number of J' W J exceeds 1e10 (typically the upper asymptote is
#' not reached; the standard error of log EC50 is then > 10). If the matrix
#' cannot be inverted at all, a pseudo-inverse is used (`exact = FALSE`) so
#' that concentrations still get confidence intervals.
#'
#' @param model `"4PL"` or `"5PL"`.
#' @param par Internal parameters.
#' @param x,w Concentrations and weights.
#' @param sigma Residual standard error.
#'
#' @return A list with `vcov`, `identifiable` and `exact`.
#' @keywords internal
logistic_vcov <- function(model, par, x, w, sigma) {
    shape <- list(model = model, log_scale = FALSE)
    jac <- vapply(seq_along(par), function(i) {
        h <- 1e-6 * max(1, abs(par[[i]]))
        up <- par
        dn <- par
        up[i] <- par[i] + h
        dn[i] <- par[i] - h
        (curve_fun(shape, x, up) - curve_fun(shape, x, dn)) / (2 * h)
    }, numeric(length(x)))
    jac <- matrix(jac, nrow = length(x))
    info <- crossprod(jac * sqrt(w))
    inv <- tryCatch(solve(info), error = function(e) NULL)
    exact <- !is.null(inv) && all(is.finite(inv)) && all(diag(inv) > 0)
    identifiable <- exact && kappa(info, exact = TRUE) < 1e10
    if (!exact) {
        s <- svd(info)
        keep <- s$d > max(s$d) * 1e-12
        inv <- s$v[, keep, drop = FALSE] %*% (t(s$u[, keep, drop = FALSE]) / s$d[keep])
    }
    vc <- sigma^2 * inv
    dimnames(vc) <- list(names(par), names(par))
    list(vcov = vc, identifiable = identifiable, exact = exact)
}


#' Fit a Calibration Curve
#'
#' @param standard_data Background-subtracted standards with `conc` and `OD`.
#' @param model Model code (see [calibration_models()]).
#' @param linear For LRM and QRM: fit on untransformed values instead of log10 scales.
#' @param weights Weighting code (see [weighting_schemes()]).
#'
#' @return A `lin_fit` object: list with `model`, `log_scale`, `weights`,
#'   `par` (internal parameters), `vcov`, `df`, `sigma`, `rss` (weighted
#'   residual sum of squares), `loglik`, `x`, `y`, `w`, `conc_range`,
#'   `weight_fun`, `w_scale`, `theta`, `notes` and, for LRM and QRM, the
#'   underlying `lm` object in `object`.
#' @export
fit_curve <- function(standard_data, model = 'LRM', linear = FALSE, weights = 'none') {
    x <- standard_data$conc
    y <- standard_data$OD
    log_scale <- model %in% c('LRM', 'QRM') && !linear

    wf <- weight_function(weights, x, y)
    w_raw <- wf$fun(x, wf$level_mean)
    w_scale <- mean(w_raw)
    w <- w_raw / w_scale

    if (model %in% c('LRM', 'QRM')) {
        df <- data.frame(u = if (log_scale) log10(x) else x,
                         v = if (log_scale) log10(y) else y,
                         w = w)
        formula <- if (model == 'LRM') v ~ u else v ~ I(u^2) + u
        object <- lm(formula, data = df, weights = w)
        cf <- coef(object)
        par <- c(a = if (model == 'QRM') unname(cf[2]) else 0,
                 b = unname(cf[if (model == 'QRM') 3 else 2]), c = unname(cf[1]))
        names_map <- if (model == 'QRM') c('c', 'a', 'b') else c('c', 'b')
        # summary.lm warns about essentially perfect fits (synthetic data); the matrix is still valid
        vc <- suppressWarnings(stats::vcov(object))
        dimnames(vc) <- list(names_map, names_map)
        if (model == 'LRM') {
            par <- par[c('b', 'c')]
        }
        vc <- vc[names(par), names(par)]
        df_res <- stats::df.residual(object)
        sigma <- stats::sigma(object)
        rss <- stats::deviance(object)
        loglik <- as.numeric(stats::logLik(object))
        vcov_exact <- TRUE
        notes <- wf$notes
    } else {
        best <- fit_logistic(model, x, y, w)
        if (is.null(best)) lin_abort('error.fit_failed', model = model)
        object <- NULL
        par <- best$par
        rss <- best$rss
        n <- length(y)
        df_res <- n - length(par)
        sigma <- sqrt(rss / max(df_res, 1))
        # Gaussian log-likelihood of a weighted least-squares fit (as logLik.nls)
        loglik <- 0.5 * (sum(log(w)) - n * (log(2 * pi) + 1 - log(n) + log(rss)))
        cov_info <- logistic_vcov(model, par, x, w, sigma)
        vc <- cov_info$vcov
        vcov_exact <- cov_info$exact
        notes <- wf$notes
        if (!cov_info$identifiable) {
            notes <- c(notes, list(lin_message('note.par_unidentifiable', model = model)))
        }
    }

    fit <- structure(list(model = model, log_scale = log_scale, weights = weights,
                          par = par, vcov = vc, vcov_exact = vcov_exact, df = df_res,
                          sigma = sigma, rss = rss,
                          loglik = loglik, x = x, y = y, w = w,
                          conc_range = range(x), weight_fun = wf$fun, w_scale = w_scale,
                          theta = wf$theta, notes = notes, object = object),
                     class = 'lin_fit')
    fit
}


#' Transform OD to the Scale of the Fit
#' @keywords internal
fit_scale <- function(fit, od) if (fit$log_scale) log10(od) else od


#' Residuals of the Fit
#'
#' @param fit A `lin_fit` object.
#'
#' @return A data.frame with `fitted` (OD), `resid` (scale of the fit) and
#'   `wresid` (weighted residuals).
#' @keywords internal
fit_residuals <- function(fit) {
    fitted <- curve_fun(fit, fit$x)
    resid <- fit_scale(fit, fit$y) - fit_scale(fit, fitted)
    data.frame(conc = fit$x, fitted = fitted, resid = resid, wresid = sqrt(fit$w) * resid)
}


#' Numerical Gradient
#' @keywords internal
num_grad <- function(f, p) {
    vapply(seq_along(p), function(i) {
        h <- 1e-6 * max(1, abs(p[i]))
        up <- p
        dn <- p
        up[i] <- p[i] + h
        dn[i] <- p[i] - h
        (f(up) - f(dn)) / (2 * h)
    }, numeric(1))
}


#' Inverse Prediction with Confidence Intervals
#'
#' The standard error of log(concentration) is obtained with the delta method
#' from the uncertainty of the curve parameters and of a single OD
#' measurement (residual variance divided by the weight of that OD).
#'
#' @param fit A `lin_fit` object.
#' @param od Background-subtracted ODs.
#' @param level Confidence level.
#'
#' @return A data.frame with `conc`, `lower`, `upper` and `cv` (%, from the
#'   standard error of log concentration).
#' @export
inverse_predict_ci <- function(fit, od, level = 0.95) {
    conc <- inverse_fun(fit, od)
    out <- data.frame(conc = conc, lower = NA_real_, upper = NA_real_, cv = NA_real_)
    t_q <- stats::qt(1 - (1 - level) / 2, max(fit$df, 1))
    for (i in which(!is.na(conc) & conc > 0)) {
        g_par <- num_grad(function(p) log(inverse_fun(fit, od[i], p)), fit$par)
        y_fit <- fit_scale(fit, od[i])
        g_y <- num_grad(function(v) {
            o <- if (fit$log_scale) 10^v else v
            log(inverse_fun(fit, o))
        }, y_fit)
        w_new <- fit$weight_fun(conc[i], od[i]) / fit$w_scale
        var_log <- drop(t(g_par) %*% fit$vcov %*% g_par) + g_y^2 * fit$sigma^2 / w_new
        if (!is.finite(var_log) || var_log < 0) next
        se <- sqrt(var_log)
        out$lower[i] <- conc[i] * exp(-t_q * se)
        out$upper[i] <- conc[i] * exp(t_q * se)
        out$cv[i] <- 100 * sqrt(exp(var_log) - 1)
    }
    out
}


#' Coefficients and Confidence Intervals of a Fit
#' @keywords internal
coef_table_fit <- function(fit, level = 0.95) {
    se <- sqrt(pmax(diag(fit$vcov), 0))
    t_q <- stats::qt(1 - (1 - level) / 2, max(fit$df, 1))
    est <- fit$par
    lower <- est - t_q * se
    upper <- est + t_q * se
    if (isFALSE(fit$vcov_exact)) {
        # pseudo-inverse: the intervals of the parameters would be misleadingly narrow
        lower[] <- NA_real_
        upper[] <- NA_real_
    }
    if ('lc' %in% names(est)) {
        i <- which(names(est) == 'lc')
        est[i] <- exp(est[i])
        lower[i] <- exp(lower[i])
        upper[i] <- exp(upper[i])
        names(est)[i] <- 'c'
    }
    tab <- data.frame(coef = names(est), est = unname(est),
                      ci.lower = unname(lower), ci.upper = unname(upper))
    tab[order(tab$coef), ]
}


#' Lack-of-Fit F Test
#'
#' Compares the residual variance with the variance of replicates (pure error).
#'
#' @param fit A `lin_fit` object.
#'
#' @return A list with `F`, `df1`, `df2` and `p` (`NA` without replicates).
#' @keywords internal
lack_of_fit <- function(fit) {
    v <- fit_scale(fit, fit$y)
    r <- fit_residuals(fit)$resid
    rss <- sum(fit$w * r^2)
    groups <- split(seq_along(v), fit$x)
    ss_pe <- sum(vapply(groups, function(i) {
        m <- stats::weighted.mean(v[i], fit$w[i])
        sum(fit$w[i] * (v[i] - m)^2)
    }, 0))
    m <- length(groups)
    n <- length(v)
    p <- length(fit$par)
    df1 <- m - p
    df2 <- n - m
    if (df1 <= 0 || df2 <= 0 || ss_pe <= 0) return(list(F = NA_real_, df1 = df1, df2 = df2, p = NA_real_))
    f_stat <- ((rss - ss_pe) / df1) / (ss_pe / df2)
    list(F = f_stat, df1 = df1, df2 = df2, p = stats::pf(f_stat, df1, df2, lower.tail = FALSE))
}


#' Back-Calculated Concentrations of the Standards
#'
#' @param fit A `lin_fit` object.
#'
#' @return A data.frame with `conc` (nominal), `OD`, `calc` and `recovery` (%).
#' @export
back_calculate <- function(fit) {
    calc <- inverse_fun(fit, fit$y)
    data.frame(conc = fit$x, OD = fit$y, calc = calc, recovery = 100 * calc / fit$x)
}


#' Information Criteria on the OD Scale
#'
#' For models fitted on log10(OD) the log-likelihood is converted to the OD
#' scale (Jacobian of the transformation), so that AIC and BIC are comparable
#' between log-log and logistic models.
#'
#' @param fit A `lin_fit` object.
#'
#' @return Named vector `AIC`, `BIC`.
#' @keywords internal
information_criteria <- function(fit) {
    ll <- fit$loglik
    k <- length(fit$par) + 1
    if (fit$log_scale) ll <- ll - sum(log(fit$y * log(10)))
    n <- length(fit$y)
    c(AIC = -2 * ll + 2 * k, BIC = -2 * ll + log(n) * k)
}


#' Compare Calibration Models
#'
#' Fits all models to the same standards with the same weighting and reports
#' the number of parameters, AIC and BIC on the OD scale, R-squared (OD scale),
#' the mean and maximum absolute error of back-calculated standards (%) and the
#' lack-of-fit p-value. The recommended model has the lowest AIC.
#'
#' @param standard_data Background-subtracted standards used in the fit.
#' @param linear,weights See [fit_curve()].
#'
#' @return A data.frame with one row per model; models that cannot be fitted
#'   have `NA` statistics.
#' @export
compare_models <- function(standard_data, linear = FALSE, weights = 'none') {
    rows <- lapply(calibration_models(), function(m) {
        empty <- data.frame(model = m, n.par = n_params(m), AIC = NA_real_, BIC = NA_real_,
                            R2 = NA_real_, mean.abs.error = NA_real_, max.abs.error = NA_real_,
                            lof.p = NA_real_)
        std <- standard_data
        if (m %in% c('LRM', 'QRM') && !linear) std <- std[std$OD > 0, ]
        if (length(unique(std$conc)) <= n_params(m)) return(empty)
        fit <- tryCatch(fit_curve(std, m, linear, weights), error = function(e) NULL)
        if (is.null(fit)) return(empty)
        ic <- information_criteria(fit)
        res <- fit$y - curve_fun(fit, fit$x)
        bc <- back_calculate(fit)
        err <- abs(bc$recovery - 100)
        empty$AIC <- ic[['AIC']]
        empty$BIC <- ic[['BIC']]
        empty$R2 <- 1 - sum(res^2) / sum((fit$y - mean(fit$y))^2)
        empty$mean.abs.error <- if (all(is.na(err))) NA_real_ else mean(err, na.rm = TRUE)
        empty$max.abs.error <- if (all(is.na(err))) NA_real_ else max(err, na.rm = TRUE)
        empty$lof.p <- lack_of_fit(fit)$p
        empty
    })
    out <- do.call(rbind, rows)
    best <- if (all(is.na(out$AIC))) NA_real_ else min(out$AIC, na.rm = TRUE)
    out$recommended <- !is.na(out$AIC) & out$AIC == best
    out
}
