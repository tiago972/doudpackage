######### Weights (IPTW, matching) ##############################
# Name of the internal column that carries the weights through the analysis.
# It is added by descTab() and removed before the result is returned.
weightCol<-".doudpackage.weights"

# Turn the weights argument of descTab() into a numeric vector, or NULL.
# Accepted: a column name of data, a numeric vector, or an object holding a
# $weights element (matchit from MatchIt, weightit from WeightIt).
resolveWeights<-function(weights, data){
  if (is.null(weights))
    return(NULL)
  if (weightCol %in% colnames(data))
    stop(sprintf("data cannot have a column named %s, it is used internally", weightCol))
  source<-"weights"
  if (is.character(weights)){
    if (length(weights) != 1 || !weights %in% colnames(data))
      stop(sprintf("weights is not a variable of data: \"%s\" not in %s",
                   paste(weights, collapse = ", "), paste(colnames(data), collapse = ", ")))
    source<-weights
    weights<-data[, weights]
  }
  else if (inherits(weights, c("matchit", "weightit")) ||
           (is.list(weights) && !is.null(weights$weights))){
    source<-sprintf("the weights of the %s object", class(weights)[1])
    w<-weights$weights
    # WeightIt sampling weights multiply the balancing weights
    if (!is.null(weights$s.weights))
      w<-w * weights$s.weights
    weights<-w
  }
  if (!is.numeric(weights))
    stop(sprintf("weights must be a column name, a numeric vector or a matchit/weightit object, not %s",
                 paste(class(weights), collapse = "/")))
  weights<-as.vector(weights)
  if (length(weights) != nrow(data))
    stop(sprintf("%s has length %d but data has %d rows. With MatchIt/WeightIt, give the full data the model was fitted on, not match.data()",
                 source, length(weights), nrow(data)))
  if (any(!is.finite(weights)) || any(weights < 0))
    stop(sprintf("%s must be finite, non negative and without NA", source))
  if (sum(weights) == 0)
    stop(sprintf("%s are all zero", source))
  return(weights)
}

# Weights of a subset of data, or NULL when the table is not weighted
getWeights<-function(data){
  if (weightCol %in% colnames(data))
    return(data[, weightCol])
  return(NULL)
}

## Weighted statistics. With w all equal to 1 they give the usual estimates.
wMean<-function(x, w){
  keep<-!is.na(x)
  return(sum(x[keep] * w[keep]) / sum(w[keep]))
}

# Weighted variance with the reliability weights correction, as cobalt and
# Hmisc do: for matching weights of 0/1 it is the variance of the matched rows
wVar<-function(x, w){
  keep<-!is.na(x) & w > 0
  x<-x[keep]
  w<-w[keep]
  sw<-sum(w)
  m<-sum(x * w) / sw
  return(sum(w * (x - m)^2) / (sw - sum(w^2) / sw))
}

# Weighted quantile: inverse of the weighted empirical distribution function,
# averaging the two values when a probability falls exactly on a step
# (with weights all equal to 1, the median is the usual median)
wQuantile<-function(x, w, probs){
  keep<-!is.na(x) & w > 0
  x<-x[keep]
  w<-w[keep]
  o<-order(x)
  x<-x[o]
  cw<-cumsum(w[o]) / sum(w)
  eps<-sqrt(.Machine$double.eps)
  return(vapply(probs, function(p){
    i<-which(cw >= p - eps)[1]
    if (abs(cw[i] - p) < eps && i < length(x))
      return((x[i] + x[i + 1]) / 2)
    return(x[i])
  }, numeric(1)))
}

# Weighted counts of every level of a factor, then of the missing values,
# laid out as table(x, useNA = "always")
wCounts<-function(x, w){
  counts<-vapply(levels(x), function(l) sum(w[!is.na(x) & x == l]), numeric(1))
  return(c(counts, "NA" = sum(w[is.na(x)])))
}

######### Standardized mean differences ##############################
# SMD between two groups, following cobalt's "pooled" convention: means and
# proportions are weighted, the standard deviation of the denominator is the
# one of the unweighted sample, so that the denominator is the same before
# and after weighting.
smdNumeric<-function(x, g, w, lv){
  x1<-x[g == lv[1]]; w1<-w[g == lv[1]]
  x2<-x[g == lv[2]]; w2<-w[g == lv[2]]
  sd.pooled<-sqrt((stats::var(x1, na.rm = TRUE) + stats::var(x2, na.rm = TRUE)) / 2)
  return(abs(wMean(x1, w1) - wMean(x2, w2)) / sd.pooled)
}

# For a factor, the multivariate SMD of Yang and Dalton (2012), which is the
# usual SMD of a proportion for a binary factor
smdFactor<-function(x, g, w, lv){
  props<-function(sel, weighted){
    ww<-if (weighted) w[sel] else rep(1, sum(sel))
    xx<-x[sel]
    keep<-!is.na(xx)
    vapply(levels(x), function(l) sum(ww[keep & xx == l]) / sum(ww[keep]), numeric(1))
  }
  s1<-g == lv[1]; s2<-g == lv[2]
  # The first level is the reference: the proportions sum to 1
  diff<-(props(s1, TRUE) - props(s2, TRUE))[-1]
  covProp<-function(p){
    p<-p[-1]
    S<--outer(p, p)
    diag(S)<-p * (1 - p)
    return(S)
  }
  S<-(covProp(props(s1, FALSE)) + covProp(props(s2, FALSE))) / 2
  return(sqrt(as.numeric(t(diff) %*% solve(S) %*% diff)))
}

# SMD of a variable between the levels of group. With more than two levels, the
# largest of the pairwise SMDs is reported.
smdVar<-function(x, data, group, digits){
  w<-getWeights(data)
  if (is.null(w))
    w<-rep(1, nrow(data))
  g<-data[, group]
  pairs<-utils::combn(levels(g), 2, simplify = FALSE)
  fun<-if (x@type == "factor") smdFactor else smdNumeric
  failure<-NULL
  res<-tryCatch(withCallingHandlers(
    max(vapply(pairs, function(lv) fun(data[, x@name], g, w, lv), numeric(1))),
    warning = function(w) invokeRestart("muffleWarning")),
    error = function(e){
      failure<<-conditionMessage(e)
      return(NA_real_)
    })
  if (!is.null(failure) || !is.finite(res)){
    warning(sprintf("No SMD for %s%s", x@name,
                    if (is.null(failure)) "" else paste(":", failure)), call. = FALSE)
    return(NA_real_)
  }
  return(round(res, digits))
}

# Add a SMD column to the table, on the rows of each variable (not on the rows
# of the missing values)
addSmdColumn<-function(df, var_list, data, group, digits){
  vars<-Filter(function(x) x@name != group, var_list@List)
  smd<-vapply(vars, smdVar, numeric(1), data = data, group = group, digits = digits)
  names(smd)<-vapply(vars, function(x) x@name, character(1))
  row.var<-rowVarName(df$var, names(smd))
  missing.row<-grepl("Missing values", df$var, fixed = TRUE)
  df$SMD<-ifelse(is.na(row.var) | missing.row, NA_real_, smd[row.var])
  return(df)
}

######### Weighted tests (survey package) ##############################
surveyDesign<-function(data){
  if (!requireNamespace("survey", quietly = TRUE))
    stop("p values on weighted data need the survey package: install.packages(\"survey\"), or set pvalue = FALSE")
  data<-data[data[, weightCol] > 0, , drop = FALSE]
  return(survey::svydesign(ids = ~1, weights = stats::as.formula(paste0("~`", weightCol, "`")),
                           data = data))
}

bq<-function(name) paste0("`", name, "`")

surveyQuantiPvalue<-function(x, data, group){
  return(safePvalue(function(){
    design<-surveyDesign(data)
    f<-stats::as.formula(paste(bq(x@name), "~", bq(group)))
    if (isFALSE(x@normal))
      return(survey::svyranktest(f, design)$p.value)
    if (nlevels(data[, group]) == 2)
      return(survey::svyttest(f, design)$p.value)
    fit<-survey::svyglm(f, design)
    return(survey::regTermTest(fit, stats::as.formula(paste("~", bq(group))))$p)
  }, x@name, if (isFALSE(x@normal)) "svyranktest" else "svyttest/svyglm"))
}

surveyQualiPvalue<-function(x, data, group){
  return(safePvalue(function(){
    design<-surveyDesign(data)
    f<-stats::as.formula(paste("~", bq(x@name), "+", bq(group)))
    return(survey::svychisq(f, design)$p.value)
  }, x@name, "svychisq"))
}
