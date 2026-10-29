# Assess normality of a numeric variable with a Shapiro-Wilk test.
# shapiro.test needs 3 to 5000 finite observations; outside that range we fall
# back on the safe assumption (non normal) rather than aborting.
assessNormality<-function(values, name, alpha = 0.05){
  values<-values[!is.na(values)]
  if (length(values) < 3 || length(values) > 5000 ||
      length(unique(values)) < 3)
    return(FALSE)
  p<-safePvalue(function() stats::shapiro.test(values)$p.value, name, "shapiro.test")
  if (is.na(p))
    return(FALSE)
  return(p > alpha)
}

# Function to get var types in a custom object list
varType<-function(data, normality, data.normality = data){
  list_names<-colnames(data)
  list_names<-list_names[order(list_names)]
  list_names<-as.list(list_names)
  type_list<-lapply(data, class)
  type_list<-type_list[order(names(type_list))]

  resolved<-mapply(resolveType, type_list, list_names)
  keep<-!is.na(resolved)
  if (any(!keep))
    warning(sprintf("Type unrecognised, variable(s) ignored: %s",
                    paste(sprintf("%s (%s)", unlist(list_names)[!keep],
                                  sapply(type_list[!keep], paste, collapse = "/")),
                          collapse = ", ")))
  if (!any(keep))
    stop(sprintf("No variable of a handled type (numeric, integer, factor) in data: %s",
                 paste(unlist(list_names), collapse = ", ")))
  list_names<-list_names[keep]
  type_list<-as.list(resolved[keep])

  if (normality == "assess")
    n<-mapply(function(name, type){
      if (type != "numeric")
        return(FALSE)
      return(assessNormality(data.normality[, name], name))
    }, list_names, type_list)
  else
    n<-rep(normality == "normal", length(list_names))

  list_res<-mapply(Var, list_names, type_list, normal = as.list(n))
  list_res<-methods::new("listVar", List = list_res)
  return(list_res)
}

#' Create a table of descriptive analysis of a dataset
#'
#' Displays together the univariate analysis (mean/median; SD/IQR; proportions)
#' and the bivariate analysis (t test/Wilcoxon, ANOVA/Kruskal-Wallis, Chi2 or
#' Fisher) of every variable of a dataset. The univariate analysis can be
#' sub-grouped by a variable of interest of n levels; the test applied to each
#' variable follows from its type and from `normality`.
#'
#' Only numeric, integer and factor (including ordered factor) variables are
#' described. A variable of any other type is ignored with a warning, as is a
#' variable whose test cannot be computed: its p value is then `NA` and the rest
#' of the table is still produced.
#'
#' @param data A dataset. Needs to be a data.frame or a tibble.
#' @param group Optional. The name of the factor variable to make sub-groups
#'   comparisons with; it must have at least two non empty levels. Omitted,
#'   `""` or `NULL`, the table has a single Total column. Rows with a missing
#'   value for it are dropped, and its unused levels are dropped, both with a
#'   warning.
#' @param quanti,quali,na.print,pvalue Logical. If false, won't display
#'   quantitative/qualitative/Missing values/pvalues variable results. `quanti`
#'   and `quali` cannot both be `FALSE`.
#' @param digits.p Integer. Significant digits for p value
#' @param digits.qt Integer. Significant digits for mean/median, SD/IQR
#' @param digits.ql Integer. Significant digits for proportions
#' @param normality One of "normal", "non normal" or "assess". "normal" applies
#'   the parametric tests (mean (SD), t test/ANOVA) to every quantitative
#'   variable, "non normal" the non parametric ones (median (IQR),
#'   Wilcoxon/Kruskal-Wallis), and "assess" decides variable by variable with a
#'   Shapiro-Wilk test at the 5% level.
#' @param parallel Logical. Make analysis using parallel from [parallel::mclapply()].
#' @param mc.cores If parallel is TRUE, how many cores to use. The default, 0,
#'   uses all the cores but one.
#' @param weights Optional. Weights to describe a weighted (pseudo-)population,
#'   typically after inverse probability of treatment weighting (IPTW) or
#'   matching. One of: the name of a numeric column of `data`, a numeric vector
#'   with one weight per row of `data`, or a `matchit` (MatchIt) or `weightit`
#'   (WeightIt) object, whose `$weights` are used. With MatchIt/WeightIt, give
#'   the full data the model was fitted on (not `match.data()`): unmatched rows
#'   have a weight of 0 and are not described. See Details.
#' @param smd Logical. Add a `SMD` column with the absolute standardized mean
#'   difference of every variable between the levels of `group`. Defaults to
#'   `TRUE` when `weights` is given. Needs a group.
#'
#' @details
#' **Weights.** With `weights`, means, standard deviations, medians, IQR,
#' counts and proportions are weighted, and so are the counts in the header of
#' [parseClassFun()]. Counts are then sums of weights, rounded to `digits.ql`.
#' The standard deviation uses the reliability weights correction, so that with
#' matching weights of 0/1 it is the usual standard deviation of the matched
#' sample. The normality assessment is not weighted.
#'
#' Balance after weighting or matching is assessed with standardized mean
#' differences rather than tests, so `pvalue` defaults to `FALSE` with weights.
#' Set `pvalue = TRUE` to get design based tests from the survey package
#' (Rao-Scott chi-squared `svychisq`, `svyttest` or a Wald test of `svyglm` for
#' more than two groups, `svyranktest` for non normal variables).
#'
#' **SMD.** For a quantitative variable,
#' \eqn{|m_1 - m_0| / \sqrt{(s_1^2 + s_0^2)/2}}; for a factor, the
#' multivariate SMD of Yang and Dalton (2012), which is the usual SMD of a
#' proportion for a binary factor. Means and proportions are weighted; the
#' standard deviations (or the variances of the proportions) of the denominator
#' are computed on the unweighted data, as cobalt does with
#' `s.d.denom = "pooled"`, so that the denominator is the same before and after
#' weighting. With more than two groups, the largest pairwise SMD is shown. It
#' is rounded to `digits.p`.
#'
#' @return An S4 object of class `parseClass`. Its `["table"]` element is a
#'   `data.frame` (whether or not there is a group, and also when `data` is a
#'   tibble) with a `var` column, one column per level of `group`, a `Total`
#'   column and, if there is a group and `pvalue = TRUE`, a `pvalue` column.
#' @seealso [parseClassFun()] to turn the result into an HTML/LaTeX table.
#' @export
#'
#' @examples
#' # A small simulated clinical trial
#' set.seed(42)
#' n <- 200
#' patients <- data.frame(
#'   arm      = factor(sample(c("Placebo", "Treatment"), n, replace = TRUE)),
#'   age      = round(rnorm(n, mean = 65, sd = 10)),
#'   crp      = round(rlnorm(n, meanlog = 2, sdlog = 1), 1),
#'   sex      = factor(sample(c("Female", "Male"), n, replace = TRUE)),
#'   diabetes = factor(sample(c("No", "Yes"), n, replace = TRUE, prob = c(0.7, 0.3))),
#'   nyha     = factor(sample(c("I", "II", "III", "IV"), n, replace = TRUE),
#'                     ordered = TRUE)
#' )
#' patients$crp[sample(n, 15)] <- NA
#'
#' # Compare the two arms. With normality = "assess", age (normal) is described
#' # by mean (SD) with a t test, crp (skewed) by median (IQR) with a Wilcoxon test
#' tab <- descTab(patients, group = "arm", normality = "assess", na.print = TRUE)
#' tab["table"]
#'
#' # No group: a single Total column, no test
#' descTab(patients)["table"]
#'
#' # Quantitative variables only, two decimals
#' descTab(patients, group = "arm", quali = FALSE, digits.qt = 2)["table"]
#'
#' # Render it (see parseClassFun() for the layout options)
#' parseClassFun(tab)
#'
#' # Inverse probability of treatment weighting (ATE weights): weighted
#' # description and SMD instead of p values
#' ps <- glm(arm ~ age + sex + diabetes, data = patients,
#'           family = binomial)$fitted.values
#' patients$iptw <- ifelse(patients$arm == "Treatment", 1 / ps, 1 / (1 - ps))
#' descTab(patients, group = "arm", weights = "iptw")["table"]
#'
#' # The same table with the SMD of the crude data, for comparison
#' descTab(patients, group = "arm", smd = TRUE)["table"]
#'
#' \donttest{
#' # From MatchIt: give the matchit object and the full data
#' if (requireNamespace("MatchIt", quietly = TRUE)) {
#'   m <- MatchIt::matchit(arm ~ age + sex + diabetes, data = patients)
#'   descTab(patients, group = "arm", weights = m)["table"]
#' }
#' }
descTab<-function(data, group="", quanti=TRUE, quali=TRUE, na.print = FALSE,
                      pvalue=is.null(weights), digits.p=3L, digits.qt = 1L,
                  digits.ql = 1L, normality = "normal", parallel = FALSE, mc.cores = 0,
                  weights = NULL, smd = !is.null(weights))
{
  
  if (is.null(group))
    group<-""
  data<-as.data.frame(data, stringsAsFactors = FALSE)
  checkVarDescTab(data, group, quanti, quali, na.print, pvalue, digits.p, digits.qt,
           digits.ql, normality, parallel, mc.cores)
  if (!is.logical(smd) || length(smd) != 1 || is.na(smd))
    stop(sprintf("smd must be TRUE or FALSE, not %s", paste(smd, collapse = ", ")))

  if (isTRUE(parallel) && mc.cores == 0)
    mc.cores = parallel::detectCores() - 1
  # The weights travel with the rows in an internal column, so that they
  # follow every subset of the data
  w<-resolveWeights(weights, data)
  weighted<-!is.null(w)
  if (weighted)
    data[, weightCol]<-w
  data<-checkData(data, group)
  if (weighted && group != ""){
    w.group<-tapply(data[, weightCol], data[, group], sum)
    if (any(w.group == 0))
      stop(sprintf("Every level of %s needs a positive sum of weights: %s",
                   group, paste(names(w.group)[w.group == 0], collapse = ", ")))
  }
  # Neither the weights nor the column they come from are described
  described<-setdiff(colnames(data), c(weightCol, if (is.character(weights)) weights))
  data.normality<-if (weighted) data[data[, weightCol] > 0, , drop = FALSE] else data
  var_list<-varType(data[, described, drop = FALSE], normality,
                    data.normality = data.normality)
  test<-if (!weighted) "classic" else if (isTRUE(pvalue)) "survey" else "none"
  ana.biv_list<-anaBiv(var_list, parallel, data = data, group = group,
                       digits.p = digits.p, mc.cores = mc.cores, test = test)
  if (is.null(ana.biv_list))
    ana.biv_list<-lapply(var_list@List, methods::as, Class = "VarGroup")
  ana.biv_list<-methods::new("listVar", List = ana.biv_list)
  ana.univ_list<-anaUniv(ana.biv_list, parallel, group = group, data = data,
                         digits.qt = digits.qt,
                         digits.ql = digits.ql, quali = quali, quanti = quanti,
                         mc.cores = mc.cores)
  return<-makeTable(ana.univ_list, group, pvalue, na.print, parallel, mc.cores)
  if(group == "")
    pvalue = F
  if (isTRUE(smd) && group != "")
    return$df<-addSmdColumn(return$df, var_list, data, group, digits.p)
  if (weighted){
    w<-data[, weightCol]
    data[, weightCol]<-NULL
  }
  else
    w<-numeric(0)
  return.table<-parseClass(table = return$df, group = group, quanti = quanti,
                           quali = quali, na.print = na.print,
                           pvalue = return$pvalue,
                           var_list = var_list, data = data,
                           digits.qt = digits.qt, digits.ql = digits.ql,
                           weights = w)
  return(return.table)
}
