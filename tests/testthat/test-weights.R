weightData <- function(seed = 1, n = 300) {
  set.seed(seed)
  d <- data.frame(
    arm = factor(sample(c("Control", "Treated"), n, TRUE)),
    age = rnorm(n, 60, 10),
    crp = rlnorm(n, 2, 1),
    sex = factor(sample(c("F", "M"), n, TRUE)),
    stage = factor(sample(c("I", "II", "III"), n, TRUE))
  )
  d$age <- d$age + 5 * (d$arm == "Treated")
  d$crp[sample(n, 12)] <- NA
  d$sex[sample(n, 7)] <- NA
  ps <- glm(arm ~ age + sex, data = d, family = binomial, na.action = na.exclude)$fitted.values
  ps[is.na(ps)] <- mean(ps, na.rm = TRUE)
  d$w <- ifelse(d$arm == "Treated", 1 / ps, 1 / (1 - ps))
  d
}

cell <- function(res, row, col) res["table"][res["table"]$var == row, col]

test_that("weights of 1 give the unweighted table", {
  d <- weightData()
  ref <- descTab(d[, -6], group = "arm", pvalue = FALSE, na.print = TRUE)
  res <- descTab(d[, -6], group = "arm", pvalue = FALSE, na.print = TRUE,
                 weights = rep(1, nrow(d)), smd = FALSE)
  expect_equal(res["table"], ref["table"])
})

test_that("weighted means, SD and proportions are right", {
  d <- weightData()
  res <- descTab(d, group = "arm", weights = "w", digits.qt = 3, digits.ql = 2)
  tr <- d[d$arm == "Treated", ]
  m <- weighted.mean(tr$age, tr$w)
  v <- sum(tr$w * (tr$age - m)^2) / (sum(tr$w) - sum(tr$w^2) / sum(tr$w))
  expect_equal(cell(res, "age mean (SD)", "Treated"),
               sprintf("%s (%s)", round(m, 3), round(sqrt(v), 3)))
  # Same as the survey package
  design <- survey::svydesign(ids = ~1, weights = ~w, data = tr)
  expect_equal(round(as.numeric(survey::svymean(~age, design)), 3), round(m, 3))
  f <- sum(tr$w[tr$sex %in% "F"])
  expect_equal(cell(res, "sex, F", "Treated"),
               sprintf("%s (%s)", round(f, 2), round(f / sum(tr$w) * 100, 2)))
})

test_that("weighted medians are right", {
  d <- data.frame(g = factor(rep(c("a", "b"), each = 4)), x = c(1, 2, 3, 4, 1, 2, 3, 4))
  res <- descTab(d, group = "g", weights = c(1, 1, 1, 1, 3, 1, 1, 1),
                 normality = "non normal")
  expect_equal(cell(res, "x median (IQR)", "a"), "2.5 (2)")
  # b: weight 3 on 1 out of 6, so 50% of the weight is reached at 1.5
  expect_equal(cell(res, "x median (IQR)", "b"), "1.5 (2)")
})

test_that("the weights column is not described", {
  res <- descTab(weightData(), group = "arm", weights = "w")
  expect_false(any(grepl("^w ", res["table"]$var)))
  expect_false(any(grepl("doudpackage", colnames(res["data"]))))
})

test_that("SMD follows the pooled SD of the unweighted data", {
  d <- weightData()
  res <- descTab(d, group = "arm", weights = "w", digits.p = 6)
  c0 <- d$arm == "Control"
  sd.p <- sqrt((var(d$age[c0]) + var(d$age[!c0])) / 2)
  smd <- abs(weighted.mean(d$age[!c0], d$w[!c0]) - weighted.mean(d$age[c0], d$w[c0])) / sd.p
  expect_equal(cell(res, "age mean (SD)", "SMD"), round(smd, 6))
  # Binary: proportions of the last level, among non missing values
  k <- !is.na(d$sex)
  p <- function(sel, w) sum(w[sel & k & d$sex %in% "M"]) / sum(w[sel & k])
  one <- rep(1, nrow(d))
  denom <- sqrt((p(c0, one) * (1 - p(c0, one)) + p(!c0, one) * (1 - p(!c0, one))) / 2)
  expect_equal(cell(res, "sex, M", "SMD"), round(abs(p(!c0, d$w) - p(c0, d$w)) / denom, 6))
  # No SMD on the missing value rows
  res <- descTab(d, group = "arm", weights = "w", na.print = TRUE)
  expect_true(all(is.na(res["table"]$SMD[grepl("Missing", res["table"]$var)])))
})

test_that("the SMD matches the one of MatchIt", {
  skip_if_not_installed("MatchIt")
  d <- weightData()
  d$sex[is.na(d$sex)] <- "F"
  d$crp[is.na(d$crp)] <- 1
  m <- MatchIt::matchit(arm ~ age + sex, data = d, method = "subclass", estimand = "ATE")
  s <- summary(m, un = TRUE)
  res <- descTab(d, group = "arm", weights = m, digits.p = 6)
  expect_equal(cell(res, "age mean (SD)", "SMD"),
               round(abs(s$sum.across["age", "Std. Mean Diff."]), 6))
  expect_equal(cell(res, "sex, M", "SMD"),
               round(abs(s$sum.across["sexM", "Std. Mean Diff."]), 6))
  res <- descTab(d, group = "arm", smd = TRUE, digits.p = 6)
  expect_equal(cell(res, "age mean (SD)", "SMD"),
               round(abs(s$sum.all["age", "Std. Mean Diff."]), 6))
})

test_that("a matchit object describes the matched sample", {
  skip_if_not_installed("MatchIt")
  d <- weightData()
  d$sex[is.na(d$sex)] <- "F"
  m <- suppressWarnings(MatchIt::matchit(arm ~ age + sex, data = d))
  matched <- d[m$weights > 0, ]
  res <- descTab(d, group = "arm", weights = m, smd = FALSE)
  ref <- descTab(matched, group = "arm", pvalue = FALSE)
  expect_equal(res["table"][, c("var", "Control", "Treated", "Total")],
               ref["table"][, c("var", "Control", "Treated", "Total")])
  expect_error(descTab(matched, group = "arm", weights = m), "match.data")
})

test_that("multi-level factors use the multivariate SMD, and more than 2 groups the max", {
  d <- weightData()
  res <- descTab(d, group = "arm", smd = TRUE, digits.p = 6)
  c0 <- d$arm == "Control"
  pr <- function(sel) as.numeric(prop.table(table(d$stage[sel])))
  S <- function(p) { p <- p[-1]; s <- -outer(p, p); diag(s) <- p * (1 - p); s }
  diff <- (pr(!c0) - pr(c0))[-1]
  smd <- sqrt(as.numeric(t(diff) %*% solve((S(pr(c0)) + S(pr(!c0))) / 2) %*% diff))
  expect_equal(cell(res, "stage, I", "SMD"), round(smd, 6))

  d$g3 <- factor(sample(c("a", "b", "c"), nrow(d), TRUE))
  res <- descTab(d[, c("g3", "age")], group = "g3", smd = TRUE, digits.p = 6)
  pair <- function(a, b) {
    x <- d$age[d$g3 == a]; y <- d$age[d$g3 == b]
    abs(mean(x) - mean(y)) / sqrt((var(x) + var(y)) / 2)
  }
  expect_equal(cell(res, "age mean (SD)", "SMD"),
               round(max(pair("a", "b"), pair("a", "c"), pair("b", "c")), 6))
})

test_that("p values are off by default with weights, and come from survey if asked", {
  d <- weightData()
  expect_false("pvalue" %in% colnames(descTab(d, group = "arm", weights = "w")["table"]))
  res <- descTab(d, group = "arm", weights = "w", pvalue = TRUE, digits.p = 6)
  design <- survey::svydesign(ids = ~1, weights = ~w, data = d)
  expect_equal(cell(res, "age mean (SD)", "pvalue"),
               round(unname(survey::svyttest(age ~ arm, design)$p.value), 6))
  expect_equal(unique(res["table"]$pvalue[grepl("^stage", res["table"]$var)]),
               round(unname(survey::svychisq(~stage + arm, design)$p.value), 6))
  res <- descTab(d, group = "arm", weights = "w", pvalue = TRUE,
                 normality = "non normal", digits.p = 6)
  expect_equal(cell(res, "crp median (IQR)", "pvalue"),
               round(unname(survey::svyranktest(crp ~ arm, design)$p.value), 6))
})

test_that("bad weights are reported", {
  d <- weightData()
  expect_error(descTab(d, group = "arm", weights = "nope"), "not a variable")
  expect_error(descTab(d, group = "arm", weights = 1:3), "length 3")
  expect_error(descTab(d, group = "arm", weights = -d$w), "non negative")
  w <- d$w; w[1] <- NA
  expect_error(descTab(d, group = "arm", weights = w), "without NA")
  expect_error(descTab(d, group = "arm", weights = "sex"), "numeric")
  expect_error(descTab(d, group = "arm", weights = ifelse(d$arm == "Control", 0, 1)),
               "positive sum")
  expect_error(descTab(d, group = "arm", smd = NA), "smd")
})

test_that("weights and group missing values stay aligned", {
  d <- weightData()
  d$arm[1:5] <- NA
  expect_warning(res <- descTab(d, group = "arm", weights = "w", digits.ql = 3),
                 "5 rows have been deleted")
  keep <- !is.na(d$arm)
  expect_equal(cell(res, "stage, I", "Total"),
               sprintf("%s (%s)", round(sum(d$w[keep & d$stage == "I"]), 3),
                       round(sum(d$w[keep & d$stage == "I"]) / sum(d$w[keep]) * 100, 3)))
})

test_that("parseClassFun shows weighted counts and the SMD column", {
  d <- weightData()
  res <- descTab(d, group = "arm", weights = "w")
  html <- as.character(parseClassFun(res))
  expect_true(grepl(sprintf("n = %s", round(sum(d$w), 1)), html, fixed = TRUE))
  expect_true(grepl("SMD", html, fixed = TRUE))
  html <- as.character(parseClassFun(res, col.order = c("Treated", "Control")))
  expect_true(grepl("SMD", html, fixed = TRUE))
})

test_that("weights without a group describe the weighted population", {
  d <- weightData()
  res <- descTab(d, weights = "w", smd = TRUE)
  expect_equal(colnames(res["table"]), c("var", "Total"))
})
