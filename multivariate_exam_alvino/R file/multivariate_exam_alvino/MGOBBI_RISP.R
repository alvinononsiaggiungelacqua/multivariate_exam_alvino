# Set working directory to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Install github packages
devtools::install_github("IQSS/RobustSE")

# Load packages
pacman::p_load(
  tidyverse,
  magrittr,
  patchwork
)

# Load data
read_csv("MGOBBI_RISP.csv") -> BBGIMO_RISP

# Figure 1 ----------------------
bind_rows(
  BBGIMO_RISP %>%
    select(country, error.immigrants.trim) %>%
    Rmisc::summarySE(
      measurevar = "error.immigrants.trim",
      groupvars = "country", na.rm = T
    ) %>%
    rename(error = error.immigrants.trim) %>%
    mutate(Question = "Immigrants"),
  BBGIMO_RISP %>%
    select(country, error.asylum.seekers.trim) %>%
    Rmisc::summarySE(
      measurevar = "error.asylum.seekers.trim",
      groupvars = "country", na.rm = T
    ) %>%
    rename(error = error.asylum.seekers.trim) %>%
    mutate(Question = "Asylum seekers")
) %>%
  ggplot(data = ., aes(x = country, y = error, fill = Question)) +
  geom_bar(
    stat = "identity", position = "dodge",
    col = "black", size = 0.3
  ) +
  geom_errorbar(aes(ymin = error - se, ymax = error + se),
    width = .4,
    position = position_dodge(1), col = "gray20"
  ) +
  theme_bw() +
  coord_flip() +
  scale_fill_grey(
    start = 0.5,
    end = 0.9
  ) +
  scale_x_discrete(limits = rev(sort(unique(BBGIMO_RISP$country)))) +
  ylab("Average error (percentage points)") +
  xlab("") +
  theme(text = element_text(size = 13)) +
  theme(text = element_text(family = "Roboto"))


# Descriptive statistics table (Table 1) ----------
fastDummies::dummy_cols(BBGIMO_RISP$abit) %>%
  select(starts_with(".data_") |
    starts_with(".data_2") |
    starts_with(".data_3")) %>%
  `names<-`(paste("abit_",
    sort(unique(na.omit(BBGIMO_RISP$abit))),
    sep = ""
  )) %>%
  bind_cols(BBGIMO_RISP, .) %>%
  select(
    error.immigrants.trim,
    error.asylum.seekers.trim,
    error.trim,
    consp_01,
    confidence,
    trust.stat,
    edu.tertiary,
    lr,
    age,
    female,
    concern.imm,
    news.tv,
    news.paper,
    news.social,
    income,
    starts_with("abit_")
  ) %>%
  as.data.frame() %>%
  stargazer::stargazer(type = "text")



# Regression models ------------------------

## Set reference category for area of residence
BBGIMO_RISP$abit %<>% factor(levels = c(
  "2. Other town/urban centre",
  "1. Metropolitan zone",
  "3. Rural zone"
))

## Lambda for Yeo-Johnson transformation ------

### Error in estimation of EU immigrants --------
# Estimating optimal lambda
lm(BBGIMO_RISP$error.immigrants.trim ~ 1) %>%
  car::boxCox(., family = "yjPower") %>%
  {
    .$x[which.max(.$y)]
  } -> lambda.err.immigrants.trim

ggplot(
  data = enframe(BBGIMO_RISP$error.immigrants.trim),
  aes(x = value)
) +
  geom_density(lwd = 0.3, fill = wesanderson::wes_palette("Royal2")[3]) +
  theme_bw() +
  xlab("Error in estimation (EU immigrants) not transformed") +
  ylab("Density") -> lambda.err.immigrants.trim_dens

ggplot(
  data = enframe(car::yjPower(BBGIMO_RISP$error.immigrants.trim,
    lambda = lambda.err.immigrants.trim
  )),
  aes(x = value)
) +
  geom_density(lwd = 0.3, fill = wesanderson::wes_palette("Royal2")[3]) +
  theme_bw() +
  xlab("Error in estimation (EU immigrants) transformed") +
  ylab("Density") -> lambda.err.immigrants.trim.yj_dens

# Compare distributions
lambda.err.immigrants.trim_dens / lambda.err.immigrants.trim.yj_dens


### Error in estimation of asylum seekers ----------
# Estimating optimal lambda
lm(BBGIMO_RISP$error.asylum.seekers.trim ~ 1) %>%
  car::boxCox(., family = "yjPower") %>%
  {
    .$x[which.max(.$y)]
  } -> lambda.err.asylum.seekers.trim

ggplot(
  data = enframe(BBGIMO_RISP$error.asylum.seekers.trim),
  aes(x = value)
) +
  geom_density(lwd = 0.3, fill = wesanderson::wes_palette("Royal2")[3]) +
  theme_bw() +
  xlab("Error in estimation (asylum seekers) not transformed") +
  ylab("Density") -> lambda.err.asylum.seekers.trim_dens

ggplot(
  data = enframe(car::yjPower(BBGIMO_RISP$error.asylum.seekers.trim,
    lambda = lambda.err.asylum.seekers.trim
  )),
  aes(x = value)
) +
  geom_density(lwd = 0.3, fill = wesanderson::wes_palette("Royal2")[3]) +
  theme_bw() +
  xlab("Error in estimation (asylum seekers) transformed") +
  ylab("Density") -> lambda.err.asylum.seekers.trim.yj_dens

# Compare distributions
lambda.err.asylum.seekers.trim_dens / lambda.err.asylum.seekers.trim.yj_dens


### Error in estimation (combined) ------------
# Estimating optimal lambda
lm(BBGIMO_RISP$error.trim ~ 1) %>%
  car::boxCox(., family = "yjPower") %>%
  {
    .$x[which.max(.$y)]
  } -> lambda.err.trim

ggplot(
  data = enframe(BBGIMO_RISP$error.trim),
  aes(x = value)
) +
  geom_density(lwd = 0.3, fill = wesanderson::wes_palette("Royal2")[3]) +
  theme_bw() +
  xlab("Error in estimation (combined measure) not transformed") +
  ylab("Density") -> error.trim_dens

ggplot(
  data = enframe(car::yjPower(BBGIMO_RISP$error.trim, lambda = lambda.err.trim)),
  aes(x = value)
) +
  geom_density(lwd = 0.3, fill = wesanderson::wes_palette("Royal2")[3]) +
  theme_bw() +
  xlab("Error in estimation (combined measure) transformed") +
  ylab("Density") -> error.trim.yj_dens

# Compare distributions
error.trim_dens / error.trim.yj_dens


## DV: error in estimation of non-EU immigrants -----------
lm(car::yjPower(error.immigrants.trim, lambda.err.immigrants.trim) ~
consp_01 +
  confidence +
  trust.stat +
  news.tv + news.paper + news.social +
  concern.imm +
  edu.tertiary +
  lr +
  age +
  female +
  income +
  abit +
  country,
data = BBGIMO_RISP
) -> m.immigrants.trim

# N
m.immigrants.trim$fitted.values %>% length()

# Summary table with normal standard errors
summary(m.immigrants.trim)

# Robust standard errors
lmtest::coeftest(m.immigrants.trim,
  vcov. = sandwich::vcovHC(m.immigrants.trim,
    type = "HC3", cluster = "country"
  )
)

## DV: error in estimation of asylum seekers -----------
lm(car::yjPower(error.asylum.seekers.trim, lambda.err.asylum.seekers.trim) ~
consp_01 +
  confidence +
  trust.stat +
  news.tv + news.paper + news.social +
  concern.imm +
  edu.tertiary +
  lr +
  age +
  female +
  abit +
  income +
  country,
data = BBGIMO_RISP
) -> m.asylum.seekers.trim

# N
m.asylum.seekers.trim$fitted.values %>% length()

# Summary table with normal standard errors
summary(m.asylum.seekers.trim)

# Robust standard errors
lmtest::coeftest(m.asylum.seekers.trim,
  vcov. = sandwich::vcovHC(m.asylum.seekers.trim,
    type = "HC3", cluster = "country"
  )
)


## DV: error in estimation (combined) -----------
lm(car::yjPower(error.trim, lambda.err.trim) ~
consp_01 +
  confidence +
  trust.stat +
  news.tv + news.paper + news.social +
  concern.imm +
  edu.tertiary +
  lr +
  age +
  female +
  abit +
  income +
  dummy.imm.as +
  country,
data = BBGIMO_RISP
) -> m.combined.trim

# N
m.combined.trim$fitted.values %>% length()

# Summary table with normal standard errors
summary(m.combined.trim)

# Robust standard errors
lmtest::coeftest(m.combined.trim,
  vcov. = sandwich::vcovHC(m.combined.trim,
    type = "HC3", cluster = "country"
  )
)


# Generalized Information Matrix tests with RobustSE --------
glm(car::yjPower(error.immigrants.trim, lambda.err.immigrants.trim) ~
consp_01 +
  confidence +
  trust.stat +
  news.tv + news.paper + news.social +
  concern.imm +
  edu.tertiary +
  lr +
  age +
  female +
  abit +
  income +
  country,
data = BBGIMO_RISP
) %>%
  RobustSE::GIM(., full = F)

glm(car::yjPower(error.asylum.seekers.trim, lambda.err.asylum.seekers.trim) ~
consp_01 +
  confidence +
  trust.stat +
  news.tv + news.paper + news.social +
  concern.imm +
  edu.tertiary +
  lr +
  age +
  female +
  abit +
  income +
  country,
data = BBGIMO_RISP
) %>%
  RobustSE::GIM(., full = F)

glm(car::yjPower(error.trim, lambda.err.trim) ~
consp_01 +
  confidence +
  trust.stat +
  news.tv + news.paper + news.social +
  concern.imm +
  edu.tertiary +
  lr +
  age +
  female +
  abit +
  income +
  dummy.imm.as +
  country,
data = BBGIMO_RISP
) %>%
  RobustSE::GIM(., full = F)

# Figure 2 ----------------

## Error in estimation of non-EU immigrants --------
prediction::prediction(m.immigrants.trim,
  at = list(
    consp_01 = seq(0, 1, by = 0.005)
  ),
  calculate_se = T
) %>%
  summary() %>%
  mutate(
    prediction_transformed =
      nlmixr::iYeoJohnson(Prediction,
        lambda = lambda.err.immigrants.trim
      ),
    lower_transformed =
      nlmixr::iYeoJohnson(lower,
        lambda = lambda.err.immigrants.trim
      ),
    upper_transformed =
      nlmixr::iYeoJohnson(upper,
        lambda = lambda.err.immigrants.trim
      )
  ) %>%
  ggplot(., aes(`at(consp_01)`, prediction_transformed)) +
  geom_ribbon(aes(ymin = lower_transformed, ymax = upper_transformed),
    fill = "grey80"
  ) +
  geom_line(
    lwd = 0.7,
    color = "grey20"
  ) +
  labs(x = "Conspiracy thinking", 
       y = "Error in estimation of non-EU immigrants") +
  theme_bw() +
  geom_rug(
    data = BBGIMO_RISP[!is.na(BBGIMO_RISP$error.immigrants.trim), ],
    aes(consp_01), sides = "b",
    inherit.aes = F,
    col = "grey30"
  ) +
  theme(text = element_text(size = 13)) +
  theme(text = element_text(family = "Roboto"))

# Min/max predicted values as cited in the main text
prediction::prediction(m.immigrants.trim,
                       at = list(
                         consp_01 = c(0,1)
                       ),
                       calculate_se = T
) %>%
  summary() %>%
  mutate(
    prediction_transformed =
      nlmixr::iYeoJohnson(Prediction,
                          lambda = lambda.err.immigrants.trim
      ),
    lower_transformed =
      nlmixr::iYeoJohnson(lower,
                          lambda = lambda.err.immigrants.trim
      ),
    upper_transformed =
      nlmixr::iYeoJohnson(upper,
                          lambda = lambda.err.immigrants.trim
      )
  )


## Error in estimation of asylum seekers --------
prediction::prediction(m.asylum.seekers.trim,
  at = list(
    consp_01 = seq(0, 1, by = 0.005)
  ),
  calculate_se = T
) %>%
  summary() %>%
  mutate(
    prediction_transformed =
      nlmixr::iYeoJohnson(Prediction,
        lambda = lambda.err.asylum.seekers.trim
      ),
    lower_transformed =
      nlmixr::iYeoJohnson(lower,
        lambda = lambda.err.asylum.seekers.trim
      ),
    upper_transformed =
      nlmixr::iYeoJohnson(upper,
        lambda = lambda.err.asylum.seekers.trim
      )
  ) %>%
  ggplot(., aes(`at(consp_01)`, prediction_transformed)) +
  geom_ribbon(aes(ymin = lower_transformed, ymax = upper_transformed),
    fill = "grey80"
  ) +
  geom_line(
    lwd = 0.7,
    color = "grey20"
  ) +
  labs(x = "Conspiracy thinking", 
       y = "Error in estimation of asylum seekers") +
  theme_bw() +
  geom_rug(
    data = BBGIMO_RISP[!is.na(BBGIMO_RISP$error.asylum.seekers.trim), ],
    aes(consp_01), sides = "b",
    inherit.aes = F,
    col = "grey30"
  ) +
  theme(text = element_text(size = 13)) +
  theme(text = element_text(family = "Roboto"))

# Min/max predicted values as cited in the main text
prediction::prediction(m.asylum.seekers.trim,
                       at = list(
                         consp_01 = c(0,1)
                       ),
                       calculate_se = T
) %>%
  summary() %>%
  mutate(
    prediction_transformed =
      nlmixr::iYeoJohnson(Prediction,
                          lambda = lambda.err.asylum.seekers.trim
      ),
    lower_transformed =
      nlmixr::iYeoJohnson(lower,
                          lambda = lambda.err.asylum.seekers.trim
      ),
    upper_transformed =
      nlmixr::iYeoJohnson(upper,
                          lambda = lambda.err.asylum.seekers.trim
      )
  )


# APPENDIX -----------------

## Tables A11 and A12 ------
read_csv("perc_nonEU_imm.csv") -> perc_nonEU_imm
read_csv("perc_asylum_seekers.csv") -> perc_asylum_seekers

perc_nonEU_imm %>%
  mutate(Percentage = round(perc, digits = 2)) %>%
  select(-perc) %>%
  stargazer::stargazer(summary = F, type = "text")

perc_asylum_seekers %>%
  mutate(Percentage = round(perc, digits = 2)) %>%
  select(-perc) %>%
  stargazer::stargazer(summary = F, type = "text")

## Figure A1 -------------
ggplot(BBGIMO_RISP, aes(x = error.immigrants)) +
  geom_density(lwd = 0.3, fill = wesanderson::wes_palette("Royal2")[3]) +
  theme_bw() +
  theme(text = element_text(size = 12)) +
  theme(text = element_text(family = "Roboto")) +
  xlab("Error in estimation of non-EU immigrants (without excluding outliers)") +
  ylab("Density") -> den1

ggplot(BBGIMO_RISP, aes(x = error.immigrants.trim)) +
  geom_density(lwd = 0.3, fill = wesanderson::wes_palette("Royal2")[3]) +
  theme_bw() +
  theme(text = element_text(size = 12)) +
  theme(text = element_text(family = "Roboto")) +
  xlab("Error in estimation of non-EU immigrants (excluding outliers)") +
  ylab("Density") -> den2

ggplot(BBGIMO_RISP, aes(x = error.asylum.seekers)) +
  geom_density(lwd = 0.3, fill = wesanderson::wes_palette("Royal2")[3]) +
  theme_bw() +
  theme(text = element_text(size = 12)) +
  theme(text = element_text(family = "Roboto")) +
  xlab("Error in estimation of asylum seekers (without excluding outliers)") +
  ylab("") -> den3

ggplot(BBGIMO_RISP, aes(x = error.asylum.seekers.trim)) +
  geom_density(lwd = 0.3, fill = wesanderson::wes_palette("Royal2")[3]) +
  theme_bw() +
  theme(text = element_text(size = 12)) +
  theme(text = element_text(family = "Roboto")) +
  xlab("Error in estimation of asylum seekers (excluding outliers)") +
  ylab("") -> den4

(den1 / den2 | den3 / den4)

## Figure A2 -------------
bind_rows(
  BBGIMO_RISP %>%
    select(country, error.immigrants) %>%
    Rmisc::summarySE(
      measurevar = "error.immigrants",
      groupvars = "country", na.rm = T
    ) %>%
    rename(error = error.immigrants) %>%
    mutate(Question = "Immigrants"),
  BBGIMO_RISP %>%
    select(country, error.asylum.seekers) %>%
    Rmisc::summarySE(
      measurevar = "error.asylum.seekers",
      groupvars = "country", na.rm = T
    ) %>%
    rename(error = error.asylum.seekers) %>%
    mutate(Question = "Asylum seekers")
) %>%
  ggplot(data = ., aes(x = country, y = error, fill = Question)) +
  geom_bar(
    stat = "identity", position = "dodge",
    col = "black", size = 0.3
  ) +
  geom_errorbar(aes(ymin = error - se, ymax = error + se),
    width = .4,
    position = position_dodge(1), col = "gray20"
  ) +
  theme_bw() +
  coord_flip() +
  scale_fill_grey(
    start = 0.5,
    end = 0.9
  ) +
  scale_x_discrete(limits = rev(sort(unique(BBGIMO_RISP$country)))) +
  ylab("Average error (percentage points)") +
  xlab("") +
  theme(text = element_text(size = 13)) +
  theme(text = element_text(family = "Roboto"))

## Figure A3 --------------
BBGIMO_RISP %>%
  select(QG8c_1_T4, QG8d_1_T4, country) %>%
  group_by(country) %>%
  summarise(
    Immigrants = mean(QG8c_1_T4, na.rm = T),
    `Asylum seekers` = mean(QG8d_1_T4, na.rm = T)
  ) %>%
  pivot_longer(cols = c("Immigrants", "Asylum seekers")) %>%
  ggplot(
    data = .,
    aes(
      y = country,
      x = value,
      fill = name
    )
  ) +
  geom_bar(
    stat = "identity", position = "dodge",
    col = "black", size = 0.3
  ) +
  theme_bw() +
  scale_fill_grey("Question",
    start = 0.5,
    end = 0.9
  ) +
  scale_y_discrete(limits = rev(sort(unique(BBGIMO_RISP$country)))) +
  xlab("Average response") +
  ylab("") +
  theme(text = element_text(size = 13)) +
  theme(text = element_text(family = "Roboto"))

## Tables A13 and A14 ------------
# Subset of data
BBGIMO_RISP %>%
  select(
    QG16_1_T4,
    QG16_2_T4,
    QG16_3_T4,
    QG16_4_T4,
    country_cod
  ) -> cosp.items

# Convert 98s to NAs
cosp.items[cosp.items == 98] <- NA

# Reverse the four items
apply(cosp.items[, 1:4], 2, function(x) {
  max(x, na.rm = T) + 1 - x
}) %>% cbind(., select(cosp.items, country_cod)) -> cosp.items

# Turn data frame into a matrix
cosp.items %<>% as.matrix()

### Test for measurement invariance --------

# Model synthax
cfa_model_cosp.items <- "
                    conspiracy_thinkin =~ QG16_1_T4 + QG16_2_T4 +
                                          QG16_3_T4 + QG16_4_T4
                    "

# Fit baseline model
fit_cosp.items_conf <- lavaan::cfa(cfa_model_cosp.items,
  data = cosp.items,
  group = "country_cod",
  std.lv = TRUE
)

# Test for measurement invariance
semTools::measurementInvariance(
  model = cfa_model_cosp.items,
  data = cosp.items, group = "country_cod"
)

# Tables by country
semTable::semTable(fit_cosp.items_conf,
  paramSets = c("loadings", "intercepts", "residualvariances"),
  groups = 1
)
semTable::semTable(fit_cosp.items_conf,
  paramSets = c("loadings", "intercepts", "residualvariances"),
  groups = 2
)
semTable::semTable(fit_cosp.items_conf,
  paramSets = c("loadings", "intercepts", "residualvariances"),
  groups = 3
)
semTable::semTable(fit_cosp.items_conf,
  paramSets = c("loadings", "intercepts", "residualvariances"),
  groups = 4
)
semTable::semTable(fit_cosp.items_conf,
  paramSets = c("loadings", "intercepts", "residualvariances"),
  groups = 5
)
semTable::semTable(fit_cosp.items_conf,
  paramSets = c("loadings", "intercepts", "residualvariances"),
  groups = 6
)
semTable::semTable(fit_cosp.items_conf,
  paramSets = c("loadings", "intercepts", "residualvariances"),
  groups = 7
)
semTable::semTable(fit_cosp.items_conf,
  paramSets = c("loadings", "intercepts", "residualvariances"),
  groups = 8
)
semTable::semTable(fit_cosp.items_conf,
  paramSets = c("loadings", "intercepts", "residualvariances"),
  groups = 9
)
semTable::semTable(fit_cosp.items_conf,
  paramSets = c("loadings", "intercepts", "residualvariances"),
  groups = 10
)

## Figure A5 ---------------

# Select variables
BBGIMO_RISP %>%
  select(
    consp_01,
    confidence,
    trust.stat,
    news.tv,
    news.paper,
    news.social,
    income,
    concern.imm,
    edu.tertiary,
    lr,
    age
  ) %>%
  rename(
    `Conspiracy thinking` = consp_01,
    `Self-confidence` = confidence,
    `Trust in offical statistics` = trust.stat,
    `News on TV` = news.tv,
    `News on paper` = news.paper,
    `News of social media` = news.social,
    `Income` = income,
    `Concerned abt. imm.` = concern.imm,
    `High education` = edu.tertiary,
    `Left-right (scale)` = lr,
    `Age` = age
  ) -> corrplot_vars


ggcorrplot::ggcorrplot(cor(na.omit(
  corrplot_vars
)),
method = "circle",
colors = c("red", "white", "dark blue")
)

## Regression analysis robustness checks ------------

### Weighted, trimmed (Table A15) --------

#### DV: error in estimation of non-EU immigrants -----------
lm(car::yjPower(error.immigrants.trim, lambda.err.immigrants.trim) ~
consp_01 +
  confidence +
  trust.stat +
  news.tv + news.paper + news.social +
  concern.imm +
  edu.tertiary +
  lr +
  age +
  female +
  income +
  abit +
  country,
weights = int_wt1_cap_T4,
data = BBGIMO_RISP
) -> m.immigrants.trim_w

# N
m.immigrants.trim_w$fitted.values %>% length()

# Summary table with normal standard errors
summary(m.immigrants.trim_w)

# Robust standard errors
lmtest::coeftest(m.immigrants.trim_w,
  vcov. = sandwich::vcovHC(m.immigrants.trim_w,
    type = "HC3", cluster = "country"
  )
)

#### DV: error in estimation of asylum seekers -----------
lm(car::yjPower(error.asylum.seekers.trim, lambda.err.asylum.seekers.trim) ~
consp_01 +
  confidence +
  trust.stat +
  news.tv + news.paper + news.social +
  concern.imm +
  edu.tertiary +
  lr +
  age +
  female +
  abit +
  income +
  country,
weights = int_wt1_cap_T4,
data = BBGIMO_RISP
) -> m.asylum.seekers.trim_w

# N
m.asylum.seekers.trim_w$fitted.values %>% length()

# Summary table with normal standard errors
summary(m.asylum.seekers.trim_w)

# Robust standard errors
lmtest::coeftest(m.asylum.seekers.trim_w,
  vcov. = sandwich::vcovHC(m.asylum.seekers.trim_w,
    type = "HC3", cluster = "country"
  )
)


#### DV: error in estimation (combined) -----------
lm(car::yjPower(error.trim, lambda.err.trim) ~
consp_01 +
  confidence +
  trust.stat +
  news.tv + news.paper + news.social +
  concern.imm +
  edu.tertiary +
  lr +
  age +
  female +
  abit +
  income +
  dummy.imm.as +
  country,
weights = int_wt1_cap_T4,
data = BBGIMO_RISP
) -> m.combined.trim_w

# N
m.combined.trim_w$fitted.values %>% length()

# Summary table with normal standard errors
summary(m.combined.trim_w)

# Robust standard errors
lmtest::coeftest(m.combined.trim_w,
  vcov. = sandwich::vcovHC(m.combined.trim_w,
    type = "HC3", cluster = "country"
  )
)

### Weighted, untrimmed (Table A16) --------

#### Lambda for Yeo-Johnson transformation (untrimmed DVs) ------

##### Error in estimation of EU immigrants --------
lm(BBGIMO_RISP$error.immigrants ~ 1) %>%
  car::boxCox(., family = "yjPower") %>%
  {
    .$x[which.max(.$y)]
  } -> lambda.err.immigrants

ggplot(
  data = enframe(BBGIMO_RISP$error.immigrants),
  aes(x = value)
) +
  geom_density(lwd = 0.3, fill = wesanderson::wes_palette("Royal2")[3]) +
  theme_bw() +
  xlab("Error in estimation (EU immigrants) not transformed") +
  ylab("Density") -> lambda.err.immigrants_dens

ggplot(
  data = enframe(car::yjPower(BBGIMO_RISP$error.immigrants,
    lambda = lambda.err.immigrants
  )),
  aes(x = value)
) +
  geom_density(lwd = 0.3, fill = wesanderson::wes_palette("Royal2")[3]) +
  theme_bw() +
  xlab("Error in estimation (EU immigrants) transformed") +
  ylab("Density") -> lambda.err.immigrants.yj_dens

lambda.err.immigrants_dens / lambda.err.immigrants.yj_dens


##### Error in estimation of asylum seekers ----------
lm(BBGIMO_RISP$error.asylum.seekers ~ 1) %>%
  car::boxCox(., family = "yjPower") %>%
  {
    .$x[which.max(.$y)]
  } -> lambda.err.asylum.seekers

ggplot(
  data = enframe(BBGIMO_RISP$error.asylum.seekers),
  aes(x = value)
) +
  geom_density(lwd = 0.3, fill = wesanderson::wes_palette("Royal2")[3]) +
  theme_bw() +
  xlab("Error in estimation (asylum seekers) not transformed") +
  ylab("Density") -> lambda.err.asylum.seekers_dens

ggplot(
  data = enframe(car::yjPower(BBGIMO_RISP$error.asylum.seekers,
    lambda = lambda.err.asylum.seekers
  )),
  aes(x = value)
) +
  geom_density(lwd = 0.3, fill = wesanderson::wes_palette("Royal2")[3]) +
  theme_bw() +
  xlab("Error in estimation (asylum seekers) transformed") +
  ylab("Density") -> lambda.err.asylum.seekers.yj_dens

lambda.err.asylum.seekers_dens / lambda.err.asylum.seekers.yj_dens


##### Error in estimation (combined) ------------
lm(BBGIMO_RISP$error ~ 1) %>%
  car::boxCox(., family = "yjPower") %>%
  {
    .$x[which.max(.$y)]
  } -> lambda.err

ggplot(
  data = enframe(BBGIMO_RISP$error),
  aes(x = value)
) +
  geom_density(
    lwd = 0.3,
    fill = wesanderson::wes_palette("Royal2")[3]
  ) +
  theme_bw() +
  xlab("Error in estimation (combined measure) not transformed") +
  ylab("Density") -> error_dens

ggplot(
  data = enframe(car::yjPower(BBGIMO_RISP$error, lambda = lambda.err)),
  aes(x = value)
) +
  geom_density(
    lwd = 0.3,
    fill = wesanderson::wes_palette("Royal2")[3]
  ) +
  theme_bw() +
  xlab("Error in estimation (combined measure) transformed") +
  ylab("Density") -> error.yj_dens

error_dens / error.yj_dens



#### DV: error in estimation of non-EU immigrants -----------
lm(car::yjPower(error.immigrants, lambda.err.immigrants) ~
consp_01 +
  confidence +
  trust.stat +
  news.tv + news.paper + news.social +
  concern.imm +
  edu.tertiary +
  lr +
  age +
  female +
  income +
  abit +
  country,
weights = int_wt1_cap_T4,
data = BBGIMO_RISP
) -> m.immigrants_w

# N
m.immigrants_w$fitted.values %>% length()

# Summary table with normal standard errors
summary(m.immigrants_w)

# Robust standard errors
lmtest::coeftest(m.immigrants_w,
  vcov. = sandwich::vcovHC(m.immigrants_w,
    type = "HC3", cluster = "country"
  )
)

#### DV: error in estimation of asylum seekers -----------
lm(car::yjPower(error.asylum.seekers, lambda.err.asylum.seekers) ~
consp_01 +
  confidence +
  trust.stat +
  news.tv + news.paper + news.social +
  concern.imm +
  edu.tertiary +
  lr +
  age +
  female +
  abit +
  income +
  country,
weights = int_wt1_cap_T4,
data = BBGIMO_RISP
) -> m.asylum.seekers_w

# N
m.asylum.seekers_w$fitted.values %>% length()

# Summary table with normal standard errors
summary(m.asylum.seekers_w)

# Robust standard errors
lmtest::coeftest(m.asylum.seekers_w,
  vcov. = sandwich::vcovHC(m.asylum.seekers_w,
    type = "HC3", cluster = "country"
  )
)


#### DV: error in estimation (combined) -----------
lm(car::yjPower(error, lambda.err) ~
consp_01 +
  confidence +
  trust.stat +
  news.tv + news.paper + news.social +
  concern.imm +
  edu.tertiary +
  lr +
  age +
  female +
  abit +
  income +
  dummy.imm.as +
  country,
weights = int_wt1_cap_T4,
data = BBGIMO_RISP
) -> m.combined_w

# N
m.combined_w$fitted.values %>% length()

# Summary table with normal standard errors
summary(m.combined_w)

# Robust standard errors
lmtest::coeftest(m.combined_w,
  vcov. = sandwich::vcovHC(m.combined_w,
    type = "HC3", cluster = "country"
  )
)

### Not weighted, untrimmed (Table A17) --------

#### DV: error in estimation of non-EU immigrants -----------
lm(car::yjPower(error.immigrants, lambda.err.immigrants) ~
consp_01 +
  confidence +
  trust.stat +
  news.tv + news.paper + news.social +
  concern.imm +
  edu.tertiary +
  lr +
  age +
  female +
  income +
  abit +
  country,
data = BBGIMO_RISP
) -> m.immigrants

# N
m.immigrants$fitted.values %>% length()

# Summary table with normal standard errors
summary(m.immigrants)

# Robust standard errors
lmtest::coeftest(m.immigrants,
  vcov. = sandwich::vcovHC(m.immigrants,
    type = "HC3", cluster = "country"
  )
)

#### DV: error in estimation of asylum seekers -----------
lm(car::yjPower(error.asylum.seekers, lambda.err.asylum.seekers) ~
consp_01 +
  confidence +
  trust.stat +
  news.tv + news.paper + news.social +
  concern.imm +
  edu.tertiary +
  lr +
  age +
  female +
  abit +
  income +
  country,
weights = int_wt1_cap_T4,
data = BBGIMO_RISP
) -> m.asylum.seekers

# N
m.asylum.seekers$fitted.values %>% length()

# Summary table with normal standard errors
summary(m.asylum.seekers)

# Robust standard errors
lmtest::coeftest(m.asylum.seekers,
  vcov. = sandwich::vcovHC(m.asylum.seekers,
    type = "HC3", cluster = "country"
  )
)


#### DV: error in estimation (combined) -----------
lm(car::yjPower(error, lambda.err) ~
consp_01 +
  confidence +
  trust.stat +
  news.tv + news.paper + news.social +
  concern.imm +
  edu.tertiary +
  lr +
  age +
  female +
  abit +
  income +
  dummy.imm.as +
  country,
weights = int_wt1_cap_T4,
data = BBGIMO_RISP
) -> m.combined

# N
m.combined$fitted.values %>% length()

# Summary table with normal standard errors
summary(m.combined)

# Robust standard errors
lmtest::coeftest(m.combined,
  vcov. = sandwich::vcovHC(m.combined,
    type = "HC3", cluster = "country"
  )
)

