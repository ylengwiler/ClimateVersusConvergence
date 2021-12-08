# ----------------------------------------------------------------------------
# Climate Protection versus Economic Convergence?
#
# Author: Yvan Lengwiler
# Date: 2021-10-19
#
# SOFTWARE USED:
# platform       x86_64-w64-mingw32
# arch           x86_64
# os             mingw32
# system         x86_64, mingw32
# status
# major          4
# minor          1.1
# year           2021
# month          08
# day            10
# svn rev        80725
# language       R
# version.string R version 4.1.1 (2021-08-10)
# nickname       Kick Things
# ----------------------------------------------------------------------------

cat('Climate Protection versus Economic Convergence?\n',
    'Author: Yvan Lengwiler\n\n',
    'This skript downloads the data and runs the key regressions.\n',
    'The skript produces regular standard errors in the regressions.\n',
    'For the paper, EViews was used with the option to compute\n',
    'Huber-White heteroscedasticity adjusted standard errors. The\n',
    'reported significance levels are therefore different in the\n',
    'skript and in the paper.\n\n',
    'The paper is available at\n',
    'https://doi.org/10.5451/unibas-ep85420\n',
    '--------------------------------------------------------------\n\n',
    sep = '')

# **** preparations **********************************************************

# install and load some packages if they are missing
packages <- c('rstudioapi','openxlsx','readxl','writexl','httr','jsonlite','texreg','broom')
#packages <- c('rstudioapi','openxlsx','readxl','httr','jsonlite','rjson','knitr')
missing_packages <- !(packages %in% rownames(installed.packages()))
if (any(missing_packages)) {install.packages(packages[missing_packages])}
invisible(lapply(packages, library, character.only = TRUE))

# select location of this file as working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# **** read data *************************************************************

year0 <- 1975
year1 <- 2018
Dt    <- year1 - year0

# Penn World Table, Version 10.0

cat('Downloading Penn World Data ... ')

pwt <- read.xlsx('https://www.rug.nl/ggdc/docs/pwt100.xlsx','Data')

pwt <- subset(pwt, year >= year0 & year <= year1)
y0  <- pwt[pwt$year %in% year0, c('countrycode','rgdpna','pop')]
y1  <- pwt[pwt$year %in% year1, c('countrycode','rgdpna','pop')]
sk  <- aggregate(pwt$csh_i, by = list(pwt$countrycode), FUN = mean)

cat('done.\n')

# Cohen-Soto-Leker data on education

cat('Downloading Cohen-Soto-Leker education data ... ')

url <- 'https://www.parisschoolofeconomics.eu/docs/cohen-daniel/csl-database.xls'
fh  <- tempfile()
download.file(url, fh, mode="wb", quiet=TRUE)
csl <- read_xls(path = fh)

csl <- subset(csl, agefrom==25 & ageto==64)
csl$edu <- (
  csl$edu_1970 + csl$edu_1980 + csl$edu_1990 +
    csl$edu_2000 + csl$edu_2010
  ) / 5

cat('done.\n')

# Potsdam Institute for Climate Impact Research
# Green House Gas emissions data

cat('Downloading Potsdam Institute Climate Data via ClimateWatch ... ')

# data_sources - source_ids[] - view data source id at https://www.climatewatchdata.org/api/v1/data/historical_emissions/data_sources
# gases - gas_ids[] - view gas id at https://www.climatewatchdata.org/api/v1/data/historical_emissions/gases
# sectors - sector_ids[] - view sector id at https://www.climatewatchdata.org/api/v1/data/historical_emissions/sectors
# regions - regions[] - region ISO code 3 (ISO Codes for World and European Union (27) are WORLD and EUU, respectively)
# start_year - start_year - Show results from this year onwards
# end_year - end_year - Show results up to this year
# sort_col - sort_col - column to sort the table by
# sort_dir - sort_dir - sort direction (ASC or DESC)
url <- 'https://www.climatewatchdata.org/api/v1/data/historical_emissions'
url <- paste(
  url,
  '?start_year=', year0,
  '&end_year=', year1,
  '&gas_ids[]=330',
  '&source_ids[]=112',
  '&sector_ids[]=1313',
  '&per_page=1000&page=1',
  sep = '')
response <- GET(url)
pik.json <- fromJSON(rawToChar(response$content))

pik <- data.frame()
for (r in 1:length(pik.json$data$id)) {
  add <- data.frame(
    iso = pik.json$data$iso_code3[[r]],
    C0  = pik.json$data$emissions[[r]]$value[1],
    C1  = pik.json$data$emissions[[r]]$value[Dt+1]
  )
  pik = rbind(pik, add)
}

cat('done.\n')

# **** consolidate data ******************************************************

cat('Consolidating data ... ')

dat <- data.frame(
  row.names = y0$countrycode,
  Y0 = log(y0$rgdpna), L0 = log(y0$pop), ypc0 = log(y0$rgdpna/y0$pop),
  Y1 = log(y1$rgdpna), L1 = log(y1$pop), ypc1 = log(y1$rgdpna/y1$pop),
  sk = sk$x
)

dat$edu <- NA
for(k in csl$code){
  dat[k,'edu'] <- subset(csl, code %in% k)$edu
}

dat$C0 <- NA
dat$C1 <- NA
for(k in pik$iso){
  dat[k,'C0'] <- log(subset(pik, iso %in% k)[,'C0'])
  dat[k,'C1'] <- log(subset(pik, iso %in% k)[,'C1'])
}

# compute average growth rates from year0 to year1
dat$gy <- (dat$ypc1-dat$ypc0) / Dt
dat$n  <- (dat$L1-dat$L0) / Dt
dat$p  <- (dat$C1-dat$C0) / Dt

# drop countries with missing data
dat <- na.omit(dat)
# drop too small countries
idx <- (exp(dat$L0) >= 2.0)   # at least 2 million population in 1975
dat <- dat[idx,]
# drop Venezuela (extreme outlier)
iso <- rownames(dat)
iso <- setdiff(iso,'VEN')
dat <- dat[iso,]

# subsets: richer and poorer countries
dat      <- dat[order(dat$ypc0, decreasing = TRUE),]
dat_rich <- dat[1:42,]
dat_poor <- dat[43:nrow(dat),]
datset   <- list(dat,dat_rich,dat_poor)
setnames <- c('all countries','richer countries','poorer countries')

cat('done.\n')

# **** equation (5) **********************************************************

cat('\nEQUATION (5), unconstrained')

regr <- list()

for (s in 1:3){
  regr[[s]] <- lm(
    formula = gy ~ p + n,
    data = datset[[s]]
  )
}

print(
  screenreg(
    regr,
    single.row = FALSE,
    stars = c(0.001,0.01,0.05,0.1),
    symbol = '+',
    digits = 4,
    custom.model.names = setnames,
    custom.coef.names = c('Const','p','n'),
    leading.zero = TRUE
  )
)

cat('\nEQUATION (5), constrained')

regr           <- list()
g              <- vector()
gamma_psi      <- vector()
neutral_growth <- vector()

for (s in 1:3){
  regr[[s]] <- lm(
    formula = gy ~ I(p - n),
    data = datset[[s]]
  )
  gamma_psi[s] <- coef(regr[[s]])[2]
  g[s] <- coef(regr[[s]])[1] / (1-gamma_psi[s])
  neutral_growth[s] = coef(regr[[s]])[1]
}

print(
  screenreg(
    regr,
    single.row = FALSE,
    stars = c(0.001,0.01,0.05,0.1),
    symbol = '+',
    digits = 4,
    custom.model.names = setnames,
    custom.coef.names = c('Const','p - n'),
    leading.zero = TRUE,
    custom.gof.rows = list('g' = g, 'gamma*psi' = gamma_psi,
                         '(1-gamma*psi)*g' = neutral_growth)
  )
)

# **** equation (8) **********************************************************

cat('\nEQUATION (8), linear')

regr <- list()

for (s in 1:3){
  regr[[s]] <- lm(
    formula = gy ~ p + n + sk + edu + Y0 + L0 + C0,
    data = datset[[s]]
  )
}

print(
  screenreg(
    regr,
    single.row = FALSE,
    stars = c(0.001,0.01,0.05,0.1),
    symbol = '+',
    digits = 4,
    custom.model.names = setnames,
    custom.coef.names =
      c('Const','p','n','s_k','edu','log Y(0)','log L(0)','log C(0)'),
    leading.zero = TRUE
  )
)

cat('\nEQUATION (8), non-linear')

gamma_psi      <- vector()
neutral_growth <- vector()
labor_elast    <- vector()

regr <- list()
# These are the starting values I use in EViews
init0 <- list(
  alpha = 0.3,
  beta  = 0.3,
  gamma = 0.2,
  delta = 0.04,
  g     = 0.05,
  const = 0.0
)
# However, R's nls has trouble converging, so we have to
# start very close to the solution.
init <- list()
init[[1]] <- list(
  alpha = 0.25,
  beta  = 0.22,
  gamma = 0.39,
  delta = 0.01,
  g     = 0.07,
  const = 4.8
)
init[[2]] <- list(
  alpha = 0.43,
  beta  = 0.27,
  gamma = 0.14,
  delta = 0.05,
  g     = 0.00,
  const = 3.6
)
init[[3]] <- list(
  alpha = 0.19,
  beta  = 0.23,
  gamma = 0.51,
  delta = 0.00,
  g     = 0.11,
  const = 5.1
)

settings <- nls.control(tol = 2e-4, minFactor = 2^(-16))

for (s in 1:3){
#  cat('sampe:',s,'\n')
  regr[[s]] <- nls(
    gy ~ (1-alpha-beta-gamma)/(1-alpha-beta)*g
    + gamma/(1-alpha-beta)*(p-n)
      - (1/Dt) *
      (1 - exp(
        - Dt * (1-alpha-beta)
          * (
            (1-alpha-beta-gamma)/(1-alpha-beta)*g
            + gamma/(1-alpha-beta)*(p-n)
            + n + delta
          )
        )
      ) *
      (
        Y0 -
          (
            (1-alpha-beta-gamma)/(1-alpha-beta)*L0 +
            gamma/(1-alpha-beta)*C0 + const
          )
          - 1/(1-alpha-beta) *
            (
              alpha*log(sk) +
              beta*log(edu) -
              (alpha+beta) *
                log(
                  (1-alpha-beta-gamma)/(1-alpha-beta) * g
                    + gamma/(1-alpha-beta) * (p-n)
                  + n + delta
                )
            )
      ),
    start = init[[s]],
    control = settings,
    data = datset[[s]],
#    trace = TRUE
  )
  alpha <- coef(regr[[s]])[1]
  beta  <- coef(regr[[s]])[2]
  gamma <- coef(regr[[s]])[3]
  g     <- coef(regr[[s]])[5]
  gamma_psi[s]    <- gamma / (1-alpha-beta)
  neutral_growth[s] <- (1-gamma_psi[s]) * g
  labor_elast[s]  <- 1-alpha-beta-gamma
}

print(
  screenreg(
    regr,
    single.row = FALSE,
    stars = c(0.001,0.01,0.05,0.1),
    symbol = '+',
    digits = 4,
    custom.model.names = setnames,
    custom.gof.rows = list(
      '1-alpha-beta-gamma' = labor_elast,
      'gamma*psi' = gamma_psi,
      '(1-gamma*psi)*g' = neutral_growth),
    leading.zero = TRUE
  )
)

# **** two charts ************************************************************

# compute counterfactual with p = 0

s <- 1  # use sample with all countries

resid <- summary(regr[[s]])$residuals

alpha <- coef(regr[[s]])[1]
beta  <- coef(regr[[s]])[2]
gamma <- coef(regr[[s]])[3]
delta <- coef(regr[[s]])[4]
g     <- coef(regr[[s]])[5]
const <- coef(regr[[s]])[6]

psi    <- 1 / (1-alpha-beta);
xi     <- (1-psi*gamma)*g + psi*gamma*(dat$p-dat$n);
lambda <- (1-alpha-beta) * (xi+dat$n+delta);

loggap <- (
  dat$Y0 - (
    (1-psi*gamma)*dat$L0 + psi*gamma*dat$C0 + const
  ) - psi * (
    alpha*log(dat$sk) + beta*log(dat$edu)
    - (alpha+beta)*log(xi + dat$n + delta)
  )
)
fcstGR <- (xi
  - (1-exp(-lambda*Dt))/Dt * loggap
  + resid
)
fcstYPC = dat$ypc0 + Dt*fcstGR
# fcstYPC should be the same as dat$ypc1

# Now we compute the same thing again,
# but setting p = 0 for all countries

xi_zero_p <- (1-psi*gamma)*g + psi*gamma*(0-dat$n);

loggap_zero_p <- (
  dat$Y0 - (
    (1-psi*gamma)*dat$L0 + psi*gamma*dat$C0 + const
  ) - psi * (
      alpha*log(dat$sk) + beta*log(dat$edu)
      - (alpha+beta)*log(xi_zero_p + dat$n + delta)
  )
)
fcstGR_zero_p <- (xi_zero_p
  - (1-exp(-lambda*Dt))/Dt * loggap_zero_p
  + resid
)
fcstYPC_zero_p <- dat$ypc0 + Dt*fcstGR_zero_p

# make two charts: realized and counterfactual

plot(
  dat$ypc0, dat$ypc1,
  xlab = paste('income per capita (log) in',year0),
  ylab = paste('income per capita (log) in',year1),
  main = 'some convergence',
  type = 'n',
  asp  = 1,
)
text(dat$ypc0, dat$ypc1, rownames(dat), cex = 0.5)
abline(0,1)
grid(NULL,NULL)

plot(
  dat$ypc0, fcstYPC_zero_p,
  xlab = paste('income per capita (log) in',year0),
  ylab = paste('income per capita (log) in',year1),
  main = 'no convergence',
  type = 'n',
  asp  = 1,
)
text(dat$ypc0, fcstYPC_zero_p, rownames(dat), cex = 0.5)
abline(0,1)
grid(NULL,NULL)

plot(
  dat$ypc0, 100 * (fcstGR_zero_p - dat$gy),
  xlab = paste('income per capita (log) in',year0),
  ylab = 'loss of annual growth rate from fixed emissions [%]',
  type = 'n',
)
text(dat$ypc0, 100 * (fcstGR_zero_p - dat$gy),
     rownames(dat), cex = 0.5)
abline(0,0)
grid(NULL,NULL)
