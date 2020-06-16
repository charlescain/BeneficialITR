# BeneficialITR

- The BeneficialITR.R provides functions to calculate the power to identify a beneficial ITR.
- This project was created by Charles Cain, David Vock, Thomas Murray and maintained by Charles Cain (cainx191@umn.edu)

### one.value: provides a single value under given beta coefficients and data
- n: sample size of simulation,
- beta1: vector of beta1 values (main effects of predictors), 
- beta2: coefficient for main effect of treatement,
- beta3: vector of beta3 value (interaction effects of predictors with treatment), 
- sigma: residual variance of outcome,
- x.valids: Validation matrix of predictors, 
- model.selection: character specification of model selection method; options: none, forward, lasso, modified, elastic, rf
- lasso.penalty: character spectification of the type of penalty parameter used when using LASSO for model selection
- g.model.type: character specification of whether parameteric or non-parametric sampling should be used

### beneficial.lasso: provides a model where the predictors are selected to guarantee identification of a static rule when no treatment effect heterogeneity exists
- y: vector of outcome data
- a: vector treatment assignment, -1 or 1
- x: matrix of predictor covariates
- q: user specfiied value for minimum proportion of time identifying static rule

### betas.fun: function to identify beta coefficients from other values
- delta: main treatment effect,
- nu: percent that benefit from control,
- V_y: variance of outcome in control group,
- R_t: R^2 value in treatment group, 
- p: number of predictors (dimension of beta1 and beta3),
- betas.type: how the coefficients are specified (single: single predictor non-zero, even: all equal, diminishing: harmonic diminishing effect),
- sig.p: covariance structure of predictors

### power.fun: function to calculate the power to identify a beneficial ITR using model coefficients
- n: sample size
- beta1: vector of beta1 values (main effects of predictors), 
- beta2: coefficient for main effect of treatement
- beta3: vector of beta3 value (interaction effects of predictors with treatment), sigma: residual variance of outcome
- betas.type: how the coefficients are specified (single: single predictor non-zero, even: all equal, diminishing: harmonic diminishing effect),
- sig.p: covariance structure of predictors, model.selection: model selection method (none, forward, lasso, elastic)

### power.tailor: function to calculate the power or minimum sample size to identify a beneficial ITR using easier to understand values
- n: sample size of simulation
- power: probability of loss less than that of a homogeneous treatment rule. 
- delta: main treatment effect,
- nu: percent that benefit from control,
- V_y: variance of outcome in control group,
- R_c: R^2 value in control group, 
- p: number of predictors (dimension of beta1 and beta3),
- betas.type: how the coefficients are specified (single: single predictor non-zero, even: all equal, diminishing: harmonic diminishing effect),
- sig.p: covariance structure of predictors,
- model.selection: model selection method; options: none, forward, lasso, modified, elastic, rf











