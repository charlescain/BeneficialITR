# BeneficialITR

- The BeneficialITR.R provides functions to calculate the power to identify a beneficial ITR.
- This project was created by Charles Cain, David Vock, Thomas Murray and maintained by Charles Cain (cainx191@umn.edu)

## one.value: provides a single value under given beta covariates and data
- n: sample size of simulation,
- beta1: vector of beta1 values (main effects of predictors), 
- beta2: coefficient for main effect of treatement,
- beta3: vector of beta3 value (interaction effects of predictors with treatment), 
- sigma: residual variance of outcome,
- x.valids: Validation set of predictors, 
- model.selection: model selection method (none, forward, lasso, modified, elastic, rf)
- lasso.penalty: type of penalty parameter used when using LASSo for model selection
- g.model.type: character specification of whether parameteric or non-parametric sampling should be used

