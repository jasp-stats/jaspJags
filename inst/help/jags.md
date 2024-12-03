JAGS
===

JAGS -- Just Another Gibbs Sampler -- is software for general purpose Bayesian inference.
One can specify a model using JAGS syntax and let JAGS draw samples from the posterior distribution.

### Input
-------

#### Model Input
- Model: Enter the desired model. Columns in the data can be directly referred to. If these contain spaces, then the reference must also contain spaces.
- Parameters in model: A box that automatically detects which parameters the model contains.
- Show results for these parameters: Determines which parameters are shown in tables and plots.

#### Observed Values
Sometimes it is practical to directly specify the observed data without loading a data set.
Here such observations can be listed.
- Parameter: The name to be used in the JAGS model code.
- R Code: the value for the data. This can also be R code.

### Customizable Inference
#### Parameter selection
For which parameters to show custom results.
- Parameter: Select a parameter from the model.
- Parameter subset: Optionally specify a subset of the parameter, e.g., 1:4 or 1, 3, 5:8.

#### Superimpose data
Optional, should data be superimposed on the plots?
- Data: a variable in the dataset.
- Split by: a variable in the dataset.

#### Plots
- Plot type: The type of plot to show. Currently, only "None" or "Stacked density" are supported.
- Order parameters by: The metric used to order the parameters in the stacked density plot.
- Plot type for superimposed data: The type of plot to show for the superimposed data.
- Shade interval: Whether to shade an interval in the plot, and if so, which interval.
  - Intervals: Either Credible intervals, HDI, or a custom interval.

#### Estimation
Specify which statistics are computed for each parameter and shown in a table.
##### Summary Statistics
- Mean: The average value of the posterior samples.
- Median: The middle value of the posterior samples.
- SD: The standard deviation of the posterior samples.
- R-hat: The potential scale reduction factor, used to diagnose convergence of the MCMC chains.
- Effective sample size: An estimate of the number of independent samples from the posterior distribution.

##### Intervals
- Credible Interval: An interval within which a certain percentage of the posterior samples fall.
- HDI (Highest Density Interval): The narrowest interval containing a specified percentage of the posterior samples.
- Custom Interval: A user-defined interval for the posterior samples.

#### Plots
- Color scheme for plots: determines the color scheme.
- Aggregate chains for densities and histograms: If checked, the samples of different chains are aggregated in density plots and histograms. If unchecked, there are separate colors per chain.
- Density: Show a density plot of the posterior samples.
- Histogram: Show a histogram of the posterior samples.
- Trace: Show a trace plot of the posterior samples.
- Autocorrelation: Plot the autocorrelation of the posterior samples.
  - No. lags: the maximum number of lags to show in the autocorrelation plot.
  - Type: whether to display the autocorrelation as a bar at each lag, or as a line that connects subsequent lags.
- Bivariate scatter: Show a bivariate scatter plot of all pairs of variables. Only shows output when more than 1 parameter is sampled.
  - Diagonal plot type: Show a density plot or a histogram on the diagonal entries of the scatter plot.
  - Off-diagonal plot type: Show a hexagonal bivariate density plot, or a contour plot on the off-diagonal entries of the scatter plot.

#### Initial Values
For each parameter in the model, it is possible to specify an initial value.
Initial values can be numbers, but also R code.
The R code is evaluated separately for each chain, so `rnorm(1)` yields different initial values for each chain.

#### Advanced
- MCMC parameters
  - No. samples: The number of samples to draw from the posterior distribution that are used for results (tables, plots).
  - No. burnin samples: The number of samples to draw from the posterior distribution and immediately discard.
  - Thinning: Every nth value of `No. samples` is kept for the results, where n is given by `Thinning`.
  - No. chains: The number of MCMC chains to run.

Note: Unlike some other software programs, JASP first draws `No. burnin samples` from the posterior distribution and afterward `No. samples`, keeping every nth sample as specified by `Thinning`.

- Set seed: set a seed for JAGS. The same seed and model specification yields the same results.

- Show results for: By default, `all monitored parameters` is selected which implies that JASP stores the MCMC samples for all parameters in the model. However, for large JAGS models storing all MCMC samples may take too much memory. By selecting `selected parameters`, one can first decide for which parameters the MCMC samples should be stored, and in a next box, decide which of these parameters should be shown in the results.

- Show Deviance: show the Deviance statistic.

### Output
-------

#### MCMC Summary

#### Plots

##### Marginal Density
Shows the marginal density of the posterior samples for each parameter selected under `Show results for these parameters`.
If `Aggregate chains for densities and histograms` is checked then a single density line for each parameter is shown.
Otherwise, different colors are used to represent the different chains.

##### Marginal Histogram
Shows the marginal histogram of the posterior samples for each parameter selected under `Show results for these parameters`.
If `Aggregate chains for densities and histograms` is checked then a single histogram for each parameter is shown.
Otherwise, different colors are used to represent the different chains.

##### Trace Plots
Shows a trace plot of the posterior samples for each parameter selected under `Show results for these parameters`.

##### Autocorrelation Plots
Plots the autocorrelation of the posterior samples for each parameter selected under `Show results for these parameters`.

##### Bivariate Scatter Plot
Show a matrix plot of all pairs of variables.
The diagonal plot entries show a density plot or histogram.
The off-diagonal plot entries show a bivariate plot, either a hexagonal histogram or a contour plot.

### References
-------
Plummer, M. (2003). JAGS: A program for analysis of Bayesian graphical models using Gibbs sampling. In K. Hornik, F. Leisch, & A. Zeileis (Eds.), *Proceedings of the 3rd international workshop on distributed statistical computing.* Vienna, Austria.

### R Packages
---
- coda
- graphics
- ggplot2
- hexbin
- rjags
- stringr
- stats




