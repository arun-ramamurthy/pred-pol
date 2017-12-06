Simple Models for Predictive Policing
========================================================
author: Stat 157 Group 1
date: 2017.12.07
autosize: true
font-family: 'Avenir'
transition: linear


First Slide
========================================================

For more details on authoring R presentations please visit <https://support.rstudio.com/hc/en-us/articles/200486468>.

- Bullet 1
- Bullet 2
- Bullet 3

Introduction
========================================================
type: section


Predictive Policing
========================================================


ETAS Models
========================================================

Drawbacks of Current Predictive Policing Models
========================================================


Literature Review
========================================================
type: section


Lum 2016
========================================================


Luen 2010
========================================================


Mohler 2011 & Mohler 2016
========================================================


Methodology
========================================================
type: section

Data
========================================================
- drug_crimes_with_bins.csv ("bin","OCCURRED","LAG")
- oakland_grid_data.rds (geographical coverage of Oakland bins)

Conceptual Overview of Model
========================================================

Two features that may help us predict in which bins crime may take place:

* Historical data from each bin
* Historical data from each bin's neighbor bins

We will combine these two to arrive at a $\textit{total bin score}$ for each bin on each date.

Historical Crime Score
========================================================

To assess today's historical crime score for each bin, we looked at crime data for the trailing 365 days and calculated a weighted sum. The weights were a simple exponential decay function, as shown below:

$$W_{t} = e^{-rt}$$

where $\textit{t}$ is the lag for that particular historical date. Formally, the historical crime score equation is reproduced below:

$$H_{i,\tau} = \sum_{t=1}^{365} W_{t} \times C_{i, \tau - t}$$

where:

* $C_{i, \tau - t}$ = Number of crimes that occurred in bin $\textit{i}$ at time $\textit{\tau - t}$
* $\tau$ = Today's date
* $\textit{t}$ = The time lag
* $\textit{r}$ = The exponential decay rate

Neighbor Crime Score
========================================================

Tuning Our Model
========================================================

We performed a grid search to find the optimal parameter set for our exponential decay rate (r) and our neighbor coefficient (s). The results are shown below:

![grid search](presentation_figure/grid_search.jpg)

Unit Testing Our Model
========================================================
We utilized the `testthat` testing framework in R to unit test our functions.

Tested functions:
- `get_trailing_table`
- `get_bin_score`
- `get_bin_scores`
- `get_neighbor_adjusted_bin_score`
- `get_maximal_capture`
- `get_predicted_bins`

Results
========================================================
type: section


In Comparison to PredPol
========================================================


Discussion
========================================================
type: section


Interpretation of Results
========================================================


Limitations
========================================================


Social Consequences
========================================================
