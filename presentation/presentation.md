Simple Models for Predictive Policing
========================================================
author: Stat 157 Group 1
date: 2017.12.07
autosize: true
font-family: 'Avenir'
transition: linear


Introduction
========================================================
type: section


Predictive Policing
========================================================
- Predictive Policing is the usage of mathematical and analytical techniques in law enforcement to identify potential criminal activity
- Police officers can anticipate crime in advance and be present on the purported crime scene to prevent the crime from happening
- Requires some sort of training data to predict crimes
    - PredPol, a leading predictive policing software company, only requires geographical location, timestamp, and crime type

ETAS Models
========================================================
- ETAS: Epidemic-type Aftershock Sequences
- Models a self-exciting point process
  - One event can cause aftershock events, those aftershock events can cause more aftershocks, etc.
- Key properties of the ETAS model are “very sensitive to distributional assumptions and parameter values” (Luen 2010)
- 2 main assumptions:

1) Earthquakes can be split into "parent" and "offspring" events through seismicity information

2) Arrival of "parent" earthquakes is Poisson-distributed

Drawbacks of Predictive Policing Models
========================================================
- Since police collected crime data tends to suffer from systemic bias, predictive model outputs suffer from bias as well
- In the case of PredPol, software is disproportionately expensive for its relatively weak predictive power
  - $$$ comes from public funds
- Police departments blindly trust model outputs
  - Officials can hide behind authority of "math" when following biased model outputs

Literature Review
========================================================
type: section

Lum 2016
========================================================
- “police officers – either implicitly or explicitly – consider race and ethnicity in their determination of which persons to detain and search and which neighbourhoods to patrol”
- “[feedback loops facilite] increasingly disproportionate policing of historically over-policed communities”

Luen 2010
========================================================
- "[Since] there may or may not be physical differences between ‘background’ and ‘offspring’ events, all existing methods to differentiate between them based on times, locations, and magnitudes are arbitrary"
- Found that “automatic alarm strategies have almost all of the predictive success of the ETAS model, while avoiding many of that model’s drawbacks”

Mohler 2011 & Mohler 2016
========================================================
- Motivates ETAS as applied to crime: "criminological research has shown that crime can spread through local environments via a contagion-like process"
- Tests ETAS on crime data: "The difference in the accuracy between analyst and ETAS predictions reflects advantages that algorithmic approaches have in characterizing dynamic spatio-temporal patterns"
- Shows ETAS is superior to simpler 3-day and 7-day hotspot methods: "ETAS doubles the amount of crime predicted relative to hotspot maps with short measurement time windows"

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

where $\textit{t}$ is the lag for that particular historical date.

Historical Crime Score (cont.)
========================================================

Formally, the historical crime score equation is reproduced below:

$$H_{i,\tau} = \sum_{t=1}^{365} W_{t} \times C_{i, \tau - t}$$

where:

* $C_{i, \tau - t}$ = Number of crimes that occurred in bin $\textit{i}$ at time $\textit{\tau - t}$
* $\tau$ = Today's date
* $\textit{t}$ = The time lag
* $\textit{r}$ = The exponential decay rate

Neighbor Crime Score
========================================================

In addition to crime in the bin itself, we incorporated the historical crime from neighboring bins in order to capture "spillover rates" and bins in high crime areas. The neighbor crime score of bin $\textit{i}$ was simply the sum of the $\textit{historical crime scores}$ of the neighbors of $\textit{i}$:

$$N_{i,\tau} = \sum_{n \in N_{i}} H_{n,\tau}$$

where:

* $\textit{i}$ = Bin number
* $N_{i}$ = List of neighbors of bin $\textit{i}$
* $\tau$ = Today's date

Bin Score
========================================================
To compute the final bin score, we calculated a weighted sum of the $\textit{historical crime score}$ and the $\textit{neighbor crime scores}$, with weight $\textit{s}$, which we call the neighbor coefficient:

$$score_{i,\tau} = (1-s) \times \sum_{t=1}^{365} e^{-rt} C_{i, \tau - t} + s \times \sum_{n \in N_{i}} \sum_{t=1}^{365} e^{-rt} C_{n, \tau - t}$$


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


Tuning Our Model
========================================================

We performed a grid search to find the optimal parameter set for our exponential decay rate (r) and our neighbor coefficient (s). The results are shown below:
<p align="center"><img src="grid_search.png" /></p>

**Final Chosen Model**: r = 0.00, s = 0.00

AUC Curve Comparison
========================================================
For varying number of police officers deployed, we compared the percent of crimes missed by the model between our model and PredPol.
<p align="center"><img src="AUCCurve.png" /></p>

Discussion
========================================================
type: section


Interpretation of Results
========================================================
- Why were the final tuned hyperparameters r=0, s= 0?
  - r:
  - s:
- Our model, which makes much fewer distributional assumptions and is very basic to implement, performs comparably to PredPol's complex ETAS model

Limitations
========================================================


Social Consequences
========================================================
- PredPol costs the public thousands of dollars each year
  - Our model is free
- PredPol predictive power is overstated
  - Our model performs just as well (~30% accuracy)
- PredPol does not release the mechanisms behind its software
  - Our model uses only open-source tools
- PredPol leads to feedback loops that enforce systemic bias
  - Our model will suffer from these same biases, but is simpler so police officials and policy groups can evaluate bias without the assumptional artifacts of more complex models

