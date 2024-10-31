UNDER WORK!!!




# Age of onset with Dementia as an example (AOO_Dementia)
This is a github repository for the code used for the analysis conducted in the paper: "A New Method for Calculating the Mean Age of Onset in the Presence of Competing Risks and Censoring: Usiing Dementia as an Example" published in YYYYYY. [Paper]()

## Functions

Below is the R-functions created to make the analysis for the paper. 

| R-function            | Calculates                                               |
| --------------------- | -------------------------------------------------------- |
| `CIF()`               | The cumulative incidence function                        |
| `median_age()`        | The median age from the CIF                              |
| `quantile_age()`      | The quantile age from the CIF                            |
| `mean_age()`          | The mean age from the CIF                                |
| `mean_age_of_onset()` | Combines the functions above to one function             |
| `pseudo_theta()`      | Used to compute AOO in creating the pseudo-observations  |

### Libraries used

| Library       | Used in                                         |
| ------------- | ----------------------------------------------- |
| prodlim       | Computing CIF in mean_age_of_onset()            |
| pracma        | mean_age() for calculating area below the curve |
| doParallel    | Run the code quicker                            |
| foreach       | Run the code quicker                            |
| ggplot2       | Creating plots                                  |
| gridExtra     | Creating panel plots                            |
| ggExtra       | Adding marginal histogram                       |
| mgcv          | Computing GAM                                   |
| splines       | Creating splines                                |
| dplyr         | Data manipulation                               |

At each of the R-scripts, the packages used in a function is listed. 

## Description of input data 

We work in a competing risk set-up. The competing states are: Healthy, Diagnosed with dimentia (F00-F09), and Death & Emigration. Where the two last states are exiting states.

### Scenarios

Complete follow-up: 
The study looks at individuals who are alive and healthy in Denmark who turns 75 years old between 01/01/1994 and 31/12/2001. The study ends at the end of 2021, or when the indivuals turn 95 years old, or when entering an exiting state, whatever comes first. In this scenario we will possible have full follow-up of the individuals.

Total cohort: 
The study looks at individuals who are alive and healthy in Denmark who turns 75 years old between 01/01/1994 and 31/12/2021. The study ends at the end of 2021, or when the indivuals turn 95 years old, or when entering an exiting state, whatever comes first. In this scenario not all individuals can be in the study until there 95'th birthday. 

The data structure is similar for the two scenarios. The code described is generalized, since the analysis is similar for the two scenarios. In practise the code is run for each of the scenarios. 

### Cleaned data

| Column number | Column name   | Description                                                                          |
| ------------- | ------------- | ------------------------------------------------------------------------------------ |
| 1             | PID	          |	Person ID                                                                            |
| 2	            | KQN	          |	Sex                                                                                  |
| 3	            | fdato	        |	Date of birth                                                                        |
| 5	            | start_date	   |	Date of entry in study                                                               |
| 6	            | end_date	     |	Date of exit of study                                                                |
| 7	            | censor_stat	  |	Censoring status at end of study (1: Healthy, 2: Diagnosed, 3: Death and emigration) |
| 8	            | Tstart	       |	Age at entry of study                                                                |
| 9	            | Tslut	        |	Age at exit of study                                                                 |

In the code it will be calles "dat".

### Data with pseudo observations 

| Column number | Column name   | Description                                                                          |
| ------------- | ------------- | ------------------------------------------------------------------------------------ |
| 1             | PID	          |	Person ID                                                                            |
| 2	            | KQN	          |	Sex                                                                                  |
| 3	            | fdato	        |	Date of birth                                                                        |
| 5	            | start_date	   |	Date of entry in study                                                               |
| 6	            | end_date	     |	Date of exit of study                                                                |
| 7	            | censor_stat	  |	Censoring status at end of study (1: Healthy, 2: Diagnosed, 3: Death and emigration) |
| 8	            | Tstart	       |	Age at entry of study                                                                |
| 9	            | Tslut	        |	Age at exit of study                                                                 |
| 10	           | theta	        |	Pseudo-observation                                                                   |
| 11	           | coefYear2001  |	                                                                                     |


## Analysis


File with computations of pseudo observations

Computing linear model with splines

This is done similar in both scenarios, therefore the code is fairly similar and therefore will only be one example of the code. 

### Availability of data and materials

Data for this study os property of Statistic Denmark and the Danish Health Data Authority. The data are available from the authorities, but restrictions apply.

## Math

Lets define $A$ as the maximum age of the participants. In this paper we have set $A=8$. Further let $Ã$ denote the median age of onset. 
we wish to compute the mean age of onset for cause $i$ in a competing risk set-up. 
Note that the cumulative incidence function (CIF), in a competing risk set-up, is defined as the Aalen-Johansen estimator:
$$F_i (t) = \int^t_0 S(u) \alpha_i (u) du,$$
where $S(t)$ is the survival function and $\alpha_i (t)$ is the cause-specific hazard for cause $i$ at time $t$. 
Since we are only interested in the cause *Diagnosed*, we will suppress the $i$ in the future. 
The median can be found as $Â := \frac{F (A)}{2}$. The idea is then to transform the CIF to a probability; 
therefore, we divide the the CIF with $F(A)$, this is then subtracted from 1, s.t.
$$G(t) = 1 - \frac{F(t)}{F(A)} .$$
The mean age of onset can then be computed as 
$$\mu = \int^A_0 G(t) dt = \int^A_0 1 -  \frac{F(t)}{F(A)}  dt = A - \frac{1}{F(A)} \int^A_0 F(t) dt.$$
It can be seen as a restricted mean age of onset. 

![image](https://github.com/CBeck96/AOO2023/assets/43062260/9e70d6d5-71ee-4d21-9239-aec82975b0e5)
![image](https://github.com/CBeck96/AOO2023/assets/43062260/b60396c4-0586-4026-8e54-aeee9a9e2cef)


## Simulations

General information about the concept of the simulations.

### Aim for simulation:
We wish to compare methods for estimating the mean age of onset (MAOO) in competing risk survival analysis. We wish to validate our method.

### Size of simulation:

Number of observations and number of replications.

### Metrics for methods

- Average of means
- Average of se(means)
- Absolute bias
- Relative bias
- Relative root mean squared error
- MSE/RMSE
- Quantiles
- Coverage probability

(Monte Carlo SE of estimates)
