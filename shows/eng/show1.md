## Analysis of variance (ANOVA)

Analysis of variance (ANOVA) is a classical method to compare means among groups, two groups or more, and determine the statistical significance (or non-significance) of the differences between population means (expected values). 


**Observation**. ANOVA involves certain assumptions about the probabilistic model for the response: uncorrelation, normality, centering and constant variance. Important desviations in these assumptios can lead to misleading and inaccurate results. In some situations, there are transformations that can be used to avoid the violation of these assumptions, such as the *logarithmic* transformation that is usually applied, among others, to the gene expression data.

Assuming $N$ equally distributed observations in $k$ groups, we define $n=\frac{N}{k}$. Then, $x_{ij}$ is the observation of the subject $j$ corresponding to the group $i$. In this case, the study is called *balanced*, the number of subjects is equally represented in each study group (replicas), but the results are extended to the non-balanced case. $\bar{x}$ is denoted as the mean of the global sample, and $\bar{x_{i}}$ as the mean of the group $i$. The observations can be written as: 

$$x_{ij} = \bar{x.} + (\bar{x_{i}} - \bar{x.}) + (x_{ij} - \bar{x_{i}})$$


This brings us to the next linear model:

$$X_{ij} = \mu + \alpha_{i} + \epsilon_{ij}$$

where $\mu$ is the global expected value and $\alpha_{i}$ is the differential part of the group $i$. Based on some assumptions, $\epsilon_{ij}$ are iid with normal distribution:

$$\epsilon_{ij} \sim \mathcal{N}(0,\,\sigma^{2})$$

Th null hypothesis of ANOVA is that the theoretical mean is the same for all groups. That is to say, the differential parts are zero:

$$\alpha_1 = \alpha_2 = ... = \alpha_k=0$$
The ANOVA test statistic is based on descomposing the variability. The total variability of the data can be measured with the sum of the square of the differences between $x_{ij}$ and $\bar{x}$:


$$SST\,(\text{Total sum of squares}) = \sum_{i=1}^{k} \sum_{j=1}^{n_{i}} (x_{ij} - \bar{x})^2$$

The total variability can be broken in two summands
$$SST = SSG + SSE$$
expressing:

- The variability between groups:

$$SSG \,(\text{Sum of squares between groups}) = \sum_{i=1}^{k} n_{i}(\bar{x}_{i} - \bar{x})^2$$

with $k-1$ degrees of freedom and chi-squared distribution, when the null hypothesis is true.

- The variability "withing" or "intra" groups, also named as residual variability:

$$SSE \,(\text{Residual sum of squares}) = \sum_{i=1}^{k} \sum_{j=1}^{n_{i}} (x_{ij} - \bar{x_{i}})^2$$

with $N-k$ degrees of freedom and chi-squared distribution.

If the variability between groups is large in relation to the within groups variability, then the data suggests that the population means are significantly different. Because if there are not differences between groups, the sum of squares between groups $SSG$, and the quadratic mean between groups $MSG$, will be close to 0, while if there are differences between the groups, while if there are differences between the groups, we would expect a large $MSG$ compared to the quadratic mean within group, also called residual or error quadratic mean $MSE$:

$$MSG = \frac{SSG}{(k-1)} \qquad MSE = \frac{SSE}{(N-k)}$$


The ANOVA test statistic is defined as the ratio between the two quadratic means:

$$F = \frac{MSG}{MSE}$$

If the null hypothesis is true, the statistic $F$ follows a Fisher-Snedecor distribution with $k-1$ and $N-k$ degrees of freedom and 
the value of $F$ is expected to be close to 0. On the other hand, if the quadratic mean between groups $MSG$ is large, this implies a large value of the $F$ statistic that will be far from 0 in the right tail of the distribution, that is to say, a small $p$-value, with the consequent rejection of the null hypothesis and the acceptance of significant differents between the group means. 

*Remark:* Since ANOVA examines the two sources of the total variance and note which part contributes more, it is called analysis of the variance, although the purpose is to compare the group means.
