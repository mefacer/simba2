## Tukey's method (*honestly-significant-difference* HSD)

Set a significance level $\alpha$, when the number of possible comparisons is high, there could be a Type error 1 higher than $\alpha$.

To identify which treatments are significantly different between considering all possible pairwise differences of means and afterwards correct the problem with the Type error 1 inflation, Tukey's method is proposed. The hypothesis are:
$$H_{0}: \mu_j = \mu_k \text{ to all pair of means}$$

$$H_{1}: \mu_j \neq \mu_k \text{ at least one pair of means}$$

The test statistic is:
$$Q = \frac{\bar{X}_j - \bar{X}_k}{\sqrt{\frac{MSE}{r^*}}}$$
where $\hat{X}_j$ and $\hat{X}_k$ are the means of treatments $j$ and $k$, respectively, $MSE$ is the error or residual variance estimator of the adjusted ANOVA model; and $r^*=r$ is the number of replicas per treatment, for the balanced case. Under the null hypothesis and the model assumptions, this statistic follows the *studentized range* distribution with parameters $I$ and $N-I$, where  $I$ is the number of levels that the factor has and $N$ is the sample size. For the non-balanced case, the test is called *Tukey-Kramer*, which propose to use $r^*=r_H$ the harmonic mean for the number of replicas of the two treatments. Alternatively, in some texts is recommended the harmonic mean of the set of treatments replicas to have the same margin of error for all the differences, that is,
$$r^* = \frac{2}{\frac{1}{r_j}+\frac{1}{r_k}}\quad \text{o bé}\quad r^* =r_H= \frac{I}{\sum_{j=1}^I\frac{1}{r_j}}$$
To do the Tukey-Kramer test, setting a significance level $\alpha$, is equal to consider those differences named *honestly significants* as significant, which are not over the $HSD$ threshold
$$|\bar{X}_j - \bar{X}_k | > HSD = q_{\alpha,I,N-I}\sqrt{\frac{MSE}{r^*}}$$
where $q_{\alpha,I,N-I}$ is the theorical quantile of the studentized range distribution determining a right tail of area equal to $\alpha$. 

The confidence interval of the mean differences is:
$$IC(\mu_j - \mu_k)= (\bar{X}_j - \bar{X}_k) \pm HSD$$
An alternative way of deciding the test is checking if 0 is contained within the confidence interval. If 0 is not within the confidence interval, the hypothesis of significantly different means for these groups is accepted.