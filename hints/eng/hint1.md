## Interpretation of the table with the statistic, ANOVA-p-value and FDR-p-value

The statistic and the ANOVA-p-value correspond to the test for differences between means for each gene, individually. The statistic value marks the right tail of the distribution, the tail area is the ANOVA-p-value. 

On the other hand, the FDR p-value is referred to the set of tests made by all the genes. It is a combined or experimental p-value. 

**Ficticious examples for the differential expression interpretation of a gene**

Fixing a threshold $\alpha=$**0.05** for the ANOVA-p-value, $\alpha$ is the maximum permissible nominal error rate for each ANOVA test (each gene).

- Gene: GGGGG1
- ANOVA p-value= 0.1275

We will say that gene GGGGG1 does not show any significant differences between the average expressions because the ANOVA-p-value is larger than the threshold (0.1275 $>$ 0.05). No need to look at the FDR-p-value.

- Gene: HHHHH2
- ANOVA-p-value = 0.0423
- FDR-p-value = 0.0572

There is enough evidence to accept that HHHHH2 gene shows statistically significant differences between the treatments means, as the ANOVA-p-value is fewer or equal to the establised threshold (0.0423 $\leq$ 0.05). However, as the FDR-p-value is greater than 0.05 (0.0572), this indicates that if we accept the gene has significant differences, the decision set of the error probability (genes marked as significant) will surpass the nominal 5\% and will stand at about 6\%.

- Gene: KKKKK3
- ANOVA-p-value = 0.0102
- FDR-p-value = 0.0374

There is enough evidence to accept that KKKKK3 gene presents statistically significant differences between the treatments means, as the p-value (ANOVA) is fewer or equal to the stablished threshold (0.0102 $\leq$ 0.05). Besides, FDR is below 0.05 (0.0374), this indicates that if we accept the gene has significant differences, the decision set of the error probability (genes marked as significants) is controlled by the nominal 5\%.

**Recommendation:** <font color='blue'> In our field, where the number of samples is limited, if you have a moderate amount of genes (50 or less), the decision will be taken based on the ANOVA-p-value, but monitoring and reporting the FDR-p-value in order not to exceed "unacceptables levels" (suggestion: the double of the $\alpha$ value).</font>