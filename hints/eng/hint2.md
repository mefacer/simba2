### Pairwise comparisons (Tukey-Kramer)

We consider the pairs comparison of treatments as a post-hoc test, after it is accepted the existance of significant differences between treatment means for certain genes, with ANOVA. That is why, we will use the same value $\alpha$ set by ANOVA. For the genes where there are not significant differences according to ANOVA, it is no longer appropiate the Tukey-Kramer's test.

**Tukey-Table 1**

This table contains the p-value of Tukey-Kramer, for each pair of treatments in column and for each significant genes, in row. The non-significant genes, according to ANOVA, are not in the table.

*Example of a significant comparison between a pair of treatments:*

- Gene: GGGGG1
- Comparison: 2-1
- Tukey-Kramer p-value: 0.0251 
- Significance level (threshold $\alpha$ (ANOVA)) : $0.05$


There are statistically significant evidences to reject the null hypothesis, in other words, the treatment 2 mean and treatment 1 mean are significantly different.

*Example of a NON-significant comparison between a pair of treatments:*

- Gen: HHHHH2
- Comparison: 3-2
- Tukey-Kramer p-value: 0.1504
- Significance level (threshold $\alpha$ (ANOVA)) : $0.05$

The means of treatment 3 and treatment 2 are not significantly different.

**Tukey-Table 2**

In this table, while this is in line with the previous information, we have supplementary information:

-The values of the columns 1,2 ...,$I$ are the means for the treatments and the rows is every gene, whether it is significant or not. 

-Set one gene, if none of the means does not have a letter, it is because there are not significant differences in ANOVA, as it is also checked in the column p-value.

-Set one gen, the means that have the same letter (exponent) are not significantly different.

-Set one gen, the means that have different letters (exponent) are significantly different. 

-Column "p-value" is, as we said before, the same as ANOVA.

-Column "harmonic_sample_size" is $r^*$: harmonic mean of the number of replicas. As it is considered valid replicas, it may vary according to the gene.

-Column "mse" is the variance estimator of the model obtained from ANOVA. 

-Column "hsd" gives the threshold for the honestly significant differences (HSD=q·SEM). 

(You can revise the theory of the Tukey's test from the dropdown.)



