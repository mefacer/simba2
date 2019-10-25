## False discovery rate (FDR)

There are several methods to correct the problem of multiple comparisons between pair of treatments. The simplest method is Bonferroni, where each $p$-value is multiplied by the number of performed tests (delimiting the maximum probability to 1). It is a conservative method and it is not recommended for the analysis of gene expression data.

If there is a large number of tests (*large-scale multiple testing*), as in expression data, the classical correction methods (Bonferroni, Tukey, etc.) can be conservative and could avoid to detect real differences. An alternative way is to control the FDR of Benjamini $\&$ Hochberg (FDR or BH-FDR).

FDR is defined as the expected proportion of false negatives among all the test considered as significant. The aim of controlling the FDR is establish the significance threshold for a set of tests such that, of all of these tests considered as significants, the false discovery rate is not longer than a certain threshold. 

A further benefit of FDR is the easy interpretation, for example, if a study publishes statistically significant results for a 10$\%$ $FDR$, the lector has the assurance that, no more than 10$\%$ of the results, considered as significants, can be truly false positive.

The first approximation to control the FDR was decribed by Benjamini $\&$ Hochberg on 1995. If it is desired to control that FDR do not exceed a $d$ percentage in a study with $n$ comparisons, then:

- Order the $p$-values ($p_{1}$,$p_{2}$,..., $p_n$) of the $n$ test in increasing order.
- $k$ is defined as the last position that satisfies $p_i \leq d\frac{i}{n}$.
- All the $p$-values until the position $k$ ($p_{1}$,$p_{2}$,..., $p_k$) are considered as significants.
