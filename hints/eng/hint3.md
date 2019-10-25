### Example: PCA cases = genes, variables =  treatments mean

<img src="PCA1.png" width="800">

**Graphic of variables (average expression of treatments)**

 - The first component explains approximately 82% of total variability of the means along the different genes. The second component contributes in a 13% approximately. Globally, in dimension 2, the loss of information of the average expression levels is only 5%.
 - All variables (the three means in this case) are well represented with the first two components space.
 -  There is a very high and positive correlation between treatment 3 and treatment 1, on average.
 - The correlation between treatment 2 and the other treatments is weaker, it is positive as well (angle smaller than 90º).
 - *First component* (horitzontal axis) is positively correlated with the three variables (group means), indicating that high values in the first component corresponds to high average expression values in all three treatments. 
 
**Obs:** The existance of correlated means it does not imply that these means are significantly different. Correlation and differences are two complementary aspects in the analysis. For example, two treatments could be highly correlated and present significant differences in various genes or not present any difference. Inversely, two treatments lowly correlated could not have any significant difference or have it in many genes.

 **Individuals (genes) factor map:** *scores plot* 

 - The right plot shows the genes' scores in the two first principal components (each point is a gene) and its genetic funcionality (colors in the legend). *No pattern of grouping by color seems to be clear*, in this case.
 - There is one prominent gene on the positive side (right) of the horizontal axis: this gene has high average expression levels in all the treatments (recall that all the treatments' mean place its arrows on this band).
 - The genes located in the negative side of the horizontal axis tend to present lower levels of expression on average in all treatments. 
- The gens whose scores are in the upper half plane (first quadrant) tend to have greatest mean values in treatment 1, and the converse for scores in the lower half plane (fourth quadrant) that tend to have highest values in treatments 2 and 3, on average. 