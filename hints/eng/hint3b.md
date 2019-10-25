### Example: ACP cases = samples, variables =  genes

<img src="PCA1.png" width="800">

**Graphic of variables (genes expressions in the samples)**

**Obs**: Only the significant genes (ANOVA) and well explained genes are represented in the space of the first two components.  

 - The first component explains approximately 31% of the total variability of the means in the different genes. The second component contributes in a 22,5% approximately. Globally, in dimension 2, the loss of information of the average expression levels is 47,5%, because we are using all the genes, the significant and non-significant genes. 
 - There is a moderate and positive correlation between the next significant genes: "IL8, IFNGR1, MUC2, SOD2m", which explain the first component.
 - The correlation between the previous genes and "PPARGC1alfa" gene is null, and between the previous genes and "SLC11A2/DMT1" gene is negative. These two genes explain the second component.
- Other significant genes in ANOVA are not represented in dim2, other components are needed.
- Most of the significant and well represented genes are located in the fourth quadrant.
- The gen function is represented by the color of the arrow (colors legends). No clear pattern.

**Graphic of cases (samples)** 

 - The right plot shows the samples (dots) and the treatment applied to the samples (colors legens).
 - Most of the treatment 1 samples (black) are located in the fourth quadrant. So, treatment 1 has higher expression levels in the "IL8, IFNGR1, MUC2, SOD2m" genes and are located in the same quadrant.
 - There is a prominent sample at the positive side (top) of the vertical axis: This sample is from treatment 3 and has high levels in the "PPARGC1alfa" gene and low levels in the "IL8, IFNGR1, MUC2, SOD2m" genes, located in the opposite quadrant.
