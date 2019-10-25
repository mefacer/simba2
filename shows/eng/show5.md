## Heatmap

A *heatmap* is a graphical representation of data where the individual values contained in a matrix are represented as colors.
Heatmaps used for expression genes, the data is displayed in a grid where each row represents a gene and each column represents a sample or case. The color of the boxes is used to represent changes of gene expression. The heatmap may also be combined with *clustering* methods which are grouping genes and samples together based on the similarity of their gene expression pattern. The gene clusters can be useful for identifying genes that are commonly regulated and detect patterns under particular conditions (covariables), such as genic function. Equally, the clustering of samples will be used to see if the samples are grouped naturally according to the treatments. These clusters are called hierarchical clustering, which implement agglomeration procedures by steps: starting from a *distance matrix*,  each step will determine new groups by grouping two of the previous ones according to a *link criterion*: simple, complete, Ward's method, etc.

The implementation with R package "ComplexHeatmap" allows to use distances matrices and different link functions by rows and columns. We have chosen the *Euclidean distance* and *Ward's method* for the samples, while for the genes we have chosen the *Pearson distance* ($d=\frac{1-r}2$, where $r$ is the Pearson correlation coefficient) and *Complete-linkage clustering method*. <font color='blue'> We believe the Pearson coefficient is more suitable for comparing expressions based on *openarrays* because we can compare the correlations between the expressions of the two genes, without comparing the specific expression levels.</font>


**Complete-linkage clustering**

At each intermediate step of the analysis, the possibility of joining each pair of the existing $C_p$ and $C_q$ groups is considered. That is why the distance between the two groups is "recalculated" as the distance between the *farthest neighbors* of the two groups. That is, if $C_p$ and $C_q$ are two clusters from the previous step, it is recalculated

$$d(C_p, C_q)\colon = \max_{i,j} d( x_{i\cdot}^p , x_{j\cdot}^{q} ); \qquad x_{i\cdot}^p \in C_p ,\, x_{j\cdot}^{q} \in C_q$$
where $x_{i\cdot}^p$ is the vector of observations in all variables of the subject $i$, that belongs to the group $C_p$ in the previous step. 
In the current step, it is opted for the merging of those two groups that have a lower "recalculated distance". The complete linkage method can be associated to several distances or dissimilarities.

**Ward's method**

Ward suggested a method associated with the Euclidean distance. The idea is that the loss of information occurring integrating different individuals into clusters can be measured through the total sum of the squares of deviations between each point (subject) and the mean of the cluster in which it integrates. To have an optimal clustering (the groups formed distorted the original data as low as possible), he proposed the following strategy: *At each step of the analysis, the possibility of the union of all pairs of groups is considered and it is opted for the merging of those two groups that least increase the sum of the squares of deviations when joining.*

To do it more formal, we define:

- $x_{ik}^p$ as the value of the $k$-th variable over the $i$-th subject of the $p$-th cluster, with $n_p$ subjects.
- $m^p$ as the centroid of the cluster $C_p$, with $m_{k}^p$ components in the variables $X_k$.
- $E_p$ as the error sum of squares of the cluster $C_p$, that is, the square Euclidean distance divided by each subject of the cluster $p$ to its centroid:
$$E_p = \sum_{i=1}^{n_p}\sum_{k=1}^{K}(x_{ik}^p - m_{k}^p)^2$$

- $E$ as the error sum of squares for all the clusters, that is, if assuming $h$ clusters: $E = \sum_{p=1}^{P} E_p$

The process begins with $n$ clusters, as many as subjects, and each cluster has only one subject, which, therefore, coincides with the center of the cluster. In this first step, $E_p = 0$ for each cluster and we have $E = 0$. Assuming that $C_p$ and $C_q$ clusters bind to a new cluster $C_t$, then we define the $E$ increment as,
$$\bigtriangleup E_{pq} = E_t - E_p - E_q$$
In this phase, those two clusters are joined, whose union provides the smallest increase in the total sum of squares of the errors $E$, equally the smallest increment $\bigtriangleup E_{pq}$.
The process is repeated until the grouping of all the subjects in a single cluster and the dendrogram is drawn. 
 
