## Interpret the *heatmap*

The *heatmap* is the joint representation of two *dendograms*: a vertical dendogram showing clusters of rows (genes or variables) and a horizontal dendogram showing cluster of columns (samples or cases). A dendrogram is a tree that represents a hierarchy of partitions (from the maximum where each element is a cluster to the minimum where all the elements are grouped into a single cluster).

<font color='blue'> To decide which partition to choose, a possible criterion is to cut the tree where the *branches are longer* </font>, because it corresponds to maximizing the distances between the resulting clusters.

- The heatmap boxes have a color indicative of a level expression, according to the legend. Dark grey boxes are expression values not obtained (NA).
- There are cluster of genes in the vertical dendogram. After choosing the partition, the genes grouped in the same cluster tend to have similar expression patterns (more correlated, due to the use of the Pearson distance) in the set of samples. 
- The cathegorical variable *gene function* is displayed according to the colors that we see below the vertical dendogram, and it is useful to explore if the genes grouped in the same cluster (correlated expressions) tend to have the same functionality or, by contrast, the grouping by clusters does not correspond to the grouping by functionalities.
- In the horizontal dendogram, there are clusters of the samples. After choosing the partition, the samples grouped in the same cluster tend to have similar expression patterns (that is, less distance according to the Euclidean distance) in the set of genes.
- The variable *treatment* is displayed according to the colors that we see below the horizontal dendogram, and it is useful to explore if the samples of the same cluster (similar expressions) tend to correspond to a same treatment or, by contrast, the cluster grouping is not well associated to any treatment cluster.
