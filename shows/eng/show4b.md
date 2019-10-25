## Interpretation of PCA graphs

The graphs of variables and cases of subjects are interpreted together. The position of the variables in the graph allows to visualize the structure of correlation of the data and suggests possible interpretations of the principal components. The graphs are exploratory and do not inform about the statistical significance. 

% in the axes labels indicate the amount of explained variability for this dimension or components. 

#### Graph of variables

By default, it is worked with the correlation matrix (equivalent to consider typified data). In this case, all the variables are represented by arrows of length between 0 and 1.

**Length and quality of representation**. The length of the arrow indicates the size of its projection in the subspace of the components (preferably from the first two) and, therefore, its quality of representation: *One variable is well represented in the subspace of the two components, if the arrow has a length close to 1, that is, if it is close to the circumference of radius 1*. <font color='blue'> Only the well-represented variables can be interpreted! If appropiate, representations can be made in other dimensions. </font>

**Angles and correlation**. The cosinus of the angle formed by the two vectors is proportional to its correlation, therefore: *The angles that form two arrows (or variables) between each other, show the correlation between the corresponding variables*. *The direction of the arrows indicate the sign of the correlation*. The angle that forms an arrow (variable) and an axis (component) indicates the correlation between the variable and the component. So:

-If two variables are well represented, the smaller (close to zero) the angle between its arrows, the stronger and more positive is its correlation.
-If two variables are well represented, the closer to 180$^o$ ($\pi$) is the angle between its arrows, the stronger and the negative is its correlation.
-If two variables are well represented and the angle between the two arrows is close to the straight line, the correlation is practically zero.

-If there is a group of variables that are well represented and they are close to each other in the graph, then all the variables are very related to each other and are providing the same information, we say that they conform *a latent factor* or they are *colineals*.

-If we see several groups of correlated variables, it means there are *several latent factors* in the data matrix. 

-If one or more variables are well represented and have a small angle with one of the axes of coordinates, then one of the components is strongly correlated with this/these variable(s) and, therefore, the component can be interpreted (labeled) based on the meaning of this/these variable(s).

#### Graph of subjects

The subjects or cases are represented by a dispersion diagram of *scores* in the space of the components. The cases with higher scores in a component can be interpreted based on the most correlated variables with this component. The colors of the scores graph are usually used to visualize a qualitative or covariant variable. 
