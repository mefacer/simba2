## Principal Component Analysis (PCA): graphics

PCA is a method to analyze the structure of a given covariate o correlation matrix, and based on existing correlations, eliminate the possible redundancy to reduce the size of the data set. If the dimension reduction of two or three components is efficients (collects enough information), then PCA also constitutes a powerful visualization tool for multinomial data.

**PCA aim**

Given $p$ variables $X_1,X_2, \ldots,X_p$, assuming to be centered
(if we work with the correlation matrix, it is considered normalized variables with variance equal to 1), we want to build $p$ new variables $Y_1,Y_2, \ldots,Y_p$, that are called components (principal components will be a certain subset of these components) with the next conditions:

 1. The new variables (components) must be a linear combination of the original variables.
 2. The components must be uncorrelated.
 3. The set of all the components must explain the same total variance as the original variables, and the components will be ordered in decreasing order based on the variance.

The theorem proving the existence and unicity of the components, and gives the calculation procedure is:

**Theorem**
*The components are given by the linear transformation
$$Y =V^tX \qquad\qquad  (1)$$
where the matrix $V$ is the orthogonal matrix of column eigenvectors given by the Spectral decomposition (Jordan) of $\Sigma$ - the covarince matrix of $X$ -, with amb eigenvectors ordered in decreasing order based on the pertinent eigenvalue $\lambda_j$*.

As the covariance matrix is symmetric and semi-defined non-negative, the eigenvalues are real non-negative, with decomposition:
$$\Sigma=V\Lambda V^t \qquad\qquad  (2)$$
with $\Lambda$ diagonal matrix of eigenvalues. Equivalently,

$$\Lambda=V^t \Sigma V=\Sigma_Y. \qquad(3)$$
The equality (3) proves that the components defined by (1) are uncorrelated because they have diagonal covariance matrix and the variance is ordered in decreasing order. Particularly,
$$Var (Y_j ) = \lambda_j,\quad\mbox{with}\quad \lambda_1\geq\lambda_2\geq \ldots \geq \lambda_p.$$
From the equations (3) and (2), matching the traces, is easily deducted:
 $$VT(Y):= tr(\Sigma_Y)=\lambda_1+ \cdots + \lambda_p=tr(\Sigma)=\sigma_1^2  +\cdots + \sigma_p^2=: VT(X)\ $$
where $VT$ indicates the total variance. This equality is called *principle of conservation of variance*. In the most habitual case in which the baseline variables are not only centred but standardized, that is $\sigma_i^2=1$, $\,\forall i$, and $\lambda_1+\lambda_2+\cdots + \lambda_p=p.$

It follows from the above that it is logical to choose the first $k$ components to represent all the information, if it is represented an "enough important" percentage ($70\%$,
$80\%$, etc.,) of the total variance:
$$
\frac{\lambda_1+\ldots+\lambda_k}{\lambda_1+\ldots+\lambda_p} \geq
0.7  \mbox{ (or either 0.8, etc.)}
$$
The first $k$ components (where $k$ is determined by the previous criteria or other criteria) are named **principal components**.


**Graphic of the variables**

Assuming standarized variables. In this case, the matrix $W=V\Lambda^{1/2}$ is named weight matrix (*loadings*). The weights are the correlations between variables and components and, at the same time, the coordinates to express the variables as a linear combination (or weighted mean) of the components.  If we assume $k=2$, the original variable $X_i$ is represented with the coordinates $(w_{i1},w_{i2})$ as a shape of "arrow". The first equality of the theorem is equal to
 $$
 \Sigma=W^tW
 $$
 For the diagonal term, derive:
 $$
 \sigma_i^2=1=w_{i1}^2+w_{i2}^2+\cdots+w_{ip}^2
 $$
Thus, the sum of the coefficients $w_{ij}^2$, or squared cosinus, is equal to 1. 
If the variable is well represented in the $k=2$ dimension, then
$$
\mbox{long.vector}^2=w_{i1}^2+w_{i2}^2\approx 1
$$
The arrow of this variable is close to the circumference of radius 1. On the other hand, a short arrow tells that the variable is not well represented on this bidimensional subarea.

**Subject graphics**

The subjects are represented by their *scores* in the new variables. Variables and subjects are interpreted together.