## Ratio falsos positius (*false discovery rate (FDR)*)

Existeixen molts mètodes per a corregir el problema de la multiplicitat de contrastos entre parelles de tractaments. El més simple és el mètode de Bonferroni, on cada $p$-valor es multiplica pel nombre de tests realitzats (acotant la probabilitat màxima a 1). És un mètode molt conservador i no és el més indicat per a l'anàlisi de dades d'expressió gènica.

Per a escenaris d'un gran nombre de tests simultanis (*large-scale multiple testing*), com en dades d'expressió, el resultat dels mètodes clàssics de correcció (Bonferroni, Tukey, etc.) és massa conservador i impedeix que es detectin diferències reals. Una alternativa és controlar el FDR de Benjamini $\&$ Hochberg (FDR o BH-FDR).

El FDR es defineix com la proporció esperada de falsos positius d'entre tots els tests considerats com significatius. L'objectiu de controlar el FDR és establir un límit de significació per a un conjunt de tests de manera que, d'entre tots els tests considerats com significatius, la proporció de falsos positius no superi un determinat llindar.

Un altre avantatge del FDR és la seva fàcil interpretació, per exemple, si un estudi publica resultats estadísticament significatius per a un FDR del 10%, el lector entendrà que, com a màxim, un 10% dels resultats considerats com a significatius poden ser realment falsos positius.

La primera aproximació per controlar el FDR va ser descrita per Benjamini $\&$ Hochberg en 1995. Si es desitja controlar que en un estudi amb $n$ comparacions el FDR no superi un percentatge $d$ hem de:

- Ordenar els $n$ tests de menor a major $p$-valor ($p_{1}$,$p_{2}$,..., $p_n$).
- Es defineix $k$ com l'última posició per la qual es compleix que $p_i \leq d\frac{i}{n}$.
- Es consideren significatius tots els $p$-valors fins a la posició $k$ ($p_{1}$,$p_{2}$,..., $p_k$).
