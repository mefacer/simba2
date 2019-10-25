## Mètode de Tukey (*honestly-significant-difference* HSD)

Fixat un nivell de significació $\alpha$, quan el nombre de possibles comparacions és elevat podem tenir un error de tipus I més elevat que $\alpha$, com també hem vist quan parlàvem de contrasts en multiplicitat de gens.

Per identificar quins tractaments són significativament diferents entre ells en mitjana i corregir el problema de la inflació de l'error de tipus I, proposem el mètode de Tukey. Les seves hipòtesis són:
$$H_{0}: \mu_j = \mu_k \text{ per a totes les parelles de mitjanes}$$

$$H_{1}: \mu_j \neq \mu_k \text{ en almenys una parella de mitjanes}$$

L'estadístic de contrast del test de Tukey és:
$$Q = \frac{\bar{X}_j - \bar{X}_k}{\sqrt{\frac{MSE}{r^*}}}$$
on $\hat{X}_j$ i $\hat{X}_k$ són les mitjanes dels tractaments $j$ i $k$, respectivament, $MSE$ és l'estimació de la variància del error o residual del model ANOVA ajustat; i $r^*=r$ és el nombre de rèpliques per tractament, pel cas balancejat. Sota la hipòtesi nul·la i les assumpcions del model, aquest estadístic segueix l'anomenada llei del *rang estudentitzat* amb paràmetres $I$ i $N-I$, on  $I$ és el nombre de nivells que té el factor i $N$ és la grandària mostral. Pel cas no balancejat, el test s'anomena de *Tukey-Kramer*, el qual proposa utilitzar $r^*=r_H$ la mitjana harmònica del nombre de rèpliques dels dos tractaments. Alternativament, en alguns texts es recomana la mitjana harmònica de les rèpliques del conjunt de tractaments per tenir un mateix marge d'error per a totes les diferències, és a dir,
$$r^* = \frac{2}{\frac{1}{r_j}+\frac{1}{r_k}}\quad \text{o bé}\quad r^* =r_H= \frac{I}{\sum_{j=1}^I\frac{1}{r_j}}$$
Fer el test de Tukey-Kramer, fixant un nivell de significació $\alpha$, equival a considerar significatives aquelles diferències anomenades *honestament significatives*, que són les que superen el llindar $HSD$
$$|\bar{X}_j - \bar{X}_k | > HSD = q_{\alpha,I,N-I}\sqrt{\frac{MSE}{r^*}}$$
on $q_{\alpha,I,N-I}$ és la quantila teòrica de la distribució del rang estudentitzat que deixa una cua dreta d'àrea $\alpha$. 

L'interval de confiança per a la diferència de mitjanes és:
$$IC(\mu_j - \mu_k)= (\bar{X}_j - \bar{X}_k) \pm HSD$$
Una manera alternativa de decidir el test és en base a mirar si el zero està contingut o no en l'interval de confiança. Si no està en l'interval de confiança, s'accepta la hipòtesi de mitjanes significativament diferents per aquests grups ("honestament"). 