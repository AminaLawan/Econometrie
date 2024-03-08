* Analyse des les variables DISCRETE
* Tri à plat

*Variable femme
tab femme

*Elever les obervations avec le sexe manquant (femme==99) 

replace femme=. if femme==99

* on peut utiliser 
* drop if femme==99

* POUR AVOIR LE TABLEAU AVEC TOUTES LES VARIABLES

tab femme,m
*------------------------------------------------


* Analyse des les variables continues ou approxivmativement CONTINUE

* Statistiques 

*Variable salaire 
sum salaire
sum salaire, detail 

sum salaire if femme==0
sum salaire if femme==1
  

* Graphiques 

*Graphique (distribution - histogramme) pour salaire
histogram salaire, percent
* CONNAITRE LES SALAIRES SUP A 6000
tab salaire if salaire >=6000
histogram salaire if salaire <6000, percent
histogram salaire if salaire <6000, bin(10) percent


*Graphique par catégorie
*Effectif
graph bar (count) salaire, over(femme)
*Salaire moyen
graph bar (mean) salaire, over(femme)
*
graph bar (mean) salaire, over(femme) over(IdF)

* Distribition par par catégorie
graph box salaire, over(femme)   
*Sans outliers (>1.5IQR)
graph box salaire, over(femme)    nooutsides  



**************************************************
*        2.	Analysez le lien entre les variables *
**************************************************

* Analyse des les VARIABLES DISCRETE

*2.1.  Tris croisés - Tableau  double entrée 

*Effectifs
tab femme IdF
*Frequences (en % du total)
tab femme IdF, cell nofreq
*% en colonne 
tab femme IdF, col nofreq
*% en ligne
tab femme IdF, row nofreq

*2.2 - Test du Chi2 
tab femme IdF,  chi2 
* P = 0,001 et < à 5% et 1% alors on rejette l'independance des 2 variables, il ya bien un lien entre les 2 variables discretes


*2.3 - Graphiques empilées (stacked bars)

graph hbar (count)  ,over(femme) over(IdF) percent  stack asyvars 
gen region = "Ile-de-France" if IdF==1
replace region ="Autre" if IdF==0
gen genre = "Femme" if femme==1
replace genre = "Homme" if femme==0 
graph hbar (count)  ,over(genre) over(region) percent  stack asyvars 

*Bar
graph bar (mean) IdF, over(femme)

*------------------------------------------------

* Analyse des les VARIABLES CONTINUE ou approxivmativement continues 

*2.4 Variances convariances
*Variance - dans les stats descriptives V=(std*std)
sum etud salaire
*Correlation
corr etud salaire
* CORRELATION POSITIVE 
*Avec le niveau de significativité
pwcorr etud salaire, sig
*Covariance 
corr etud salaire,cov


*2.5 Graphique (lien entre deux variables)
*Nuage de points 
graph twoway scatter salaire etud 

*Droite regression 
graph twoway lfit salaire etud 

*nuage de points + droite de reg
graph twoway (scatter salaire etud  ) (lfit salaire etud )
graph twoway (scatter salaire etud  ) (lfit salaire etud ) if salaire<6000

**************************************************
*        3.	Estimation d'une équation de salaire  *
**************************************************

* 3.1. -  REGRESSION SIMPLE 

regress salaire etud 
*Predire le salaire d'un iundividu avec 12 années d'etude (etud=12)
*Calculer
display _b[_cons]+_b[etud]*12
*Créer une varibales yhat
predict yhat,xb

* 3.2. -  REGRESSION MULTIPLE 
regress salaire etud anciennete

*Regression sans outliers
regress salaire etud if salaire <6000



**************************************************
*        4.	Estimation d'une équation de salaire  *
*            (specification)                      * 
**************************************************

*4.1.- 4.2 Regression multiple en log
gen lnsalaire=ln(salaire)
*Effet de passage en ln -> distribution est se rapproche d'une distribution normale
histogram salaire, percent
histogram lnsalaire, percent

*Regression
regress lnsalaire etud anciennete femme IdF 
*L'intérprertion se fait en % pour les effects sur le salaire (les beta). Nous interpretions ici les semi-elasticités (les variables etud, anciennete ne sont pas en ln). 

*4.3. Hétérogénéité de l'effet de l'ancienneté (avec le carré)
gen sqanciennete=anciennete*anciennete
regress lnsalaire etud anciennete sqanciennete  femme IdF 

*Calculer l'effet le l'annciennete pour anciennete=0
di (_b[anciennete])*100
di (_b[anciennete] + 2*_b[sqanciennete]*0)*100

*Calculer l'effet le l'annciennete pour anciennete=10
di (_b[anciennete] + 2*_b[sqanciennete]*10)*100

*Calculer l'effet le l'annciennete pour anciennete=20
di (_b[anciennete] + 2*_b[sqanciennete]*20)*100

*Calculer l'effet le l'annciennete pour anciennete=30
di (_b[anciennete] + 2*_b[sqanciennete]*30)*100

*Calculer l'effet le l'annciennete pour anciennete=40
di (_b[anciennete] + 2*_b[sqanciennete]*40)*100

*Calculer l'effet le l'annciennete pour anciennete moyenne (20.9 = 21 années)
sum anciennete
di (_b[anciennete] + 2*_b[sqanciennete]*21)*100

*Graphique
regress lnsalaire etud c.anciennete##c.anciennete femme IdF 
margins, dydx(anciennete) at(anciennete=(0(1)45))
marginsplot


*TD 1 - PRIX IMMOBILIER


*Ouverture de la base de données
use prixmaisons.dta, clear

*Nature des variables :
* Continues : price age agesq land  
* Discretes non-binaires ordonnées : rooms baths
* Discretes binaires : B2
*----------------------------------------------------------------------
*VARIABLE CONTINUE PRIX
* Statistiques descriptives permettant de caractériser la distribution (la moyenne, la médiane, l'écart-type, le minimum, le maximum) et repérer les valeurs extrêmes
sum price
sum price, detail

*Graphique (répresenter toute la distribution) pour mieux visualiser la distribution des données
*Histogramme
histogram price, percent

*Boite à moustaches 
graph box price
*Sans outliers (>1.5IQR)
graph box price,  nooutsides


* ANALYSE DES FACTEURS EXTREMES
* Combien de maisons avec les prix >200000  ? -> 5 maisons sur 142
sum price if price >200000

*----------------------------------------------------------------------
*LIEN ENTRE VARIABLE DISCRETE (ROOMS et BATHS)
* Tableau - double entrée 
* Nombre (effectifs)
tab rooms baths
*Frequences (en % du total)
tab rooms baths, cell nofreq
* Pourcentage en colonne (fréquences marginales et conditionnelles) :
* Les fréquences marginales se trouvent sur la marge droite (colonne "Total").
* Les fréquences conditionnelles sont à l'intérieur du tableau (colonnes 1 à 4).
tab rooms baths, col nofreq

*2.2 - Test du Chi2  -> lien entre rooms et baths
tab rooms baths,  chi2
*On rejette l'indépendance entre les deux variables (Chi2 = 78.7183 et probabilité critique < 0.05). 
*Il y a bel et bien un lien statistique entre les deux variables (discrètes) : Les maisons possédant un plus grand nombre de pièces (rooms) disposent également de plus de salles de bains(baths) EN MOYENNE.

* Visualisation - prix en focntion de nombre de pieces (rooms) 
* Un lien globalement croissant, mais non linéaire, est observé : les maisons avec 4 salles de bains disposent en moyenne de moins de pièces que celles avec 3 salles de bains."
graph bar rooms, over(baths) 

*----------------------------------------------------------------------

* LIEN ENTRE VARIABLE CONTINUE (PRICE AREA LAND)
* Correlations
* Toutes les variables sont positivement corrélées entre elles : Les maisons possédant une plus grande surface (de maison et de terrain) sont également plus chères. 
pwcorr price area land, sig 
corr price area land, cov

*----------------------------------------------------------------------
*EQUATION DE PRIX - LIEN ENTRE VARIABLE CONTINUE PRICE et AREA 
*Visualisation : nuage de points
graph twoway scatter price area

*Droite regression (lien positif)
graph twoway lfit price area 

*nuage de points + droite de reg
graph twoway (scatter price area   ) (lfit price area  ) 

*REGRESSION 1
regress price area
* Interprétation : Chaque mètre carré de surface supplémentaire augmente le prix d'une maison de 425 USD en moyenne. Cet effet est statistiquement significatif aux seuils de 1 % et 5 % (p-value < 0,01 et < 0,05). 
sum price area
* Pour analyser l'ampleur de cet effet, il faut "contextualiser" l'effet de "425 USD". Par exemple, une maison "moyenne" coûte 120 647 USD et a une surface de 208 m². Augmenter sa surface de 10 % (20 m²) augmenterait le prix de 8 500 USD (20 * 425 USD), soit près de 7 % (8 500 / 120 000). On peut dire que l'effet de la surface est économiquement important.

* R2 : La surface de la maison explique 44 % de son prix (R² = 0,4387), le reste étant donc expliqué par d'autres facteurs non inclus dans la régression.

*Calculer predictions
*le prix prédit d'une maison de 100 m2  = 74650 USD 
display _b[_cons]+_b[area]*100
*le prix prédit d'une maison de 200 m2  = 117137 USD
display _b[_cons]+_b[area]*200
*le prix prédit d'une maison de 300 m2  = 159624 USD
display _b[_cons]+_b[area]*300

*REGRESSION 2 
regress price area rooms
pwcorr area rooms, sig 
*INTERPRETATION : avec l'inclusion de la variable ROOMS, le coefficient de la variable AREA diminue et passe de 425 USD à 318 USD.
*Cette différence s'explique par le fait que dans la Régression 1, nous ne tenons pas compte du nombre de pièces, alors que c'est un élément important dans les décisions d'achat (qui influence le  prix de vente). Les variables AREA et ROOMS sont positivement corrélées entre elles, et l'effet de ROOMS sur le PRICE est également positif. En utilisant la formule de la page 26, le biais d'omission est donc positif, et le coefficient de la variable AREA était donc surestimé dans la Régression 1. La Régression 2 compare les maisons "toutes choses égales par ailleurs", c'est-à-dire avec à nombre de pièces donné, et fournit un résultat moins biaisé.


*Bonus : Nous pouvons également reprendre les régressions sans les 5 maisons ayant des prix très élevés (> 200 000 USD) afin d'observer si leur présence affecte les résultats (et si oui, présenter les régressions avec et sans ces observations extrêmes)


*TD 12- Prix immobilier  - specification


*Ouverture de la base de données
use prixmaisons.dta, clear

* 1. Modèle 1 (en ln)

regress lnprice area rooms land age

*2.L'effet sur le prix d'une augmentation de surface (AREA) d'un m2 ? 10 m2 ? De 100 m2 ?  
* Effet d'une augmentation de 1m2 = beta=> 0.227%
di (_b[area]*100)
* Effet d'une augmentation de 10m2 = beta*10=2.27%
di (_b[area]*10)*100
* Effet d'une augmentation de 100m2 = beta*100=22.7%
di (_b[area]*100)*100


*3. Transformez l'échelle de la variable AREA : la surface d'une maison est maintenant exprimée en dizaines de m2 (nouvelle variable AREA10M2). 
gen area10m2=area/10
regress lnprice  area10m2 rooms land age
*b=2.27% - même que beta du modèle1* 10 
*Quand area10m2 augmente d'une unité (i.e surface augmente d'1m2), le prix augmente de 2.27 %

*4. Quel est l'effet sur le prix d'une augmentation de l'âge d'une maison (variable AGE) ? Cet effet est-il similaire pour une maison neuve et une maison âgée de 20 ans ?
* Le prix diminue de 0.64% avec chaque année d'age. cet effet est constant. 
di (_b[age]*100)

*5. Modèle avec l'age au carré (Modèle 2).

regress lnprice area rooms land age agesq
*Quel est l'effet d'une année supplémentaire pour une maison neuve ? -> -1.4%
di (_b[age])*100
di (_b[age] +2*_b[agesq]*0)*100
* Maison âgée de 20 ans ?  -1.09%
di (_b[age] +2*_b[agesq]*20)*100
* Maison âgée de 50 ans ?  -0.6%
di (_b[age] +2*_b[agesq]*50)*100
* Maison âgée de 100 ans ?  -.15%
di (_b[age] +2*_b[agesq]*100)*100
*Qu'en concluez -vous concernant l'hétérogénéité de l'effet de l'AGE?  L'effet est negatif, mais croissant (car le coef estimé de la variable age est negatf et celui de la variable agesq est positif).
*bonus
*Graphique
regress lnprice area rooms land c.age##c.age
margins, dydx(age) at(age=(0(10)150))
marginsplot

*6.	Nous reprenons le modèle  de base (modèle 1) en ajoutant une variables binaire suivante : B2. 
regress lnprice area rooms land age B2
*L'écart de prix entre les maisons  avec moins de 2 salles de bains (B2=0) et les autres (B2=1, une salle de bains seulement) est de 23.3% 


*7 Nous reprenons le modèle  de base (modèle 1) en ajoutant une variables binaire suivante : BATHS 
regress lnprice area rooms land age baths 
* Chaque salle de bain supplémentaire augmente le prix de vente de 19,8% en moyenne.

*8.Nous allons désormais considérer la variable BATHS comme étant discrète.

regress lnprice area rooms land age i.baths 
