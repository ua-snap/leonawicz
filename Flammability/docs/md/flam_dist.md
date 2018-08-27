






##
##
## Exploratory plots of marginal and conditional flammability distributions.

Each plot is broken out by vegetation class, alpine tundra, boreal forest, combined CAVM tundra, and each of three CAVM component classes, shrub, graminoid, and wetland tundra.

### Marginal distribution

The first plot shows a density function proportional to joint probability distribution for flammability in time and space,
marginalized with repsect to time. The time period is 1950 - 2013. The second plot is the same, but with flammability truncated to [0.12, 0.27].

![](flam_dist_files/figure-html/plot01a-1.png) 

![](flam_dist_files/figure-html/plot01b-1.png) 

### Conditional distribution

This plot shows multiple overlapping spatial flammability distributions conditioned on each year, also truncated to [0.12, 0.27].

![](flam_dist_files/figure-html/plot02-1.png) 

The table below provides distribution quantiles by vegetation class.


Table: Critical values associated with flammability by vegetation class

Vegetation     Pct05    Pct10    Pct25    Pct50    Pct75    Pct90    Pct95
-----------  -------  -------  -------  -------  -------  -------  -------
CAVM          0.1363   0.1372   0.1386   0.1405   0.1437   0.1487   0.1547
shrub         0.1362   0.1364   0.1368   0.1374   0.1383   0.1397   0.1412
graminoid     0.1356   0.1363   0.1372   0.1382   0.1394   0.1427   0.1491
wetland       0.1363   0.1364   0.1367   0.1372   0.1382   0.1399   0.1421
alptun        0.1370   0.1373   0.1378   0.1385   0.1396   0.1410   0.1422
forest        0.1509   0.1566   0.1703   0.1913   0.2199   0.2694   0.3243
