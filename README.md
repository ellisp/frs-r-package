# frs-r-package
R package for miscellaneous functions associated with the [Free Range Statistics](http://freerangestats.info) blog


```r
devtools::install_github("ellisp/frs-r-package/pkg")
```

So far, the bits and pieces include:

- choropleth maps of the Pacific
- dual axis plots
- so-called "modulus" transformation for ggplot2 scales, which is like a Box Cox of the absolute value then with the sign returned.  Excellent for visualising economic variables that can be zero or negative
- French death rates data in 2015
- Colour palette and ggplot2 theme for International Finance Corporation branding (disclaimer - not associated at all with the IFC, but I did use it successfully in a report for them once)
- turn levels of a factor into letters, useful for quick and dirty confidentialisation
- utility to download files only if fresh
- some ODBC database odds and ends to help with R interacting with SQL Server


## Choroplethmaps of the Pacific


```r
pac_map_sf
```

```
## Simple feature collection with 24 features and 6 fields
## Geometry type: POLYGON
## Dimension:     XYZ
## Bounding box:  xmin: 129.5088 ymin: -31.24447 xmax: 238.891 ymax: 23.89565
## z_range:       zmin: 0 zmax: 0
## Geodetic CRS:  WGS 84
## First 10 features:
##    id                           name2        X          Y geo_pict iso3                       geometry
## 1   1                  American Samoa 190.9359 -13.855010       AS  ASM POLYGON Z ((186.2253 -11.04...
## 2   2                    Cook Islands 198.6964 -15.020082       CK  COK POLYGON Z ((191.4764 -10.02...
## 3   3                            Fiji 177.6701 -17.979529       FJ  FJI POLYGON Z ((180.2498 -14.19...
## 4   4                French Polynesia 215.9955 -18.732371       PF  PYF POLYGON Z ((204.9461 -12.48...
## 5   5 Micronesia, Federated States of 150.3232   6.768882       FM  FSM POLYGON Z ((135.3156 11.486...
## 6   6                            Guam 144.0046  12.930932       GU  GUM POLYGON Z ((142.0952 15.726...
## 7   7                        Kiribati 173.8879  -0.258948       KI  KIR POLYGON Z ((180.0038 -1.594...
## 8   8                        Kiribati 205.2727  -3.819746       KI  KIR POLYGON Z ((196.9363 2.6611...
## 9   9                        Kiribati 187.5461  -3.731807       KI  KIR POLYGON Z ((182.547 -3.0148...
## 10 10                Marshall Islands 167.4853  10.146401       MH  MHL POLYGON Z ((157.4639 10.423...
```

```r
draw_pac_map()
```

![plot of chunk unnamed-chunk-20](figure/unnamed-chunk-20-1.png)


```r
country_data <- tribble(~name2, ~var,
                        "Cook Islands", 5,
                        "Fiji", 10,
                        "Guam", 4,
                        "Palau", 5,
                        "Niue", 20,
                        "Tonga", 3,
                        "Tuvalu", 17,
                        "Papua New Guinea", 12)

draw_pac_map(fill_df = country_data, join_col = "name2", fill_col = "var") +
  scale_fill_viridis_c() +
  labs(title = "Some random variables",
       fill = "")
```

![plot of chunk unnamed-chunk-21](figure/unnamed-chunk-21-1.png)



## Modulus transformation
Like a Box-Cox transformation, but it works with negative numbers too:

```r
library(ggplot2)
library(frs)
set.seed(123)
eg_data <- data.frame(x = exp(rnorm(1000)) * 
               sample(c(-1, 1), 1000, replace = TRUE, prob = c(0.2, 0.8)))

p1 <- ggplot(eg_data, aes(x = x)) +
  geom_density() 

p2 <- p1 +
  scale_x_continuous("Transformed scale",
                     trans = modulus_trans(0.1),
                     breaks = modulus_breaks(lambda = 0.1))
gridExtra::grid.arrange(p1 + labs(x= "Original scale"), p2, ncol = 2)
```

![plot of chunk unnamed-chunk-22](figure/unnamed-chunk-22-1.png)
                     
See my blog posts on this:

* [Creating a scale transformation](http://ellisp.github.io/blog/2015/09/05/creating-a-scale-transformation)
* [Transforming the breaks to match a scale](http://ellisp.github.io/blog/2015/09/07/transforming-breaks-in-a-scale)
                     
## Dual axis line charts

Yes, they're not as bad as you've been told!


```r
data(dairy)
data(fonterra)
# with defaults for positioning scales, but exemplifying custom colours, etc:
 dualplot(x1 = dairy$Date, y1 = dairy$WMP_Total, 
          x2 = fonterra$Date, y2 = fonterra$FCGClose,
          ylab1 = "Whole milk powder price index\n",
          ylab2 = "Fonterra Cooperative Group\nshare price ($)",
          col = c("#AA6800", "#7869CD"),
          colgrid = "grey90",
          main = "Fonterra share prices are less volatile than global milk prices")
```

```
## The two series will be presented visually as though they had been converted to indexes.
```

![plot of chunk unnamed-chunk-23](figure/unnamed-chunk-23-1.png)

See my blog posts on this:

* [Dual axes time series plots may be ok sometimes after all](http://ellisp.github.io/blog/2016/08/18/dualaxes)
* [Dual axes time series plots with various more awkward data](http://ellisp.github.io/blog/2016/08/28/dualaxes2)
 
