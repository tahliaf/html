Pretreatment TAG Assay
================
Tahlia Fulton
04/09/2020

## Experiment Methods:

Female flies were pretreated with one of 8 diets for 7 days prior to
being frozen in liquid nitrogen for TAG analysis. The diets included:

  - 100N
  - 0N
  - 0Ile
  - 25Thr

And the diets either contained 10µM rapamycin or did not

Exactly 5 female flies were ground in diluted tween using a pestle and
the fat and protein content from this mixture was analysed using colour
changing reagents and a spectrophotometer (protocol/reagent list
available from Tahlia) There were 5 biological replicates per diet,
which were analysed as technical triplicates

## Results

TAG standard curve: - Consistent across plates - Decision to take the
coefficients of this line to solve for all TAG samples

Protein standard curve: - Loses linearity at higher concentrations -
Absorbance of samples is \< absorbance of 1mg/mL BSA - Removed 2mg/mL
from standard curve - Linearity improved, but differences between plates
are noticeable (did not test statistically) - Decision to use the
standard curve from the same plate as the samples - Made a sweet for
loop to do this automatically

``` r
## y = mx + c 
## Conc estimate = m = tidy[2,2]
## intercept estimate = c = tidy[1,2]

TAGCurve <- lm(Abs ~ Conc, StandTAG)
TM <- as.double(tidy(TAGCurve)[2,2])
TC <- as.double(tidy(TAGCurve)[1,2])

ggplot(StandTAG, aes(Conc, Abs)) +
  geom_point() +
  stat_smooth(method = "lm", col = "red") +
  labs(y = "Absorbance at 540nm (TAG)",
       x = "Concentration of glycerol (mg/mL)") 
```

    ## `geom_smooth()` using formula 'y ~ x'

![](Results-TAG_files/figure-gfm/Models%20for%20TAG%20and%20PRO%20standard%20curves-1.png)<!-- -->

``` r
PROCurve <- lm(Abs ~ Conc, StandPRO)
tidy(PROCurve)
```

    ## # A tibble: 2 x 5
    ##   term        estimate std.error statistic  p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 (Intercept)   0.0769   0.00772      9.97 3.75e-12
    ## 2 Conc          0.171    0.00945     18.1  2.99e-20

``` r
ggplot(StandPRO, aes(Conc, Abs, color = Run)) +
  geom_point() +
  geom_smooth(se = F) +
  labs(y = "Absorbance at 562nm (Protein)",
       x = "Concentration of BSA (mg/mL)") 
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](Results-TAG_files/figure-gfm/Models%20for%20TAG%20and%20PRO%20standard%20curves-2.png)<!-- -->

``` r
subStandPRO <- StandPRO %>% subset(Conc != "2")

subPROCurve <- lm(Abs ~ Conc, subStandPRO)

ggplot(subStandPRO, aes(Conc, Abs, color = Run)) +
  geom_point() +
  geom_smooth(se = F) +
  labs(y = "Absorbance at 562nm (Protein)",
       x = "Concentration of BSA (mg/mL)/nReomved 2mg/mL to improve linearity") 
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](Results-TAG_files/figure-gfm/Models%20for%20TAG%20and%20PRO%20standard%20curves-3.png)<!-- -->
\#\# TAG content of flies

(stats are below plots)

Before looking at stats (sorry Christen haha) it looks like rapa has no
effect on fat levels, but diet does, where 0Ile and 0N flies have less
fat than 25Thr and 100N flies

(kind of the opposite of what we expected\! where is all of that energy
going? the flies are still eating but are not laying eggs…. why aren’t
they fat?)

![](Results-TAG_files/figure-gfm/TAG%20plot-1.png)<!-- --> \#\# Protein
content of flies

Again, just looking at the data, looks like 0Ile and 0N flies have less
protein than 25Thr and 100N flies, but including rapamycin in the diet
increases their total protein amount. So rapa fed flies also don’t lay
eggs, so they would be eating excess protein, except you know.. for the
flies that aren’t even eating any protein….. I’m not sure what I think
about this result yet

![](Results-TAG_files/figure-gfm/Protein%20Plot-1.png)<!-- --> \#\# TAG
normalised to protein

I actually looked at the stats for this plot before I looked at the plot
itself and was a little disappointed The stats imply that none of these
groups are statistaclly different from each other (aov and a tukey test
for post-hoc)

Tell me that 0Ile + Rapa looks different from 0N + no Rapa\!

Overall, I don’t think that fat levels are associating with the
phenotype that I am seeing (I would have expected to see 0Ile and 25Thr
somehow pattern together in the results, which is not the case)

![](Results-TAG_files/figure-gfm/Normalised%20Plot-1.png)<!-- --> Here
are the stats:

Please feel free to suggest another way to analyse the data if this
isn’t the best way. I really want to stress how incompetent I am at
statistics specifically (absolutely no formal training in it, holding
out for an online stats workshop) - it’s just not something that I’ve
had “click” for me yet\! One day… (maybe)

``` r
lmTAG <- lm(Conc ~ Diet*Rapa, data = TAGData1)
lmTAG.a <- aov(lmTAG)
TukeyHSD(lmTAG.a)
```

    ##   Tukey multiple comparisons of means
    ##     95% family-wise confidence level
    ## 
    ## Fit: aov(formula = lmTAG)
    ## 
    ## $Diet
    ##                     diff           lwr         upr     p adj
    ## 0N-0Ile     0.0003529312 -2.205932e-03 0.002911794 0.9839842
    ## 100N-0Ile   0.0030126660  4.538029e-04 0.005571529 0.0140710
    ## 25Thr-0Ile  0.0028603641  3.015010e-04 0.005419227 0.0219938
    ## 100N-0N     0.0026597348  1.008717e-04 0.005218598 0.0383533
    ## 25Thr-0N    0.0025074329 -5.143019e-05 0.005066296 0.0570206
    ## 25Thr-100N -0.0001523019 -2.711165e-03 0.002406561 0.9986601
    ## 
    ## $Rapa
    ##                     diff          lwr         upr     p adj
    ## Rapa-NoRapa 7.260906e-05 -0.001302023 0.001447241 0.9168348
    ## 
    ## $`Diet:Rapa`
    ##                                   diff           lwr          upr     p adj
    ## 0N:NoRapa-0Ile:NoRapa    -6.805181e-04 -0.0049667158 0.0036056796 0.9996846
    ## 100N:NoRapa-0Ile:NoRapa   2.366622e-03 -0.0019195752 0.0066528201 0.6836298
    ## 25Thr:NoRapa-0Ile:NoRapa  1.989921e-03 -0.0022962766 0.0062761188 0.8394408
    ## 0Ile:Rapa-0Ile:NoRapa    -1.202359e-03 -0.0054885565 0.0030838389 0.9883983
    ## 0N:Rapa-0Ile:NoRapa       1.840217e-04 -0.0041021760 0.0044702194 1.0000000
    ## 100N:Rapa-0Ile:NoRapa     2.456351e-03 -0.0018298470 0.0067425484 0.6412493
    ## 25Thr:Rapa-0Ile:NoRapa    2.528448e-03 -0.0017577495 0.0068146459 0.6064670
    ## 100N:NoRapa-0N:NoRapa     3.047141e-03 -0.0012390572 0.0073333382 0.3621304
    ## 25Thr:NoRapa-0N:NoRapa    2.670439e-03 -0.0016157585 0.0069566369 0.5372320
    ## 0Ile:Rapa-0N:NoRapa      -5.218407e-04 -0.0048080384 0.0037643570 0.9999470
    ## 0N:Rapa-0N:NoRapa         8.645397e-04 -0.0034216580 0.0051507374 0.9984931
    ## 100N:Rapa-0N:NoRapa       3.136869e-03 -0.0011493289 0.0074230665 0.3246492
    ## 25Thr:Rapa-0N:NoRapa      3.208966e-03 -0.0010772314 0.0074951640 0.2961293
    ## 25Thr:NoRapa-100N:NoRapa -3.767013e-04 -0.0046628990 0.0039094964 0.9999943
    ## 0Ile:Rapa-100N:NoRapa    -3.568981e-03 -0.0078551790 0.0007172164 0.1773307
    ## 0N:Rapa-100N:NoRapa      -2.182601e-03 -0.0064687985 0.0021035969 0.7652562
    ## 100N:Rapa-100N:NoRapa     8.972827e-05 -0.0041964694 0.0043759260 1.0000000
    ## 25Thr:Rapa-100N:NoRapa    1.618257e-04 -0.0041243720 0.0044480234 1.0000000
    ## 0Ile:Rapa-25Thr:NoRapa   -3.192280e-03 -0.0074784776 0.0010939178 0.3025975
    ## 0N:Rapa-25Thr:NoRapa     -1.805899e-03 -0.0060920972 0.0024802982 0.8965248
    ## 100N:Rapa-25Thr:NoRapa    4.664296e-04 -0.0038197681 0.0047526273 0.9999753
    ## 25Thr:Rapa-25Thr:NoRapa   5.385270e-04 -0.0037476707 0.0048247247 0.9999344
    ## 0N:Rapa-0Ile:Rapa         1.386380e-03 -0.0028998172 0.0056725782 0.9736902
    ## 100N:Rapa-0Ile:Rapa       3.658710e-03 -0.0006274882 0.0079449072 0.1539777
    ## 25Thr:Rapa-0Ile:Rapa      3.730807e-03 -0.0005553907 0.0080170047 0.1369507
    ## 100N:Rapa-0N:Rapa         2.272329e-03 -0.0020138686 0.0065585268 0.7265279
    ## 25Thr:Rapa-0N:Rapa        2.344427e-03 -0.0019417712 0.0066306242 0.6939007
    ## 25Thr:Rapa-100N:Rapa      7.209745e-05 -0.0042141002 0.0043582951 1.0000000

``` r
lmPRO <- lm(Conc ~ Diet*Rapa, data = PROData1)
lmPRO.a <- aov(lmPRO)
TukeyHSD(lmPRO.a)
```

    ##   Tukey multiple comparisons of means
    ##     95% family-wise confidence level
    ## 
    ## Fit: aov(formula = lmPRO)
    ## 
    ## $Diet
    ##                     diff           lwr          upr     p adj
    ## 0N-0Ile    -0.0033798230 -7.592959e-03 0.0008333134 0.1619682
    ## 100N-0Ile   0.0037752358 -4.379006e-04 0.0079883722 0.0958571
    ## 25Thr-0Ile  0.0042680752  5.493882e-05 0.0084812116 0.0458508
    ## 100N-0N     0.0071550588  2.941922e-03 0.0113681952 0.0001288
    ## 25Thr-0N    0.0076478982  3.434762e-03 0.0118610346 0.0000381
    ## 25Thr-100N  0.0004928394 -3.720297e-03 0.0047059758 0.9900932
    ## 
    ## $Rapa
    ##                    diff          lwr         upr    p adj
    ## Rapa-NoRapa 0.003153311 0.0008899967 0.005416626 0.006745
    ## 
    ## $`Diet:Rapa`
    ##                                   diff           lwr         upr     p adj
    ## 0N:NoRapa-0Ile:NoRapa    -0.0027822228 -9.839394e-03 0.004274949 0.9250366
    ## 100N:NoRapa-0Ile:NoRapa   0.0048215873 -2.235584e-03 0.011878759 0.4147291
    ## 25Thr:NoRapa-0Ile:NoRapa  0.0049756269 -2.081545e-03 0.012032798 0.3730151
    ## 0Ile:Rapa-0Ile:NoRapa     0.0043290630 -2.728109e-03 0.011386235 0.5573161
    ## 0N:Rapa-0Ile:NoRapa       0.0003516399 -6.705532e-03 0.007408811 0.9999999
    ## 100N:Rapa-0Ile:NoRapa     0.0070579474  7.758280e-07 0.014115119 0.0499535
    ## 25Thr:Rapa-0Ile:NoRapa    0.0078895866  8.324151e-04 0.014946758 0.0172567
    ## 100N:NoRapa-0N:NoRapa     0.0076038101  5.466386e-04 0.014660982 0.0252288
    ## 25Thr:NoRapa-0N:NoRapa    0.0077578497  7.006781e-04 0.014815021 0.0205960
    ## 0Ile:Rapa-0N:NoRapa       0.0071112858  5.411430e-05 0.014168457 0.0468474
    ## 0N:Rapa-0N:NoRapa         0.0031338627 -3.923309e-03 0.010191034 0.8680822
    ## 100N:Rapa-0N:NoRapa       0.0098401702  2.782999e-03 0.016897342 0.0009080
    ## 25Thr:Rapa-0N:NoRapa      0.0106718094  3.614638e-03 0.017728981 0.0002205
    ## 25Thr:NoRapa-100N:NoRapa  0.0001540396 -6.903132e-03 0.007211211 1.0000000
    ## 0Ile:Rapa-100N:NoRapa    -0.0004925243 -7.549696e-03 0.006564647 0.9999988
    ## 0N:Rapa-100N:NoRapa      -0.0044699474 -1.152712e-02 0.002587224 0.5156470
    ## 100N:Rapa-100N:NoRapa     0.0022363601 -4.820811e-03 0.009293532 0.9765446
    ## 25Thr:Rapa-100N:NoRapa    0.0030679993 -3.989172e-03 0.010125171 0.8802518
    ## 0Ile:Rapa-25Thr:NoRapa   -0.0006465638 -7.703735e-03 0.006410608 0.9999924
    ## 0N:Rapa-25Thr:NoRapa     -0.0046239870 -1.168116e-02 0.002433185 0.4706837
    ## 100N:Rapa-25Thr:NoRapa    0.0020823205 -4.974851e-03 0.009139492 0.9844024
    ## 25Thr:Rapa-25Thr:NoRapa   0.0029139598 -4.143212e-03 0.009971131 0.9060139
    ## 0N:Rapa-0Ile:Rapa        -0.0039774231 -1.103459e-02 0.003079748 0.6606017
    ## 100N:Rapa-0Ile:Rapa       0.0027288844 -4.328287e-03 0.009786056 0.9319601
    ## 25Thr:Rapa-0Ile:Rapa      0.0035605236 -3.496648e-03 0.010617695 0.7736045
    ## 100N:Rapa-0N:Rapa         0.0067063075 -3.508641e-04 0.013763479 0.0751751
    ## 25Thr:Rapa-0N:Rapa        0.0075379467  4.807752e-04 0.014595118 0.0274787
    ## 25Thr:Rapa-100N:Rapa      0.0008316392 -6.225532e-03 0.007888811 0.9999575

``` r
lmNorm <- lm(NormConc ~ Diet*Rapa, data = NormData)
lmNorm.a <- aov(lmNorm)
TukeyHSD(lmNorm.a)
```

    ##   Tukey multiple comparisons of means
    ##     95% family-wise confidence level
    ## 
    ## Fit: aov(formula = lmNorm)
    ## 
    ## $Diet
    ##                    diff         lwr        upr     p adj
    ## 0N-0Ile     0.115184857 -0.06259709 0.29296680 0.3339124
    ## 100N-0Ile   0.035150096 -0.14263185 0.21293204 0.9551957
    ## 25Thr-0Ile  0.026363458 -0.15141849 0.20414540 0.9802470
    ## 100N-0N    -0.080034761 -0.25781670 0.09774718 0.6443997
    ## 25Thr-0N   -0.088821399 -0.26660334 0.08896054 0.5629728
    ## 25Thr-100N -0.008786638 -0.18656858 0.16899531 0.9992306
    ## 
    ## $Rapa
    ##                    diff        lwr        upr     p adj
    ## Rapa-NoRapa -0.07186756 -0.1673728 0.02363765 0.1387779
    ## 
    ## $`Diet:Rapa`
    ##                                  diff        lwr        upr     p adj
    ## 0N:NoRapa-0Ile:NoRapa     0.063411872 -0.2343800 0.36120372 0.9978696
    ## 100N:NoRapa-0Ile:NoRapa  -0.018390679 -0.3161825 0.27940117 0.9999995
    ## 25Thr:NoRapa-0Ile:NoRapa -0.024668921 -0.3224608 0.27312293 0.9999962
    ## 0Ile:Rapa-0Ile:NoRapa    -0.150040629 -0.4478325 0.14775122 0.7748068
    ## 0N:Rapa-0Ile:NoRapa       0.016917213 -0.2808746 0.31470906 0.9999997
    ## 100N:Rapa-0Ile:NoRapa    -0.061349758 -0.3591416 0.23644209 0.9982744
    ## 25Thr:Rapa-0Ile:NoRapa   -0.072644792 -0.3704366 0.22514705 0.9950101
    ## 100N:NoRapa-0N:NoRapa    -0.081802551 -0.3795944 0.21598930 0.9897548
    ## 25Thr:NoRapa-0N:NoRapa   -0.088080793 -0.3858726 0.20971105 0.9841820
    ## 0Ile:Rapa-0N:NoRapa      -0.213452501 -0.5112443 0.08433935 0.3514183
    ## 0N:Rapa-0N:NoRapa        -0.046494659 -0.3442865 0.25129719 0.9997178
    ## 100N:Rapa-0N:NoRapa      -0.124761630 -0.4225535 0.17303022 0.8992562
    ## 25Thr:Rapa-0N:NoRapa     -0.136056664 -0.4338485 0.16173518 0.8502637
    ## 25Thr:NoRapa-100N:NoRapa -0.006278242 -0.3040701 0.29151360 1.0000000
    ## 0Ile:Rapa-100N:NoRapa    -0.131649950 -0.4294418 0.16614190 0.8707221
    ## 0N:Rapa-100N:NoRapa       0.035307891 -0.2624840 0.33309974 0.9999557
    ## 100N:Rapa-100N:NoRapa    -0.042959080 -0.3407509 0.25483277 0.9998335
    ## 25Thr:Rapa-100N:NoRapa   -0.054254113 -0.3520460 0.24353773 0.9992200
    ## 0Ile:Rapa-25Thr:NoRapa   -0.125371708 -0.4231636 0.17242014 0.8969010
    ## 0N:Rapa-25Thr:NoRapa      0.041586133 -0.2562057 0.33937798 0.9998661
    ## 100N:Rapa-25Thr:NoRapa   -0.036680838 -0.3344727 0.26111101 0.9999426
    ## 25Thr:Rapa-25Thr:NoRapa  -0.047975871 -0.3457677 0.24981598 0.9996525
    ## 0N:Rapa-0Ile:Rapa         0.166957842 -0.1308340 0.46474969 0.6665675
    ## 100N:Rapa-0Ile:Rapa       0.088690871 -0.2091010 0.38648272 0.9835374
    ## 25Thr:Rapa-0Ile:Rapa      0.077395837 -0.2203960 0.37518768 0.9926522
    ## 100N:Rapa-0N:Rapa        -0.078266971 -0.3760588 0.21952488 0.9921380
    ## 25Thr:Rapa-0N:Rapa       -0.089562005 -0.3873539 0.20822984 0.9825827
    ## 25Thr:Rapa-100N:Rapa     -0.011295034 -0.3090869 0.28649681 1.0000000
