# crosswisePermTest: different randomization functions

    Code
      crosswisePermTest(Alist = AlienRSList_narrow_small, Blist = AlienRSList_narrow_small,
        sampling = FALSE, genome = AlienGenome, per.chromosome = TRUE, ranFUN = "resampleGenome",
        evFUN = "numOverlaps", ntimes = 5, mc.cores = 2)
    Output
      [1] "Note: The minimum p-value with only 5 permutations is 0.166666666666667. You should consider increasing the number of permutations."
      [1] "Note: The minimum p-value with only 5 permutations is 0.166666666666667. You should consider increasing the number of permutations."
      [1] "Note: The minimum p-value with only 5 permutations is 0.166666666666667. You should consider increasing the number of permutations."
      An object of class "genoMatriXeR"
      Slot "parameters":
      $Alist
      [1] "AlienRSList_narrow_small"
      
      $Blist
      [1] "AlienRSList_narrow_small"
      
      $sampling
      [1] "FALSE"
      
      $fraction
      [1] "0.15"
      
      $min_sampling
      [1] "0.15"
      
      $ranFUN
      [1] "resampleGenome"
      
      $evFUN
      [1] "numOverlaps"
      
      $ntimes
      [1] 5
      
      $universe
      NULL
      
      $adj_pv_method
      [1] "BH"
      
      $nc
      NULL
      
      $matOrder
      NULL
      
      
      Slot "multiOverlaps":
      $regA
        order.id  name n_regionA n_regionB  z_score   p_value n_overlaps
      1        3 regAB       100       100 113.5923 0.1666667         51
      2        2  regB       100       100  -0.7303 0.6666667          1
      3        1  regA       100       100 118.5667 0.1666667        100
        mean_perm_test sd_perm_test norm_zscore adj.p_value
      1            0.2    0.4472136    11.35923      0.2500
      2            1.4    0.5477226    -0.07303      0.6667
      3            0.8    0.8366600    11.85667      0.2500
      
      $regB
        order.id  name n_regionA n_regionB  z_score   p_value n_overlaps
      1        3 regAB       100       100  35.3553 0.1666667         51
      2        2  regB       100       100 118.5667 0.1666667        100
      3        1  regA       100       100   0.2390 0.6666667          1
        mean_perm_test sd_perm_test norm_zscore adj.p_value
      1            1.0     1.414214     3.53553      0.2500
      2            0.8     0.836660    11.85667      0.2500
      3            0.8     0.836660     0.02390      0.6667
      
      $regAB
        order.id  name n_regionA n_regionB  z_score   p_value n_overlaps
      1        3 regAB       100       100 113.5923 0.1666667        102
      2        2  regB       100       100 113.5923 0.1666667         51
      3        1  regA       100       100  38.1949 0.1666667         51
        mean_perm_test sd_perm_test norm_zscore adj.p_value
      1            0.4    0.8944272    11.35923      0.1667
      2            0.2    0.4472136    11.35923      0.1667
      3            1.2    1.3038405     3.81949      0.1667
      
      
      Slot "matrix":
      [[1]]
      NULL
      
      

---

    Code
      crosswisePermTest(Alist = AlienRSList_narrow_small, Blist = AlienRSList_narrow_small,
        sampling = FALSE, genome = AlienGenome, ranFUN = "resampleRegions", evFUN = "numOverlaps",
        ntimes = 5, mc.cores = 2)
    Warning <simpleWarning>
      resampleRegions function need that 'universe' is not NULL, universe was created using all the regions present in Alist
    Output
      [1] "Note: The minimum p-value with only 5 permutations is 0.166666666666667. You should consider increasing the number of permutations."
      [1] "Note: The minimum p-value with only 5 permutations is 0.166666666666667. You should consider increasing the number of permutations."
      [1] "Note: The minimum p-value with only 5 permutations is 0.166666666666667. You should consider increasing the number of permutations."
      An object of class "genoMatriXeR"
      Slot "parameters":
      $Alist
      [1] "AlienRSList_narrow_small"
      
      $Blist
      [1] "AlienRSList_narrow_small"
      
      $sampling
      [1] "FALSE"
      
      $fraction
      [1] "0.15"
      
      $min_sampling
      [1] "0.15"
      
      $ranFUN
      [1] "resampleRegions"
      
      $evFUN
      [1] "numOverlaps"
      
      $ntimes
      [1] 5
      
      $universe
      NULL
      
      $adj_pv_method
      [1] "BH"
      
      $nc
      NULL
      
      $matOrder
      NULL
      
      
      Slot "multiOverlaps":
      $regA
        order.id  name n_regionA n_regionB  z_score   p_value n_overlaps
      1        3 regAB       100       100  -0.1739 0.5000000         51
      2        2  regB       100       100 -19.3047 0.1666667          1
      3        1  regA       100       100  20.9640 0.1666667        100
        mean_perm_test sd_perm_test norm_zscore adj.p_value
      1           52.4     8.049845    -0.01739        0.50
      2           52.8     2.683282    -1.93047        0.25
      3           47.8     2.489980     2.09640        0.25
      
      $regB
        order.id  name n_regionA n_regionB  z_score   p_value n_overlaps
      1        3 regAB       100       100   0.0000 0.6666667         51
      2        2  regB       100       100  14.0856 0.1666667        100
      3        1  regA       100       100 -12.5169 0.1666667          1
        mean_perm_test sd_perm_test norm_zscore adj.p_value
      1           51.0     2.738613     0.00000      0.6667
      2           50.6     3.507136     1.40856      0.2500
      3           49.8     3.898718    -1.25169      0.2500
      
      $regAB
        order.id  name n_regionA n_regionB z_score   p_value n_overlaps
      1        3 regAB       100       100 18.6226 0.1666667        102
      2        2  regB       100       100  1.0359 0.1666667         51
      3        1  regA       100       100 -0.4264 0.6666667         51
        mean_perm_test sd_perm_test norm_zscore adj.p_value
      1           51.0     2.738613     1.86226      0.2500
      2           48.4     2.509980     0.10359      0.2500
      3           52.0     2.345208    -0.04264      0.6667
      
      
      Slot "matrix":
      [[1]]
      NULL
      
      

---

    Code
      crosswisePermTest(Alist = AlienRSList_narrow_small, Blist = AlienRSList_narrow_small,
        sampling = TRUE, genome = AlienGenome, ranFUN = "randomizeRegions", evFUN = "numOverlaps",
        ntimes = 5, mc.cores = 2)
    Output
      [1] "Note: The minimum p-value with only 5 permutations is 0.166666666666667. You should consider increasing the number of permutations."
      [1] "Note: The minimum p-value with only 5 permutations is 0.166666666666667. You should consider increasing the number of permutations."
      [1] "Note: The minimum p-value with only 5 permutations is 0.166666666666667. You should consider increasing the number of permutations."
      An object of class "genoMatriXeR"
      Slot "parameters":
      $Alist
      [1] "AlienRSList_narrow_small"
      
      $Blist
      [1] "AlienRSList_narrow_small"
      
      $sampling
      [1] "TRUE"
      
      $fraction
      [1] "0.15"
      
      $min_sampling
      [1] "0.15"
      
      $ranFUN
      [1] "randomizeRegions"
      
      $evFUN
      [1] "numOverlaps"
      
      $ntimes
      [1] 5
      
      $universe
      NULL
      
      $adj_pv_method
      [1] "BH"
      
      $nc
      NULL
      
      $matOrder
      NULL
      
      
      Slot "multiOverlaps":
      $regA
        order.id  name n_regionA n_regionB  z_score   p_value n_overlaps
      1        3 regAB       100       100  56.3489 0.1666667         51
      2        2  regB       100       100   0.2390 0.6666667          1
      3        1  regA       100       100 111.1326 0.1666667        100
        mean_perm_test sd_perm_test norm_zscore adj.p_value
      1            0.6    0.8944272     5.63489      0.2500
      2            0.8    0.8366600     0.02390      0.6667
      3            0.6    0.8944272    11.11326      0.2500
      
      $regB
        order.id  name n_regionA n_regionB  z_score   p_value n_overlaps
      1        3 regAB       100       100  92.3825 0.1666667         51
      2        2  regB       100       100 223.1596 0.1666667        100
      3        1  regA       100       100   1.0954 0.5000000          1
        mean_perm_test sd_perm_test norm_zscore adj.p_value
      1            0.4    0.5477226     9.23825        0.25
      2            0.2    0.4472136    22.31596        0.25
      3            0.4    0.5477226     0.10954        0.50
      
      $regAB
        order.id  name n_regionA n_regionB  z_score   p_value n_overlaps
      1        3 regAB       100       100 227.6317 0.1666667        102
      2        2  regB       100       100 113.5923 0.1666667         51
      3        1  regA       100       100  92.3825 0.1666667         51
        mean_perm_test sd_perm_test norm_zscore adj.p_value
      1            0.2    0.4472136    22.76317      0.1667
      2            0.2    0.4472136    11.35923      0.1667
      3            0.4    0.5477226     9.23825      0.1667
      
      
      Slot "matrix":
      [[1]]
      NULL
      
      

---

    Code
      crosswisePermTest(Alist = AlienRSList_narrow_small, Blist = AlienRSList_narrow_small,
        sampling = TRUE, genome = AlienGenome, ranFUN = "circularRandomizeRegions",
        evFUN = "numOverlaps", ntimes = 5, mc.cores = 2)
    Output
      [1] "Note: The minimum p-value with only 5 permutations is 0.166666666666667. You should consider increasing the number of permutations."
    Warning <simpleWarning>
      All permuted values are equal to 1. Z-score is infinite.
    Output
      [1] "Note: The minimum p-value with only 5 permutations is 0.166666666666667. You should consider increasing the number of permutations."
      [1] "Note: The minimum p-value with only 5 permutations is 0.166666666666667. You should consider increasing the number of permutations."
      An object of class "genoMatriXeR"
      Slot "parameters":
      $Alist
      [1] "AlienRSList_narrow_small"
      
      $Blist
      [1] "AlienRSList_narrow_small"
      
      $sampling
      [1] "TRUE"
      
      $fraction
      [1] "0.15"
      
      $min_sampling
      [1] "0.15"
      
      $ranFUN
      [1] "circularRandomizeRegions"
      
      $evFUN
      [1] "numOverlaps"
      
      $ntimes
      [1] 5
      
      $universe
      NULL
      
      $adj_pv_method
      [1] "BH"
      
      $nc
      NULL
      
      $matOrder
      NULL
      
      
      Slot "multiOverlaps":
      $regA
        order.id  name n_regionA n_regionB  z_score   p_value n_overlaps
      1        3 regAB       100       100      Inf 0.1666667         51
      2        2  regB       100       100  -0.2390 0.6666667          1
      3        1  regA       100       100 181.4787 0.1666667        100
        mean_perm_test sd_perm_test norm_zscore adj.p_value
      1            1.0    0.0000000         Inf      0.2500
      2            1.2    0.8366600    -0.02390      0.6667
      3            0.6    0.5477226    18.14787      0.2500
      
      $regB
        order.id  name n_regionA n_regionB  z_score   p_value n_overlaps
      1        3 regAB       100       100  92.0174 0.1666667         51
      2        2  regB       100       100 181.8439 0.1666667        100
      3        1  regA       100       100   0.7303 0.6666667          1
        mean_perm_test sd_perm_test norm_zscore adj.p_value
      1            0.6    0.5477226     9.20174      0.2500
      2            0.4    0.5477226    18.18439      0.2500
      3            0.6    0.5477226     0.07303      0.6667
      
      $regAB
        order.id  name n_regionA n_regionB  z_score   p_value n_overlaps
      1        3 regAB       100       100 185.1302 0.1666667        102
      2        2  regB       100       100  56.3489 0.1666667         51
      3        1  regA       100       100  92.0174 0.1666667         51
        mean_perm_test sd_perm_test norm_zscore adj.p_value
      1            0.6    0.5477226    18.51302      0.1667
      2            0.6    0.8944272     5.63489      0.1667
      3            0.6    0.5477226     9.20174      0.1667
      
      
      Slot "matrix":
      [[1]]
      NULL
      
      

# crosswisePermTest: different evaluation functions

    Code
      crosswisePermTest(Alist = AlienRSList_narrow_small, Blist = AlienRSList_narrow_small,
        sampling = FALSE, genome = AlienGenome, per.chromosome = TRUE, ranFUN = "circularRandomizeRegions",
        evFUN = "numOverlaps", ntimes = 5, mc.cores = 2)
    Output
      [1] "Note: The minimum p-value with only 5 permutations is 0.166666666666667. You should consider increasing the number of permutations."
      [1] "Note: The minimum p-value with only 5 permutations is 0.166666666666667. You should consider increasing the number of permutations."
      [1] "Note: The minimum p-value with only 5 permutations is 0.166666666666667. You should consider increasing the number of permutations."
      An object of class "genoMatriXeR"
      Slot "parameters":
      $Alist
      [1] "AlienRSList_narrow_small"
      
      $Blist
      [1] "AlienRSList_narrow_small"
      
      $sampling
      [1] "FALSE"
      
      $fraction
      [1] "0.15"
      
      $min_sampling
      [1] "0.15"
      
      $ranFUN
      [1] "circularRandomizeRegions"
      
      $evFUN
      [1] "numOverlaps"
      
      $ntimes
      [1] 5
      
      $universe
      NULL
      
      $adj_pv_method
      [1] "BH"
      
      $nc
      NULL
      
      $matOrder
      NULL
      
      
      Slot "multiOverlaps":
      $regA
        order.id  name n_regionA n_regionB  z_score   p_value n_overlaps
      1        3 regAB       100       100  92.0174 0.1666667         51
      2        2  regB       100       100   1.0954 0.5000000          1
      3        1  regA       100       100 140.0071 0.1666667        100
        mean_perm_test sd_perm_test norm_zscore adj.p_value
      1            0.6    0.5477226     9.20174        0.25
      2            0.4    0.5477226     0.10954        0.50
      3            1.0    0.7071068    14.00071        0.25
      
      $regB
        order.id  name n_regionA n_regionB  z_score   p_value n_overlaps
      1        3 regAB       100       100  92.3825 0.1666667         51
      2        2  regB       100       100 111.1326 0.1666667        100
      3        1  regA       100       100   1.0954 0.5000000          1
        mean_perm_test sd_perm_test norm_zscore adj.p_value
      1            0.4    0.5477226     9.23825        0.25
      2            0.6    0.8944272    11.11326        0.25
      3            0.4    0.5477226     0.10954        0.50
      
      $regAB
        order.id  name n_regionA n_regionB  z_score   p_value n_overlaps
      1        3 regAB       100       100 120.9571 0.1666667        102
      2        2  regB       100       100  70.7107 0.1666667         51
      3        1  regA       100       100  92.0174 0.1666667         51
        mean_perm_test sd_perm_test norm_zscore adj.p_value
      1            0.8    0.8366600    12.09571      0.1667
      2            1.0    0.7071068     7.07107      0.1667
      3            0.6    0.5477226     9.20174      0.1667
      
      
      Slot "matrix":
      [[1]]
      NULL
      
      

---

    Code
      crosswisePermTest(Alist = AlienRSList_narrow_small, Blist = AlienRSList_narrow_small,
        sampling = FALSE, genome = AlienGenome, ranFUN = "circularRandomizeRegions",
        evFUN = "meanDistance", ntimes = 5, mc.cores = 2)
    Output
      [1] "Note: The minimum p-value with only 5 permutations is 0.166666666666667. You should consider increasing the number of permutations."
      [1] "Note: The minimum p-value with only 5 permutations is 0.166666666666667. You should consider increasing the number of permutations."
      [1] "Note: The minimum p-value with only 5 permutations is 0.166666666666667. You should consider increasing the number of permutations."
      An object of class "genoMatriXeR"
      Slot "parameters":
      $Alist
      [1] "AlienRSList_narrow_small"
      
      $Blist
      [1] "AlienRSList_narrow_small"
      
      $sampling
      [1] "FALSE"
      
      $fraction
      [1] "0.15"
      
      $min_sampling
      [1] "0.15"
      
      $ranFUN
      [1] "circularRandomizeRegions"
      
      $evFUN
      [1] "meanDistance"
      
      $ntimes
      [1] 5
      
      $universe
      NULL
      
      $adj_pv_method
      [1] "BH"
      
      $nc
      NULL
      
      $matOrder
      NULL
      
      
      Slot "multiOverlaps":
      $regA
        order.id  name n_regionA n_regionB  z_score   p_value n_overlaps
      1        3 regAB       100       100  -8.1853 0.1666667    8437.48
      2        2  regB       100       100  -0.6198 0.3333333   17277.30
      3        1  regA       100       100 -16.1696 0.1666667       0.00
        mean_perm_test sd_perm_test norm_zscore adj.p_value
      1       15626.33     878.2618    -0.81853      0.2500
      2       18614.15    2157.0150    -0.06198      0.3333
      3       15704.38     971.2258    -1.61696      0.2500
      
      $regB
        order.id  name n_regionA n_regionB  z_score   p_value n_overlaps
      1        3 regAB       100       100 -11.2445 0.1666667    9355.01
      2        2  regB       100       100  -9.7372 0.1666667       0.00
      3        1  regA       100       100   0.0404 0.6666667   17674.54
        mean_perm_test sd_perm_test norm_zscore adj.p_value
      1       19019.48     859.4867    -1.12445      0.2500
      2       20490.17    2104.3241    -0.97372      0.2500
      3       17598.46    1884.7821     0.00404      0.6667
      
      $regAB
        order.id  name n_regionA n_regionB z_score   p_value n_overlaps
      1        3 regAB       100       100 -4.6879 0.1666667       0.00
      2        2  regB       100       100 -4.9924 0.1666667    9403.62
      3        1  regA       100       100 -2.8982 0.1666667    8452.50
        mean_perm_test sd_perm_test norm_zscore adj.p_value
      1       16868.50     3598.337    -0.46879      0.1667
      2       20700.20     2262.755    -0.49924      0.1667
      3       16791.15     2877.158    -0.28982      0.1667
      
      
      Slot "matrix":
      [[1]]
      NULL
      
      

# crosswisePermTest: resampling and fraction

    Code
      crosswisePermTest(Alist = AlienRSList_narrow_small, Blist = AlienRSList_narrow_small,
        sampling = TRUE, fraction = 0.25, genome = AlienGenome, per.chromosome = TRUE,
        ranFUN = "resampleGenome", evFUN = "numOverlaps", ntimes = 5, mc.cores = 2)
    Output
      [1] "Note: The minimum p-value with only 5 permutations is 0.166666666666667. You should consider increasing the number of permutations."
      [1] "Note: The minimum p-value with only 5 permutations is 0.166666666666667. You should consider increasing the number of permutations."
      [1] "Note: The minimum p-value with only 5 permutations is 0.166666666666667. You should consider increasing the number of permutations."
      An object of class "genoMatriXeR"
      Slot "parameters":
      $Alist
      [1] "AlienRSList_narrow_small"
      
      $Blist
      [1] "AlienRSList_narrow_small"
      
      $sampling
      [1] "TRUE"
      
      $fraction
      [1] "0.25"
      
      $min_sampling
      [1] "0.25"
      
      $ranFUN
      [1] "resampleGenome"
      
      $evFUN
      [1] "numOverlaps"
      
      $ntimes
      [1] 5
      
      $universe
      NULL
      
      $adj_pv_method
      [1] "BH"
      
      $nc
      NULL
      
      $matOrder
      NULL
      
      
      Slot "multiOverlaps":
      $regA
        order.id  name n_regionA n_regionB  z_score   p_value n_overlaps
      1        3 regAB       100       100 113.5923 0.1666667         51
      2        2  regB       100       100  -0.7303 0.6666667          1
      3        1  regA       100       100 118.5667 0.1666667        100
        mean_perm_test sd_perm_test norm_zscore adj.p_value
      1            0.2    0.4472136    11.35923      0.2500
      2            1.4    0.5477226    -0.07303      0.6667
      3            0.8    0.8366600    11.85667      0.2500
      
      $regB
        order.id  name n_regionA n_regionB  z_score   p_value n_overlaps
      1        3 regAB       100       100  35.3553 0.1666667         51
      2        2  regB       100       100 118.5667 0.1666667        100
      3        1  regA       100       100   0.2390 0.6666667          1
        mean_perm_test sd_perm_test norm_zscore adj.p_value
      1            1.0     1.414214     3.53553      0.2500
      2            0.8     0.836660    11.85667      0.2500
      3            0.8     0.836660     0.02390      0.6667
      
      $regAB
        order.id  name n_regionA n_regionB  z_score   p_value n_overlaps
      1        3 regAB       100       100 113.5923 0.1666667        102
      2        2  regB       100       100 113.5923 0.1666667         51
      3        1  regA       100       100  38.1949 0.1666667         51
        mean_perm_test sd_perm_test norm_zscore adj.p_value
      1            0.4    0.8944272    11.35923      0.1667
      2            0.2    0.4472136    11.35923      0.1667
      3            1.2    1.3038405     3.81949      0.1667
      
      
      Slot "matrix":
      [[1]]
      NULL
      
      

---

    Code
      crosswisePermTest(Alist = AlienRSList_narrow_small, Blist = AlienRSList_narrow_small,
        sampling = TRUE, fraction = 0.5, genome = AlienGenome, per.chromosome = TRUE,
        ranFUN = "resampleGenome", evFUN = "numOverlaps", ntimes = 5, mc.cores = 2)
    Output
      [1] "Note: The minimum p-value with only 5 permutations is 0.166666666666667. You should consider increasing the number of permutations."
    Warning <simpleWarning>
      All permuted values are equal to 0. Z-score is infinite.
    Output
      [1] "Note: The minimum p-value with only 5 permutations is 0.166666666666667. You should consider increasing the number of permutations."
      [1] "Note: The minimum p-value with only 5 permutations is 0.166666666666667. You should consider increasing the number of permutations."
      An object of class "genoMatriXeR"
      Slot "parameters":
      $Alist
      [1] "AlienRSList_narrow_small"
      
      $Blist
      [1] "AlienRSList_narrow_small"
      
      $sampling
      [1] "TRUE"
      
      $fraction
      [1] "0.5"
      
      $min_sampling
      [1] "0.5"
      
      $ranFUN
      [1] "resampleGenome"
      
      $evFUN
      [1] "numOverlaps"
      
      $ntimes
      [1] 5
      
      $universe
      NULL
      
      $adj_pv_method
      [1] "BH"
      
      $nc
      NULL
      
      $matOrder
      NULL
      
      
      Slot "multiOverlaps":
      $regA
        order.id  name n_regionA n_regionB z_score   p_value n_overlaps
      1        3 regAB       100       100 92.0174 0.1666667         51
      2        2  regB       100       100  0.2390 0.6666667          1
      3        1  regA       100       100     Inf 0.1666667        100
        mean_perm_test sd_perm_test norm_zscore adj.p_value
      1            0.6    0.5477226     9.20174      0.2500
      2            0.8    0.8366600     0.02390      0.6667
      3            0.0    0.0000000         Inf      0.2500
      
      $regB
        order.id  name n_regionA n_regionB  z_score   p_value n_overlaps
      1        3 regAB       100       100  60.0005 0.1666667         51
      2        2  regB       100       100 118.5667 0.1666667        100
      3        1  regA       100       100   0.0000 0.5000000          1
        mean_perm_test sd_perm_test norm_zscore adj.p_value
      1            0.8     0.836660     6.00005        0.25
      2            0.8     0.836660    11.85667        0.25
      3            1.0     1.414214     0.00000        0.50
      
      $regAB
        order.id  name n_regionA n_regionB z_score   p_value n_overlaps
      1        3 regAB       100       100 75.5791 0.1666667        102
      2        2  regB       100       100 32.7053 0.1666667         51
      3        1  regA       100       100 92.3825 0.1666667         51
        mean_perm_test sd_perm_test norm_zscore adj.p_value
      1            0.6    1.3416408     7.55791      0.1667
      2            1.4    1.5165751     3.27053      0.1667
      3            0.4    0.5477226     9.23825      0.1667
      
      
      Slot "matrix":
      [[1]]
      NULL
      
      

