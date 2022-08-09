# crosswisePermTest

    Code
      crosswisePermTest(Alist = AlienRSList_narrow_small, Blist = AlienRSList_narrow_small,
        sampling = FALSE, genome = AlienGenome, per.chromosome = TRUE, ranFUN = "resampleGenome",
        evFUN = "numOverlaps", ntimes = 10, mc.cores = 2)
    Output
      [1] "Note: The minimum p-value with only 10 permutations is 0.0909090909090909. You should consider increasing the number of permutations."
      [1] "Note: The minimum p-value with only 10 permutations is 0.0909090909090909. You should consider increasing the number of permutations."
      [1] "Note: The minimum p-value with only 10 permutations is 0.0909090909090909. You should consider increasing the number of permutations."
      [1] "Note: The minimum p-value with only 10 permutations is 0.0909090909090909. You should consider increasing the number of permutations."
      [1] "Note: The minimum p-value with only 10 permutations is 0.0909090909090909. You should consider increasing the number of permutations."
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
      [1] 10
      
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
        order.id    name n_regionA n_regionB  z_score    p_value n_overlaps
      1        5   regAB       100       100  52.3760 0.09090909         51
      2        4 regB_08       100       100   1.9261 0.18181818          2
      3        3    regB       100       100  -0.1006 0.72727273          1
      4        2 regA_08       100       100  39.8917 0.09090909         21
      5        1    regA       100       100 142.1613 0.09090909        100
        mean_perm_test sd_perm_test norm_zscore adj.p_value
      1            0.4    0.9660918     5.23760      0.1515
      2            0.7    0.6749486     0.19261      0.2273
      3            1.1    0.9944289    -0.01006      0.7273
      4            0.4    0.5163978     3.98917      0.1515
      5            0.6    0.6992059    14.21613      0.1515
      
      $regA_08
        order.id    name n_regionA n_regionB  z_score    p_value n_overlaps
      1        5   regAB       100       100  18.0204 0.09090909         13
      2        4 regB_08       100       100  -0.8581 0.54545455          0
      3        3    regB       100       100  -0.5883 0.72727273          0
      4        2 regA_08       100       100 210.5390 0.09090909        102
      5        1    regA       100       100  42.8531 0.09090909         21
        mean_perm_test sd_perm_test norm_zscore adj.p_value
      1            0.4    0.6992059     1.80204      0.1515
      2            0.6    0.6992059    -0.08581      0.6818
      3            0.5    0.8498366    -0.05883      0.7273
      4            0.3    0.4830459    21.05390      0.1515
      5            0.3    0.4830459     4.28531      0.1515
      
      $regB
        order.id    name n_regionA n_regionB  z_score    p_value n_overlaps
      1        5   regAB       100       100  63.6401 0.09090909         51
      2        4 regB_08       100       100  37.9552 0.09090909         20
      3        3    regB       100       100 116.9252 0.09090909        100
      4        2 regA_08       100       100  -0.7071 0.63636364          0
      5        1    regA       100       100   0.4743 0.45454545          1
        mean_perm_test sd_perm_test norm_zscore adj.p_value
      1            0.8    0.7888106     6.36401      0.1515
      2            0.4    0.5163978     3.79552      0.1515
      3            1.4    0.8432740    11.69252      0.1515
      4            0.5    0.7071068    -0.07071      0.6364
      5            0.6    0.8432740     0.04743      0.5682
      
      $regB_08
        order.id    name n_regionA n_regionB  z_score    p_value n_overlaps
      1        5   regAB       100       100  17.7344 0.09090909         13
      2        4 regB_08       100       100 145.3077 0.09090909        102
      3        3    regB       100       100  12.7456 0.09090909         20
      4        2 regA_08       100       100  -0.7746 0.63636364          0
      5        1    regA       100       100   2.1213 0.18181818          2
        mean_perm_test sd_perm_test norm_zscore adj.p_value
      1            0.6    0.6992059     1.77344      0.1515
      2            0.4    0.6992059    14.53077      0.1515
      3            1.0    1.4907120     1.27456      0.1515
      4            0.4    0.5163978    -0.07746      0.6364
      5            0.5    0.7071068     0.21213      0.2273
      
      $regAB
        order.id    name n_regionA n_regionB  z_score    p_value n_overlaps
      1        5   regAB       100       100  95.6247 0.09090909        102
      2        4 regB_08       100       100  17.6777 0.09090909         13
      3        3    regB       100       100  47.4820 0.09090909         51
      4        2 regA_08       100       100  17.6777 0.09090909         13
      5        1    regA       100       100 104.1309 0.09090909         51
        mean_perm_test sd_perm_test norm_zscore adj.p_value
      1            0.7    1.0593499     9.56247      0.0909
      2            0.5    0.7071068     1.76777      0.0909
      3            0.7    1.0593499     4.74820      0.0909
      4            0.5    0.7071068     1.76777      0.0909
      5            0.7    0.4830459    10.41309      0.0909
      
      
      Slot "matrix":
      [[1]]
      NULL
      
      

