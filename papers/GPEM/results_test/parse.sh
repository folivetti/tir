#!/bin/bash

for i in "192_vineyard" "228_elusage" "485_analcatdata_vehicle" "1096_FacultySalaries" "523_analcatdata_neavote" "663_rabe_266" "687_sleuth_ex1605" "659_sleuth_ex1714" "678_visualizing_environmental" "611_fri_c3_100_5" "594_fri_c2_100_5" "624_fri_c0_100_5" "656_fri_c1_100_5" "210_cloud" "706_sleuth_case1202" "1089_USCrime" "712_chscase_geyser1"; do
    echo $i 
    grep r2_train ${i}_TIRMOO_*
    grep r2_test ${i}_TIRMOO_*
done
