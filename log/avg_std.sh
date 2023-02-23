#!/bin/bash 

cat $1 | jq .$2 | awk '{for(i=1;i<=NF;i++) {sum[i] += $i; sumsq[i] += ($i)^2}} 
          END {for (i=1;i<=NF;i++) {
          printf "$%.2e \\pm %.2e$ \n", sum[i]/NR, sqrt((sumsq[i]-sum[i]^2/NR)/NR)}
         }'

    #awk '{sum+=$1; sumsq+=$1*$1}END{print sqrt(sumsq/NR - (sum/NR)**2)}'
