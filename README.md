# Association Rule Mining and Obesity Clustering

**Timeline:** April 2026  
**Category:** Main project

This project combines market-basket style association analysis with unsupervised obesity-profile clustering. The workflow ranks Apriori rules by support, confidence, and lift, then applies K-Means clustering and visual diagnostics to normalized health attributes.

## Stack

R, tidyverse, arules, arulesViz, dplyr, cluster, factoextra, reshape2, ggplot2, Apriori, and K-Means.

## Repository contents

- `src/association_rules.R` — Apriori mining and rule summaries.
- `src/obesity_clustering.R` — preprocessing, clustering, and plots.
- `results/` — small derived rule summaries.
- `reports/` — milestone reports.
- `data/README.md` — provenance and excluded-data instructions.
