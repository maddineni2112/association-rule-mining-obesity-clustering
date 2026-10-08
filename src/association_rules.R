# Project 2 - Milestone 1
#==============================
# Task 1(b): Pre-processing the votes dataset.
#==============================

# Load required libraries
library(tidyverse)
#install.packages("arules")
#install.packages("arulesViz")
library(arules)
library(arulesViz)

# Read the dataset
# -----------------------------
votes <- read.csv("C:\\Users\\SampathNagaMaddineni\\Downloads\\datamining\\project2_module2\\house-votes-84.data",
                  header = FALSE,
                  stringsAsFactors = FALSE)
print(votes[1:5, ])

#Assigning column names From the summary we can see there are no column names in .data file 
#so we add these column name details that are in the .names file 
# -----------------------------
print("adding the column names.....")
colnames(votes) <- c(
  "party",
  "handicapped_infants",
  "water_project_cost_sharing",
  "adoption_of_budget_resolution",
  "physician_fee_freeze",
  "el_salvador_aid",
  "religious_groups_in_schools",
  "anti_satellite_test_ban",
  "aid_to_nicaraguan_contras",
  "mx_missile",
  "immigration",
  "synfuels_corporation_cutback",
  "education_spending",
  "superfund_right_to_sue",
  "crime",
  "duty_free_exports",
  "export_administration_act_south_africa"
)
print("Processesing.......\nColumns Names Sucessfully Added:\n")
print(votes[1:5, ])

#Inspect the raw data
# -----------------------------
cat("Dimensions of dataset:\n")
print(dim(votes))

cat("\nStructure of dataset:\n")
str(votes)

cat("\nFirst 6 rows:\n")
print(head(votes))

cat("\nUnique values in each column:\n")
print(lapply(votes, unique))

# Handling missing "?" or unknown data
# -----------------------------
# In this dataset, ? does NOT mean ordinary missing data which is mentioned in the .names file.
# It represents an unknown / abstain / present / no-position disposition.
# So we keep it as its own category and relabel as "unknown" it for clarity 
#instead of just removing or droping the missing values.
votes[votes == "?"] <- "unknown"

# Convert all attributes to factors
# -----------------------------
# This is appropriate because all attributes are categorical
# and Apriori works with transactional/categorical data.

votes <- votes %>%
  mutate(across(everything(), as.factor))
cat("\nStructure after factor conversion:\n")
str(votes)

# Convert to transaction format
# -----------------------------
# Apriori in arules requires transaction data.
votes_trans <- as(votes, "transactions")
cat("\nTransaction summary:\n")
print(summary(votes_trans))




# ============================================================
# Task 2A: Generate TOP20 association rules using Apriori 
# using rule Lift and with a Confidence value of 0.9 or greater.
# ============================================================
# We set:
# supp = 0.05  -> low enough to get a reasonable number of rules
# conf = 0.90  -> as required in the assignment
# min length = 2   -> at least one item on LHS and one item on RHS
#Max length =17 -> generates all possible rules with out stopping in the middle
rules <- apriori(
  votes_trans,
  parameter = list(
    supp = 0.05,
    conf = 0.90,
    minlen = 2,
    maxlen = 17,
    target = "rules"
  )
)

# Summary of generated rules
# -----------------------------
cat("\nSummary of generated rules:\n")
summary(rules)

cat("\nTotal number of rules generated:\n")
print(length(rules))

# Sorting rules by Lift in descending order to extract TOP20
# -----------------------------
rules_sorted <- sort(rules, by = "lift", decreasing = TRUE)
top20_rules <- head(rules_sorted, 20)
top20_rules_df <- as(top20_rules, "data.frame")
cat("\nTop 20 rules by Lift (Confidence >= 0.9) as a data frame::\n")
print(top20_rules_df)

#saving as csv
#-----------------
write.csv(top20_rules_df, "top20_rules_by_lift.csv", row.names = FALSE)




# ============================================================
# Task 3A: Identify and remove redundant rules
# Combined code


#  Check redundancy on ALL rules
# -----------------------------
redundant_flags <- is.redundant(rules_sorted)

cat("\nNumber of redundant rules:\n")
print(sum(redundant_flags))

cat("\nNumber of non-redundant rules:\n")
print(sum(!redundant_flags))

# -----------------------------
# 2. Function to find the source rule
# A rule is redundant if a more general rule
# with the same RHS has equal or higher confidence
# -----------------------------
find_source_rule <- function(i, ruleset) {
  lhs_i <- labels(lhs(ruleset[i]))[1]
  rhs_i <- labels(rhs(ruleset[i]))[1]
  conf_i <- quality(ruleset[i])$confidence
  
  lhs_items_i <- unlist(strsplit(gsub("[\\{\\}]", "", lhs_i), ","))
  
  candidate_indices <- c()
  
  for (j in seq_along(ruleset)) {
    if (i == j) next
    
    lhs_j <- labels(lhs(ruleset[j]))[1]
    rhs_j <- labels(rhs(ruleset[j]))[1]
    conf_j <- quality(ruleset[j])$confidence
    
    lhs_items_j <- unlist(strsplit(gsub("[\\{\\}]", "", lhs_j), ","))
    
    same_rhs <- (rhs_i == rhs_j)
    more_general <- all(lhs_items_j %in% lhs_items_i) &&
      length(lhs_items_j) < length(lhs_items_i)
    conf_ok <- conf_j >= conf_i
    
    if (same_rhs && more_general && conf_ok) {
      candidate_indices <- c(candidate_indices, j)
    }
  }
  
  if (length(candidate_indices) == 0) {
    return(NA)
  }
  
  # choose the best source rule:
  # highest confidence first, then shortest antecedent
  candidate_conf <- quality(ruleset[candidate_indices])$confidence
  candidate_len <- size(lhs(ruleset[candidate_indices]))
  best_pos <- order(-candidate_conf, candidate_len)[1]
  
  return(candidate_indices[best_pos])
}


# Checking redundancy within TOP 20 rules only
# ============================================================

rules_top20 <- top20_rules

# get printed rule IDs like 75165, 75290, ...
top20_rule_ids <- rownames(as(rules_top20, "data.frame"))

source_rule_indices_top20 <- sapply(
  seq_along(rules_top20),
  find_source_rule,
  ruleset = rules_top20
)

source_rule_text_top20 <- rep(NA_character_, length(rules_top20))
valid_idx_top20 <- which(!is.na(source_rule_indices_top20))

if (length(valid_idx_top20) > 0) {
  source_rule_text_top20[valid_idx_top20] <- labels(
    rules_top20[source_rule_indices_top20[valid_idx_top20]]
  )
}

source_rule_id_top20 <- rep(NA_character_, length(rules_top20))
if (length(valid_idx_top20) > 0) {
  source_rule_id_top20[valid_idx_top20] <- top20_rule_ids[source_rule_indices_top20[valid_idx_top20]]
}

top20_redundancy_check <- data.frame(
  top20_position = seq_along(rules_top20),
  rule_id = top20_rule_ids,
  rule = labels(rules_top20),
  is_redundant = !is.na(source_rule_indices_top20),
  source_rule_position = source_rule_indices_top20,
  source_rule_id = source_rule_id_top20,
  source_rule = source_rule_text_top20,
  support = quality(rules_top20)$support,
  confidence = quality(rules_top20)$confidence,
  coverage = quality(rules_top20)$coverage,
  lift = quality(rules_top20)$lift,
  count = quality(rules_top20)$count,
  stringsAsFactors = FALSE
)

print(top20_redundancy_check)

write.csv(
  top20_redundancy_check,
  "top20_redundancy_check.csv",
  row.names = FALSE
)

#dividing the rules dataframe into reduntant and nonreduntant data frames:
#----------------------------------------------------------------------
# Extract indices
redundant_rule_indices <- which(redundant_flags)
non_redundant_rule_indices <- which(!redundant_flags)
rule_ids_all <- rownames(as(rules_sorted, "data.frame"))

# Create redundant rules data frame
redundant_rules_df <- data.frame(
  rule_number = redundant_rule_indices,
  rule_id = rule_ids_all[redundant_rule_indices],
  rule = labels(rules_sorted[redundant_rule_indices]),
  support = quality(rules_sorted[redundant_rule_indices])$support,
  confidence = quality(rules_sorted[redundant_rule_indices])$confidence,
  coverage = quality(rules_sorted[redundant_rule_indices])$coverage,
  lift = quality(rules_sorted[redundant_rule_indices])$lift,
  count = quality(rules_sorted[redundant_rule_indices])$count,
  stringsAsFactors = FALSE
)

# Create non-redundant rules data frame
non_redundant_rules_df <- data.frame(
  rule_number = non_redundant_rule_indices,
  rule_id = rule_ids_all[non_redundant_rule_indices],
  rule = labels(rules_sorted[non_redundant_rule_indices]),
  support = quality(rules_sorted[non_redundant_rule_indices])$support,
  confidence = quality(rules_sorted[non_redundant_rule_indices])$confidence,
  coverage = quality(rules_sorted[non_redundant_rule_indices])$coverage,
  lift = quality(rules_sorted[non_redundant_rule_indices])$lift,
  count = quality(rules_sorted[non_redundant_rule_indices])$count,
  stringsAsFactors = FALSE
)

# Save both full data frames
write.csv(
  redundant_rules_df,
  "redundant_rules.csv",
  row.names = FALSE
)

write.csv(
  non_redundant_rules_df,
  "non_redundant_rules.csv",
  row.names = FALSE
)

# ============================================================
# Print Top 20 Non-Redundant Rules
# ============================================================

# Get row indices of top 20 non-redundant rules by lift
top20_non_redundant_idx <- order(
  non_redundant_rules_df$lift,
  decreasing = TRUE
)[1:20]

# Create top 20 non-redundant rules data frame
top20_non_redundant_df <- non_redundant_rules_df[top20_non_redundant_idx, ]

cat("\nTop 20 Non-Redundant Rules:\n")
print(top20_non_redundant_df)

# Save top 20 non-redundant rules
write.csv(
  top20_non_redundant_df,
  "top20_non_redundant_rules.csv",
  row.names = FALSE
)
