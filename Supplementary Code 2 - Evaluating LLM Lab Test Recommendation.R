dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dir)
library(tidyverse)
options(scipen = 999)
rm(dir)

gold_standard_orders <- read.csv("Gold Standard Orders.csv")
uzl_testmenu <- read.csv("ALL_uzl_menu.csv")
gold_standard_orders <- gold_standard_orders %>% left_join(uzl_testmenu, by="uzl_test_name")

zeroshot_recommendations <- readxl::read_xlsx("recommended_tests_asst_XevRWKsCfz07yPbhi8VtUxz9.xlsx")
zeroshot_recommendations$recommender <- "zeroshot"
cot_recommendations <- readxl::read_xlsx("recommended_tests_asst_FsskhpyafxhnE3rsFhAEFbI8.xlsx")
cot_recommendations$recommender <- "cot"
gpt_recommendations <- rbind(zeroshot_recommendations, cot_recommendations)
threads_by_case <- gpt_recommendations %>% group_by(case_id, recommender) %>% summarise(threads_n = n_distinct(thread_id), .groups = "drop")
rm(zeroshot_recommendations, cot_recommendations, threads_by_case)

#### assessing correctness of test codes ####
gpt_recommendations$test_code <- gsub(",", ", ", gsub("[^0-9,]", "", gpt_recommendations$test_code))
names(gpt_recommendations)[names(gpt_recommendations) == "test_name"] <- "recommended_test_name"
recommended_tests <- gpt_recommendations %>% group_by(recommended_test_name, test_code) %>% summarize(n = n(), .groups = "drop") %>% arrange(desc(n))

# step 1: checking for correct names & codes
recommended_tests <- recommended_tests %>% left_join(uzl_testmenu, by="test_code")
recommended_tests$correctness <- ifelse(recommended_tests$recommended_test_name == recommended_tests$uzl_test_name, "correct", NA)
recommended_tests_clean1 <- recommended_tests %>% filter(correctness == "correct") %>% select(-uzl_test_name, -category, -menu)
recommended_tests_clean1$reviewed_test_code <- recommended_tests_clean1$test_code
sum(recommended_tests_clean1$n)*100/length(gpt_recommendations$recommended_test_name)

# step 2: checking for correct codes: names not matched to be manually checked
recommended_tests2 <- recommended_tests %>% filter(is.na(correctness) & !is.na(uzl_test_name))
#write.csv(recommended_tests2, "recommended_tests_manual_check_names.csv", row.names = FALSE, na="")
recommended_tests2 <- read.csv("intermediary_data/recommended_tests_manual_check_names CHECKED.csv") %>% select(-uzl_test_name, -category, -menu)
recommended_tests_clean2 <- recommended_tests2 %>% filter(correctness == "correct_code")
recommended_tests_clean2$reviewed_test_code <- recommended_tests_clean2$test_code
sum(recommended_tests_clean2$n)*100/length(gpt_recommendations$recommended_test_name)

recommended_tests2 <- recommended_tests2 %>% filter(correctness != "correct_code") %>% select(test_code, recommended_test_name, n)
recommended_tests <- recommended_tests %>% filter(is.na(correctness) & is.na(uzl_test_name)) %>% select(test_code, recommended_test_name, n)
recommended_tests <- rbind(recommended_tests, recommended_tests2) 
rm(recommended_tests2)

# step 3: checking for correct names, wrong codes
recommended_tests <- recommended_tests %>% left_join(uzl_testmenu, by=c("recommended_test_name"="uzl_test_name"))
recommended_tests_clean3 <- recommended_tests %>% filter(!is.na(test_code.y)) %>% select(-category, -menu)
recommended_tests_clean3$correctness <- "correct_name"
names(recommended_tests_clean3)[names(recommended_tests_clean3) == "test_code.y"] <-"reviewed_test_code"
names(recommended_tests_clean3)[names(recommended_tests_clean3) == "test_code.x"] <-"test_code"
sum(recommended_tests_clean3$n)*100/length(gpt_recommendations$recommended_test_name)

# step 4: expanding and assessing correctness of multiple codes in the same row
recommended_tests <- recommended_tests %>% filter(is.na(test_code.y)) %>% select(test_code.x, recommended_test_name, n)
recommended_tests$test_code <- recommended_tests$test_code.x
expanded_tests <- recommended_tests %>%
  filter(grepl(",", test_code)) %>%
  separate_rows(test_code, sep = ", ") %>%
  select(recommended_test_name, test_code.x, test_code, n) %>%
  left_join(uzl_testmenu, by="test_code")
#write.csv(expanded_tests, "recommended_tests_manual_check_multiple_codes.csv", row.names = FALSE)
rm(expanded_tests)
recommended_tests_clean4 <- read.csv("intermediary_data/recommended_tests_manual_check_multiple_codes CHECKED.csv") %>% 
  select(recommended_test_name, test_code, n, correctness, reviewed_test_code) %>%
  distinct_all()
sum(recommended_tests_clean4$n)*100/length(gpt_recommendations$recommended_test_name)

# step 5: checking remaining texts
recommended_tests <- recommended_tests %>% filter(!grepl(",", test_code)) %>% left_join(uzl_testmenu, by="test_code")
#write.csv(recommended_tests, "recommended_tests_manual_check_remaining.csv", row.names = FALSE)
recommended_tests_clean5 <- read.csv("intermediary_data/recommended_tests_manual_check_remaining CHECKED.csv") %>% 
  select(recommended_test_name, test_code, n, correctness, reviewed_test_code)
rm(recommended_tests)
recommended_tests_clean5$test_code <- as.character(recommended_tests_clean5$test_code)

recommended_tests_clean <- rbind(recommended_tests_clean1, recommended_tests_clean2, recommended_tests_clean3, recommended_tests_clean4, recommended_tests_clean5)
rm(recommended_tests_clean1, recommended_tests_clean2, recommended_tests_clean3, recommended_tests_clean4, recommended_tests_clean5)

gpt_recommendations$test_code[gpt_recommendations$test_code == ""] <- NA
recommended_tests_clean <- recommended_tests_clean %>% group_by(recommended_test_name, test_code, correctness, reviewed_test_code) %>% summarise(n=sum(n), .groups = "drop")
gpt_recommendations <- gpt_recommendations %>%
  left_join(recommended_tests_clean, by = c("test_code", "recommended_test_name")) 
not_reviewed_tests <- gpt_recommendations %>% filter(reviewed_test_code == "")
codes_correctness1 <- not_reviewed_tests %>% group_by(correctness) %>% summarise(n=n())

reviewed_gpt_recommendations <- gpt_recommendations %>%
  filter(reviewed_test_code != "") %>%
  select(case_id, recommender, thread_id, test_type, reviewed_test_code, correctness) %>% 
  separate_rows(reviewed_test_code, sep = ", ") %>%
  mutate(reviewed_test_code = as.numeric(reviewed_test_code))

codes_correctness2 <- reviewed_gpt_recommendations %>% group_by(correctness) %>% summarise(n=n(), .groups = "drop")
codes_correctness <- rbind(codes_correctness1, codes_correctness2) %>% arrange(desc(n))
rm(codes_correctness1, codes_correctness2)
codes_correctness$percent <- codes_correctness$n * 100 / sum(codes_correctness$n)

# correcting category of AST >> always moved to conditional 
reviewed_gpt_recommendations$test_type[reviewed_gpt_recommendations$reviewed_test_code == 4217] <- "conditional"
reviewed_gpt_recommendations <- reviewed_gpt_recommendations %>% select(-correctness) %>% distinct_all()

#write.csv(codes_correctness, "Results Data/code_correctness.csv", row.names = FALSE)
#write.csv(not_reviewed_tests, "Results Data/not_included_tests.csv", row.names = FALSE, na="")
rm(gpt_recommendations, recommended_tests_clean, uzl_testmenu, codes_correctness, not_reviewed_tests)

#### Assessing self-consistency of assistant recommendations, compared to expert first delphi responses ####
# Define the Jaccard similarity function
compute_jaccard <- function(set1, set2) {
  intersection <- length(intersect(set1, set2))
  union <- length(union(set1, set2))
  return(ifelse(union > 0, intersection / union, 0))  # Handle empty union case
}
# Generalized function to compute Jaccard similarity for each case
compute_jaccard_results <- function(data, group_vars, test_column) {
  data %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(
      jaccard_matrix = list({
        case_data <- cur_data()
        n <- nrow(case_data)
        
        # Pre-allocate Jaccard similarity matrix
        jaccard_matrix <- matrix(0, n, n)
        
        # Compute pairwise Jaccard similarity for upper triangle
        for (i in seq_len(n)) {
          for (j in seq(i, n)) {
            jaccard_matrix[i, j] <- compute_jaccard(
              case_data[[test_column]][[i]],
              case_data[[test_column]][[j]]
            )
            jaccard_matrix[j, i] <- jaccard_matrix[i, j]  # Fill symmetric value
          }
        }
        jaccard_matrix
      }),
      .groups = "drop"
    ) %>%
    mutate(
      # Calculate mean Jaccard similarity for off-diagonal elements
      mean_jaccard = sapply(
        jaccard_matrix,
        function(mat) mean(mat[upper.tri(mat, diag = FALSE)])
      )
    )
}
# Grouping by case_id and thread_id, summarizing test_code into a list
lab_tests_by_thread1 <- reviewed_gpt_recommendations %>%
  group_by(case_id, recommender, thread_id, test_type) %>%
  summarise(recommended_tests = list(reviewed_test_code), .groups = "drop") %>%
  mutate(recommended_test_count = sapply(strsplit(as.character(recommended_tests), ","), length))
missing_categories <- lab_tests_by_thread1 %>% group_by(thread_id) %>% filter(n()<2)
missing_categories$test_type <- "conditional"
missing_categories$recommended_tests <- NA
missing_categories$recommended_test_count <- 0
lab_tests_by_thread1 <- rbind(lab_tests_by_thread1, missing_categories)

lab_tests_by_thread2 <- reviewed_gpt_recommendations %>%
  group_by(case_id, recommender, thread_id) %>%
  summarise(recommended_tests = list(reviewed_test_code), .groups = "drop") %>%
  mutate(recommended_test_count = sapply(strsplit(as.character(recommended_tests), ","), length))
lab_tests_by_thread2$test_type <- "combined"
lab_tests_by_thread <- rbind(lab_tests_by_thread1, lab_tests_by_thread2) %>% arrange(case_id, recommender, thread_id, test_type)
rm(missing_categories, lab_tests_by_thread1, lab_tests_by_thread2, reviewed_gpt_recommendations)

# Apply the function to GPT recommendations
jaccard_results_gpt <- compute_jaccard_results(
  data = lab_tests_by_thread,
  group_vars = c("case_id", "recommender", "test_type"),
  test_column = "recommended_tests"
)
recommender_consistency <-jaccard_results_gpt %>% group_by(recommender, test_type) %>% 
  summarize(
    mean_jaccard_recommender = mean(mean_jaccard),
    sd_jaccard_recommender = sd(mean_jaccard), .groups = "drop")

# Assessing Self-consistency between experts
# Create a list of tests for each thread within each case
first_delphi_round <- read.csv("intermediary_data/First_Delphi_expert_tests.csv")
names(first_delphi_round)[names(first_delphi_round) == "recommendation_type"] <- "test_type"
lab_tests_by_expert <- first_delphi_round %>%
  separate_rows(test_code, sep = ", ") %>%
  select(case_id, expert_name, test_code, test_type) %>% 
  distinct_all() %>%
  mutate(test_code = as.numeric(test_code))
lab_tests_by_expert1 <- lab_tests_by_expert %>% group_by(case_id, expert_name, test_type) %>%
  summarise(expert_tests = list(unique(test_code)), .groups = "drop")
lab_tests_by_expert2 <- lab_tests_by_expert %>% group_by(case_id, expert_name) %>%
  summarise(expert_tests = list(unique(test_code)), .groups = "drop")
lab_tests_by_expert2$test_type <- "combined"
lab_tests_by_expert <- rbind(lab_tests_by_expert1, lab_tests_by_expert2)
rm(lab_tests_by_expert1, lab_tests_by_expert2)

# Apply the function to expert first delphi responses
jaccard_results_experts <- compute_jaccard_results(
  data = lab_tests_by_expert,
  group_vars = c("case_id", "test_type"),
  test_column = "expert_tests"
)
expert_consistency <-jaccard_results_experts %>% group_by(test_type) %>% 
  summarize(
    mean_jaccard_recommender = mean(mean_jaccard, na.rm = TRUE),
    sd_jaccard_recommender = sd(mean_jaccard, na.rm = TRUE), .groups = "drop")
expert_consistency$recommender <- "experts"
recommender_consistency <- rbind(recommender_consistency, expert_consistency) %>% arrange(desc(mean_jaccard_recommender))
jaccard_results_experts$recommender <- "experts"
jaccard_results <- rbind(jaccard_results_gpt, jaccard_results_experts) %>% select(-jaccard_matrix) %>% arrange(desc(mean_jaccard))

# Perform pairwise comparisons with Holm adjustment
run_pairwise_wilcox <- function(data, group_var, comparison_var, group_name) {
  cat("\nGroup by:", group_name, "\n")
  unique_groups <- unique(data[[group_var]])
  results <- list()
  
  for (group in unique_groups) {
    cat("\n", group_name, ":", group, "\n")
    group_data <- data %>% filter(.data[[group_var]] == group)
    
    pairwise_results <- pairwise.wilcox.test(
      x = group_data[[comparison_var]],
      g = group_data[[setdiff(c("test_type", "recommender"), group_var)]],
      p.adjust.method = "holm",
      exact = FALSE,
      correct = TRUE
    )
    print(pairwise_results)
    results[[group]] <- pairwise_results
  }
  return(results)
}

# Run for test categories (compare recommenders within each category)
run_pairwise_wilcox(
  data = jaccard_results,
  group_var = "test_type",
  comparison_var = "mean_jaccard",
  group_name = "Test Category"
)

# Run for recommenders (compare test categories within each recommender)
run_pairwise_wilcox(
  data = jaccard_results,
  group_var = "recommender",
  comparison_var = "mean_jaccard",
  group_name = "Recommender"
)

#write.csv(jaccard_results, "Results Data/jaccard_results.csv", row.names = FALSE)
#write.csv(recommender_consistency, "Results Data/recommender_consistency.csv", row.names = FALSE)
rm(expert_consistency, jaccard_results_gpt, jaccard_results_experts, compute_jaccard, compute_jaccard_results, 
   jaccard_results, run_pairwise_wilcox, lab_tests_by_expert)

#### Evaluation of recommended tests against gold standard ####
# Grouping gold standard orders, Comparing lists of tests for each recommender
names(gold_standard_orders)[names(gold_standard_orders) == "agreed_on_category"] <- "test_type"
gold_standard_lists <- gold_standard_orders %>%
  separate_rows(test_code, sep = ", ") %>%
  select(case_id, test_code, test_type) %>% 
  distinct_all() %>%
  mutate(test_code = as.numeric(test_code))
gold_standard_lists1 <- gold_standard_lists %>% group_by(case_id, test_type) %>%
  summarise(gold_standard_tests = list(unique(test_code)), .groups = "drop") %>%
  mutate(gold_test_count = sapply(strsplit(as.character(gold_standard_tests), ","), length))
gold_standard_lists2 <- gold_standard_lists %>% group_by(case_id) %>%
  summarise(gold_standard_tests = list(unique(test_code)), .groups = "drop") %>%
  mutate(gold_test_count = sapply(strsplit(as.character(gold_standard_tests), ","), length))
gold_standard_lists2$test_type <- "combined"
gold_standard_lists <- rbind(gold_standard_lists1, gold_standard_lists2)
lab_tests_by_thread <- lab_tests_by_thread %>% left_join(gold_standard_lists, by=c("case_id", "test_type"))
rm(gold_standard_lists, gold_standard_lists1, gold_standard_lists2)

# Define the function to calculate precision, recall, and F1 score
calculate_metrics <- function(recommended, gold_standard) {
  # Calculate True Positives (TP), False Positives (FP), and False Negatives (FN)
  TP_list <- intersect(recommended, gold_standard)
  FP_list <- setdiff(recommended, gold_standard)
  FN_list <- setdiff(gold_standard, recommended)
  
  TP <- length(TP_list)
  FP <- length(FP_list)
  FN <- length(FN_list)
  TN <- 1259 - FN # 1259 is the length of uzl_testmenu
  
  # Calculate Precision, Recall, and F1 Score
  precision <- ifelse(TP + FP == 0, 0, TP / (TP + FP))
  recall <- ifelse(TP + FN == 0, 0, TP / (TP + FN))
  f1_score <- ifelse(precision + recall == 0, 0, 2 * (precision * recall) / (precision + recall))
  
  # Return the results as a data frame, including the FP and FN lists
  return(data.frame(TP = TP, TP_list = I(list(TP_list)), TN = TN, FP = FP, FP_list = I(list(FP_list)), FN = FN, FN_list = I(list(FN_list)), 
                    precision = precision, recall = recall, f1_score = f1_score)
  )
}

# Thread-level evaluation with Linear Mixed Effect Modelling ####
evaluation_thread_level <- lab_tests_by_thread %>%
  rowwise() %>%
  mutate(metrics = list(calculate_metrics(recommended_tests, gold_standard_tests))) %>%  # Calculate metrics
  unnest(metrics)  # Expand list columns to separate columns
evaluation_thread_level <- evaluation_thread_level %>%
  select(case_id, recommender, thread_id, test_type, TP, TP_list, TN, FP, FP_list, FN, FN_list, precision, recall, f1_score)
evaluation_thread_level <- evaluation_thread_level %>% mutate(accuracy = (TP+TN)/(TN+TP+FN+FP))

case_complexity <- read.csv("intermediary_data/case_complexity.csv")
evaluation_thread_level <- evaluation_thread_level %>% left_join(case_complexity, by="case_id")
rm(case_complexity, calculate_metrics, lab_tests_by_thread)
#write.table(evaluation_thread_level, file = "Results Data/evaluation_thread_level.csv", sep = ";", row.names = FALSE, col.names = TRUE, quote = FALSE)

library(lme4)
library(ggplot2)
library(broom.mixed)

# Fit a linear mixed-effects model
# Change reference level of test_type to "essential"
evaluation_thread_level$recommender <- relevel(as.factor(evaluation_thread_level$recommender), ref = "zeroshot")
evaluation_thread_level$test_type <- relevel(as.factor(evaluation_thread_level$test_type), ref = "combined")
evaluation_thread_level$context <- relevel(as.factor(evaluation_thread_level$context), ref = "GP")
evaluation_thread_level$complexity <- relevel(as.factor(evaluation_thread_level$complexity), ref = "Simple")
evaluation_thread_level$repeated_threads_id <- interaction( #identifies all repeated threads that truly belong to the same exact conditions
  evaluation_thread_level$case_id, 
  evaluation_thread_level$recommender, 
  evaluation_thread_level$test_type, 
  drop = TRUE
)
model <- lmer(f1_score ~ recommender + test_type + context + complexity + (1|case_id) + (1|thread_id) + (1|repeated_threads_id), data = evaluation_thread_level)
model_without_interaction <- lmer(f1_score ~ recommender + test_type + context + complexity + (1|case_id) + (1|thread_id), data = evaluation_thread_level)
anova(model, model_without_interaction)
rm(model_without_interaction)
# Summary of the model
summary(model)

# Plot residuals vs. fitted values
residuals <- resid(model)
fitted_values <- fitted(model)
p <- ggplot(data.frame(fitted_values, residuals), aes(x = fitted_values, y = residuals)) +
  geom_point(alpha = 0.4) +
  #geom_point(aes(color = NULL), shape = 1, size = 2) +
  geom_hline(yintercept = 0, color = "#4C80A3", size = 1) +
  theme_minimal() +
  labs(title = "Residuals vs Fitted Plot",
       x = "Fitted Values",
       y = "Residuals")
ggsave(filename = "figures/Residuals vs Fitted Plot.svg", plot = p, device = "svg")
# Combine data with original dataset
diagnostic_data <- data.frame(fitted_values, residuals, evaluation_thread_level)
# Filter for observations with low fitted values (explaining left trend line)
low_fitted_subset <- diagnostic_data %>% filter(fitted_values == -residuals & f1_score == 0)
# All this subset belong to the "conditional" test category
rm(diagnostic_data, low_fitted_subset, fitted_values, residuals, p)

# Plot normal Q-Q plot for residuals
p <- ggplot(data.frame(sample = resid(model)), aes(sample = sample)) +
  stat_qq(geom = "point", alpha = 0.4) +
  #stat_qq(aes(color = NULL), geom = "point", shape = 1, size = 3) + # Open circles
  stat_qq_line(color = "#4C80A3", size = 1) +  # Add the QQ line
  theme_minimal() +
  labs(
    title = "QQ-Plot of Residuals",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  )
ggsave(filename = "figures/QQ-Plot of Residuals.svg", plot = p, device = "svg")
# Extract fixed & random effects
fixed_effects_data <- broom.mixed::tidy(model, effects = "fixed", conf.int = TRUE)
fixed_effects_data <- fixed_effects_data %>%
  mutate(
    p.value = 2 * pt(abs(statistic), df = df.residual(model), lower.tail = FALSE),
    error_bar_value = estimate - conf.low
  )
random_effects_variance <- broom.mixed::tidy(model, effects = "ran_pars")

# Fixed Effects Coefficients with the intercept set as the baseline (zero)
# Separate intercept from other fixed effects
intercept <- fixed_effects_data %>% filter(term == "(Intercept)")
fixed_effects <- fixed_effects_data %>% filter(term != "(Intercept)")

# Create a combined dataset for plotting
fixed_effects <- fixed_effects %>%
  mutate(
    estimate_relative = estimate + intercept$estimate, # Adjust estimates relative to the intercept
    conf.low_relative = estimate_relative - error_bar_value, # Adjust CI lower bounds
    conf.high_relative = estimate_relative + error_bar_value # Adjust CI upper bounds
  )
# Order the terms
fixed_effects$term <- factor(
  fixed_effects$term,
  levels = c("recommendercot", "test_typeconditional", "test_typeessential",
             "contextER", "complexityModerate", "complexityComplex")
)
intercept$term <- factor(intercept$term, levels = c("(Intercept)"))
# Combine data back for consistent plotting
combined_data <- bind_rows(intercept, fixed_effects)
# Plot
p<- ggplot() +
  # Add gridlines for the bottom scale
  geom_vline(xintercept = intercept$estimate, color = "#4C80A3", size=1) +
  geom_vline(xintercept = intercept$estimate - 0.05, linetype = "dashed", color = "gray", alpha = 0.5) +
  geom_vline(xintercept = intercept$estimate + 0.05, linetype = "dashed", color = "gray", alpha = 0.5) +
  geom_vline(xintercept = intercept$estimate - 0.1, linetype = "dashed", color = "gray", alpha = 0.5) +
  geom_vline(xintercept = intercept$estimate + 0.1, linetype = "dashed", color = "gray", alpha = 0.5) +
  geom_vline(xintercept = intercept$estimate - 0.2, linetype = "dashed", color = "gray", alpha = 0.5) +
  geom_vline(xintercept = intercept$estimate - 0.3, linetype = "dashed", color = "gray", alpha = 0.5) +
  geom_vline(xintercept = intercept$estimate - 0.4, linetype = "dashed", color = "gray", alpha = 0.5) +
  geom_vline(xintercept = 0, color = "gray", alpha = 0.5) +
  # Intercept bar with its confidence interval
  geom_bar(
    data = intercept,
    aes(x = estimate, y = term),
    stat = "identity",
    fill = "#4C80A3",
    alpha = 1
  ) +
  geom_errorbarh(
    data = intercept,
    aes(xmin = conf.low, xmax = conf.high, y = term),
    height = 0.2,
    color = "black"
  ) +
  # Fixed effects as deviations from the intercept
  geom_point(
    data = fixed_effects,
    aes(x = estimate_relative, y = term),
    size = 3
  ) +
  geom_errorbarh(
    data = fixed_effects,
    aes(xmin = conf.low_relative, xmax = conf.high_relative, y = term),
    height = 0.2,
    color = "black"
  ) +
  # Adjust scales
  scale_x_continuous(
    name = "Intercept Scale",
    sec.axis = sec_axis(~ . - intercept$estimate, name = "Effect Estimate (Relative to Intercept)"),
    position = "top"
  ) +
  # Customizing the layout
  theme_minimal() +
  labs(
    title = "Fixed Effects Coefficient Plot",
    y = NULL, # No y-axis label
    caption = "Error bars represent 95% confidence intervals"
  ) +
  theme(
    axis.text.y = element_text(size = 12), # Adjust y-axis text size
    axis.title.x = element_text(size = 14), # Adjust x-axis title size
    axis.title.x.top = element_text(size = 14), # Adjust top axis title size
    plot.title = element_text(size = 16, face = "bold"), # Adjust plot title
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.x.top = element_text(color = "black") # Add top scale text
  ) +
  # Reorder y-axis categories
  scale_y_discrete(
    limits = rev(c("(Intercept)", "recommendercot", "test_typeconditional",
               "test_typeessential", "contextER", "complexityModerate",
               "complexityComplex")),
    labels = rev(c("(Intercept)", "CoT", "Conditional", "Essential", "ER", "Moderate", "Complex"))
  )
ggsave(filename = "figures/Fixed Effects Coefficient Plot.svg", plot = p, device = "svg")
rm(combined_data, intercept, fixed_effects, p)

# Caterpillar Plot random effects for case_id
# Extract random effects with conditional variances
random_effects <- ranef(model, condVar = TRUE)

# Create separate caterpillar plots for each grouping variable
plots <- lapply(names(random_effects), function(grpvar) {
  # Extract random effects for the current grouping variable
  random_effects_grp <- as.data.frame(random_effects[[grpvar]])
  
  # Add the group names as a column
  random_effects_grp$grp <- rownames(random_effects[[grpvar]])
  
  # Add conditional SDs for confidence intervals
  condsd <- attr(ranef(model, condVar = TRUE)[[grpvar]], "postVar")
  random_effects_grp$condsd <- sqrt(diag(as.matrix(condsd)))
  
  # Rename columns for consistent plotting
  colnames(random_effects_grp) <- c("condval", "grp", "condsd")
  
  # Add confidence intervals
  random_effects_grp <- random_effects_grp %>%
    mutate(
      lower = condval - 1.96 * condsd,
      upper = condval + 1.96 * condsd
    )
  
  # Generate the plot for this grouping variable
  ggplot(random_effects_grp, aes(x = reorder(grp, condval), y = condval)) +
    geom_point(alpha = 0.4) +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1) +
    coord_flip() +
    theme_minimal() +
    theme(axis.text.y = element_blank()) +  # Remove x-axis text
    geom_hline(yintercept = 0, color = "#4C80A3", size = 1) +
    geom_hline(yintercept = -0.1, linetype = "dashed", color = "#4C80A3", size = 0.8) +
    geom_hline(yintercept = +0.1, linetype = "dashed", color = "#4C80A3", size = 0.8) +
    geom_hline(yintercept = -0.2, linetype = "dashed", color = "#4C80A3", size = 0.8) +
    geom_hline(yintercept = +0.2, linetype = "dashed", color = "#4C80A3", size = 0.8) +
    labs(title = paste("Caterpillar Plot for", grpvar),
         y = "Random Effect (Conditional Modes)",
         x = NULL)
})
plots[[1]]
ggsave(filename = "figures/Caterpillar Plot for threads.svg", plot = plots[[1]], device = "svg")
plots[[2]]
ggsave(filename = "figures/Caterpillar Plot for combination of repeated prompting.svg", plot = plots[[2]], device = "svg")
plots[[3]]
ggsave(filename = "figures/Caterpillar Plot for unique repeated prompts (threads).svg", plot = plots[[3]], device = "svg")
rm(plots, random_effects)

# # Predicted Marginal Effects
# library(ggeffects)
# marginal_effects <- ggpredict(model, terms = "recommender")
# plot(marginal_effects)
# rm(marginal_effects)

# assess random effects (variability) of case_id for each recommender >> no significant difference between recommenders
model_with_slopes <- lmer(f1_score ~ recommender + (1 + recommender | case_id) ,#+ (1 | thread_id),
                          data = evaluation_thread_level %>% filter(test_type == "combined"))
summary(model_with_slopes)
confint(model_with_slopes, method = "boot")

model_without_slopes <- lmer(f1_score ~ recommender + 
                               (1 | case_id), 
                             data = evaluation_thread_level %>% filter(test_type == "combined"))
anova(model_with_slopes, model_without_slopes)
# Interpretation >> no significant difference between recommenders
rm(model_with_slopes, model_without_slopes)

# Aggregated Evaluation: Case-level & Recommender-level ####
evaluation_case_level <- evaluation_thread_level %>% group_by(case_id, recommender, test_type) %>%
  summarise(median_precision = median(precision),
            mean_precision = mean(precision),
            sd_precision = sd(precision),
            median_recall = median(recall),
            mean_recall = mean(recall),
            sd_recall = sd(recall),
            median_f1 = median(f1_score),
            mean_f1 = mean(f1_score),
            sd_f1 = sd(f1_score),
            median_accuracy = median(accuracy),
            .groups = "drop")
intra_case_variability <- evaluation_thread_level %>% filter(test_type == "combined") %>%
  select(case_id, recommender, f1_score) %>%
  group_by(case_id, recommender) %>%
  summarise(min_f1 = min(f1_score),
            P25_f1 = quantile(f1_score, probs=0.25, type=6, na.rm=T),                      
            median_f1 = quantile(f1_score, probs=0.5, type=6, na.rm=T), 
            P75_f1 = quantile(f1_score, probs=0.75, type=6, na.rm=T),
            max_f1 = max(f1_score),
            mean_f1 = mean(f1_score),
            sd_f1 = sd(f1_score),
            .groups = "drop")

evaluation_recommender_level <- evaluation_case_level %>% group_by(recommender, test_type) %>%
  summarise(mean_precision_gold = mean(median_precision),
            mean_recall_gold = mean(median_recall),
            mean_f1_gold = mean(median_f1),
            mean_accuracy_gold = mean(median_accuracy),
            sd_precision = sd(median_precision),
            sd_recall = sd(median_recall),
            sd_f1 = sd(median_f1),
            .groups = "drop")
#write.csv(evaluation_case_level, "Results Data/evaluation_case_level.csv", row.names = FALSE)
#write.csv(intra_case_variability, "Results Data/intra_case_variability.csv", row.names = FALSE)
#write.csv(evaluation_recommender_level, "Results Data/evaluation_recommender_level.csv", row.names = FALSE)

# Review Different Evaluation Categories ####
evaluate_list <- function(df, test_category, test_column, evaluation_category) {
  df %>%
    filter(test_type == test_category) %>%
    mutate(test = as.character({{ test_column }})) %>%
    mutate(test = gsub("^c\\(|\\)$", "", test)) %>% # Remove "c(" at start and ")" at end
    separate_rows(test, sep = ", ") %>%
    mutate(test = as.numeric(test)) %>% # Convert to numeric where applicable
    filter(!is.na(test)) %>% # Remove NA values
    group_by(case_id, test, recommender) %>%
    summarize(freq = n(), .groups = "drop") %>%
    pivot_wider(names_from = recommender, values_from = freq, values_fill = list(freq = 0)) %>%
    mutate(total_freq = rowSums(select(., starts_with("cot"), starts_with("zeroshot")), na.rm = TRUE)) %>%
    mutate(evaluation = evaluation_category) 
}
# Reviewing evaluation of Gold Standard Orders (TP & FN)
TP_list <- evaluate_list(evaluation_thread_level, "combined", TP_list, "TP")
FN_list <- evaluate_list(evaluation_thread_level, "combined", FN_list, "FN")
TP_and_FN <- rbind(TP_list,
                   FN_list %>% filter(total_freq == 20))
#TP_and_FN <- TP_and_FN %>% left_join(FN_list, by=c("case_id", "test"))

# Add information about correct categorization to TP tests
TP_list_essential <- evaluate_list(evaluation_thread_level, "essential", TP_list, "TP") 
TP_list_conditional <- evaluate_list(evaluation_thread_level, "conditional", TP_list, "TP") 
TP_list_category <- rbind(TP_list_essential, TP_list_conditional) %>% select(-evaluation)
names(TP_list_category)[3:5] <- c("zero_shot_right_category", "cot_right_category", "total_freq_right_category")
TP_and_FN <- TP_and_FN %>% left_join(TP_list_category, by = c("case_id", "test"))
rm(TP_list, TP_list_category, TP_list_essential, TP_list_conditional)

# Add information about gold standard orders
panel_codes <- read.csv("intermediary_data/panel codes.csv")
TP_and_FN <- TP_and_FN %>% left_join(panel_codes, by="test")
TP_and_FN$test <- ifelse(!is.na(TP_and_FN$test_code), TP_and_FN$test_code, TP_and_FN$test)
TP_and_FN <- TP_and_FN %>% select(-test_code) %>% distinct_all()
TP_and_FN <- TP_and_FN %>% left_join(gold_standard_orders, by = c("case_id", "test"="test_code"))
TP_and_FN <- TP_and_FN %>% pivot_wider(names_from = evaluation, values_from = c(zeroshot, cot, total_freq))
TP_and_FN$zeroshot_FN <- ifelse(is.na(TP_and_FN$zeroshot_FN), (10 - TP_and_FN$zeroshot_TP), TP_and_FN$zeroshot_FN)
TP_and_FN$zeroshot_TP <- ifelse(is.na(TP_and_FN$zeroshot_TP), (10 - TP_and_FN$zeroshot_FN), TP_and_FN$zeroshot_TP)
TP_and_FN$cot_FN <- ifelse(is.na(TP_and_FN$cot_FN), (10 - TP_and_FN$cot_TP), TP_and_FN$cot_FN)
TP_and_FN$cot_TP <- ifelse(is.na(TP_and_FN$cot_TP), (10 - TP_and_FN$cot_FN), TP_and_FN$cot_TP)
TP_and_FN$total_freq_FN <- ifelse(is.na(TP_and_FN$total_freq_FN), (10 - TP_and_FN$total_freq_TP), TP_and_FN$total_freq_FN)
TP_and_FN$total_freq_TP <- ifelse(is.na(TP_and_FN$total_freq_TP), (20 - TP_and_FN$total_freq_FN), TP_and_FN$total_freq_TP)
TP_and_FN$intersection_TP <- pmin(TP_and_FN$zeroshot_TP, TP_and_FN$cot_TP)
#write.csv(TP_and_FN, "Results Data/TP_and_FN_data.csv", row.names = FALSE, na="")

#### Reviewing FP tests (against the first Delphi round) ####
FP_list <- evaluate_list(evaluation_thread_level, "combined", FP_list, "FP")
FP_list$intersection_FP <- pmin(FP_list$zeroshot, FP_list$cot)
uzl_testmenu <- read.csv("ALL_uzl_menu.csv")
FP_list <- FP_list %>% left_join(panel_codes, by="test")
FP_list$test <- ifelse(!is.na(FP_list$test_code), FP_list$test_code, FP_list$test)
FP_list <- FP_list %>% select(-test_code) %>% distinct_all()
FP_list <- FP_list %>% left_join(uzl_testmenu, by = c("test"="test_code"))

first_delphi_lists <- first_delphi_round %>%
  select(case_id, test_code, test_type) %>% 
  group_by_all() %>% summarize(n = n(), .groups = "drop") %>%
  pivot_wider(names_from = test_type, values_from = n)
FP_list <- FP_list %>% left_join(first_delphi_lists, by=c("case_id", "test"="test_code"))
#write.csv(FP_list, "Results Data/FP_tests.csv", row.names = FALSE, na="")

# Generate the Venn (or Euler) diagram ####
# Define the areas and intersections
fit <- eulerr::euler(c(
  "Gold Standard Orders" = length(TP_and_FN$test) * 10,                 # Area 1
  "Zeroshot Recommender" = sum(TP_and_FN$zeroshot_TP) + sum(FP_list$zeroshot), # Area 2
  "CoT Recommender" = sum(TP_and_FN$cot_TP) + sum(FP_list$cot), # Area 3
  "Gold Standard Orders&Zeroshot Recommender" = sum(TP_and_FN$zeroshot_TP),    # Intersection of 1 and 2
  "Gold Standard Orders&CoT Recommender" = sum(TP_and_FN$cot_TP),             # Intersection of 1 and 3
  "Zeroshot Recommender&CoT Recommender" = sum(TP_and_FN$intersection_TP) + sum(FP_list$intersection_FP), # Intersection of 2 and 3
  "Gold Standard Orders&Zeroshot Recommender&CoT Recommender" = sum(TP_and_FN$intersection_TP) # Intersection of all 3
))
# Plot the proportional Venn diagram
p <- plot(fit, 
     fills = c("green", "#94A298", "#4C80A3"), 
     alpha = 0.5, 
     labels = TRUE, # Show labels for areas and intersections
     adjust_labels = TRUE, # Adjust label positioning
     rotation = 1.5) # Rotate diagram for better visualization
ggsave(filename = "figures/Venn diagram.svg", plot = p, device = "svg")

p <- venn.plot <- VennDiagram::draw.triple.venn(
  area1 = length(TP_and_FN$test) * 10,       # Size of set1: Gold Standard Orders
  area2 = sum(TP_and_FN$zeroshot_TP) + sum(FP_list$zeroshot),       # Size of set2: Zeroshot Recommender
  area3 = sum(TP_and_FN$cot_TP) + sum(FP_list$cot),    # Size of set3: CoT Recommender
  n12 = sum(TP_and_FN$zeroshot_TP),       # Intersection of Set 1 and Set 2
  n13 = sum(TP_and_FN$cot_TP),       # Intersection of Set 1 and Set 3
  n23 = sum(TP_and_FN$intersection_TP) + sum(FP_list$intersection_FP),       # Intersection of Set 2 and Set 3
  n123 = sum(TP_and_FN$intersection_TP),      # Intersection of Sets 1, 2, and 3
  category = c("Gold Standard Orders", "Zeroshot Recommender", "CoT Recommender"), # Labels
  fill = c("#4C80A3", "red", "green"),       # Colors
  lty = "blank",                             # No line type
  alpha = 0.5                                # Transparency
)
ggsave(filename = "figures/Venn diagram numbers.svg", plot = p, device = "svg")
