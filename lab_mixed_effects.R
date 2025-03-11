setwd('/Users/castilv/Documents/Cachexia/cac_data/')
library(ggplot2)
library(data.table)
library(dtwclust)
library(ggrepel)
library(ggsignif)
library(tidyverse)
library(lme4)
library(dplyr)
library(tidyr)
library(broom)
library(Matrix)

spans_labtests= fread('../cac_data/spans_labtests_0307.csv')

spans_labtests <- spans_labtests %>%mutate(MRN = as.factor(MRN),span = as.factor(span), GENDER = as.factor(GENDER),CANCER_TYPE_DETAILED = as.factor(CANCER_TYPE_DETAILED))
spans_labtests <- spans_labtests %>%arrange(MRN, start_day) %>% group_by(MRN) %>% mutate(SpanID = cumsum(!duplicated(start_day))) 

labs2use <- c('HGB', 'HCT', 'RBC', 'MCV', 'MCH', 'WBC', 'Platelets', 'MCHC', 'RDW', 'Neut', 
              'Creatinine', 'CO2', 'Glucose', 'Sodium', 'Chloride', 'BUN', 'Calcium', 'Potassium', 
              'Lymph', 'Mono', 'Eos', 'Baso', 'Albumin', 'ALK', 'ALT', 'Bilirubin, Total', 
              'Protein, Total', 'AST', 'Nucleated RBC', 'Immature Granulocyte')

ccounts <- spans_labtests %>%
  select(MRN, CANCER_TYPE_DETAILED) %>%
  distinct() %>%
  group_by(CANCER_TYPE_DETAILED) %>%
  summarise(count = n(), .groups = 'drop') %>%
  arrange(desc(count))

c2use <- ccounts %>% filter(count >= 500)

length(unique(spans_labtests$MRN))
spans_labtests <- spans_labtests %>%filter(CANCER_TYPE_DETAILED %in% c2use$CANCER_TYPE_DETAILED)
length(unique(spans_labtests$MRN))
spans_labtests[names(spans_labtests) %in% labs2use] <- lapply(spans_labtests[names(spans_labtests) %in% labs2use], function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
})
results_cc <- list()

for (cancer_type in c2use$CANCER_TYPE_DETAILED) {
  results <- data.frame(test = character(), cancer_type = character(), 
                        estimate = numeric(),logor=numeric(), lower_ci = numeric(), 
                        upper_ci = numeric(), p = numeric(), stringsAsFactors = FALSE)
  
  for(test_name in labs2use) {
    print(paste(cancer_type, test_name, sep=": ")) 
    
    current_data <- spans_labtests %>% 
      filter(CANCER_TYPE_DETAILED == cancer_type) %>%  
      filter(!is.na(.data[[test_name]])) %>%  
      mutate(result = as.numeric(.data[[test_name]])) 
    
    if (nrow(current_data) > 1) {
      unique_sex <- length(unique(current_data$GENDER))
      
      if (unique_sex > 1) {
        formula <- as.formula(paste("span ~ result + GENDER + (1 | MRN)"))
      } else {
        formula <- as.formula(paste("span ~ result  + (1 | MRN)"))
      }
      
      model <- tryCatch({
        glmer(formula, data = current_data, family = binomial(link = "logit"))
      }, error = function(e) {
        print(paste("Error:", cancer_type, ":", test_name))
        return(NULL)
      })
      
      if (!is.null(model) && !isSingular(model, tol = 1e-5)) {
        coef_summary <- summary(model)$coefficients
        log_or <- coef_summary["result", "Estimate"]
        se_log_or <- coef_summary["result", "Std. Error"]
        
        odds_ratio <- exp(log_or)
        lower_ci <- exp(log_or - 1.96 * se_log_or)
        upper_ci <- exp(log_or + 1.96 * se_log_or)
        p_value <- coef_summary["result", "Pr(>|z|)"]
        
        print(paste("Results for", test_name, "in", cancer_type, "Log_OR", log_or))
        
        
        results <- rbind(results, data.frame(test = test_name, cancer_type = cancer_type, 
                                             estimate = odds_ratio, logor=log_or, lower_ci = lower_ci, 
                                             upper_ci = upper_ci, p = p_value))
      } else {
        print(paste("Singular fit or model fitting issue for", cancer_type, ":", test_name))
      }
    }
  }
  
  results_cc[[cancer_type]] <- results
}

results <- do.call(rbind, results_cc)
results <- results %>% mutate(index = row_number())
results$log_or <- log(results$estimate)
write.csv(results, "glmm_results_labs_0307.csv", row.names = FALSE)

results= fread('../cac_data/glmm_results_labs_0307.csv')
results <- results %>%
  mutate(cancer_type = str_replace(cancer_type, "Chronic Lymphocytic Leukemia.*", "CLL/SLL"))

log_or_limits <- c(-1, 0.7)
results$log_or_capped <- pmax(pmin(results$log_or, log_or_limits[2]), log_or_limits[1])
results$adjusted_p <- p.adjust(results$p, method = "BH")

adjusted_p_value_threshold <- 0.05
results <- results %>%
  mutate(test = fct_relevel(test, sort(unique(test))))

p <- ggplot(results, aes(x = test, y = fct_rev(cancer_type))) +  # Swapped axes
  geom_point(aes(size = abs(log_or_capped), color = log_or_capped)) +
  geom_point(data = results[results$adjusted_p < adjusted_p_value_threshold, ],
             aes(x = test, y = fct_rev(cancer_type), size = abs(log_or_capped)),
             shape = 21, color = "black", stroke = 0.5) +  
  scale_size_continuous(range = c(1, 10)) +
  scale_color_gradient2(low = "#2c7bb6", mid = "white", high = "#d7191c", midpoint = 0, limits = log_or_limits) +
  labs(
    x = "Serological Test",  # Renamed
    y = "Cancer Type",
    color = "Log Odds Ratio"
  ) +  
  guides(size = "none") +  
  theme_minimal() +  
  theme(
    axis.title.x = element_text(size = 16, face = "bold"),  
    axis.title.y = element_text(size = 16, face = "bold"), 
    axis.text.x = element_text(angle = 90, hjust = 1, size = 12),  
    axis.text.y = element_text(size = 12),  
    axis.line.x = element_line(color = "black"),  
    axis.line.y = element_line(color = "black"),  
    panel.grid = element_blank(),  # Removed the grid  
    legend.position = "right"
  )

ggsave(file = "EpsMXFX_labvalues_0307.pdf", p, width = 14, height = 12)

library(ggplot2)
library(dplyr)
library(patchwork)  
log_or_limits <- c(-1.2, 0.7)
results <- results %>%
  mutate(test = factor(test, levels = unique(results$test)))

results$log_or_capped <- pmax(pmin(results$log_or, log_or_limits[2]), log_or_limits[1])
summary_results <- summary_results %>%
  mutate(test = factor(test, levels = levels(results$test)))  # Match order
summary_results$mean_log_or <- pmax(pmin(summary_results$mean_log_or, log_or_limits[2]), log_or_limits[1])


heatmap_plot <- ggplot(results, aes(x = test, y = fct_rev(cancer_type))) +
  geom_point(aes(size = abs(log_or_capped), color = log_or_capped)) +
  geom_point(data = results[results$adjusted_p < adjusted_p_value_threshold, ],
             aes(x = test, y = fct_rev(cancer_type), size = abs(log_or_capped)),
             shape = 21, color = "black", stroke = 0.5) +
  scale_size_continuous(
    name = "abs(log OR)", 
    breaks = c(0.25, 0.5, 0.75, 0.9),
    labels = c("0.25", "0.50", "0.75", "> 0.9"),
    range = c(1, 10),
    guide=guide_legend(override.aes = list(color="gray"))) +
  scale_color_gradient2(
    name = "Log OR",  # 🔥 New title for color legend
    low = "#2c7bb6", mid = "white", high = "#d7191c",
    midpoint = 0, limits = log_or_limits
  ) +
  
  labs(x = NULL, y = "Cancer Type") +
  theme_minimal() +
  theme(
    axis.title.y = element_text(size = 16, face = "bold"),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 12),
    panel.grid = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    legend.position = "right"
  )


bar_plot <- ggplot(summary_results, aes(x = test, y = mean_log_or, fill = mean_log_or)) +
  geom_bar(stat = "identity", width = 0.6) +
  scale_fill_gradient2(low = "#2c7bb6", mid = "white", high = "#d7191c", midpoint = 0, limits = log_or_limits) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed", size = 1) +
  labs(x = "Serological Test", y = "Mean Log Odds Ratio") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.title.y = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1, size = 12),
    panel.grid = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    legend.position = "none"
  )

final_plot <- heatmap_plot / bar_plot + plot_layout(heights = c(5, 1)) 
final_plot 
ggsave("Lab_Values_Summary_Heatmap_Aligned.png", final_plot, width = 14, height = 10)


cancer_ranking <- results %>%
  group_by(cancer_type) %>%
  summarise(mean_log_or = mean(abs(log_or), na.rm = TRUE),
            sig_count = sum(adjusted_p < 0.05, na.rm = TRUE)) %>%
  arrange(desc(sig_count), desc(mean_log_or))

test_ranking <- results %>%
  group_by(test) %>%
  summarise(mean_log_or = mean(abs(log_or), na.rm = TRUE),
            sig_count = sum(adjusted_p < 0.05, na.rm = TRUE)) %>%
  arrange(desc(sig_count), desc(mean_log_or))

results$cancer_type <- factor(results$cancer_type, levels = cancer_ranking$cancer_type)
results$test <- factor(results$test, levels = test_ranking$test)

p <- ggplot(results, aes(x = cancer_type, y = test)) +
  geom_point(aes(size = abs(log_or), color = log_or)) +
  geom_point(data = results[results$adjusted_p < 0.05, ],
             aes(x = cancer_type, y = test, size = abs(log_or)),
             shape = 21, color = "black", stroke = 0.5) +
  scale_size_continuous(range = c(1, 10)) +
  scale_color_gradient2(low = "#2c7bb6", mid = "white", high = "#d7191c", midpoint = 0) +
  labs(x = "Cancer Type", y = "Test", color = "Log Odds Ratio") +
  guides(size = "none") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("Sorted_EpsMXFX_labvalues_0307.pdf", p, width = 14, height = 12)





lab_categories <- data.frame(
  Lab_Test = c('HGB', 'HCT', 'RBC', 'WBC', 'Platelets', 'MCH', 'MCHC', 'MCV', 'RDW', 
               'Neut', 'Abs Neut', 'Abs Lymph', 'Eos', 'Lymph', 'Mono', 'Baso', 
               'Abs Baso', 'Abs Eos', 'Nucleated RBC', 'Immature Granulocyte', # CBC (20 total)
               
               'Albumin', 'ALK', 'ALT', 'AST', 'Bilirubin, Total', 'Protein, Total', 
               'Creatinine', 'CO2', 'Glucose', 'Sodium', 'Chloride', 'BUN', 'Calcium', 'Potassium'), # CMP (14 total)
  
  Category = c(rep("Complete Blood Count", 20), rep("Comprehensive Metabolic Panel", 14))  # Fixed category length
)

lab_counts <- spans_labtests %>%
  select(MRN, all_of(labs2use)) %>% 
  pivot_longer(cols = -MRN, names_to = "Lab_Test", values_to = "Value") %>%
  filter(!is.na(Value)) %>%
  group_by(Lab_Test) %>%
  summarise(Patient_Count = n_distinct(MRN), .groups = "drop") %>%
  arrange(desc(Patient_Count)) %>%
  left_join(lab_categories, by = "Lab_Test")  


cbc_color <- "gray70" 
cmp_color <- "gray30" 

p <- ggplot(lab_counts, aes(x = reorder(Lab_Test, -Patient_Count), y = Patient_Count, fill = Category)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Category, scales = "free_x", nrow = 1) +  # Separate CBC & CMP
  scale_fill_manual(values = c("Complete Blood Count" = cbc_color, "Comprehensive Metabolic Panel" = cmp_color)) +
  
  labs(x = "Routinely Done Serological Test", 
       y = "# of Patients", 
       title = "Serological Tests") +
  
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5), 
    axis.text.x = element_text(angle = 90, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    strip.text = element_text(face = "bold", size = 12), 
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),
    panel.border = element_blank(), 
    axis.line.x = element_line(color = "black", size = 1), 
    axis.line.y = element_line(color = "black", size = 1),  
    legend.position = "none"
  ) +
  scale_y_continuous(breaks = seq(0, max(lab_counts$Patient_Count, na.rm = TRUE), by = 5000))  # Tick marks every 5000

p
ggsave("MSK_lab_tests.png", p, width = 10, height = 6)