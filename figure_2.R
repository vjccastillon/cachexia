setwd('/Users/castilv/Documents/Cachexia/cac_data/')
library(ggplot2)
library(dtwclust)
library(ggrepel)
library(dplyr)
library(tidyr)
library(pheatmap)
library(data.table)
library(lubridate)
library(grid)
library(broom)

#Figure 2
cac_clin <- fread("../cac_data/processed_cac_clin.csv")
#CAC distribution
cachexia_counts <- table(cac_clin$has_cachexia)
cachexia_proportions <- prop.table(cachexia_counts) * 100

cachexia_data <- data.frame(
  Status = c("No Cachexia", "Cachexia"),
  Proportion = as.numeric(cachexia_proportions)
)

cachexia_plot <- ggplot(cachexia_data, aes(x = Status, y = Proportion, fill = Status)) +
  geom_bar(stat = "identity", width = 0.9) +
  scale_fill_manual(values = c("No Cachexia" = "gray", "Cachexia" = "#66c2a5")) +
  labs(title = "Cachexia Distribution", x = "", y = "Proportion (%)") +
  theme_minimal(base_size = 8) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    axis.ticks = element_line(color = "black", linewidth = 0.4),
    axis.line = element_line(color = "black", linewidth = 0.4),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )

ggsave("cachexia_distribution.pdf", plot = cachexia_plot, width = 2, height = 2, dpi = 300)
print(cachexia_plot)

#CAC by WL
library(dplyr)
library(ggplot2)

file_5 <- "/Users/castilv/Documents/Cachexia/cac_data/processed_episodes/data/processed_data/cachexia_episodes_all_patients_20241111.csv"
file_10 <- "/Users/castilv/Documents/Cachexia/cac_data/processed_episodes/data/processed_data/cachexia_episodes_all_patients_20250220_precomp10.csv"
file_15 <- "/Users/castilv/Documents/Cachexia/cac_data/processed_episodes/data/processed_data/cachexia_episodes_all_patients_20250220_precomp15.csv"

# Read files
eps_5 <- read.csv(file_5)
eps_10 <- read.csv(file_10)
eps_15 <- read.csv(file_15)

cachectic_5 <- eps_5 %>% filter(!is.na(start_day)) %>% distinct(MRN) %>% nrow()
cachectic_10 <- eps_10 %>% filter(!is.na(start_day)) %>% distinct(MRN) %>% nrow()
cachectic_15 <- eps_15 %>% filter(!is.na(start_day)) %>% distinct(MRN) %>% nrow()

total_patients <- eps_5 %>% distinct(MRN) %>% nrow()

weight_loss_summary <- data.frame(
  Weight_Loss_Category = c("≥5%", "≥10%", "≥15%"),
  Count = c(cachectic_5, cachectic_10, cachectic_15),
  Proportion = c(cachectic_5, cachectic_10, cachectic_15) / total_patients
)

weight_loss_summary$Weight_Loss_Category <- factor(
  c(">=5%", ">=10%", ">=15%"),
  levels = c(">=5%", ">=10%", ">=15%")
)

output_file <- "/Users/castilv/Documents/Cachexia/cac_data/cachexia_weight_loss_distribution.pdf"

max_prop <- max(weight_loss_summary$Proportion) * 100 
ggplot(weight_loss_summary, aes(x = Weight_Loss_Category, y = Proportion * 100, fill = Weight_Loss_Category)) +
  geom_bar(stat = "identity", width = 0.8) +
  scale_fill_brewer(palette = "Set2")  +
  scale_y_continuous(limits = c(0, max_prop + 5), expand = c(0, 0)) +
  
  labs(
    x = "Weight Loss Threshold",
    y = "Proportion",
    fill = "WL Class",
    title = "Cachexia by WL tresholds"
  ) +
  theme_minimal(base_size = 8) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_text(size = 8, face = "bold"),
    legend.key.size = unit(0.3, "cm"), 
    legend.background = element_blank(),
    axis.line = element_line(color = "black", size = 0.5),
    plot.title = element_text(hjust = 0.5, size = 8, face = "bold")
  )

ggsave(output_file, width = 2.4, height = 2, dpi = 300)

#OR demographics

uqc_clin <- uqc_cac[CANCER_TYPE_DETAILED %in% uqctypes & ANCESTRY_LABEL != "Ancestry_Unknown"]

ref_gender <- "FEMALE"     
ref_bmi <- "Normal"      
ref_ancestry <- "ADM"     
ref_age <- "<60"

results_list <- list()

for (cancer_type in uqctypes) {
  subset_data <- uqc_clin[CANCER_TYPE_DETAILED == cancer_type]
  
  if (nrow(subset_data) < 2) next
  
  # Gender Model
  if (length(unique(subset_data$GENDER)) > 1) {
    gender_model <- glm(has_cachexia ~ GENDER, data = subset_data, family = binomial)
    gender_results <- tidy(gender_model, exponentiate = TRUE, conf.int = TRUE)
    gender_results$Variable <- "Gender"
  } else {
    gender_results <- NULL
  }
  
  # BMI Model
  if (length(unique(subset_data$bmicat)) > 1) {
    bmi_model <- glm(has_cachexia ~ bmicat, data = subset_data, family = binomial)
    bmi_results <- tidy(bmi_model, exponentiate = TRUE, conf.int = TRUE)
    bmi_results$Variable <- "BMI"
  } else {
    bmi_results <- NULL
  }
  
  # Ancestry Model
  if (length(unique(subset_data$ANCESTRY_LABEL)) > 1) {
    ancestry_model <- glm(has_cachexia ~ ANCESTRY_LABEL, data = subset_data, family = binomial)
    ancestry_results <- tidy(ancestry_model, exponentiate = TRUE, conf.int = TRUE)
    ancestry_results$Variable <- "Ancestry"
  } else {
    ancestry_results <- NULL
  }
  
  if (length(unique(subset_data$age)) > 1) {
    age_model <- glm(has_cachexia ~ age, data = subset_data, family = binomial)
    age_results <- tidy(age_model, exponentiate = TRUE, conf.int = TRUE)
    age_results$Variable <- "Age"
  } else {
    age_results <- NULL
  }
  
  combined_results <- rbindlist(list(gender_results, bmi_results, ancestry_results, age_results), fill = TRUE)
  combined_results$Cancer_Type <- cancer_type
  
  results_list[[cancer_type]] <- combined_results
}

or_results <- rbindlist(results_list, fill = TRUE)

or_results$term <- gsub("GENDER", "Gender_", or_results$term)
or_results$term <- gsub("bmicat", "BMI_", or_results$term)
or_results$term <- gsub("ANCESTRY_LABEL", "Ancestry_", or_results$term)
or_results$term <- gsub("age<60", "Age_<60", or_results$term)
or_results$term <- gsub("age≥60", "Age_GE60", or_results$term)


or_results$adjusted_p <- p.adjust(or_results$p.value, method = "BH")
or_results$significance <- ifelse(or_results$adjusted_p < 0.05, "*", "")


or_results <- or_results[term != "(Intercept)", ]
or_matrix <- dcast(or_results, term ~ Cancer_Type, value.var = "estimate")
or_matrix <- as.data.frame(or_matrix)
rownames(or_matrix) <- or_matrix$term
or_matrix <- as.matrix(or_matrix[, -1, drop = FALSE])
or_matrix[is.na(or_matrix)] <- 1
log2_or_matrix <- log2(or_matrix)


significance_matrix <- dcast(or_results, term ~ Cancer_Type, value.var = "significance")
significance_matrix <- as.data.frame(significance_matrix)
rownames(significance_matrix) <- significance_matrix$term
significance_matrix <- as.matrix(significance_matrix[, -1, drop = FALSE])
significance_matrix[is.na(significance_matrix)] <- ""

row_names <- rownames(log2_or_matrix)


row_annotation <- data.frame(
  Category = ifelse(grepl("^Gender_", row_names), "Gender",
                    ifelse(grepl("^Age_", row_names), "Age",
                           ifelse(grepl("^Ancestry_", row_names), "Ancestry", "BMI")))
)

rownames(row_annotation) <- row_names
category_order <- c("Gender", "Age", "Ancestry", "BMI")

row_annotation <- row_annotation[order(match(row_annotation$Category, category_order)), , drop = FALSE]
log2_or_matrix <- log2_or_matrix[rownames(row_annotation), ]
row_annotation$Row <- rownames(row_annotation)

log2_or_matrix <- log2_or_matrix[!grepl("Ancestry_Unknown", rownames(log2_or_matrix)), ]
row_annotation <- row_annotation[!grepl("Ancestry_Unknown", row_annotation$Row), ]

cachexia_rates <- uqc_clin[, .(Cachexia_Rate = mean(has_cachexia, na.rm = TRUE)), by = CANCER_TYPE_DETAILED]
cachexia_rates <- cachexia_rates[order(-Cachexia_Rate)]
log2_or_matrix <- log2_or_matrix[, cachexia_rates$CANCER_TYPE_DETAILED, drop = FALSE]

significance_matrix <- significance_matrix[rownames(log2_or_matrix), colnames(log2_or_matrix), drop = FALSE]

cachexia_rates <- cachexia_rates[match(colnames(log2_or_matrix), cachexia_rates$CANCER_TYPE_DETAILED), ]

mean_or <- data.table(
  Row = rownames(log2_or_matrix),
  Mean_OR = rowMeans(log2_or_matrix, na.rm = TRUE)
)
mean_or <- mean_or[match(rownames(log2_or_matrix), mean_or$Row), ]

significance_matrix <- significance_matrix[rownames(log2_or_matrix), colnames(log2_or_matrix), drop = FALSE]
cachexia_barplot <- HeatmapAnnotation(
  Cachexia_Rate = anno_barplot(
    cachexia_rates$Cachexia_Rate,
    bar_width = 0.9,
    border = FALSE,
    #gp = gpar(fill = "#DDCC77", col = NA),
    gp = gpar(fill = "#66c2a5", col = NA),
    height = unit(2, "cm"),
    axis_param = list(
      at = seq(0, max(cachexia_rates$Cachexia_Rate, na.rm = TRUE), by = 0.10),
      labels_rot = 0
    )
  ),
  annotation_name_side = "left",
  annotation_name_gp = gpar(fontsize =8),
  show_annotation_name = FALSE
)
or_heatmap <- Heatmap(
  log2_or_matrix,
  name = "Log2(OR)",
  cluster_rows = FALSE,
  cluster_columns = FALSE,
  row_split = row_annotation$Category,  
  top_annotation = cachexia_barplot,  
  col = colorRamp2(c(-1, 0, 1), c("#88CCEE", "white", "#CC6677")),  
  row_names_side = "left",
  column_names_side = "bottom",
  row_names_gp = gpar(fontsize = 8),
  column_names_gp = gpar(fontsize =8),
  heatmap_legend_param = list(
    title = "Log2(OR)",
    legend_direction = "horizontal",
    legend_title_gp = gpar(fontsize = 8),
    legend_labels_gp = gpar(fontsize = 8)
  ),
  cell_fun = function(j, i, x, y, width, height, fill) {
    if (significance_matrix[i, j] == "*") {
      grid.text("*", x, y, gp = gpar(fontsize = 8))
    }
  }
)


pdf("CAC_OR_plot.pdf", width = 6.5, height =6.5)
draw(or_heatmap, heatmap_legend_side = "bottom")
dev.off()