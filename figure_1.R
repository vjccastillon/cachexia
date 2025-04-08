setwd('/Users/castilv/Documents/Cachexia/cac_data/')
library(ggplot2)
library(dtwclust)
library(dplyr)
library(tidyr)
library(scales)
library(data.table)

#Figure 1
cac_clin <- fread("../cac_data/processed_cac_clin.csv")
#GENDER
library(ggplot2)
library(scales)

gender_data <- cac_clin[!is.na(cac_clin$GENDER) & nzchar(trimws(cac_clin$GENDER)), ]  
gender_counts <- table(gender_data$GENDER)  
proportions <- prop.table(gender_counts) * 100  

gender_plot_data <- data.frame(
  Gender = names(proportions),
  Proportion = as.numeric(proportions)
)

gender_plot_data <- gender_plot_data[order(-gender_plot_data$Proportion),]
gender_plot_data$Gender <- factor(gender_plot_data$Gender, levels = gender_plot_data$Gender)

gender_colors <- c("MALE" = "#ccebc5", "FEMALE" = "#bc80bd")

gender_plot <- ggplot(gender_plot_data, aes(x = "", y = Proportion, fill = Gender)) +
  geom_bar(stat = "identity", position = "stack") +
  coord_flip() +
  scale_y_continuous(labels = percent_format(scale = 1, suffix = ""), expand = c(0, 0), limits = c(0, 100)) +
  scale_fill_manual(name = "Sex", values = gender_colors) +
  labs(title = "Sex Distribution", x = "", y = "") +
  theme_minimal(base_size = 8) +  # globally smaller font size
  theme(
    plot.title = element_text(hjust = 0.5, size = 8, face = "bold"),
    axis.text.x = element_text(size = 6),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.x = element_line(color = "black", size = 0.4),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.text = element_text(size = 6),
    legend.margin = margin(t = 0, b = 0, unit = "pt"),
    plot.margin = unit(c(1, 1, 0.5, 1), "lines")
  ) +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE))

ggsave("cac_gender_cohort.pdf", plot = gender_plot, width = 3, height = 1.6, units = "in", dpi = 300)

print(gender_plot)

#ANCESTRY

anc <- cac_clin[!is.na(cac_clin$ANCESTRY_LABEL) & nzchar(trimws(cac_clin$ANCESTRY_LABEL)), ]  
ancestry_counts <- table(anc$ANCESTRY_LABEL)  
proportions <- prop.table(ancestry_counts) * 100  

ancestry_data <- data.frame(
  Ancestry = names(proportions),
  Proportion = as.numeric(proportions)
)

ancestry_data <- ancestry_data[order(ancestry_data$Proportion),]
ancestry_data$Ancestry <- factor(ancestry_data$Ancestry, levels = ancestry_data$Ancestry)

ancestry_colors <- colorRampPalette(c("#ffffb3", "#fdb462"))(nrow(ancestry_data))

ancestry_plot <- ggplot(ancestry_data, aes(x = "", y = Proportion, fill = Ancestry)) +
  geom_bar(stat = "identity", position = "stack") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(scale = 1, suffix = ""), 
                     expand = c(0, 0), limits = c(0, 102)) +
  scale_fill_manual(name = "Ancestry", values = ancestry_colors) +
  labs(title = "Ancestry Distribution", x = "", y = "") +
  theme_minimal(base_size = 8) +  # globally smaller font
  theme(
    plot.title = element_text(hjust = 0.5, size = 9, face = "bold"),
    axis.text.x = element_text(size = 7),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.x = element_line(color = "black", size = 0.4),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.text = element_text(size = 6),
    legend.margin = margin(t = 0, b = 0, unit = "pt"),
    plot.margin = unit(c(1, 1, 0.5, 1), "lines")
  ) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))  # Adjust rows if needed

ggsave("cac_ancestry_cohort.pdf", plot = ancestry_plot, width = 3.4, height = 2, dpi = 300, units = "in")

print(ancestry_plot)


#cancer_type
cancer_data <- cac_clin[!is.na(cac_clin$CANCER_TYPE_DETAILED) & nzchar(trimws(cac_clin$CANCER_TYPE_DETAILED)), ]
cancer_counts <- table(cancer_data$CANCER_TYPE_DETAILED)
top_cancer_counts <- sort(cancer_counts, decreasing = TRUE)[1:20]

total_counts <- sum(top_cancer_counts)
proportions <- 100 * top_cancer_counts / total_counts

cancer_plot_data <- data.frame(
  CancerType = names(proportions),
  Proportion = as.numeric(proportions)
)

cancer_plot_data$CancerType <- as.character(cancer_plot_data$CancerType)
cancer_plot_data$CancerType[cancer_plot_data$CancerType == 
                              "Chronic Lymphocytic Leukemia/Small Lymphocytic Lymphoma"] <- "CLL/SLL"

cancer_plot_data <- cancer_plot_data[order(-cancer_plot_data$Proportion),]
cancer_plot_data$CancerType <- factor(cancer_plot_data$CancerType, levels = cancer_plot_data$CancerType)

cancer_plot <- ggplot(cancer_plot_data, aes(x = CancerType, y = Proportion, fill = CancerType)) +
  geom_bar(stat = "identity", color = "#4DB6AC", fill = "#4DB6AC") +
  coord_flip() +
  scale_y_continuous(expand = c(0, 0), limits = c(0, max(cancer_plot_data$Proportion) + 5)) +
  labs(title = "Cancer Type Distribution", x = "", y = "Proportion (%)") +
  theme_minimal(base_size = 6) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 6, face = "bold"),
    axis.text.x = element_text(size = 6),
    axis.text.y = element_text(size = 6),
    axis.ticks.y = element_blank(),
    axis.line.x = element_line(color = "black", size = 0.4),  # Add this back
    axis.line.y = element_line(color = "black", size = 0.4),  # Keep this too
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )

ggsave("cac_ctype_distribution.pdf", plot = cancer_plot, width = 2.6, height =3.4, dpi = 300)
print(cancer_plot)