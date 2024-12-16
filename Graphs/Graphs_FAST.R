library(tidyverse)
library(stargazer)
library(patchwork)
library(plotrix)
library(ggplot2)
library(lme4)
library(nlme)
library(ggplot2)
library(dplyr)
library(ggcorrplot)
library(ggplot2)
library(ggExtra)
library(ggpubr)
library(gridExtra)
library(dplyr)


##FIGURE 3
data <-subset(clinicalmeasures, select = c(Avg_Effort, Avg_Pathology, PANAS_n, PSS, RRS, PSWQ))

scatter_plot <- ggplot(data, aes(x = Avg_Effort, y = PANAS_n)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(x = "Average Valence", y = "PANAS negative") +
  theme_minimal()

marginal_plots_one <- ggMarginal(scatter_plot, type = "histogram")

filtered_data <- data %>% dplyr::filter(PSS <= 40)

scatter_plot <- ggplot(filtered_data, aes(x = Avg_Effort, y = PSS)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(x = "Average Valence", y = "PSS") +
  theme_minimal()

marginal_plots_two <- ggMarginal(scatter_plot, type = "histogram")

scatter_plot <- ggplot(data, aes(x = Avg_Effort, y = RRS)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(x = "Average Valence", y = "RRS") +
  theme_minimal()

marginal_plots_three <- ggMarginal(scatter_plot, type = "histogram")

scatter_plot <- ggplot(data, aes(x = Avg_Effort, y = PSWQ)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(x = "Average Valence", y = "PSWQ") +
  theme_minimal()

marginal_plots_four <- ggMarginal(scatter_plot, type = "histogram")

scatter_plot <- ggplot(data, aes(x = Avg_Pathology, y = PANAS_n)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(x = "Average Perceived Overthinking", y = "PANAS negative") +
  theme_minimal()

marginal_plots_five <- ggMarginal(scatter_plot, type = "histogram")

scatter_plot <- ggplot(filtered_data, aes(x = Avg_Pathology, y = PSS)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(x = "Average Perceived Overthinking", y = "PSS") +
  theme_minimal()

marginal_plots_six <- ggMarginal(scatter_plot, type = "histogram")

scatter_plot <- ggplot(data, aes(x = Avg_Pathology, y = RRS)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(x = "Average Perceived Overthinking", y = "RRS") +
  theme_minimal()

marginal_plots_seven <- ggMarginal(scatter_plot, type = "histogram")

scatter_plot <- ggplot(data, aes(x = Avg_Pathology, y = PSWQ)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(x = "Average Perceived Overthinking", y = "PSWQ") +
  theme_minimal()

marginal_plots_eight <- ggMarginal(scatter_plot, type = "histogram")


grid.arrange(marginal_plots_one, marginal_plots_two, marginal_plots_three, marginal_plots_four, marginal_plots_five, marginal_plots_six, marginal_plots_seven, marginal_plots_eight, ncol = 4)



##FIGURE 4

ggplot(summary_data, aes(x = cluster, y = mean_value, fill = as.factor(group))) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_point(data = bargraph_data, aes(x = cluster, y = Slope, color = as.factor(group)),
             position = position_jitterdodge(dodge.width = 0.9), size = 0.5, alpha = 0.2) +
  geom_errorbar(aes(ymin = mean_value - sem_value, ymax = mean_value + sem_value),
                position = position_dodge(width = 0.9), width = 0.25) +
  scale_color_manual(values = darker_point_colors)+
  labs(x = "Clusters", y = "Standardized Adjustment Coefficient", title = "Adjustment coefficient") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 20),  # Increase x-axis label size
        axis.text.y = element_text(size = 20),  # Increase y-axis label size
        axis.title = element_text(size = 20),  # Increase axis title size
        legend.title = element_text(size = 20),  # Increase legend title size
        legend.text = element_text(size = 20)) + labs(fill = "Sample") # Rotate x-axis labels for better visibility


summary_data <- bargraph_data %>%
  group_by(group, cluster) %>%
  summarise(mean_value = mean(Kernel_Density_TWA),
            sem_value = sd(Kernel_Density_TWA) / sqrt(n()))

ggplot(summary_data, aes(x = cluster, y = mean_value, fill = as.factor(group))) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_point(data = bargraph_data, aes(x = cluster, y = Kernel_Density_TWA, color = as.factor(group)),
             position = position_jitterdodge(dodge.width = 0.9), size = 0.5, alpha = 0.2) +
  geom_errorbar(aes(ymin = mean_value - sem_value, ymax = mean_value + sem_value),
                position = position_dodge(width = 0.9), width = 0.25) +
  scale_color_manual(values = darker_point_colors)+
  labs(x = "Clusters", y = "Standardized Kernel Density values", title = "Kernel Density at attractor location") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 20),  # Increase x-axis label size
        axis.text.y = element_text(size = 20),  # Increase y-axis label size
        axis.title = element_text(size = 20),  # Increase axis title size
        legend.title = element_text(size = 20),  # Increase legend title size
        legend.text = element_text(size = 20)) + labs(fill = "Sample") + ylim(-1, 5)# Rotate x-axis labels for better visibility


summary_data <- bargraph_data %>%
  group_by(group, cluster) %>%
  summarise(mean_value = mean(Min_Distance_to_Location_Chain),
            sem_value = sd(Min_Distance_to_Location_Chain) / sqrt(n()))

ggplot(summary_data, aes(x = cluster, y = mean_value, fill = as.factor(group))) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_point(data = bargraph_data, aes(x = cluster, y = Min_Distance_to_Location_Chain, color = as.factor(group)),
             position = position_jitterdodge(dodge.width = 0.9), size = 0.5, alpha = 0.2) +
  geom_errorbar(aes(ymin = mean_value - sem_value, ymax = mean_value + sem_value),
                position = position_dodge(width = 0.9), width = 0.25) +
  scale_color_manual(values = darker_point_colors)+
  labs(x = "Clusters", y = "Standardized Distance to Attractor", title = "Final distance to attractor location") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 20),  # Increase x-axis label size
        axis.text.y = element_text(size = 20),  # Increase y-axis label size
        axis.title = element_text(size = 20),  # Increase axis title size
        legend.title = element_text(size = 20),  # Increase legend title size
        legend.text = element_text(size = 20)) + labs(fill = "Sample") # Rotate x-axis labels for better visibility


summary_data <- bargraph_data %>%
  group_by(group, cluster) %>%
  summarise(mean_value = mean(Travel_to_Enjoyment),
            sem_value = sd(Travel_to_Enjoyment) / sqrt(n()))

ggplot(summary_data, aes(x = cluster, y = mean_value, fill = as.factor(group))) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_point(data = bargraph_data, aes(x = cluster, y = Travel_to_Enjoyment, color = as.factor(group)),
             position = position_jitterdodge(dodge.width = 0.9), size = 0.5, alpha = 0.2) +
  geom_errorbar(aes(ymin = mean_value - sem_value, ymax = mean_value + sem_value),
                position = position_dodge(width = 0.9), width = 0.25) +
  scale_color_manual(values = darker_point_colors)+
  labs(x = "Clusters", y = "Standardized Valence at Attractor", title = "Valence at attractor location") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 20),  # Increase x-axis label size
        axis.text.y = element_text(size = 20),  # Increase y-axis label size
        axis.title = element_text(size = 20),  # Increase axis title size
        legend.title = element_text(size = 20),  # Increase legend title size
        legend.text = element_text(size = 20)) + labs(fill = "Sample") # Rotate x-axis labels for better visibility


summary_data <- bargraph_data %>%
  group_by(group, cluster) %>%
  summarise(mean_value = mean(Travel_to_Pathology),
            sem_value = sd(Travel_to_Pathology) / sqrt(n()))

ggplot(summary_data, aes(x = cluster, y = mean_value, fill = as.factor(group))) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_point(data = bargraph_data, aes(x = cluster, y = Travel_to_Pathology, color = as.factor(group)),
             position = position_jitterdodge(dodge.width = 0.9), size = 0.5, alpha = 0.2) +
  geom_errorbar(aes(ymin = mean_value - sem_value, ymax = mean_value + sem_value),
                position = position_dodge(width = 0.9), width = 0.25) +
  scale_color_manual(values = darker_point_colors)+
  labs(x = "Clusters", y = "Standardized Pathology at Attractor", title = "Perceived Pathology at attractor location") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 20),  # Increase x-axis label size
        axis.text.y = element_text(size = 20),  # Increase y-axis label size
        axis.title = element_text(size = 20),  # Increase axis title size
        legend.title = element_text(size = 20),  # Increase legend title size
        legend.text = element_text(size = 20)) + labs(fill = "Sample") + ylim(-1.5, 2)# Rotate x-axis labels for better visibility


##SUPPLEMENTAL FIGURE

ggplot(data_all, aes(x = Kernel_Density_TWA_Mean, y = Kernel_Density_TWA_SD)) +
  geom_point() +
  geom_histogram(data = data_all, aes(x = Kernel_Density_TWA_Mean), fill = "lightblue", bins = 10) +
  geom_histogram(data = data_all, aes(y = Kernel_Density_TWA_SD), fill = "lightblue", bins = 10) +
  theme_bw() +
  labs(x = "X-axis label", y = "Y-axis label", title = "Scatter Plot with Marginal Histograms")

p <- ggplot(data_all, aes(x = Kernel_Density_TWA_Mean, y = Kernel_Density_TWA_SD)) +
  geom_point() +
  theme_bw() +
  labs(x = "Kernel Density Mean", y = "Kernel Density SD", title = "Kernel Density Descriptives")

ggMarginal(p, type = "histogram", bins = 10)

p <- ggplot(data_all, aes(x = Avg_Travel_to_Pathology, y = TraveltoPathology_SD)) +
  geom_point() +
  theme_bw() +
  labs(x = "Perceived Pathology at k Mean", y = "Perceived Pathology at k SD", title = "Perceived Pathology at k Descriptives")

ggMarginal(p, type = "histogram", bins = 10)

p <- ggplot(data_all, aes(x = Avg_Travel_to_Enjoyment, y = TraveltoEnjoyment_SD)) +
  geom_point() +
  theme_bw() +
  labs(x = "Valence at k Mean", y = "Valence at k SD", title = "Valence at k Descriptives")

ggMarginal(p, type = "histogram", bins = 10)

p <- ggplot(data_all, aes(x = Avg_Slope, y = Slope_SD)) +
  geom_point() +
  theme_bw() +
  labs(x = "Rate of Approach Mean", y = "Rate of Approach SD", title = "Rate of Approach Descriptives")

ggMarginal(p, type = "histogram", bins = 10)

p <- ggplot(data_all, aes(x = Min_Distance_to_Location_Chain_Mean, y = Min_Distance_to_Location_Chain_SD)) +
  geom_point() +
  theme_bw() +
  labs(x = "Final Distance to k Mean", y = "Final Distance to k SD", title = "Final Distance to k Descriptives")

ggMarginal(p, type = "histogram", bins = 10)

