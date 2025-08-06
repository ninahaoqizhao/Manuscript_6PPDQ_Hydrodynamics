library(tidyverse)
library(readxl)
library(corrplot)
library(purrr)
library(ggpubr)
library(ggrepel)
library(broom)

###########################
# 6PPDQ time profile plot #
###########################
file_path <- "Miller_PPDs_Hydrograph_Load_Calculation_No_Formula.xlsx"  
sheet_names <- excel_sheets(file_path)

# Read and combine 6PPDQ concentration in all sheets
df_all <- lapply(sheet_names, function(sheet) {
  read_excel(file_path, sheet = sheet) %>%
    select(event, date.time, discharge.m3s_, conc.sampled.ngL_6PPDQ)  
}) %>%
  bind_rows()

df_all$date.time <- as.POSIXct(df_all$date.time, format = "%m/%d/%y %H:%M")

df_all_filtered <- df_all %>% dplyr::filter(event != "11.3.2020")

# Set scale factor (same for all storms)
scale_factor <- 160 / 1.6 

# 6PPDQ pollutograph plotting
pollutograph_6PPDQ <- ggplot(df_all_filtered, aes(x = date.time)) +
  geom_line(aes(y = discharge.m3s_ * scale_factor), color = "#8AB6F9", size = 0.6) +
  geom_point(aes(y = conc.sampled.ngL_6PPDQ), color = "#C00000", size = 1.2) +
  geom_line(data = df_all_filtered %>% filter(!is.na(conc.sampled.ngL_6PPDQ)),
            aes(y = conc.sampled.ngL_6PPDQ),
            color = "#C00000", size = 0.6) +
  geom_hline(yintercept = 11, color = "gray50", linetype = "dotted") +  
  geom_hline(yintercept = 41, color = "gray50", linetype = "dashed") + 
  scale_y_continuous(
    name = "6PPDQ Concentration (ng/L)",
    limits = c(0, 160),
    breaks = seq(0, 160, by = 40),
    sec.axis = sec_axis(~ . / scale_factor,
                        name = "Discharge (m³/s)",
                        breaks = seq(0, 1.6, by = 0.4))
  ) +
  scale_x_datetime(
    date_breaks = "6 hours",
    date_minor_breaks = "1 hour",
    date_labels = "%H:%M",
    guide = guide_axis(minor.ticks = TRUE)
  ) +
  theme_minimal() +
  labs(x = "Time", y = NULL) +
  theme(
    axis.title.y = element_text(color = "black"),
    axis.title.y.right = element_text(color = "black"),
    panel.border = element_rect(color = "black", fill = NA, size = 0.8),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1, color = "black"),
    axis.text.y = element_text(color = "black"),
    axis.ticks.x = element_line(color = "black"),
    axis.ticks.y = element_line(color = "black"),
    axis.ticks.length = unit(3, "pt"),
    axis.minor.ticks.length = unit(1.5, "pt")
  ) +
  facet_wrap(~ event, ncol = 4, scales = "free_x")

# ggsave(plot = pollutograph_6PPDQ, file = "pollutograph_6PPDQ.svg", width = 6, height = 5.8)

# boxplot for different season
df_all_boxplot <- df_all %>% dplyr::filter(!is.na(conc.sampled.ngL_6PPDQ)) %>%
  mutate(season = case_when(
    str_starts(event, "10") | str_starts(event, "11") ~ "Fall",
    str_starts(event, "3") | str_starts(event, "4") ~ "Spring",
    str_starts(event, "2") ~ "Winter",
    TRUE ~ NA_character_
  ))
df_all_boxplot$season <- factor(df_all_boxplot$season, levels = c("Fall", "Winter", "Spring"))
boxplot_all_6ppdq <- ggplot(df_all_boxplot, aes(x = season, y = conc.sampled.ngL_6PPDQ, fill = season)) +
  geom_boxplot(outlier.shape=NA, size = 1, width = 0.5) +
  geom_point(position = position_jitterdodge(jitter.width = 0.6), size = 3, shape = 21, aes(fill = season)) +
  scale_color_manual(values = c("#6699cc","#91b187", "#cc99cc")) +
  scale_fill_manual(values = c("#6699cc","#91b187", "#cc99cc")) +
  theme_classic() +
  theme(axis.ticks.length=unit(.15, "cm"),
        axis.line.x.bottom=element_line(linewidth=1.0), axis.line.y.left = element_line(linewidth=1.0),
        axis.ticks.x.bottom = element_line(linewidth=1.0), axis.ticks.y.left = element_line(linewidth=1.0),
        axis.text.y.left = element_text(size = 12), axis.text.x.bottom = element_text(size = 12),
        axis.title.y.left = element_text(size = 12), axis.title.x.bottom = element_blank())
# ggsave(file="all_6ppdq_boxplot.svg", plot=boxplot_all_6ppdq, width=4, height=5)

kruskal.test(conc.sampled.ngL_6PPDQ ~ season, data = df_all_boxplot)
pairwise.wilcox.test(df_all_boxplot$conc.sampled.ngL_6PPDQ, df_all_boxplot$season, p.adjust.method = 'BH')

####################################################
# barplot and boxplot for peak 6PPDQ concentration #
####################################################
peak_6PPDQ <- df_all %>%
  group_by(event) %>%
  filter(conc.sampled.ngL_6PPDQ == max(conc.sampled.ngL_6PPDQ, na.rm = TRUE))

peak_6PPDQ$event <- factor(peak_6PPDQ$event, levels = unique(peak_6PPDQ$event))
peak_6PPDQ_barplot <- ggplot(peak_6PPDQ, aes(x = event, y = conc.sampled.ngL_6PPDQ)) +
  geom_bar(stat = "identity", color = "black", fill = "#AEABAB", width = 0.45) +
  geom_hline(yintercept = 95, linetype = "dashed", color = "gray40") +
  geom_hline(yintercept = 44, linetype = "dashed", color = "gray40") +
  geom_hline(yintercept = 11, linetype = "dotted", color = "gray40") +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  labs(x = "Label", y = "Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# ggsave(plot = peak_6PPDQ_barplot, filename = "peak_6PPDQ_barplot.svg",  width = 8, height = 3.9)

peak_6PPDQ$season <- c("Fall", "Fall", "Fall", "Fall", "Spring", "Spring", "Spring", "Fall", "Fall", "Fall", "Fall", "Fall", "Winter", "Winter", "Spring", "Fall", "Winter")
peak_6PPDQ$season <- factor(peak_6PPDQ$season, levels = c("Fall", "Winter", "Spring"))
boxplot_peak_6ppdq <- ggplot(peak_6PPDQ, aes(x = season, y = conc.sampled.ngL_6PPDQ, fill = season)) +
  geom_boxplot(outlier.shape=NA, size = 1, width = 0.5) +
  geom_point(position = position_jitterdodge(jitter.width = 0.4), size = 4, shape = 21, aes(fill = season)) +
  scale_color_manual(values = c("#6699cc","#91b187", "#cc99cc")) +
  scale_fill_manual(values = c("#6699cc","#91b187", "#cc99cc")) +
  theme_classic() +
  theme(axis.ticks.length=unit(.15, "cm"),
        axis.line.x.bottom=element_line(linewidth=1.0), axis.line.y.left = element_line(linewidth=1.0),
        axis.ticks.x.bottom = element_line(linewidth=1.0), axis.ticks.y.left = element_line(linewidth=1.0),
        axis.text.y.left = element_text(size = 12), axis.text.x.bottom = element_text(size = 12),
        axis.title.y.left = element_text(size = 12), axis.title.x.bottom = element_blank())
# ggsave(file="peak_6ppdq_boxplot.svg", plot=boxplot_peak_6ppdq, width=4, height=5)
kruskal.test(conc.sampled.ngL_6PPDQ ~ season, data = peak_6PPDQ)
pairwise.wilcox.test(peak_6PPDQ$conc.sampled.ngL_6PPDQ, peak_6PPDQ$season, p.adjust.method = 'BH')

####################################################
# time profile plot for multiple PPDs in fall 2021 #
####################################################
file_path <- "Miller_PPDs_Hydrograph_Load_Calculation_No_Formula.xlsx"  
sheet_names_fall_2021 <- c("10212021", "10272021", "11032021", "11112021", "11182021")

# Read and combine all sheets
df_fall <- lapply(sheet_names_fall_2021, function(sheet) {
  read_excel(file_path, sheet = sheet) %>%
    select(event, date.time, discharge.m3s_, 
           conc.sampled.ngL_6PPDQ, conc.sampled.ngL_6PPD, conc.sampled.ngL_77PD, conc.sampled.ngL_DPPD, conc.sampled.ngL_DTPD,
           conc.sampled.ngL_DTPDQ,conc.sampled.ngL_13DMBA, conc.sampled.ngL_4sDPA, conc.sampled.ngL_4NDPA)  # Pick only the columns you need
}) %>%
  bind_rows()

df_fall$date.time <- as.POSIXct(df_fall$date.time, format = "%m/%d/%y %H:%M")

# 6PPD and 6PPDQ plot
scale_factor <- 160 / 1.6
pollutograph_6PPD <- ggplot(df_fall, aes(x = date.time)) +
  geom_line(aes(y = discharge.m3s_ * scale_factor), color = "#8AB6F9", size = 0.6) +
  geom_point(aes(y = conc.sampled.ngL_6PPDQ), color = "#C00000", size = 1.2) +
  geom_point(aes(y = conc.sampled.ngL_6PPD), color = "#D48BAE", size = 1.2) +
  geom_line(data = df_fall %>% filter(!is.na(conc.sampled.ngL_6PPDQ)),
            aes(y = conc.sampled.ngL_6PPDQ),
            color = "#C00000", size = 0.6) +
  geom_line(data = df_fall %>% filter(!is.na(conc.sampled.ngL_6PPD)),
            aes(y = conc.sampled.ngL_6PPD),
            color = "#D48BAE", size = 0.6) +
  scale_y_continuous(
    name = "Concentration (ng/L)",
    limits = c(0, 160),
    breaks = seq(0, 160, by = 40),
    sec.axis = sec_axis(~ . / scale_factor,
                        name = "Discharge (m³/s)",
                        breaks = seq(0, 1.6, by = 0.4))
  ) +
  scale_x_datetime(
    date_breaks = "6 hours",
    date_minor_breaks = "1 hour",
    date_labels = "%H:%M",
    guide = guide_axis(minor.ticks = TRUE)
  ) +
  theme_minimal() +
  labs(x = "Time", y = NULL) +
  theme(
    axis.title.y = element_text(color = "black"),
    axis.title.y.right = element_text(color = "black"),
    panel.border = element_rect(color = "black", fill = NA, size = 0.8),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1, color = "black"),
    axis.text.y = element_text(color = "black"),
    axis.ticks.x = element_line(color = "black"),
    axis.ticks.y = element_line(color = "black"),
    axis.ticks.length = unit(3, "pt"),
    axis.minor.ticks.length = unit(1.5, "pt")
  ) +
  facet_wrap(~ event, ncol = 1, scales = "free_x")

# ggsave(plot = pollutograph_6PPD, file = "pollutograph_6PPD.svg", width = 2.4, height = 8.8)

# other PPD plot
scale_factor <- 40 / 1.6
pollutograph_other_PPD <- ggplot(df_fall, aes(x = date.time)) +
  geom_line(aes(y = discharge.m3s_ * scale_factor), color = "#8AB6F9", size = 0.6) +
  geom_point(aes(y = conc.sampled.ngL_DTPDQ), color = "#9fbcad", size = 1.2) +
  geom_point(aes(y = conc.sampled.ngL_DTPD), color = "#454f2c", size = 1.2) +
  geom_point(aes(y = conc.sampled.ngL_77PD), color = "#713519", size = 1.2) +
  geom_point(aes(y = conc.sampled.ngL_DPPD), color = "#b7b9a8", size = 1.2) +
  geom_line(data = df_fall %>% filter(!is.na(conc.sampled.ngL_DTPDQ)),
            aes(y = conc.sampled.ngL_DTPDQ),
            color = "#9fbcad", size = 0.6) +
  geom_line(data = df_fall %>% filter(!is.na(conc.sampled.ngL_DTPD)),
            aes(y = conc.sampled.ngL_DTPD),
            color = "#454f2c", size = 0.6) +
  geom_line(data = df_fall %>% filter(!is.na(conc.sampled.ngL_77PD)),
            aes(y = conc.sampled.ngL_77PD),
            color = "#713519", size = 0.6) +
  geom_line(data = df_fall %>% filter(!is.na(conc.sampled.ngL_DPPD)),
            aes(y = conc.sampled.ngL_DPPD),
            color = "#b7b9a8", size = 0.6) +
  scale_y_continuous(
    name = "Concentration (ng/L)",
    limits = c(0, 40),
    breaks = seq(0, 40, by = 10),
    sec.axis = sec_axis(~ . / scale_factor,
                        name = "Discharge (m³/s)",
                        breaks = seq(0, 1.6, by = 0.4))
  ) +
  scale_x_datetime(
    date_breaks = "6 hours",
    date_minor_breaks = "1 hour",
    date_labels = "%H:%M",
    guide = guide_axis(minor.ticks = TRUE)
  ) +
  theme_minimal() +
  labs(x = "Time", y = NULL) +
  theme(
    axis.title.y = element_text(color = "black"),
    axis.title.y.right = element_text(color = "black"),
    panel.border = element_rect(color = "black", fill = NA, size = 0.8),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1, color = "black"),
    axis.text.y = element_text(color = "black"),
    axis.ticks.x = element_line(color = "black"),
    axis.ticks.y = element_line(color = "black"),
    axis.ticks.length = unit(3, "pt"),
    axis.minor.ticks.length = unit(1.5, "pt")
  ) +
  facet_wrap(~ event, ncol = 1, scales = "free_x")

#ggsave(plot = pollutograph_other_PPD, file = "pollutograph_other_PPD.svg", width = 2.33, height = 8.8)

# 1,3-DMBA plot
scale_factor <- 5000 / 1.6
pollutograph_13DMBA <- ggplot(df_fall, aes(x = date.time)) +
  geom_line(aes(y = discharge.m3s_ * scale_factor), color = "#8AB6F9", size = 0.6) +
  geom_point(aes(y = conc.sampled.ngL_13DMBA), color = "#3A6D80", size = 1.2) +
  geom_line(data = df_fall %>% filter(!is.na(conc.sampled.ngL_13DMBA)),
            aes(y = conc.sampled.ngL_13DMBA),
            color = "#3A6D80", size = 0.6) +
  scale_y_continuous(
    name = "Concentration (ng/L)",
    limits = c(0, 5000),
    breaks = seq(0, 5000, by = 1000),
    sec.axis = sec_axis(~ . / scale_factor,
                        name = "Discharge (m³/s)",
                        breaks = seq(0, 1.6, by = 0.4))
  ) +
  scale_x_datetime(
    date_breaks = "6 hours",
    date_minor_breaks = "1 hour",
    date_labels = "%H:%M",
    guide = guide_axis(minor.ticks = TRUE)
  ) +
  theme_minimal() +
  labs(x = "Time", y = NULL) +
  theme(
    axis.title.y = element_text(color = "black"),
    axis.title.y.right = element_text(color = "black"),
    panel.border = element_rect(color = "black", fill = NA, size = 0.8),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1, color = "black"),
    axis.text.y = element_text(color = "black"),
    axis.ticks.x = element_line(color = "black"),
    axis.ticks.y = element_line(color = "black"),
    axis.ticks.length = unit(3, "pt"),
    axis.minor.ticks.length = unit(1.5, "pt")
  ) +
  facet_wrap(~ event, ncol = 1, scales = "free_x")

# ggsave(plot = pollutograph_13DMBA, file = "pollutograph_other_13DMBA.svg", width = 2.46, height = 8.8)

# other TP plot
scale_factor <- 25 / 1.6
pollutograph_other_TP <- ggplot(df_fall, aes(x = date.time)) +
  geom_line(aes(y = discharge.m3s_ * scale_factor), color = "#8AB6F9", size = 0.6) +
  geom_point(aes(y = conc.sampled.ngL_4sDPA), color = "#80c4b7", size = 1.2) +
  geom_point(aes(y = conc.sampled.ngL_4NDPA), color = "#A95E13", size = 1.2) +
  geom_line(data = df_fall %>% filter(!is.na(conc.sampled.ngL_4sDPA)),
            aes(y = conc.sampled.ngL_4sDPA),
            color = "#80c4b7", size = 0.6) +
  geom_line(data = df_fall %>% filter(!is.na(conc.sampled.ngL_4NDPA)),
            aes(y = conc.sampled.ngL_4NDPA),
            color = "#A95E13", size = 0.6) +
  scale_y_continuous(
    name = "Concentration (ng/L)",
    limits = c(0, 25),
    breaks = seq(0, 25, by = 5),
    sec.axis = sec_axis(~ . / scale_factor,
                        name = "Discharge (m³/s)",
                        breaks = seq(0, 1.6, by = 0.4))
  ) +
  scale_x_datetime(
    date_breaks = "6 hours",
    date_minor_breaks = "1 hour",
    date_labels = "%H:%M",
    guide = guide_axis(minor.ticks = TRUE)
  ) +
  theme_minimal() +
  labs(x = "Time", y = NULL) +
  theme(
    axis.title.y = element_text(color = "black"),
    axis.title.y.right = element_text(color = "black"),
    panel.border = element_rect(color = "black", fill = NA, size = 0.8),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1, color = "black"),
    axis.text.y = element_text(color = "black"),
    axis.ticks.x = element_line(color = "black"),
    axis.ticks.y = element_line(color = "black"),
    axis.ticks.length = unit(3, "pt"),
    axis.minor.ticks.length = unit(1.5, "pt")
  ) +
  facet_wrap(~ event, ncol = 1, scales = "free_x")

# ggsave(plot = pollutograph_other_TP, file = "pollutograph_other_TP.svg", width = 2.33, height = 8.8)

######################
# correlation matrix #
######################

file_path <- "Miller_PPDs_Hydrograph_Load_Calculation_No_Formula.xlsx"  
sheet_names_fall_2021 <- c("10212021", "10272021", "11032021", "11112021", "11182021")

# Read and combine all sheets
df_fall <- lapply(sheet_names_fall_2021, function(sheet) {
  read_excel(file_path, sheet = sheet) %>%
    select(date.time, 
           conc.sampled.ngL_6PPDQ, conc.sampled.ngL_6PPD, conc.sampled.ngL_77PD, conc.sampled.ngL_DPPD, conc.sampled.ngL_DTPD,
           conc.sampled.ngL_13DMBA, conc.sampled.ngL_4sDPA, conc.sampled.ngL_4NDPA)  # Pick only the columns you need
}) %>%
  bind_rows()

df_fall_filtered <- df_fall %>% dplyr::filter(!is.na(conc.sampled.ngL_6PPDQ)) %>% column_to_rownames("date.time")

df_fall_filtered <- df_fall_filtered %>% dplyr::select(-c("conc.sampled.ngL_77PD", "conc.sampled.ngL_DPPD"))
colnames(df_fall_filtered) <- gsub("conc.sampled.ngL_", "",colnames(df_fall_filtered))

cor_matrix <- cor(df_fall_filtered)
# svg("cor_matrix.svg", width = 6, height = 6)  
cor_matrix_plot <- corrplot.mixed(cor_matrix)    
# dev.off()

######################
# Mass - Volume plot #
######################
# for 6PPDQ
file_path <- "Miller_PPDs_Hydrograph_Load_Calculation_No_Formula.xlsx" 
sheet_names <- excel_sheets(file_path)

MV_6PPDQ <- lapply(sheet_names, function(sheet) {
  read_excel(file_path, sheet = sheet) %>%
    select(event, percent.total.runoff.vol_, percent.total.mass_6PPDQ, conc.sampled.ngL_6PPDQ)  
}) %>%
  bind_rows()

events_keep <- MV_6PPDQ %>%
  group_by(event) %>%
  summarise(DF = mean(conc.sampled.ngL_6PPDQ > 0, na.rm = TRUE)) %>%
  filter(DF > 0.5) # only plot events with >50% detection frequencies

MV_6PPDQ_filtered <- MV_6PPDQ %>% dplyr::filter(event %in% events_keep$event) # only plot for storm events with >50% detection

# Plot the overlapped M-V lines
event_colors <- c("3.18.2021" = "#D33A24", "4.23.2021" = "#7030a0", "10.23.2020" = "#EE8534", "10.27.2021" = "#3E75AE", "11.11.2021" = "#52968F",
        "2.3.2023" = "#C4B3D4", "11.3.2021" = "#9DD1C9", "2.20.2022" = "#A95C36", "10.21.2021" = "#66CCCC", 
        "4.18.2022" = "#999999", "3.4.2021" = "#D48BAE",
        "11.12.2020" = "#00B050", "11.18.2021" = "#D9C385", 
        "2.26.2022" = "#FFFF99", "10.9.2020" = "#999966")
MV_6PPDQ_filtered$event <- factor(MV_6PPDQ_filtered$event, levels = unique(MV_6PPDQ_filtered$event))
M_V_6ppdq <- ggplot(MV_6PPDQ_filtered, aes(x = percent.total.runoff.vol_, 
                          y = percent.total.mass_6PPDQ, 
                          group = event, 
                          color = event)) +
  geom_line(linewidth = 1) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black", linewidth = 0.8) +
  scale_color_manual(values = event_colors) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 1)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
  labs(x = "Percent Total Runoff Volume", 
       y = "Percent Total Mass") +
  theme_classic()
# ggsave(plot = M_V_6ppdq, file = "M_V_6ppdq.svg", width = 6.1, height = 5)

# for other PPDs and TPs
file_path <- "Miller_PPDs_Hydrograph_Load_Calculation_No_Formula.xlsx" 
sheet_names_fall_2021 <- c("10212021", "10272021", "11032021", "11112021", "11182021")

compounds <- c("6PPDQ", "6PPD", "77PD", "DPPD", "DTPD", "IPPDQ", "DTPDQ", "13DMBA", "4sDPA", "4NDPA")

# Initialize a list to store filtered results per compound
all_MV_list <- list()

for (comp in compounds) {
  mass_col <- paste0("percent.total.mass_", comp)
  conc_col <- paste0("conc.sampled.ngL_", comp)
  # Read data from all sheets
  MV <- map_dfr(sheet_names_fall_2021, function(sheet) {
    read_excel(file_path, sheet = sheet) %>%
      select(event, percent.total.runoff.vol_, !!sym(mass_col), !!sym(conc_col)) %>%
      rename(mass = !!sym(mass_col), conc = !!sym(conc_col)) %>%
      mutate(compound = comp)
  })
  # Filter events with >50% detection
  events_keep <- MV %>%
    group_by(event) %>%
    summarise(DF = mean(conc > 0, na.rm = TRUE)) %>%
    filter(DF > 0.5) %>%
    pull(event)
  MV_filtered <- MV %>% filter(event %in% events_keep)
  
  # Store filtered result
  all_MV_list[[comp]] <- MV_filtered
}

# Combine all into a single dataframe
all_MV <- bind_rows(all_MV_list)

# Plot the overlapped M-V lines
event_colors = c("3.18.2021" = "#D33A24", "4.23.2021" = "#7030a0", "10.23.2020" = "#EE8534", "10.27.2021" = "#3E75AE", "11.11.2021" = "#52968F",
                 "2.3.2023" = "#C4B3D4", "11.3.2021" = "#9DD1C9", "2.20.2022" = "#A95C36", "10.21.2021" = "#66CCCC", 
                 "4.18.2022" = "#999999", "3.4.2021" = "#D48BAE",
                 "11.12.2020" = "#00B050", "11.18.2021" = "#D9C385", 
                 "2.26.2022" = "#FFFF99", "10.9.2020" = "#999966")

all_MV$compound <- factor(all_MV$compound, levels = c("6PPDQ", "6PPD", "77PD", "DPPD", "DTPD", "IPPDQ", "DTPDQ", "13DMBA", "4sDPA", "4NDPA"))
all_MV$event <- factor(all_MV$event, levels = unique(all_MV$event))

M_V_plot_fall <- ggplot(all_MV, aes(x = percent.total.runoff.vol_, y = mass, group = event, color = event)) +
  geom_line(linewidth = 1) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black", linewidth = 0.8) +
  scale_color_manual(values = event_colors) +
  facet_wrap(~ compound, ncol = 2) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 1), breaks = c(0, 0.25, 0.5, 0.75, 1)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1), breaks = c(0, 0.25, 0.5, 0.75, 1)) +
  labs(x = "Percent Total Runoff Volume", 
       y = "Percent Total Mass") +
  theme_classic() + 
  theme(
    strip.text = element_text(face = "bold", size = 12),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 11),
    axis.title.y = element_text(size = 11),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    panel.spacing = unit(1, "cm")
  )
# ggsave(plot = M_V_plot_fall, file = "M_V_plot_fall.svg", width = 6.35, height = 11)

#########################
# box plot of other PPD #
#########################

file_path <- "Miller_PPDs_Hydrograph_Load_Calculation_No_Formula.xlsx"  
sheet_names_fall_2021 <- c("10212021", "10272021", "11032021", "11112021", "11182021")

# Read and combine all sheets
df_fall <- lapply(sheet_names_fall_2021, function(sheet) {
  read_excel(file_path, sheet = sheet) %>%
    select(conc.sampled.ngL_6PPDQ, 
           conc.sampled.ngL_6PPD, conc.sampled.ngL_IPPD, conc.sampled.ngL_7PPD, conc.sampled.ngL_77PD, conc.sampled.ngL_DPPD, conc.sampled.ngL_DTPD, conc.sampled.ngL_DNP,
           conc.sampled.ngL_IPPDQ, conc.sampled.ngL_7PPDQ, conc.sampled.ngL_DPPDQ, conc.sampled.ngL_DTPDQ, 
           conc.sampled.ngL_13DMBA, conc.sampled.ngL_4HDPA, conc.sampled.ngL_4ADPA, conc.sampled.ngL_4sDPA, conc.sampled.ngL_4NDPA)  # Pick only the columns you need
}) %>%
  bind_rows()

df_fall_filtered <- df_fall %>% dplyr::filter(!is.na(conc.sampled.ngL_6PPDQ)) 
colnames(df_fall_filtered) <- gsub("conc.sampled.ngL_", "", colnames(df_fall_filtered))

cpd <- colnames(df_fall_filtered)
DF <- c(1, 0.92, 0, 0, 0.48, 0.59, 1, 0, 0.12, 0, 0.02, 0.62, 1, 0.01, 0, 0.9, 0.84)
loq <- c(1.6, 5.3, 1.7, 1.7, 0.59, 0.1, 0.64, 2.8, 1.2, 0.45, 16, 2.3, 28, 4.4, 27, 0.34, 2.5)
freq <- data.frame(cpd, DF, loq)

df_fall_cleaned <- map2_dfc(df_fall_filtered, freq$loq, function(col, loq_val) {
  ifelse(col < loq_val, 0, col)
})
colnames(df_fall_cleaned) <- colnames(df_fall_filtered)


df_long <- df_fall_cleaned %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value")

df_long$value_log <- ifelse(df_long$value > 0, log10(df_long$value), NA)

min_log <- -2
max_log <- 4
freq <- freq %>%
  mutate(scaled_DF = DF * (max_log - min_log) + min_log)
df_long$variable <- factor(df_long$variable, levels = cpd)

# Plot with secondary axis
boxplot <- ggplot(df_long, aes(x = variable, y = value_log)) +
  geom_boxplot(outlier.shape = NA, size = 0.6, color = "black", fill = "#AEABAB", width = 0.4, alpha = 0.5) +
  geom_point(position = position_jitter(width = 0.0, height = 0),
             size = 2.2, shape = 20, color = "#AEABAB") +
  geom_point(data = freq, aes(x = cpd, y = scaled_DF),
             inherit.aes = FALSE, shape = 15, size = 3, color = "#C00000") +
  geom_segment(
    data = freq,
    aes(x = cpd, xend = cpd, y = log10(loq) - 0.01, yend = log10(loq) + 0.01),
    inherit.aes = FALSE,
    linetype = "dotted", color = "black", linewidth = 0.5
  ) +
  scale_y_continuous(
    name = "log10(concentration)",
    limits = c(min_log, max_log),
    sec.axis = sec_axis(~ (. - min_log) / (max_log - min_log),
                        name = "Detection Frequency (DF)", breaks = seq(0, 1, 0.2))
  ) +
  theme_classic() +
  theme(axis.ticks.length = unit(.15, "cm"),
        axis.line.x.bottom = element_line(linewidth = 1.0),
        axis.line.y.left = element_line(linewidth = 1.0),
        axis.ticks.x.bottom = element_line(linewidth = 1.0),
        axis.ticks.y.left = element_line(linewidth = 1.0),
        axis.title.x.bottom = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.background = element_blank(),
        strip.text = element_text(size = 12, face = "bold"))
# ggsave(plot = boxplot, file = "boxplot_fall_PPD.svg", width = 8, height = 3.5)

############################################
# correlation with hydrodynamic parameters #
############################################
file_path <- "Miller_PPDs_Hydrograph_Load_Calculation_No_Formula.xlsx"  
sheet_names <- excel_sheets(file_path)

# Read and combine all sheets
df_all <- lapply(sheet_names, function(sheet) {
  read_excel(file_path, sheet = sheet) %>%
    select(event, date.time, discharge.m3s_, conc.sampled.ngL_6PPDQ)  
}) %>%
  bind_rows()

df_all$date.time <- as.POSIXct(df_all$date.time, format = "%m/%d/%y %H:%M")

# Calculate maximum discharge and its earliest time
max_discharge <- df_all %>%
  group_by(event) %>%
  filter(discharge.m3s_ == max(discharge.m3s_, na.rm = TRUE)) %>%
  filter(date.time == min(date.time)) %>%
  select(event, time_discharge_peak = date.time, max_discharge = discharge.m3s_)

# For storms with multiple peaks (11/03/21, 11/18/21, 10/27/21), storm peak times were manually corrected to reflect the first peak 
tz_col <- attr(max_discharge$time_discharge_peak, "tzone")
max_discharge$time_discharge_peak[max_discharge$event == "11.3.2021"] <-  as.POSIXct("2021-11-03 21:45:00", tz = tz_col)
max_discharge$max_discharge[max_discharge$event == "11.3.2021"] <- 1.2003440
max_discharge$time_discharge_peak[max_discharge$event == "11.18.2021"] <- as.POSIXct("2021-11-18 17:30:00", tz = tz_col)
max_discharge$max_discharge[max_discharge$event == "11.18.2021"] <- 0.8931805
max_discharge$time_discharge_peak[max_discharge$event == "10.27.2021"] <- as.POSIXct("2021-10-28 04:15:00", , tz = tz_col)
max_discharge$max_discharge[max_discharge$event == "10.27.2021"] <- 0.6066833

# Minimum discharge and its latest time
min_discharge <- df_all %>%
  group_by(event) %>%
  filter(discharge.m3s_ == min(discharge.m3s_, na.rm = TRUE)) %>%
  filter(date.time == max(date.time)) %>%
  select(event, time_storm_start = date.time, min_discharge = discharge.m3s_)

# Maximum 6PPDQ concentration and its time
max_conc_6PPDQ <- df_all %>%
  group_by(event) %>%
  filter(conc.sampled.ngL_6PPDQ == max(conc.sampled.ngL_6PPDQ, na.rm = TRUE)) %>%
  select(event, time_6PPDQ = date.time, max_6PPDQ = conc.sampled.ngL_6PPDQ)

# Join results
result <- left_join(max_discharge, min_discharge, by = "event")
result <- left_join(result, max_conc_6PPDQ, by = "event")

# calculate time shifts between 6PPDQ peak and discharge peak
result$peak_time_dif <- result$time_discharge_peak - result$time_6PPDQ
result$slope <- result$time_discharge_peak - result$time_storm_start

# calculate flow-weighted average 6PPDQ concentrations
df_6PPDQ_load <- df_all %>%
  group_by(event) %>%
  fill(conc.sampled.ngL_6PPDQ, .direction = "down") %>%
  mutate(
    time_diff_sec = lead(date.time) - date.time,
    time_diff_sec = as.numeric(time_diff_sec, units = "secs")
  ) %>%
  mutate(
    load_6PPDQ = discharge.m3s_ * conc.sampled.ngL_6PPDQ * time_diff_sec
  ) %>%
  group_by(event) %>%
  dplyr::summarise(total_6PPDQ_mass_ng = sum(load_6PPDQ, na.rm = TRUE)) %>%
  ungroup()

# read precipitation parameters, manually recorded from Weather Underground
precipitation <- read.csv("Miller_precipitation_metadata.csv")
result_w_precipitation <- result %>% left_join(precipitation, by = "event") %>% left_join(df_6PPDQ_load, by = "event")
result_w_precipitation$flow_weighted_6PPDQ <- result_w_precipitation$total_6PPDQ_mass_ng/result_w_precipitation$Cumulative_runoff_m3

# check correlations for peak 6PPDQ concentration
x_vars <- c("Peak_precipitation_mm_hr_", "Storm_Duration_hr", "Antecedent_Dry_Period_days", "Previous_strom_peak_precip_mm_hr",
            "max_discharge","Cumulative_runoff_m3", "slope", "peak_time_dif") 

# Pivot to long format
result_long <- result_w_precipitation %>% 
  mutate(peak_time_dif = as.numeric(peak_time_dif, units = "mins")) %>%
  mutate(slope = as.numeric(slope, units = "mins")) %>%
  select(all_of(x_vars), max_6PPDQ) %>%
  pivot_longer(cols = all_of(x_vars), names_to = "variable", values_to = "value")

# Create panel plot
result_long$variable <- factor(result_long$variable, levels = x_vars)
max_6PPDQ_correlation <- ggplot(result_long, aes(x = value, y = max_6PPDQ)) +
  geom_point(shape = 20, size = 3, alpha = 0.8, color = "#2F6EBA") +
  geom_smooth(method = "lm", se = TRUE, color = "darkgray", fill = "lightgray", linewidth = 0.8, linetype = "dashed") +
  stat_cor(method = "pearson", label.x = 0.1, label.y = max(result$max_6PPDQ, na.rm = TRUE)) +
  scale_y_continuous(limits = c(0, 160), breaks = c(0, 40, 80, 120, 160)) +
  facet_wrap(~ variable, ncol = 4, scales = "free_x") +
  labs(x = NULL, y = "6PPDQ peak") +
  theme_classic()
# ggsave(plot = max_6PPDQ_correlation, file = "max_6PPDQ_correlation.svg", width = 8, height = 4.6)

# using multiparameter models to examine the relationships
result_normalized <- result_w_precipitation %>% 
  mutate(peak_time_dif = as.numeric(peak_time_dif, units = "mins")) %>%
  mutate(slope = as.numeric(slope, units = "mins")) 
numeric_cols <- sapply(result_normalized, is.numeric)
result_normalized[numeric_cols] <- scale(result_normalized[numeric_cols])

predictors <- result_normalized %>% ungroup() %>%
  select(all_of(x_vars))
corrplot(cor(predictors), method = "color", type = "upper", tl.cex = 0.8)

lm1 <- lm(max_6PPDQ ~ Previous_strom_peak_precip_mm_hr + slope + Antecedent_Dry_Period_days + Peak_precipitation_mm_hr_ + Cumulative_runoff_m3,
          data = result_normalized)
summary(lm1)

# Coefficient plot
tidy_lm <- tidy(lm1, conf.int = TRUE)
tidy_lm <- tidy_lm %>% filter(term != "(Intercept)")
coefficient_plot <- ggplot(tidy_lm, aes(x = reorder(term, estimate), y = estimate)) +
  geom_point(size = 3, color = "#2F6EBA") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, color = "gray40") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_flip() +
  labs(x = NULL, y = "Standardized Regression Coefficient (β)",
       title = "Effect of Hydrologic Predictors on Peak 6PPDQ Concentration") +
  theme_classic()
# ggsave(plot = coefficient_plot, file = "coefficient_plot_max_6PPDQ.svg", width = 4, height = 4)

# Observed vs. Predicted Plot
result_normalized$predicted <- predict(lm1)
observed_predicted_plot <- ggplot(result_normalized, aes(x = predicted, y = max_6PPDQ)) +
  geom_point(size = 3, color = "#2F6EBA", alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(x = "Predicted Peak 6PPDQ (standardized)",
       y = "Observed Peak 6PPDQ (standardized)",
       title = "Model Fit: Observed vs. Predicted 6PPDQ Concentration") +
  theme_classic()
# ggsave(plot = observed_predicted_plot, file = "observed_predicted_plot_plot_max_6PPDQ.svg", width = 4, height = 4)

# check correlations for peak shift between 6PPDQ and discharge
x_vars_peak_shift <- c("Peak_precipitation_mm_hr_", "Storm_Duration_hr", "Antecedent_Dry_Period_days", "Previous_strom_peak_precip_mm_hr",
            "max_discharge","Cumulative_runoff_m3", "slope", "max_6PPDQ")  

# Pivot to long format
result_long <- result_w_precipitation %>% 
  mutate(peak_time_dif = as.numeric(peak_time_dif, units = "mins")) %>%
  mutate(slope = as.numeric(slope, units = "mins")) %>%
  select(all_of(x_vars_peak_shift), peak_time_dif) %>%
  pivot_longer(cols = all_of(x_vars_peak_shift), names_to = "variable", values_to = "value")
result_long <- result_long %>% dplyr::filter(!(event %in% c("10.23.2020", "4.23.2021")))

# Create panel plot
result_long$variable <- factor(result_long$variable, levels = x_vars_peak_shift)
peak_shift_correlation <- ggplot(result_long, aes(x = value, y = peak_time_dif)) +
  geom_point(shape = 20, size = 3, alpha = 0.6, color = "#C00000") +
  # geom_text_repel(aes(label = event), size = 3, max.overlaps = 15) + 
  geom_smooth(method = "lm", se = TRUE, color = "darkgray", fill = "lightgray", linewidth = 0.8, linetype = "dashed") +
  stat_cor(method = "pearson", label.x = 0.1, label.y = max(result$peak_time_dif, na.rm = TRUE)) +
  scale_y_continuous(limits = c(-400, 800), breaks = c(-400, -200, 0, 200, 400, 600, 800)) +
  facet_wrap(~ variable, ncol = 4, scales = "free_x") +
  labs(x = NULL, y = "Peak Time Difference") +
  theme_classic()
# ggsave(plot = peak_shift_correlation, file = "peak_shift_correlation.svg", width = 8, height = 4.6)


# using multiparameter models to examine the relationships
result_normalized <- result_w_precipitation %>% dplyr::filter(event != "10.23.2020" & event != "4.23.2021") %>%
  mutate(peak_time_dif = as.numeric(peak_time_dif, units = "mins")) %>%
  mutate(slope = as.numeric(slope, units = "mins")) 
numeric_cols <- sapply(result_normalized, is.numeric)
result_normalized[numeric_cols] <- scale(result_normalized[numeric_cols])

predictors <- result_normalized %>% ungroup() %>%
  select(all_of(x_vars_peak_shift))
corrplot(cor(predictors), method = "color", type = "upper", tl.cex = 0.8)

lm1 <- lm(peak_time_dif ~  Storm_Duration_hr + max_discharge + slope,
          data = result_normalized)
summary(lm1)

# Coefficient plot
tidy_lm <- tidy(lm1, conf.int = TRUE)
tidy_lm <- tidy_lm %>% filter(term != "(Intercept)")
coefficient_plot <- ggplot(tidy_lm, aes(x = reorder(term, estimate), y = estimate)) +
  geom_point(size = 3, color = "#2F6EBA") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, color = "gray40") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_flip() +
  labs(x = NULL, y = "Standardized Regression Coefficient (β)",
       title = "Effect of Hydrologic Predictors on Peak 6PPDQ Concentration") +
  theme_classic()
# ggsave(plot = coefficient_plot, file = "coefficient_plot_peak_shift.svg", width = 4, height = 4)

# Observed vs. Predicted Plot
result_normalized$predicted <- predict(lm1)
observed_predicted_plot <- ggplot(result_normalized, aes(x = predicted, y = peak_time_dif)) +
  geom_point(size = 3, color = "#2F6EBA", alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(x = "Predicted peak_time_dif (standardized)",
       y = "Observed peak_time_dif (standardized)",
       title = "Model Fit: Observed vs. Predicted peak_time_dif") +
  theme_classic()
# ggsave(plot = observed_predicted_plot, file = "observed_predicted_plot_plot_peak_shift.svg", width = 4, height = 4)

#####################
# Bland-Altman plot #
#####################
conc <- read_excel("Miller_PPDs_Hydrograph_Load_Calculation.xlsx", sheet = "Data Final")

conc_filtered <- conc[134:329,4:5]
colnames(conc_filtered) <- c("single_analyte", "multi_analyte")
conc_filtered$season <- "fall_2021"
conc_filtered$season[97:196] <- "other"
conc_final <- conc_filtered[!is.na(conc_filtered$multi_analyte) & 
                              conc_filtered$single_analyte > 0 &
                              conc_filtered$multi_analyte > 0,]

conc_final$single_analyte <- as.numeric(conc_final$single_analyte)
conc_final$multi_analyte <- as.numeric(conc_final$multi_analyte)
conc_final$mean_vals <- (conc_final$single_analyte + conc_final$multi_analyte) / 2
conc_final$percent_diff <- 100 * (conc_final$single_analyte - conc_final$multi_analyte) / conc_final$mean_vals

# Bland-Altman for Fall 2021
conc_fall_2021 <- conc_final[conc_final$season == "fall_2021",]
bias_percent <- mean(conc_fall_2021$percent_diff)
loa_upper <- bias_percent + 1.96 * sd(conc_fall_2021$percent_diff)
loa_lower <- bias_percent - 1.96 * sd(conc_fall_2021$percent_diff)

# Plot
Bland_Altman <- ggplot(conc_fall_2021, aes(x = mean_vals, y = percent_diff)) +
  geom_point(shape = 20, size = 5, alpha = 0.9, color = "darkgray") +
  geom_hline(yintercept = bias_percent, color = "blue", linetype = "dashed") +
  geom_hline(yintercept = loa_upper, color = "red", linetype = "dotted") +
  geom_hline(yintercept = loa_lower, color = "red", linetype = "dotted") +
  labs(title = "Bland-Altman Plot",
       x = "Mean Concentration",
       y = "Percent Difference (%)") +
  scale_y_continuous(breaks = seq(-40, 40, by = 10), limits = c(-40, 40)) +  
  theme_minimal() +
  theme(
    panel.grid = element_blank(),     # remove all grid lines
    axis.line = element_line(color = "black"),  # show axes
    axis.ticks = element_line(color = "black")  # optional: show ticks
  )
# ggsave(plot = Bland_Altman, file = "Bland_Altman.svg", width = 8, height = 8)
