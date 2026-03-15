library(ggplot2)
library(readxl)
library(tidyr)
library(dplyr)
library(gghalves)

##1. SFigure 8A
df <- read_excel("C:\\DDA-BERT\\DDA-BERT_0105\\DDA-BERT_Figures_0105.xlsx", sheet = 1)

tool_labels <- c(
  rep("FragPipe (MSBooster)", 5),
  rep("Sage", 5),
  rep("MS2Rescore", 5),
  rep("AlphaPept", 5),
  rep("AlphaPeptDeep", 5),
  rep("DDA-BERT", 5)
)

colnames(df)[-1] <- paste0(tool_labels, "_", 1:5)
df_long <- df %>%
  pivot_longer(
    cols = -filename,
    names_to = "method_rep",
    values_to = "count"
  ) %>%
  mutate(
    method = sub("_.*", "", method_rep)
  )

df_long <- df_long %>%
  mutate(
    method_rep = case_when(
      grepl("_1$", method_rep) ~ "0.002",
      grepl("_2$", method_rep) ~ "0.004",
      grepl("_3$", method_rep) ~ "0.006",
      grepl("_4$", method_rep) ~ "0.008",
      grepl("_5$", method_rep) ~ "0.01",
      TRUE ~ method_rep
    )
  )

df_long <- df_long %>%
  mutate(
    dataset = case_when(
      grepl("180min_", filename) ~ "yeast",
      grepl("20210715_Exploris1", filename) ~ "human",
      grepl("Arabidopsis_Cold", filename) ~ "Arabidopsis",
      grepl("HeLa_digest", filename) ~ "trace_sample",
      grepl("Ref6496_VK", filename) ~ "fruit_fly",
      grepl("20230108_AST", filename) ~ "astral",
      grepl("Substrate_comp_Rapid", filename) ~ "single_cell",
      grepl("20141208", filename) ~ "hla",
      TRUE ~ "other"
    )
  )

colnames(df_long) <- c("file_name", "fdr_cutoff", "PSM_num", "Tools", "Group")

df_long$fdr_cutoff <- as.numeric(df_long$fdr_cutoff)

df_long$Tools <- factor(df_long$Tools, levels = c("AlphaPept","AlphaPeptDeep","MS2Rescore","Sage","FragPipe (MSBooster)","DDA-BERT"))


library(ggplot2)
library(patchwork)

df_long$Group <- as.character(df_long$Group)

tool_colors <- c(
  "AlphaPept"              = "#7A1A24",
  "AlphaPeptDeep"          = "#FDAE61",
  "DDA-BERT"               = "#D73027",
  "FragPipe (MSBooster)"   = "#1A9850",
  "Sage"                   = "#762A83",
  "MS2Rescore"                = "#4575B4"
)

tool_breaks <- levels(df_long$Tools)
p_hla <- ggplot(df_long %>% filter(Group == "hla"), 
                        aes(x = fdr_cutoff, y = PSM_num, color = Tools, group = Tools)) +
  stat_summary(fun = mean, geom = "line", linewidth = 0.6) +
  labs(x = "FDR threshold", y = "# PSMs", title = "hla Proteomics") +
  scale_color_manual(breaks = tool_breaks, values = tool_colors) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 9),
    axis.ticks = element_line(color = "black", size = 0.3),
    axis.ticks.length = unit(0.15, "cm"),
    legend.text = element_text(size = 9),
    legend.title = element_blank(),
    legend.background = element_rect(fill = alpha('white', 0.8), color = 'black'),
    panel.grid = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.6),
    plot.margin = margin(3, 3, 3, 3),
    legend.position = "bottom"
  ) +
  guides(color = guide_legend(nrow = 1))


ggsave("C:\\DDA-BERT\\DDA-BERT_0105\\main_figure\\SFig8_hla_psm.pdf", width = 4, height = 4, units = "in")




##2. SFigure 8B

rm(list=ls())
df <- read_excel("C:\\DDA-BERT\\DDA-BERT_0105\\DDA-BERT_Figures_0105.xlsx", sheet = 3)

df_long <- df %>%
  pivot_longer(
    cols = c('FragPipe (MSBooster)', Sage, MS2Rescore, AlphaPept, AlphaPeptDeep, `DDA-BERT`),
    names_to = "Tool",
    values_to = "precursor_num"
  )

df_long <- df_long %>%
  mutate(
    dataset = case_when(
      grepl("^20141208", file_name) ~ "hla",
      TRUE ~ "other"
    )
  )

df_long <- df_long %>%
  filter(dataset == "hla")

tool_colors <- c(
  "AlphaPept"              = "#7A1A24",
  "AlphaPeptDeep"          = "#FDAE61",
  "DDA-BERT"               = "#D73027",
  "FragPipe (MSBooster)"   = "#1A9850",
  "Sage"                   = "#762A83",
  "MS2Rescore"             = "#4575B4"
)

tool_order <- c("AlphaPept", "AlphaPeptDeep", "MS2Rescore", "Sage", "FragPipe (MSBooster)", "DDA-BERT")
df_long$Tool <- factor(df_long$Tool, levels = tool_order)

df_summary <- df_long %>%
  group_by(Tool) %>%
  summarise(
    mean_val = mean(precursor_num, na.rm = TRUE),
    sd_val = sd(precursor_num, na.rm = TRUE),
    .groups = "drop"
  )

p <- ggplot(df_summary, aes(x = Tool, y = mean_val, fill = Tool)) +
  stat_summary(fun = mean, geom = "bar", width = 0.6) +
  geom_errorbar(
    aes(ymin = mean_val - sd_val, ymax = mean_val + sd_val),
    data = df_summary,
    width = 0.2,
    position = position_dodge(0.6)
  ) +
  geom_point(data = df_long, aes(x = Tool, y = precursor_num, color = Tool), 
             position = position_jitter(width = 0.2, height = 0), 
             size = 0.3, alpha = 1) +
  facet_wrap(~ dataset, ncol = 1, scales = "free_y") +
  scale_fill_manual(values = tool_colors) +
  scale_color_manual(values = tool_colors) +
  labs(
    x = NULL,
    y = "# precursors at 1% FDR"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  theme_classic(base_size = 12) +
  theme(
    strip.text = element_blank(),
    strip.background = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(size = 11),
    legend.position = "none",
    panel.border = element_rect(color = "black", fill = NA)
  )

ggsave("C:\\DDA-BERT\\DDA-BERT_0105\\main_figure\\SFig8_hla_precursor.pdf", p,
       width = 4, height = 4, units = "in")



##3. SFigure 8C

rm(list=ls())
df <- read_excel("C:\\DDA-BERT\\DDA-BERT_0105\\DDA-BERT_Figures_0105.xlsx", sheet = 4)

df_long <- df %>%
  pivot_longer(
    cols = c('FragPipe (MSBooster)', Sage, MS2Rescore, AlphaPept, AlphaPeptDeep, `DDA-BERT`),
    names_to = "Tool",
    values_to = "peptide_num"
  )

df_long <- df_long %>%
  mutate(
    dataset = case_when(
      grepl("^20141208", file_name) ~ "hla",
      TRUE ~ "other"
    )
  )

df_long <- df_long %>%
  filter(dataset == "hla")

tool_colors <- c(
  "AlphaPept"              = "#7A1A24",
  "AlphaPeptDeep"          = "#FDAE61",
  "DDA-BERT"               = "#D73027",
  "FragPipe (MSBooster)"   = "#1A9850",
  "Sage"                   = "#762A83",
  "MS2Rescore"             = "#4575B4"
)

tool_order <- c("AlphaPept", "AlphaPeptDeep", "MS2Rescore", "Sage", "FragPipe (MSBooster)", "DDA-BERT")
df_long$Tool <- factor(df_long$Tool, levels = tool_order)

df_summary <- df_long %>%
  group_by(Tool) %>%
  summarise(
    mean_val = mean(peptide_num, na.rm = TRUE),
    sd_val = sd(peptide_num, na.rm = TRUE),
    .groups = "drop"
  )

p <- ggplot(df_summary, aes(x = Tool, y = mean_val, fill = Tool)) +
    stat_summary(fun = mean, geom = "bar", width = 0.6) +
    geom_errorbar(
        aes(ymin = mean_val - sd_val, ymax = mean_val + sd_val),
        data = df_summary,
        width = 0.2,
        position = position_dodge(0.6)
    ) +
    geom_point(data = df_long, aes(x = Tool, y = peptide_num, color = Tool), 
               position = position_jitter(width = 0.2, height = 0), 
               size = 0.3, alpha = 1) +
    facet_wrap(~ dataset, ncol = 1, scales = "free_y") +
    scale_fill_manual(values = tool_colors) +
    scale_color_manual(values = tool_colors) +
    labs(
        x = NULL,
        y = "# peptides at 1% FDR"
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    theme_classic(base_size = 12) +
    theme(
        strip.text = element_blank(),
        strip.background = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size = 11),
        legend.position = "none",
        panel.border = element_rect(color = "black", fill = NA)
    )

ggsave("C:\\DDA-BERT\\DDA-BERT_0105\\main_figure\\SFig8_hla_peptide.pdf", p,
       width = 4, height = 4, units = "in")



##4. SFigure 8D

library(ggplot2)
library(readr)
library(tidyr)
library(dplyr)

rm(list=ls())
data <-  read.csv("C:\\DDA-BERT\\DDA-BERT_0105\\DBSE_all_proteins_stats_20260111.csv")

colnames(data) <- c("filename", "FragPipe (MSBooster)", "DDA-BERT", "Sage", "AlphaPept","AlphaPeptDeep", "MS2Rescore")

df <- pivot_longer(data, cols = starts_with(c("FragPipe","Sage","MS2Rescore","AlphaPept","AlphaPeptDeep","DDA-BERT")), 
                   names_to = c("Tool"), 
                   values_to = "Proteins")
df_long <- df %>%
    mutate(
        dataset = case_when(
            grepl("^20141208", filename) ~ "hla",
            TRUE ~ "other"
        )
    )

df_long <- df_long %>%
  filter(dataset == "hla")

tool_colors <- c(
  "AlphaPept"              = "#7A1A24",
  "AlphaPeptDeep"          = "#FDAE61",
  "DDA-BERT"               = "#D73027",
  "FragPipe (MSBooster)"   = "#1A9850",
  "Sage"                   = "#762A83",
  "MS2Rescore"             = "#4575B4"
)

tool_order <- c("AlphaPept", "AlphaPeptDeep", "MS2Rescore", "Sage", "FragPipe (MSBooster)", "DDA-BERT")
df_long$Tool <- factor(df_long$Tool, levels = tool_order)

df_summary <- df_long %>%
  group_by(Tool) %>%
  summarise(
    mean_val = mean(Proteins, na.rm = TRUE),
    sd_val = sd(Proteins, na.rm = TRUE),
    .groups = "drop"
  )

p <- ggplot(df_summary, aes(x = Tool, y = mean_val, fill = Tool)) +
    stat_summary(fun = mean, geom = "bar", width = 0.6) +
    geom_errorbar(
        aes(ymin = mean_val - sd_val, ymax = mean_val + sd_val),
        position = position_dodge(0.6), width = 0.2
    ) +
    geom_point(data = df_long, aes(x = Tool, y = Proteins, color = Tool), 
               position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.6), 
               size = 0.3, alpha = 1) +
    facet_wrap(~ dataset, ncol = 1, scales = "free_y") +
    scale_fill_manual(values = tool_colors) +
    scale_color_manual(values = tool_colors) +
    labs(
        x = NULL,
        y = "# protein groups at 1% FDR"
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    theme_minimal(base_size = 12) +
    theme(
        strip.text = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA), 
        legend.position = "none",
        panel.grid = element_blank(),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size = 10)
    )

ggsave(filename = "C:\\DDA-BERT\\DDA-BERT_0105\\suppl_figure\\SFig8_hla_protein_group.pdf", 
       plot = last_plot(),
       device = "pdf",
       width = 8,
       height = 8, 
       dpi = 300,
       units = "in") 

