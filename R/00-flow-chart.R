# flow-chart

library(tidyverse)
library(igraph)
library(showtext)
library(rcartocolor)
library(RColorBrewer)
library(ggarrow)

home <- here::here()

goldilocks <- tibble(from = c("• Fit environmental SDMs \n•Identify parsimonious \ncovariates",
                              "• Fit environmental SDMs \n•Identify parsimonious \ncovariates",
                              "• Predict on grid \n•Calculate experienced \nenvironmental conditions \nas weighted quantiles",
                              "• Fit spatial trend models \nwith GMRFs and no \nenvironmental covariates",
                              "• Predict on grid \n• Extract spatial trends\nin biomass density",
                              "• Fit spatial trends in biomass \ndensity to environmental \ntrends using Random Forest"),
                     to = c("• Evalute conditional effects",
                            "• Predict on grid \n•Calculate experienced \nenvironmental conditions \nas weighted quantiles",
                            "• Evalute trends in experienced \nenvironmental conditions \nover time",
                            "• Predict on grid \n• Extract spatial trends\nin biomass density",
                            "• Fit spatial trends in biomass \ndensity to environmental \ntrends using Random Forest",
                            "• Evaluate variable importance, \nconditional effects, \npartial dependence plots"))


g = graph_from_data_frame(goldilocks, directed = TRUE)
coords = layout_as_tree(g)
colnames(coords) = c("x", "y")

output_df = as_tibble(coords) %>%
  mutate(step = vertex_attr(g, "name"),
         label = gsub("\\d+$", "", step),
         #x = x*-1,
         model = factor(c(2, 1, 2, 1, 2, 1, 1, 1)),
         type = factor(c(1, 1, 1, 1, 2, 1, 1, 2)))

output_df %>% as.data.frame()

plot_nodes = output_df %>%
  mutate(xmin = x + 0.45,
         xmax = x - 0.45,
         ymin = y - 0.3,
         ymax = y + 0.3)

plot_edges = goldilocks %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = c("from", "to"),
               names_to = "s_e",
               values_to = "step") %>%
  left_join(plot_nodes, by = "step") %>%
  select(-c(label, type, y, xmin, xmax)) %>%
  mutate(y = ifelse(s_e == "from", ymin, ymax)) %>%
  select(-c(ymin, ymax))

pal <- brewer.pal(n = 8, name = "Dark2")[c(1, 7)]

ggplot(data = plot_nodes) +
  geom_rect(aes(xmin = xmin, ymin = ymin, 
                xmax = xmax, ymax = ymax, 
                fill = type, alpha = model), color = NA) +
  geom_text(aes(x = x, y = y, label = label),
            color = "gray10") +
  geom_arrow(data = plot_edges, aes(x = x, y = y, group = id),
             colour = "gray30", length = 7,
             arrow_head = arrow_head_wings(offset = 20, inset = 70)) +
  theme_void() + 
  guides(fill = "none", color = "none", alpha = "none") +
  #scale_fill_brewer(palette = "Set1") +
  #scale_color_brewer(palette = "Set1") +
  scale_fill_manual(values = pal) +
  scale_color_manual(values = pal) +
  scale_alpha_manual(values = c(0.2, 0.7))
  
ggsave(paste0(home, "/figures/flow.pdf"), width = 19, height = 15, units = "cm", device = cairo_pdf)
