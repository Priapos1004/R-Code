suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(rlang))
suppressPackageStartupMessages(library(RColorBrewer))

load("data/ghcnd_stations.Rdata")
load("data/ghcnd_values.Rdata")

knitr::kable(ghcnd_stations)
summary(ghcnd_values)

ghcnd_values %>% group_by(ID) %>% count() %>% knitr::kable()

ghcnd <- ghcnd_values %>% left_join(ghcnd_stations, by="ID") %>%
  mutate(date = as.Date(paste0(Year, "/", Month, "/", Day)))

ghcnd_1 <- ghcnd %>% group_by(Name, date) %>%
  summarise(day_max = max(Value), day_min = min(Value)) %>%
  ungroup() %>%
  pivot_longer(cols = starts_with("day_"), names_to="metric", values_to="value", values_drop_na = T)

ggplot(ghcnd_1, aes(x = date, y = value, group = metric, colour = metric)) +
  geom_line() +
  facet_wrap(~Name,ncol=1)

plot_temperature <- function(time_group){
  ghcnd %>% group_by(Name, !!sym(time_group)) %>%
    summarise(time_max = max(Value), time_min = min(Value)) %>%
    ungroup() %>%
    pivot_longer(cols = starts_with("time_"), names_to="metric", values_to="value", values_drop_na = F) %>%
    ggplot(aes(x = !!sym(time_group), y = value, group = metric, colour = metric)) +
    geom_line() +
    facet_wrap(~Name,ncol=1)
}

plot_temperature("Year")
plot_temperature("Month")
plot_temperature("Day")

ghcnd_2 <- ghcnd %>% group_by(Name, Month, Day) %>%
  summarise(day_max = max(Value), day_min = min(Value)) %>%
  ungroup() %>%
  pivot_longer(cols = starts_with("day_"), names_to="metric", values_to="value", values_drop_na = T) %>%
  mutate(month_groups = paste0(metric, "-", Month))

unique_combinations <- ghcnd_2 %>%
  select(metric, Month) %>%
  distinct() %>%
  arrange(metric, Month)

# Assuming we may have up to 12 different months but let's count the actual unique ones
unique_months <- length(unique(unique_combinations$Month))
min_colors <- colorRampPalette(brewer.pal(9, "Blues"))(unique_months)
max_colors <- colorRampPalette(brewer.pal(9, "Reds"))(unique_months)

# Prepare a color mapping for each unique metric-month combination
colors <- c()
for (month in unique(unique_combinations$Month)) {
  if ("day_min" %in% unique_combinations$metric[unique_combinations$Month == month]) {
    colors[paste0("day_min-", month)] <- min_colors[which(unique(unique_combinations$Month) == month)]
  }
  if ("day_max" %in% unique_combinations$metric[unique_combinations$Month == month]) {
    colors[paste0("day_max-", month)] <- max_colors[which(unique(unique_combinations$Month) == month)]
  }
}

ggplot(ghcnd_2, aes(x = Day, y = value, color = month_groups)) +
  geom_line() +
  facet_wrap(~Name, ncol = 1) +
  scale_color_manual(values = colors) +
  theme_minimal() +
  labs(color = "Metric-Month")
