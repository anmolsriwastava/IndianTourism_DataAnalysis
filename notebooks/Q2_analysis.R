#Purpose of Visit

df_reason <- read_csv("C:\\Users\\gagan\\OneDrive\\Desktop\\Group 18 DARP\\India-Tourism-Statistics-2019_region-and-reason.csv")

colnames(df_reason) <- c(
  "region", "country", "total_arrivals", "business_pct", 
  "leisure_pct", "medical_pct", "diaspora_pct", "others_pct"
)

purpose_data <- df_reason %>%
  filter(country == "Grand Total") %>%
  select(business_pct, leisure_pct, medical_pct, diaspora_pct, others_pct) %>%
  pivot_longer(
    cols = everything(), 
    names_to = "purpose", 
    values_to = "percentage"
  ) %>%
  mutate(
    purpose = case_when(
      purpose == "leisure_pct" ~ "Leisure & Holiday",
      purpose == "business_pct" ~ "Business & Professional",
      purpose == "diaspora_pct" ~ "Indian Diaspora (VFR)",
      purpose == "medical_pct" ~ "Medical Tourism",
      purpose == "others_pct" ~ "Others"
    ),
    vulnerability_level = ifelse(purpose == "Leisure & Holiday", "Highly Discretionary (High Flight Risk)", "More Rigid/Essential")
  ) %>%
  arrange(desc(percentage)) %>%
  mutate(purpose = factor(purpose, levels = purpose))

ggplot(purpose_data, aes(x = purpose, y = percentage, fill = vulnerability_level)) +
  geom_bar(stat = "identity", color = "black", width = 0.7) +
  geom_text(aes(label = paste0(percentage, "%")), vjust = -0.8, fontface = "bold", size = 5) +
  scale_fill_manual(values = c("Highly Discretionary (High Flight Risk)" = "#d73027", "More Rigid/Essential" = "#4575b4")) +
  scale_y_continuous(limits = c(0, 68)) + 
  labs(
    title = "Purpose Vulnerability: Why Tourists Visited India (2019)",
    subtitle = "Over 57% of all arrivals were purely discretionary leisure travelers,\nleaving the sector highly exposed to global panic",
    
    x = "Stated Purpose of Visit",
    y = "Percentage of Total Arrivals (%)",
    fill = "Travel Elasticity"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold"),
  
    axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1, size = 7, face = "bold"),
    
    plot.background = element_rect(fill = "#f8f9fa", color = NA),
    panel.background = element_rect(fill = "#ffffff", color = "lightgray"),
    panel.grid.major.x = element_blank(),
    plot.margin = margin(t = 15, r = 20, b = 15, l = 15)
  )

# Global Benchmarks vs India

df_world <- read_csv("C:\\Users\\gagan\\OneDrive\\Desktop\\Group 18 DARP\\India-Tourism-Statistics-2001-2019-worldvsindia.csv")


colnames(df_world) <- c(
  "year", "world_millions", "world_pct_change", 
  "india_millions", "india_pct_change", "india_share_pct", "india_rank"
)

benchmark_data <- df_world %>%
  mutate(
    year = as.numeric(year),
    world_pct_change = as.numeric(world_pct_change),
    india_pct_change = as.numeric(india_pct_change)
  ) %>%
  filter(year >= 2015 & year <= 2021) %>%
  select(year, world_pct_change, india_pct_change) %>%
  pivot_longer(
    cols = c(world_pct_change, india_pct_change),
    names_to = "region",
    values_to = "growth_rate"
  ) %>%
  mutate(
    region = ifelse(region == "world_pct_change", "Global Average", "India"),
    
    label_position = ifelse(region == "India", -1, 2) 
  )

ggplot(benchmark_data, aes(x = year, y = growth_rate, color = region, group = region)) +
  geom_line(size = 0.5) +
  geom_point(size = 4) +
  geom_text(aes(label = paste0(growth_rate, "%"), vjust = label_position), fontface = "bold", show.legend = FALSE) +
  
  scale_color_manual(values = c("Global Average" = "#4575b4", "India" = "#d73027")) +
  scale_x_continuous(breaks = seq(2015, 2021, 1)) +
  scale_y_continuous(limits = c(-85, 20)) +
  labs(
    title = "The Global Benchmark: Tourism Growth Rate (2015-2021)",
    subtitle = "Comparing India's crash and delayed recovery against the global tourism market",
    x = "Year",
    y = "Year-over-Year Growth Rate (%)",
    color = "Region"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold"),
    plot.background = element_rect(fill = "#f8f9fa", color = NA),
    panel.background = element_rect(fill = "#ffffff", color = "lightgray")
  )