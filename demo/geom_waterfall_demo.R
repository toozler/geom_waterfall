energy_example <- tribble(
  
  ~category, ~value, ~type,
  "Budget",  120, "total",
  "Variation", 30, "step",
  "Entitlement", NA, "total",
  "Availability", -15, "step",
  "Performance", -20, "step",
  "Curtailment", -30, "step",
  "Unknown", -10, "step",
  "Gross", NA, "total",
  "Battery", 15, "step",
  "Line loss", -10, "step",
  "Net", NA, "total"
)


waterfall_plot(waterfall_data = energy_example, 
               fill_colors = list(total = "steelblue", up = "forestgreen", down = "tomato"),
               label = 'all', #TODO: pass fill colors to label
               connectors = FALSE,
               arrows = TRUE, 
               bar_width = 1) +
  theme_minimal(base_size = 16) +
  labs(title  = "Energy Balance", 
       subtitle = "An example for geom_waterfall with hypothetical data",
       y = "MWh")






reservoir_example <- tribble(
  
  ~category, ~value, ~type,
  
  "Forecasted\nVolume", 100, "total",
  "Anomaly", 30, "step",
  "Starting Volume", NA, "total",
  "Inflows", 30, "step",
  "Irrigation", -10, "step",
  "Evaporation", -10, "step",
  "Seepage", -5, "step",
  "Hydropower", -30, "step",
  "Ending\nVolume", NA, "total"
  
  
)

waterfall_plot(waterfall_data = reservoir_example,
               fill_colors = list(total = "steelblue4", up = "steelblue2", down = "orange"),
               label = 'none',
               arrows = TRUE,
               connectors = TRUE) +
  theme_minimal(base_size = 16) +
  labs(title  = "Reservoir Storage", 
       y = "Volume")
