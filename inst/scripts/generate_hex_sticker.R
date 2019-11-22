
# Load packages -----------------------------------------------------------

library(SpeedyMarkov)
library(hexSticker)
library(ggplot2)


# Make plot ---------------------------------------------------------------

plot <- example_simulation %>% 
        ggplot() + 
        labs(x = "", y = "") +
        theme_void() + theme_transparent()+
        theme(legend.position = "none",
              panel.background = element_blank()) 

# Make sticker ------------------------------------------------------------

sticker(subplot = plot,
        package = "SpeedyMarkov", 
        p_size = 18, 
        p_color = "#FFFFFFDD",
        s_x = 1,
        s_y= 0.75, 
        s_width=1.6, 
        s_height=0.8,
        h_fill = "#646770",
        h_color = "#b3ccff",
        filename="./man/figures/logo.png",
        url = "https://samabbott.co.uk/SpeedyMarkov",
        u_color = "#FFFFFFDD",
        u_size = 3.5,
        dpi = 330,
        asp = 1)


