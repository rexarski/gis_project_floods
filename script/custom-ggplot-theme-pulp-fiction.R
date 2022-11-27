if (!require('pacman')) install.packages('pacman')
pacman::p_load(
  tidyverse,
  ggtext,
  ggthemes,
  MetBrewer,
  glue,
  tidytext,
  showtext,
  ggrepel,
  lubridate
)

font_add_google(
  name = 'Roboto Mono',
  family = 'Roboto Mono')
font_add_google(
  name = 'Roboto Condensed',
  family = 'Roboto Condensed')

showtext_auto()

theme_pulp_fiction <- function() {
  theme_fivethirtyeight() +
    theme(
      plot.background = element_rect(
        fill = '#F9EFE6',
        color = '#F9EFE6'),
      panel.background = element_rect(
        fill = '#F9EFE6',
        color = '#F9EFE6'),
      legend.position = 'top',
      legend.background = element_rect(
        fill = '#F9EFE6',
        color = '#F9EFE6'),
      legend.key = element_rect(
        fill = '#F9EFE6',
        color = '#F9EFE6'),
      legend.title = element_text(
        size = 7,
        color = '#000000'),
      legend.text = element_text(
        size = 7,
        color = '#000000'),
      axis.title.y = element_text(
        vjust = 0.2,
        size = 14,
        family = 'Roboto Mono',
        face = 'bold'), # repositioning
      axis.title.x = element_text(
        hjust = 0.5,
        size = 14,
        family = 'Roboto Mono',
        face = 'bold'), # repositioning
      axis.text.x = element_text(
        size = 8),
      axis.text.y = element_text(
        size = 8,
        angle = 30),
      text = element_text(
        family = 'Roboto Mono',
        color = '#3B372E'),
      plot.title = element_text(
        family = 'Roboto Mono',
        face = 'bold',
        size = 20,
        hjust = 0.5),
      plot.subtitle = element_markdown(
        family = 'Roboto Condensed',
        size = 12,
        hjust = 0.5),
      plot.caption = element_markdown(
        size = 10,
        family = 'Roboto Condensed'),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.margin = margin(20, 20, 10, 10),
      strip.background = element_rect(
        fill = '#D8C8B3'),
      strip.text = element_text(
        family = 'Roboto Mono',
        face = 'bold',
        size = 8,
        color = '#654321'))
}