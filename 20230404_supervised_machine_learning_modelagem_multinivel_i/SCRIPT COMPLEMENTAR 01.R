ggplotly(
  estudante_escola %>%
  group_by(escola) %>%
  mutate(desempenho_medio = mean(desempenho, na.rm = TRUE)) %>% 
  ggplot() +
  geom_point(aes(x = escola, y = desempenho),color = "orange", alpha = 0.5, size = 4) +
  geom_line(aes(x = escola, y = desempenho_medio, 
                group = 1, color = "Desempenho Escolar Médio"), size = 1.5) +
  scale_colour_viridis_d() +
  labs(x = "Escola",
       y = "Desempenho Escolar") +
  theme(legend.title = element_blank(),
        panel.border = element_rect(NA),
        panel.grid = element_line("grey"),
        panel.background = element_rect("white"),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 90))
)



ggplotly(
  ggplot(estudante_escola, aes(x = "",y = desempenho)) +
    geom_boxplot(aes(fill = escola, alpha = 0.7)) +
    geom_jitter(width = 0.1, alpha = 0.5, size = 1.3, color = "darkorchid") +
    scale_fill_viridis_d() +
    labs(y = "Desempenho") +
    theme_classic() +
    ggtitle("Boxplots da variável 'desempenho' para as escolas")
)

# ICC (intraclass correlation):

icc <- 414.1005/(414.1005 + 142.9239)
icc
