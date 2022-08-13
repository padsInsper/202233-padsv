#' Author:
#' Subject:


# Import -----------------------------------------------------------------------
# readr::write_rds(d, "")

library(ggplot2)


# grafico 1 ---------------------------------------------------------------


items |> 
  dplyr::count(types) |> 
  dplyr::mutate(types = forcats::fct_reorder(types, n)) |> 
  dplyr::filter(n > 100) |> 
  dplyr::mutate(n = n/1000) |> 
  ggplot(aes(x = n, y = types)) +
  geom_col(fill = "#8ae3d7", width = .5) +
  geom_label(aes(label = round(n, 2), x = n/2)) +
  theme_dark(16) +
  labs(
    x = "Quantidade\n(milhares)",
    y = "Forma de pagamento",
    title = "Formas de pagamento mais comuns",
    subtitle = "Considerando tipos com mais de 100 observações",
    caption = "Fonte: Olist"
  ) +
  theme(
    panel.background = element_rect(fill = "gray20"),
    plot.background = element_rect(fill = "gray10"),
    text = element_text(family = "serif", colour = "white"),
    axis.text = element_text(family = "serif", colour = "white"),
    panel.grid.minor = element_blank()
  )


# grafico 2 ---------------------------------------------------------------

items |> 
  dplyr::mutate(
    data = as.Date(order_purchase_timestamp),
    data = lubridate::floor_date(data, "month"),
    estado = forcats::fct_other(
      seller_state, 
      keep = c("SP", "RJ"), 
      other_level = "Outros"
    )
  ) |>
  dplyr::filter(
    data >= "2017-01-01",
    data <= "2018-07-01"
  ) |> 
  dplyr::count(data, estado) |> 
  ggplot() +
  aes(x = data, y = n, colour = estado) +
  geom_line(size = 2) +
  scale_color_viridis_d(begin = .2, end = .8) +
  labs(
    x = "Data", 
    y = "Quantidade", 
    title = "São Paulo tem mais vendas",
    subtitle = "O que é esperado, pois a população é maior 😬",
    caption = "Fonte: Olist",
    color = "Estado"
  ) +
  scale_x_date(
    date_breaks = "3 month", 
    date_labels = "%b\n%Y"
  ) +
  theme_light(15) +
  theme(
    legend.position = "bottom"
  )
  

# grafico 04 --------------------------------------------------------------

estados <- geobr::read_state()

set.seed(42)
items |> 
  dplyr::count(
    seller_state,
    geolocation_lat_seller,
    geolocation_lng_seller,
    geolocation_lat_customer,
    geolocation_lng_customer
  ) |> 
  dplyr::filter(seller_state %in% c("SP", "MG", "RJ")) |> 
  dplyr::slice_sample(n = 1000) |> 
  ggplot() +
  geom_sf(data = estados, fill = "gray95", size = .1) +
  geom_curve(
    mapping = aes(
      x = geolocation_lng_seller,
      y = geolocation_lat_seller,
      xend = geolocation_lng_customer,
      yend = geolocation_lat_customer
    ), 
    arrow = arrow(length = unit(0.1, "inches")),
    curvature = .2,
    alpha = .2,
    colour = "royalblue"
  ) +
  facet_wrap(~seller_state, strip.position = "bottom") +
  theme_void(base_size = 16) +
  labs(
    title = "Para onde vão as compras?",
    subtitle = "Comparando São Paulo, Minas Gerais e Rio de Janeiro",
    caption = "Fonte: Olist"
  ) 




