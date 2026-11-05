#' Plot linked Area-Restricted Phase (ARP) clusters
#'
#' @description Generates a spatial plot of Area-Restricted Phase (ARP) clusters and the 
#'   movement links between them. It visualizes ARPs as convex hulls, highlights ARP 
#'   points, and draws the network edges connecting them.
#'
#' @param data An object of class \code{disperseR} containing movement data with ARP classifications.
#' @param ids Character vector. A selection of individual IDs to plot. If \code{NULL} (default), 
#'   plots all individuals.
#' @param grid_spacing Numeric. Spacing for background reference grid lines. If \code{NULL} 
#'   (default), no grid lines are drawn.
#' @param id.col Character. Name of the column containing individual identifiers. Default is "ID".
#'
#' @return A \code{ggplot} object showing the spatial distribution and network of ARPs.
#' @examples
#' eps <- get.epsilon(sim_states)
#' arp <- get.arp(sim_states, epsilon = eps, delta.days = 2, pct.state1 = 0.2)
#' linked <- link.arp(arp, thres = 24 * 3600)
#' draw.linked.clusters(linked)
#' draw.linked.clusters(linked, ids = "M01", grid_spacing = 5000)
#' @export
#'
#' @importFrom dplyr %>% filter group_by slice ungroup summarise left_join rename mutate distinct tibble
#' @importFrom ggplot2 ggplot geom_polygon geom_segment geom_point scale_shape_manual theme_minimal theme guides labs geom_vline geom_hline facet_wrap element_blank guide_legend aes
#' @importFrom purrr map2_dfr
#' @importFrom tidyr expand_grid unnest
#' @importFrom grDevices chull
draw.linked.clusters <- function(data, 
                                 ids = NULL, 
                                 grid_spacing = NULL,
                                 id.col = "ID") {
  
  # Checks
  if (!inherits(data, "disperseR")) {
    stop("Input must be an object of class 'disperseR'.")
  }
  
  # Datos de coordenadas
  df <- data$data
  
  # Define internal ID field for consistency
  if (!id.col %in% names(df)) {
    stop(paste0("ID column '", id.col, "' not found in the data."))
  }
  df$id_x_field <- df[[id.col]]
  
  # Filtrado por ID
  if (!is.null(ids)) {
    ids <- as.character(ids)
    df <- df %>% dplyr::filter(id_x_field %in% ids)
    
    # Check residences for individual
    if (!any(df$arp)) {
      stop(sprintf(
        "ID '%s' has no ARP points and cannot be plotted.", ids
      ))
    }
  }
  
  ids_to_plot <- unique(df$id_x_field)
  
  # Filtrar también los graphs 
  if ("cluster_graphs" %in% names(data)) {
    graphs_filtered <- data$cluster_graphs[as.character(ids_to_plot)]
  } else if ("arp_graphs" %in% names(data)) {
    graphs_filtered <- data$arp_graphs[as.character(ids_to_plot)]
  } else {
    graphs_filtered <- NULL
  }
  
  # Solo puntos de residencia
  df_res <- df %>% dplyr::filter(arp)
  df_link <- df %>% dplyr::filter(arp_link)
  has_links <- nrow(df_link) > 0
  
  # Calcular convex hull por cluster_id (Corregido: NO es linked_clusters)
  hulls <- df_res %>%
    dplyr::group_by(id_x_field, cluster_id) %>%
    dplyr::slice(grDevices::chull(x, y)) %>%
    dplyr::ungroup()
  
  # Función para edges de un ID
  get_edges_df <- function(indiv_id, g_info) {
    if (is.null(g_info) || is.null(g_info$all_edges) || nrow(g_info$all_edges) == 0) {
      return(dplyr::tibble(
        from = integer(0),
        to = integer(0),
        x_from = numeric(0),
        y_from = numeric(0),
        x_to = numeric(0),
        y_to = numeric(0),
        id_x_field = character(0)
      ))
    }
    
    # Centroides calculados sobre el cluster_id original
    centroids <- df_res %>%
      dplyr::filter(id_x_field == indiv_id) %>%
      dplyr::group_by(cluster_id) %>%
      dplyr::summarise(cx = mean(x), cy = mean(y), .groups = "drop")
    
    edges_df <- g_info$all_edges %>%
      dplyr::filter(from != to) %>%
      dplyr::distinct() %>%
      dplyr::left_join(centroids, by = c("from" = "cluster_id")) %>%
      dplyr::rename(x_from = cx, y_from = cy) %>%
      dplyr::left_join(centroids, by = c("to" = "cluster_id")) %>%
      dplyr::rename(x_to = cx, y_to = cy) %>%
      dplyr::filter(!is.na(x_from) & !is.na(x_to)) %>%
      dplyr::mutate(id_x_field = indiv_id)
    
    return(edges_df)
  }
  
  edges_all <- purrr::map2_dfr(ids_to_plot, graphs_filtered, get_edges_df, .id = NULL)
  if (nrow(edges_all) == 0) {
    edges_all <- NULL
  }  
  
  # Base plot
  p <- ggplot2::ggplot() +
    ggplot2::geom_polygon(data = hulls, ggplot2::aes(x = x, y = y, group = cluster_id, fill = as.factor(cluster_id)),
                          alpha = 0.2, color = NA) +
    {if (!is.null(edges_all)) ggplot2::geom_segment(data = edges_all, ggplot2::aes(x = x_from, y = y_from, xend = x_to, yend = y_to),
                                                    color = "grey40", linetype = "solid", linewidth = 0.8)}  +
    ggplot2::geom_point(data = df_res, ggplot2::aes(x = x, y = y, color = as.factor(cluster_id)), size = 1) +
    {if (has_links) ggplot2::geom_point(data = df_link,
                                        ggplot2::aes(x = x, y = y, shape = "ARP link"),
                                        color = "grey40", size = 2)} +
    {if (has_links) ggplot2::scale_shape_manual(
      name = NULL,
      values = c("ARP link" = 8),
      guide = ggplot2::guide_legend(override.aes = list(color = "grey40", size = 2))
    )} +  
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid = ggplot2::element_blank()) +
    ggplot2::guides(fill = "none") +
    ggplot2::labs(x = "X", y = "Y", color = "Cluster ID")
  
  # Añadir líneas de referencia tipo mapa, calculadas por individuo
  if (!is.null(grid_spacing)) {
    grid_lines <- df_res %>%
      dplyr::group_by(id_x_field) %>%
      dplyr::summarise(
        x_breaks = list(seq(floor(min(x)), ceiling(max(x)), by = grid_spacing)),
        y_breaks = list(seq(floor(min(y)), ceiling(max(y)), by = grid_spacing)),
        .groups = "drop"
      ) %>%
      tidyr::expand_grid() 
    
    # Líneas verticales
    p <- p + ggplot2::geom_vline(
      data = grid_lines %>% tidyr::unnest(x_breaks),
      ggplot2::aes(xintercept = x_breaks, group = id_x_field),
      color = "grey80", linewidth = 0.3, linetype =  "dashed"
    )
    
    # Líneas horizontales
    p <- p + ggplot2::geom_hline(
      data = grid_lines %>% tidyr::unnest(y_breaks),
      ggplot2::aes(yintercept = y_breaks, group = id_x_field),
      color = "grey80", linewidth = 0.3, linetype =  "dashed"
    )
  }
  
  # Facet solo si hay más de un individuo filtrado
  if (length(ids_to_plot) > 1) {
    p <- p + ggplot2::facet_wrap(~id_x_field, scales = "free")
  }
  
  return(p)
}