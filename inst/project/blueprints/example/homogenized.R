blueprint(
  "mt_cars",
  description = "Homogenized Qitabi School characteristics for 2016-17",
  annotate = TRUE,
  command = {
    our_mtcars <- data.table::copy(mtcars)
    data.table::setDT(our_mtcars, keep.rownames = TRUE[])

    pnl <- panelcleaner::enpanel("MTCARS_PANEL", our_mtcars) %>%
      panelcleaner::add_mapping(item_mapping) %>%
      panelcleaner::homogenize_panel() %>%
      panelcleaner::bind_waves() %>%
      as.data.frame()

    pnl_name <- get_attr(pnl, "panel_name")
    pnl_mapping <- get_attr(pnl, "mapping")

    pnl <-
      pnl

    class(pnl) <- c("mapped_df", class(pnl))
    set_attrs(pnl, mapping = pnl_mapping, panel_name = pnl_name)
  }
) %>%
  bp_include_panelcleaner_meta()
