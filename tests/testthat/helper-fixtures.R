## Shared PK simulation for VPC tests: data_sad (PK records), the bundled
## pkmodel, ODV as the observed DV. Memoised per argument set; the fixed
## default seed makes repeated simulations identical, so sharing is safe.
.sim_cache <- new.env(parent = emptyenv())

sim_pk <- function(replicates = 10, ...) {
  key <- paste(deparse(list(replicates = replicates, ...)), collapse = "")
  if (is.null(.sim_cache[[key]])) {
    .sim_cache[[key]] <- df_mrgsim_replicate(
      data = dplyr::filter(data_sad, CMT != 3),
      model = model_mread_load("pkmodel"),
      replicates = replicates, dv_var = "ODV", ...)
  }
  .sim_cache[[key]]
}
