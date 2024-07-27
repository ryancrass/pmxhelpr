#' sim_vpc
#'
#' @param data simulation input dataset 
#' @param model mrgsolve model object
#' @param seed random seed
#' @param replicates number of iterations in the simulation
#' @param output_vars character vector of numeric output variables to return (default: c("IPRED", "DV))
#' @param num_vars character vector of numeric variable names from the simulation output to return 
#' @param char_vars character vector of variable names to return
#' @param irep_name character string name of the iteration variable (default: "SIM")
#'
#' @return data.frame of simulated output
#' @export
#'
#' @examples
#' 
#' 



sim_vpc <- function(data, 
                    model,
                    seed = 123456789, 
                    replicates=1000, 
                    output_vars = c(PRED = "PRED", 
                                    IPRED = "IPRED", 
                                    DV = "DV"),
                    num_vars, 
                    char_vars, 
                    irep_name = "SIM",
                    ...) {
  ##Data Rename
  data <- data %>% 
    dplyr::rename(any_of(output_vars)) %>% 
    dplyr::rename("OBSDV" = DV)
  
  ##Define PRED
  data$PRED <- mrgsim_df(zero_re(model), data = data, carry_out = "IPRED")$IPRED 
  
  ##Run Simulation
  withr::with_seed(seed = seed, 
                   
                   simout <- lapply(
                     seq(replicates),
                     function(rep, data, model) {
                       mrgsolve::mrgsim_df(model, data = data,
                                           carry_out = paste(c("PRED", "IPRED", "DV", "OBSDV",
                                                               num_vars), 
                                                             collapse = ","),
                                           recover = paste(char_vars,collapse = ","),
                                           ...) %>% 
                         dplyr::mutate(!!irep_name := rep) %>% 
                         dplyr::select(ID, TIME, PRED, IPRED, SIMDV=DV,OBSDV, everything())} ,
                     data = data,
                     model = model) %>% 
                     dplyr::bind_rows()
  
  )
  
  return(simout)
}
