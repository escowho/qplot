fix_codes <- function(x){
  dplyr::case_when(x==1~1,
                   x==2~2,
                   x==4~3,
                   x==5~4,
                   x==6~5)
}

nps1 <- readRDS("./data-raw/odat_relationship.rds") %>%
  qpack::create_id(response_id, remove=TRUE) %>%
  dplyr::mutate(year = lubridate::year(recorded_date)) %>%
  dplyr::filter(year >= 2021) %>%
  dplyr::select(response_id=id, ltr=q2, q31_1:q31_8, year, fuel_type, payment_type) %>%
  dplyr::mutate(payment_type = case_when(payment_type=="Fixed"~1,
                                         payment_type=="Variable"~2,
                                         .default=3),
                fuel_type = case_when(fuel_type=="Electricity"~1,
                                      fuel_type=="Gas"~2,
                                      fuel_type=="Dual Fuel"~3,
                                      .default=4)) %>%
  dplyr::rename(price=q31_1,
         billing=q31_2,
         payment_options=q31_3,
         customer_service=q31_4,
         online_account=q31_5,
         registration_process=q31_6,
         services_products=q31_7,
         smart_tech=q31_8) %>%
  dplyr::mutate_at(dplyr::vars(price:smart_tech), fix_codes) %>%
  dplyr::mutate(nps=qpack::nps_score(ltr)) %>%
  dplyr::relocate(nps, .after=ltr) %>%
  dplyr::select(-ltr)

usethis::use_data(nps1, overwrite=TRUE)
rm(nps1, fix_codes)
