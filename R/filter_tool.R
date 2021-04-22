################################################################
# ローカル関数
################################################################
new_hose_filter <- function(reti_data){

 reti_data %>%
    reti::reti_filter_by_kind("R") %>%
    dplyr::filter(stringr::str_detect(`建物用途`, "^住宅$") |
                    stringr::str_detect(`建物用途`, "^住宅、駐車場$")) %>%
    dplyr::filter(howold_building <= 2) %>%
    dplyr::filter(land_size <= 500) %>%
    retiex::filter_by_sd(`取引総額`)
}
