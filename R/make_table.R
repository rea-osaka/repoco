#' make table of simple mean change
#'
#' 平均の推移に関する簡易な表を作る
#'
#' @param df data.frame of reti
#' @param title 表のタイトル
#' @param keta 平均額の省略桁
#' @param unit 平均額の単位
#'
#' @export
#'
simple_mean_table <- function(df, title = "", keta = 3, unit = "千円"){
  title <- ifelse( title == "", title, paste0(title,"の"))

  df %>%
    retiex::retiex_summary() %>%
    retiex::add_change_rate_diff_cols() %>%
    retiex::add_change_rate_cols() %>%
    dplyr::select(date, mean, cr_mean, crd_mean, count) %>%
    dplyr::arrange(desc(date)) %>%
    dplyr::transmute(`取引時点` = retiex::style_quarter_date(date),
                     `平均` = retiex::style_yen(mean, keta, unit),
                     `対前期比` = retiex::style_percent(cr_mean, 1, F),
                     `対前期増減比` = retiex::style_percent(crd_mean, 1,T),
                     `事例数` =
                       paste0(
                         format(count, scientific = F, big.mark = ","),
                         "件")) %>%

    kableExtra::kbl(
      booktabs = T,
      align = "r",
      caption = paste0(title,"平均の推移")) %>%

    kableExtra::kable_styling(
      latex_options =
        c( "striped", "scale_down", "hold_position"))
}

#' make table of rolling mean change
#'
#' 移動平均値の推移に関する簡易な表を作る
#'
#' @param df data.frame of reti
#' @param title 表のタイトル
#' @param keta 平均額の省略桁
#' @param unit 平均額の単位
#'
#' @export
#'
rolling_mean_table <- function(df, title = "", keta = 3, unit = "千円"){
  title <- ifelse( title == "", title, paste0(title,"の"))

  df %>%
    retiex::retiex_rolling_summary() %>%
    retiex::add_change_rate_cols() %>%
    retiex::add_change_rate_diff_cols() %>%
    select(roll_term, date, mean, cr_mean, crd_mean, count) %>%
    dplyr::arrange(desc(date)) %>%
    dplyr::transmute(`取引時点` = retiex::style_rolling_term_quarter(date),
                     `移動平均` = retiex::style_yen(mean, keta, unit),
                     `対前期比` = retiex::style_percent(cr_mean, 1, F),
                     `対前期増減比` = retiex::style_percent(crd_mean, 1,T),
                     `事例数` =
                       paste0(
                         format(count, scientific = F, big.mark = ","),
                         "件")) %>%

    kableExtra::kbl(
      booktabs = T,
      align = "r",
      caption = paste0(title,"移動平均の推移")) %>%

    kableExtra::kable_styling(
      latex_options =
        c( "striped", "scale_down", "hold_position"))
}
