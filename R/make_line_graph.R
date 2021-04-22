#' make line graph of simple mean change
#'
#' 平均の推移に関する折れ線グラフを作る
#'
#' @param df data.frame of reti
#' @param title 表のタイトル
#'
#' @export
#'
simple_mean_linegraph <- function(df, title = "", keta = 3, unit = "千円"){

  title <- ifelse( title == "", title, paste0(title,"に於ける"))

  df %>%

    # 平均データの作成
    retiex::retiex_summary() %>%
    retiex::add_change_rate_cols() %>%
    dplyr::select(date, mean) %>%
    dplyr::mutate(`取引時点` = retiex::style_quarter_date(date)) %>%

    # 平均推移グラフの作成
    ggplot() +
    ggtitle(paste0(title,"取引総額平均の推移"))+
    xlab(label = "取引時点") +
    ylab(label = "取引総額平均") +
    scale_y_continuous(labels =
                         function(v){
                           retiex::style_yen(v, keta, unit)
                         }) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    geom_line(
      mapping = aes(x = `取引時点`, y = mean , group = 1),
      color = "red")
}


#' make line graph of simple mean change
#'
#' 移動平均の推移に関する折れ線グラフを作る
#'
#' @param df data.frame of reti
#' @param title 表のタイトル
#'
#' @export
#'
rolling_mean_linegraph <- function(df, title = "", keta = 3, unit = "千円"){

  title <- ifelse( title == "", title, paste0(title,"に於ける"))

  df %>%
    # 移動平均データの作成
    retiex::retiex_rolling_summary() %>%
    dplyr::select(date, mean) %>%
    dplyr::mutate(`取引時点` = retiex::style_rolling_term_quarter(date)) %>%

    # 移動平均推移グラフの作成
    ggplot() +
    ggtitle(paste0(title,"取引総額移動平均の推移"))+
    xlab(label = "取引時点") +
    ylab(label = "取引総額移動平均") +
    scale_y_continuous(labels =
                         function(v){
                           retiex::style_yen(v, keta, unit)
                         }) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    geom_line(
      mapping = aes(x = `取引時点`, y = mean , group = 1),
      color = "red")
}

##########################################################
# ローカル関数
##########################################################

