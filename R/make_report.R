#' rmarkdown render wrapper
#'
#' call rmarkdown render function with template.
#'
#' @param df data.frame
#' @param title title string
#' @param template_file rmarkdown file as template
#' @param output_file output file name
#' @param output_dir output directory path
#'
#' @export
render_wrapper <-
  function(df,
           title,
           template_file,
           output_file = NULL,
           output_dir = "handouts/"){

    ##################################
    # テンプレートで予期される変数
    ##################################

    # データフレーム
    data_to_report <- df

    # 文字列
    title_to_report <- title

    # 出力ファイル名
    if(is.null(output_file)){
      output_file <- paste0(title ,".pdf")
    }

    # レンダーの呼び出し
    rmarkdown::render(input = template_file,
                      output_file = output_file,
                      output_dir = output_dir)
}

#' report of stats about newly building house price
#'
#' 新築分譲住宅に関する取引価格の推移レポートを作成します.
#'
#' @param reti_data data.frame of reti
#' @param title title string
#' @param output_dir output directory path
#'
#' @import ggplot2
#' @importFrom magrittr %>%
#'
#' @export
newhouse_quarter_report <-
  function(reti_data,
           title = NULL,
           author = "",
           output_dir = "./"){

  # タイトルの処理
  if(is.null(title)){
    title <- "ある範囲"
    output_file <- paste0("new_house_quarter_report.pdf")
  }else{
    output_file <- paste0(title,".pdf")
  }

  # 文章用タイトルの設定
  title_to_report <- title

  # レポート作成者
  author_to_report <- author

  # 新築住宅用のフィルタリング
  data_to_report <-
    reti_data %>%
    new_hose_filter()

  tryCatch({
    rmarkdown::render(
      input = system.file("template",
                          "newresi_quarter_report.Rmd",
                          package = "repoco"),
      output_file = output_file,
      output_dir = output_dir)
  },
  error = function(e){
    message(paste0("make report Error!: ",title_to_report))
    message(e)
  }

  )


}
