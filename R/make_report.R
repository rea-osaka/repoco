#' rmarkdown render wrapper
#'
#' 自作のテンプレートファイルを使って、レポートを作ります。
#' 出力ファイル名を省略すると、市名列から自動で補われます。
#'
#' @param df data.frame 主にretiデータ
#' @param template_file rmarkdown file as template
#' @param title title string
#' @param author auther string
#' @param output_file output file name
#' @param output_dir output directory path デフォルトはカレントのhandouts
#'
#' @export
render_wrapper <-
  function(df,
           template_file,
           title = "",
           author = "",
           output_file = "",
           output_dir = "handouts/"){

    ##################################
    # 入力チェック
    ##################################

    if(is.null(title)){title = ""}

    if(title == ""){
      cities <- unique(df$`市名`)
      if(is.null(cities) | length(cities) == 0){
        title = "ある地域"
      }else if(length(cities) > 1){
        title = paste0(cities[1],"等")
      }else{
        title = cities[1]
      }
    }

    # 出力ファイル名のチェック
    if(is.null(output_file) | output_file == ""){
      output_file <- paste0(title,format(Sys.Date(),"%Y%m%d"),".pdf")
    }


    ##################################
    # テンプレートへの橋渡し
    ##################################

    # データフレーム
    data_from_rwfunc <- df

    # タイトル文字列
    title_from_rwfunc <- title

    # 作者
    author_from_rwfunc <- author

    # 出力ファイル名
    if(!stringr::str_detect(output_file, "\\.pdf$")){
      output_file <- paste0(output_file ,".pdf")
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
#' @param author auther string
#' @param output_file output file name
#' @param output_dir output directory path
#'
#' @import ggplot2
#' @importFrom magrittr %>%
#'
#' @export
repoco_newhouse_report <-
  function(reti_data,
           title = "",
           author = "",
           output_file = "",
           output_dir = "./"){

    ##################################
    # 入力チェック
    ##################################

    if(is.null(title)){title = ""}

    if(title == ""){
      cities <- unique(reti_data$`市名`)
      if(is.null(cities) | length(cities) == 0){
        title = "ある地域"
      }else if(length(cities) > 1){
        title = paste0(cities[1],"等")
      }else{
        title = cities[1]
      }
    }

    # 出力ファイル名のチェック
    if(is.null(output_file) | output_file == ""){
      output_file <- paste0(title,"（新築住宅）",format(Sys.Date(),"%Y%m%d"),".pdf")
    }

    render_wrapper(reti_data,
                   template_file =
                     system.file("template",
                                 "fullsummary_newhouse_report.Rmd",
                                 package = "repoco"),
                   title = title,
                   author = author,
                   output_file = output_file,
                   output_dir = output_dir)

    }

#' report of stats about apartmenthouse price
#'
#' 共同住宅に関する取引価格の推移レポートを作成します.
#'
#' @param reti_data data.frame of reti
#' @param title title string
#' @param author auther string
#' @param output_file output file name
#' @param output_dir output directory path
#'
#' @import ggplot2
#' @importFrom magrittr %>%
#'
#' @export
repoco_apartment_report <-
  function(reti_data,
           title = "",
           author = "",
           output_file = "",
           output_dir = "./"){


    ##################################
    # 入力チェック
    ##################################

    if(is.null(title)){title = ""}

    if(title == ""){
      cities <- unique(reti_data$`市名`)
      if(is.null(cities) | length(cities) == 0){
        title = "ある地域"
      }else if(length(cities) > 1){
        title = paste0(cities[1],"等")
      }else{
        title = cities[1]
      }
    }

    # 出力ファイル名のチェック
    if(is.null(output_file) | output_file == ""){
      output_file <- paste0(title,"（共同住宅）",format(Sys.Date(),"%Y%m%d"),".pdf")
    }


    render_wrapper(reti_data,
                   template_file =
                     system.file("template",
                                 "fullsummary_apartmenthouse_report.Rmd",
                                 package = "repoco"),
                   title = title,
                   author = author,
                   output_file = output_file,
                   output_dir = output_dir)

  }

#' report of stats about office building price
#'
#' 事務所ビルに関する取引価格の推移レポートを作成します.
#'
#' @param reti_data data.frame of reti
#' @param title title string
#' @param author auther string
#' @param output_file output file name
#' @param output_dir output directory path
#'
#' @import ggplot2
#' @importFrom magrittr %>%
#'
#' @export
repoco_office_report <-
  function(reti_data,
           title = "",
           author = "",
           output_file = "",
           output_dir = "./"){


    ##################################
    # 入力チェック
    ##################################

    if(is.null(title)){title = ""}

    if(title == ""){
      cities <- unique(reti_data$`市名`)
      if(is.null(cities) | length(cities) == 0){
        title = "ある地域"
      }else if(length(cities) > 1){
        title = paste0(cities[1],"等")
      }else{
        title = cities[1]
      }
    }

    # 出力ファイル名のチェック
    if(is.null(output_file) | output_file == ""){
      output_file <- paste0(title,"（事務所）",format(Sys.Date(),"%Y%m%d"),".pdf")
    }


    render_wrapper(reti_data,
                   template_file =
                     system.file("template",
                                 "fullsummary_office_report.Rmd",
                                 package = "repoco"),
                   title = title,
                   author = author,
                   output_file = output_file,
                   output_dir = output_dir)

  }

#' report of stats about newly building house price
#'
#' 新築分譲住宅に関する取引価格の推移レポートを作成します.
#' (古い関数 obsolate)
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
