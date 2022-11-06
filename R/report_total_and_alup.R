#' report total and assumed land unit price about newhouse
#'
#' report change of total and assumed land unit price
#' about new houses.
#'
#' @param reti_data data.frame of reti
#' @param title title string
#' @param author auther string
#' @param output_file output file name
#' @param output_dir output directory path
#' @param cost Int rebuilding unit cost
#' @param durability Int durabirity year
#'
#' @import ggplot2
#' @importFrom magrittr %>%
#'
#' @export
repoco_newhouse_quarter <-
  function(reti_data,
           title = "",
           author = "",
           output_file = "",
           output_dir = "./",
           cost = 190000,
           durability = 45){

    ##################################
    # タイトルチェック
    ##################################

    title <- local_decide_title(title, reti_data)


    ##################################
    # テンプレートへの橋渡し
    ##################################

    # データフレーム
    data_from_rwfunc <- reti_data

    # 再調達建物原価
    bcost_from_rwfunc <- cost

    # 建物耐用年数
    durability_from_rwfunc <- durability

    # タイトル文字列
    title_from_rwfunc <- title

    # 作者
    author_from_rwfunc <- author

    # 出力ファイル名
    output_file <- local_output_filename(output_file, title, "（新築住宅）")

    ##################################
    # レンダーの呼び出し
    ##################################
    rmarkdown::render(input = system.file("template",
                                          "report_total_alup_newhouse.Rmd",
                                          package = "repoco"),
                      output_file = output_file,
                      output_dir = output_dir)

    }

#' report total and assumed land unit price about allresidence
#'
#' report change of total and assumed land unit price
#' about all residence.
#'
#' @param reti_data data.frame of reti
#' @param title title string
#' @param author auther string
#' @param output_file output file name
#' @param output_dir output directory path
#' @param cost Int rebuilding unit cost
#' @param durability Int durabirity year
#'
#' @import ggplot2
#' @importFrom magrittr %>%
#'
#' @export
repoco_allresidence_quarter <-
  function(reti_data,
           title = "",
           author = "",
           output_file = "",
           output_dir = "./",
           cost = 190000,
           durability = 45){

    ##################################
    # タイトルチェック
    ##################################

    title <- local_decide_title(title, reti_data)


    ##################################
    # テンプレートへの橋渡し
    ##################################

    # データフレーム
    data_from_rwfunc <- reti_data

    # 再調達建物原価
    bcost_from_rwfunc <- cost

    # 建物耐用年数
    durability_from_rwfunc <- durability

    # タイトル文字列
    title_from_rwfunc <- title

    # 作者
    author_from_rwfunc <- author

    # 出力ファイル名
    output_file <- local_output_filename(output_file, title, "（住宅）")

    ##################################
    # レンダーの呼び出し
    ##################################
    rmarkdown::render(input = system.file("template",
                                          "report_total_alup_allresidence.Rmd",
                                          package = "repoco"),
                      output_file = output_file,
                      output_dir = output_dir)

  }

#' report of total and assumed land unit price about office
#'
#' report change of total and assumed land unit price
#' about office building
#'
#' @param reti_data data.frame of reti
#' @param title title string
#' @param author auther string
#' @param output_file output file name
#' @param output_dir output directory path
#' @param cost Int rebuilding unit cost
#' @param durability Int durabirity year
#'
#' @import ggplot2
#' @importFrom magrittr %>%
#'
#' @export
repoco_office_quarter <-
  function(reti_data,
           title = "",
           author = "",
           output_file = "",
           output_dir = "./",
           cost = 200000,
           durability = 50){

    ##################################
    # タイトルチェック
    ##################################

    title <- local_decide_title(title, reti_data)


    ##################################
    # テンプレートへの橋渡し
    ##################################

    # データフレーム
    data_from_rwfunc <- reti_data

    # 再調達建物原価
    bcost_from_rwfunc <- cost

    # 建物耐用年数
    durability_from_rwfunc <- durability

    # タイトル文字列
    title_from_rwfunc <- title

    # 作者
    author_from_rwfunc <- author

    # 出力ファイル名
    output_file <- local_output_filename(output_file, title, "（事務所ビル）")

    ##################################
    # レンダーの呼び出し
    ##################################
    rmarkdown::render(input = system.file("template",
                                          "report_total_alup_office.Rmd",
                                          package = "repoco"),
                      output_file = output_file,
                      output_dir = output_dir)

  }


#' report of total and assumed land unit price about apartment
#'
#' report change of total and assumed land unit price
#' about apartment
#'
#' @param reti_data data.frame of reti
#' @param title title string
#' @param author auther string
#' @param output_file output file name
#' @param output_dir output directory path
#' @param cost Int rebuilding unit cost
#' @param durability Int durabirity year
#'
#' @import ggplot2
#' @importFrom magrittr %>%
#'
#' @export
repoco_apartment_quarter <-
  function(reti_data,
           title = "",
           author = "",
           output_file = "",
           output_dir = "./",
           cost = 180000,
           durability = 45){

    ##################################
    # タイトルチェック
    ##################################

    title <- local_decide_title(title, reti_data)


    ##################################
    # テンプレートへの橋渡し
    ##################################

    # データフレーム
    data_from_rwfunc <- reti_data

    # 再調達建物原価
    bcost_from_rwfunc <- cost

    # 建物耐用年数
    durability_from_rwfunc <- durability

    # タイトル文字列
    title_from_rwfunc <- title

    # 作者
    author_from_rwfunc <- author

    # 出力ファイル名
    output_file <- local_output_filename(output_file, title, "（共同住宅）")

    ##################################
    # レンダーの呼び出し
    ##################################
    rmarkdown::render(input = system.file("template",
                                          "report_total_alup_apartment.Rmd",
                                          package = "repoco"),
                      output_file = output_file,
                      output_dir = output_dir)

  }
