#' make tiny jirei report pdf
#'
#' This function is experimental.
#'
#' @param reti_jire_data data.frame with jirei data
#' @param output_file output file path
#' @param output_dir putput dir path
#'
#' @export
repoco_jirei_tinyreport <-
  function(reti_jire_data,
           output_file = "",
           output_dir = "./"){

    ##################################
    # データ前処理
    ##################################
    target_data <- reti_jire_data %>%
      mutate(`土地単価` = as.integer(`土地単価` / 0.3025)) %>%
      mutate(`相続税路線価１` = as.integer(`相続税路線価１` / 0.3025)) %>%
      mutate(kind =
               ifelse(stringr::str_detect(`土地種別`,"商業地"),
                      "商業地",
                      ifelse(stringr::str_detect(`土地種別`,"住宅地"),
                             "住宅地",
                             ifelse(stringr::str_detect(`土地種別`,"工業地"),
                                    "工業地",
                                    ifelse(stringr::str_detect(`土地種別`,"宅地見込み地"),
                                           "宅地見込み地",
                                           "その他")))))

    ##################################
    # タイトルチェック
    ##################################

    target_city_name <-
      target_data$city_name %>% unique()


    if(is.null(target_city_name) | length(target_city_name) == 0){
      title = "ある地域"

    }else if(length(target_city_name) > 1){
      title = paste0(target_city_name[1],"等")

    }else{
      title = target_city_name[1]
    }


    ##################################
    # テンプレートへの橋渡し
    ##################################

    # データフレーム
    data_from_rwfunc <- target_data

    # タイトル文字列
    title_from_rwfunc <- title


    # 出力ファイル名
    output_file <- local_output_filename(output_file, title, "_簡易事例分析_")

    ##################################
    # レンダーの呼び出し
    ##################################
    rmarkdown::render(input = system.file("template",
                                          "report_jirei_tiny.Rmd",
                                          package = "repoco"),
                      output_file = output_file,
                      output_dir = output_dir)

    }
