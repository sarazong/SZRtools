#' @title Load files from a aws s3 path
#' @description Load csv, tsv, or xlsx files stored in a aws s3 path
#'
#' @param s3_path aws s3 path as a string
#' @param file_format file format as a string, either csv, tsv, or xlsx
#'
#' @return a list of dataframes
#' @export

load_s3_files <- function(s3_path = "s3://my_bucket/directory/", file_format = "csv"){
  bucket = stringr::str_split(s3_path, "/")[[1]][3]
  prefix = stringr::str_replace(s3_path, paste0("s3://", bucket, "/"), "")

  file_names <- aws.s3::get_bucket_df(bucket = bucket, prefix = prefix) %>%
    dplyr::select(Key) %>%
    dplyr::filter(stringr::str_detect(Key, file_format)) %>%
    dplyr::pull()

  if (file_format == "csv" || file_format == "tsv") {
    # eval() evaluates an expression and parse() converts the text into an expression
    read_fun = eval(parse(text = paste0("readr::read_", file_format)))
  } else if (file_format == "xlsx") {
    read_fun = eval(parse(text = paste0("readxl::read_", file_format)))
  }

  suppressMessages(
    dfs <- file_names %>%
      lapply(function(x) aws.s3::s3read_using(FUN = read_fun,
                                              object = paste0("s3://", bucket, "/", x)))
  )

  df_names <- file_names %>%
    lapply(stringr::str_replace, ".*/", "") %>%
    lapply(stringr::str_replace, paste0(".", file_format), "")

  names(dfs) <- df_names
  return(dfs)
}
