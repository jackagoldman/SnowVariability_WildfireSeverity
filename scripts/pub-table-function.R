# flextable output
source("scripts/sem-table-function.R")

library(flextable)

pub.table <- function(df, response, region){
  if(response == "extreme"| response == "median"){
  df_output <- sem_results(df)
  table <- flextable(df_output, 
                     col_keys = c("Response", "Predictor", "Estimate", 
                                  "SE", "DF", "t.Value", "P.Value", 
                                  "Std.Estimate")) |> 
    theme_vanilla() |> 
    set_table_properties(layout = "autofit") 
  }else if (response == "heterogeneity"){
    df_output <- sem_results_cv(df)
    df_output <- as.data.frame(df_output)
    table <- flextable(df_output, 
                       col_keys = c("Response", "Predictor", "Estimate", 
                                    "SE", "DF", "t.Value", "P.Value", 
                                    "Std.Estimate")) |> 
      theme_vanilla() |> 
      set_table_properties(layout = "autofit") 
  }
  if (response == "extreme" & region == "NA" ){
    x_table <- table |> 
      set_caption(as_paragraph(
        as_chunk("piecewiseSEM results for extreme burn severity for boreal shield of Ontario", 
                 props = fp_text_default(font.family = "Cambria"))
      ), word_stylename = "Table Caption"
      )
  } else if (response == "median" & region == "NA" ){
    x_table <- table |>
      set_caption(as_paragraph(
        as_chunk("piecewiseSEM results for median burn severity for boreal shield of Ontario",
                  props = fp_text_default(font.family = "Cambria"))
    ), word_stylename = "Table Caption"
    )
  } else if( response == "extreme" & region == "west" ){
    x_table <- table |> 
      set_caption(as_paragraph(
        as_chunk("piecewiseSEM results for extreme burn severity in western boreal shield of Ontario",
                  props = fp_text_default(font.family = "Cambria"))
    ), word_stylename = "Table Caption"
    ) 
  }else if (response == "extreme" & region == "east" ){
    x_table <- table |> 
      set_caption( as_paragraph(
        as_chunk("piecewiseSEM results for extreme burn severity in eastern boreal shield of Ontario",
                   props = fp_text_default(font.family = "Cambria"))
    ), word_stylename = "Table Caption"
    )
  } else if (response == "median" & region == "west" ){
    x_table <- table |> 
      set_caption(as_paragraph(
        as_chunk("piecewiseSEM results for median burn severity in western boreal shield of Ontario",
                   props = fp_text_default(font.family = "Cambria"))
    ), word_stylename = "Table Caption"
    )
  } else if(response == "median" & region == "east" ){
    x_table <- table |> 
      set_caption(as_paragraph(
        as_chunk("piecewiseSEM results for median burn severity in eastern boreal shield of Ontario",
                   props = fp_text_default(font.family = "Cambria"))
    ), word_stylename = "Table Caption"
    )
  }else if(response == "heterogeneity" & region == "NA"){
    x_table <- table |> 
      set_caption(as_paragraph(
        as_chunk("piecewiseSEM results for burn severity heterogeneity for fires > 500 Ha",
                 props = fp_text_default(font.family = "Cambria"))
      ), word_stylename = "Table Caption"
      )
  }
  
  
  return(x_table)

}


