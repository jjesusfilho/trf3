psql_join_trf3<-function(con,mtbl="dados",dtbl="partes"){
  ordem <- "ordem"
  query <- glue::glue_sql("
  SELECT * 
  from {`mtbl`}, {`dtbl`}
  WHERE {`mtbl`}.{`ordem`} = {`dtbl`}.{`ordem`}
  ",.con = con)
  RPostgres::dbGetQuery(con,query)
}