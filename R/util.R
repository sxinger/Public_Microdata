##---------------------------helper functions--------------------------------------##
## install (if needed) and require packages
require_libraries<-function(package_list){
  #install missing packages
  install_pkg<-as.data.frame(installed.packages())
  new_packages<-package_list[!(package_list %in% install_pkg[which(install_pkg$LibPath==.libPaths()[1]),"Package"])]
  if(length(new_packages)>0){
    install.packages(new_packages,lib=.libPaths()[1],repos = "http://cran.us.r-project.org")
  }
  
  for (lib in package_list) {
    library(lib, character.only=TRUE,lib.loc=.libPaths()[1])
    cat("\n", lib, " loaded.", sep="")
  }
}

connect_to_db<-function(DBMS_type,config_file){
  if(DBMS_type=="Oracle"){
    require_libraries("ROracle")
    conn<-dbConnect(ROracle::Oracle(),
                    config_file$username,
                    config_file$password,
                    file.path(config_file$access,config_file$sid))
    
  }else if(DBMS_type=="tSQL"){
    require_libraries("RJDBC")
    # need to download sqljdbc.jar and put it AKI_CDM folder
    drv <- JDBC("com.microsoft.sqlserver.jdbc.SQLServerDriver","./sqljdbc.jar", "`")
    url = paste0("jdbc:sqlserver:", config_file$access,
                 ";DatabaseName=",config_file$cdm_db_name,
                 ";username=",config_file$username,
                 ";password=",config_file$password)
    conn <- dbConnect(drv, url)
    
  }else if(DBMS_type=="PostgreSQL"){
    #not tested yet!
    require_libraries("RPostgres")
    server<-gsub("/","",str_extract(config_file$access,"//.*(/)"))
    host<-gsub(":.*","",server)
    port<-gsub(".*:","",server)
    conn<-dbConnect(RPostgres::Postgres(),
                    host=host,
                    port=port,
                    dbname=config_file$cdm_db_name,
                    user=config_file$username,
                    password=config_file$password)
  }else{
    stop("the DBMS type is not currectly supported!")
  }
  attr(conn,"DBMS_type")<-DBMS_type
  return(conn)
}
