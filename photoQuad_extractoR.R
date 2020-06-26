# photoQuad extractor function
photoQuad_extractoR<-function(object,output="cover")
{
  require(tidyverse)
  library(tidyverse)
  require(mgsub)
  library(mgsub)
  # object entry standardization
  df_names<-1:length(object)
  data_list<-
  setNames(replicate(length(df_names), 
                     list(NULL)),df_names)
  # looping to apply all functions to all csv files
  for(i in 1:length(object))
  {
    data_list[[i]]<-
      object[[i]]%>%
      dplyr::mutate(site=unique(
      mgsub::mgsub(Image,c(".JPG",".jpg",".JPEG",".jpeg",
                           ".PNG",".png",".TIFF",".tiff",
                           ".TIF",".tif",".bmp",".BMP"),
                         c("","","","","","","","","","",
                           "",""))))%>%
      rename(species=`spp Name`,
             cover=`Cov% per species`)%>%
      select(species,cover,site)
  }
  # obtaining the cover table
  if(output=="cover")
  {
    cover_table_1<-
      data_list%>%
      bind_rows()%>%
      arrange(species)
    
    cover_final<-
      cover_table_1%>%
      pivot_wider(names_from=species,values_from=cover)%>%
      replace(is.na(.),0)%>%
      column_to_rownames("site")
  }
  if(output=="presence")
  {
    cover_table_1<-
      data_list%>%
      bind_rows()%>%
      arrange(species)
    
    cover_final<-
      as.data.frame(with(
      cover_table_1,table(site,species)) > 0L)+0L
  }
  # construction of the final objects, which will be shown in the screen
  return(cover_final)
}