
observe({
  
  if(!is.null(myValues$finalData$pbmc))
  {
    pbmc = myValues$finalData$pbmc
    updateSelectizeInput(session,'clusterNum',
                         choices=levels(pbmc), selected = input$clusterNum)
    updateSelectizeInput(session,'clusterNumVS1',
                         choices=levels(pbmc), selected = input$clusterNumVS1)
    updateSelectizeInput(session,'clusterNumVS2',
                         choices=levels(pbmc),selected = input$clusterNumVS2)
    
    genesToPlot = rownames( GetAssayData(pbmc, slot = "scale.data") )
    
    updateSelectizeInput(session,'genesToPlotVln',
                         choices=genesToPlot)
    
    updateSelectizeInput(session,'genesToFeaturePlot',
                         choices=genesToPlot)
  }
  
})


observe({
  findClusterMarkersReactive()
})

findClusterMarkersReactive <- eventReactive(input$findClusterMarkers, {
  withProgress(message = "Processing , please wait",{
    pbmc = myValues$finalData$pbmc
    js$addStatusIcon("findMarkersTab","loading")
    
    shiny::setProgress(value = 0.4, detail = "Finding cluster markers ...")
     
    #jay
    #plan("multiprocess", workers = 3)
    plan("multisession", workers =3)
    cluster.markers <- FindMarkers(object = pbmc, ident.1 = input$clusterNum, min.pct = input$minPct, test.use = input$testuse, only.pos = input$onlypos)
    
    myValues$scriptCommands$findMarkers = paste0("cluster.markers <- FindMarkers(object = pbmc, ident.1 = ",input$clusterNum,", min.pct = ",input$minPct,", test.use = ",input$testuse,", only.pos = ",input$onlypos,")")
    
    shiny::setProgress(value = 0.8, detail = "Done.")
    
    if(is.null(myValues$clusterGenes))
      myValues$clusterGenes = rownames(cluster.markers)
    else
      myValues$clusterGenes = c(myValues$clusterGenes, rownames(cluster.markers) )
    
    myValues$clusterGenes = unique(myValues$clusterGenes)
    
    updateSelectizeInput(session,'genesToPlotVln', choices=VariableFeatures(object = pbmc), selected=NULL)
    
    updateSelectizeInput(session,'genesToFeaturePlot', choices=VariableFeatures(object = pbmc), selected=NULL)
    
    js$addStatusIcon("findMarkersTab","done")
    return(list("clustername" = paste0("cluster",input$clusterNum),"clustermarkers"=cluster.markers))
  })
})

output$clusterMarkers <- renderDataTable({
  tmp <- findClusterMarkersReactive()
  
  if(!is.null(tmp)){
    tmp$clustermarkers
  }
  
})

output$downloadClusterMarkersCSV <- downloadHandler(
  filename = function() {
    paste0(findClusterMarkersReactive()$clustername,".csv")
  },
  content = function(file) {
    write.csv(findClusterMarkersReactive()$clustermarkers, file, row.names=TRUE)
  }
)

output$clusterMarkersAvailable <-
  reactive({
    return(!is.null(findClusterMarkersReactive()$clustername))
  })
outputOptions(output, 'clusterMarkersAvailable', suspendWhenHidden=FALSE)


observe({
  findClusterMarkersVSReactive()
})

findClusterMarkersVSReactive <- eventReactive(input$findClusterMarkersVS, {
  withProgress(message = "Processing , please wait",{
    pbmc = myValues$finalData$pbmc
    
    js$addStatusIcon("findMarkersTab","loading")
    
    shiny::setProgress(value = 0.4, detail = "Finding cluster markers ...")
    #jay
    #plan("multiprocess", workers = 3)
    plan("multisession", workers =3)
    cluster.markers <- FindMarkers(object = pbmc, ident.1 = input$clusterNumVS1, ident.2 = input$clusterNumVS2, min.pct = input$minPctvs, test.use = input$testuseVS, only.pos = input$onlyposVS)
    
    myValues$scriptCommands$findMarkers = paste0("cluster.markersVS <- FindMarkers(object = pbmc, ident.1 = ",input$clusterNumVS1,", ident.2 = ",input$clusterNumVS2,", min.pct = ",input$minPctvs,", test.use = ",input$testuseVS,", only.pos = ",input$onlyposVS,")")
    
    if(is.null(myValues$clusterGenes))
      myValues$clusterGenes = rownames(cluster.markers)
    else
      myValues$clusterGenes = c(myValues$clusterGenes, rownames(cluster.markers) )
    
    myValues$clusterGenes = unique(myValues$clusterGenes)
    
    shiny::setProgress(value = 0.8, detail = "Done.")
    js$addStatusIcon("findMarkersTab","done")
    
    
    return(list("clustername" = paste0("cluster",input$clusterNumVS1,"_vs_clusters_",paste(input$clusterNumVS2, collapse = "_")),"clustermarkers"=cluster.markers))
    
  })
})

output$clusterMarkersVS <- renderDataTable({
  tmp <- findClusterMarkersVSReactive()
  
  if(!is.null(tmp)){
    tmp$clustermarkers
  }
  
})

output$downloadClusterMarkersVSCSV <- downloadHandler(
  filename = function() {
    paste0(findClusterMarkersVSReactive()$clustername,".csv")
  },
  content = function(file) {
    write.csv(findClusterMarkersVSReactive()$clustermarkers, file, row.names=TRUE)
  }
)

output$clusterMarkersVSAvailable <-
  reactive({
    return(!is.null(findClusterMarkersVSReactive()$clustername))
  })
outputOptions(output, 'clusterMarkersVSAvailable', suspendWhenHidden=FALSE)


# ALL MARKERS
observe({
  findClusterMarkersAllReactive()
})

findClusterMarkersAllReactive <- eventReactive(input$findClusterMarkersAll, {
  withProgress(message = "Processing , please wait",{
    pbmc = myValues$finalData$pbmc
    
    myValues$cellBrowserLinkExists = F
    
    js$addStatusIcon("findMarkersTab","loading")
    
    shiny::setProgress(value = 0.4, detail = "Finding cluster markers ...")
    #jay
    #plan("multiprocess", workers = 3)
      plan("multisession", workers =3)
    cluster.markers <- FindAllMarkers(object = pbmc, min.pct = input$minPctAll, test.use = input$testuseAll, only.pos = input$onlyposAll, logfc.threshold = input$threshAll)
    
    myValues$scriptCommands$findAllMarkers = paste0("cluster.markers <- FindAllMarkers(object = pbmc, min.pct = ",input$minPctAll,", test.use = '",input$testuseAll,"', only.pos = ",input$onlyposAll,", logfc.threshold = ",input$threshAll,")")
    
    if(input$numGenesPerCluster > 0){
      cluster.markers = cluster.markers %>% group_by(cluster) %>% top_n(input$numGenesPerCluster, avg_log2FC)
      
      cluster.markers = as.data.frame(cluster.markers)
    }
  
    myValues$clusterGenes = cluster.markers$gene
    
    folderuuid = UUIDgenerate()
    
    myValues$clusterFilePath = paste0(tempdir(),"/allclustermarkers-",folderuuid,".csv")
    
    write.csv(cluster.markers, myValues$clusterFilePath, row.names= T)
    
    shiny::setProgress(value = 0.8, detail = "Done.")
    js$addStatusIcon("findMarkersTab","done")
    
    return(list("clustername" = paste0("allClusterMarkers"),"clustermarkers"=cluster.markers))
    
  })
})

output$clusterMarkersAll <- renderDataTable({
  tmp <- findClusterMarkersAllReactive()
  
  if(!is.null(tmp)){
    tmp$clustermarkers
  }
  
})

output$downloadClusterMarkersAllCSV <- downloadHandler(
  filename =  function() {
    paste0(findClusterMarkersAllReactive()$clustername,".csv")
  },
  content = function(file) {
    write.csv(findClusterMarkersAllReactive()$clustermarkers, file, row.names=TRUE)}
)

output$clusterMarkersAllAvailable <-
  reactive({
    return(!is.null(findClusterMarkersAllReactive()$clustername))
  })
outputOptions(output, 'clusterMarkersAllAvailable', suspendWhenHidden=FALSE)

output$clusterHeatmap <- renderPlot({
  if(input$generateHeatmap > 0){
    withProgress(message = "Processing , please wait", {
      pbmc = myValues$finalData$pbmc
      isolate({
        allmarkers = findClusterMarkersAllReactive()$clustermarkers
        allmarkers %>% group_by(cluster) %>% top_n(n = input$topGenesPerCluster, wt = avg_log2FC) -> selectedGenes
      })
      
      DoHeatmap(object = pbmc, features = selectedGenes$gene)
    })
  }
})

#cluster heatmap output and download handler
# clusterHeatmapFunc = reactive({
#   pbmc = myValues$finalData$pbmc
#   isolate({
#     allmarkers = findClusterMarkersAllReactive()$clustermarkers
#     allmarkers %>% group_by(cluster) %>% top_n(n = input$topGenesPerCluster, wt = avg_logFC) -> selectedGenes
#   })
#   browser()
#   DoHeatmap(object = pbmc, features = selectedGenes$gene)
# })

output$downloadClusterHeatmap <- downloadHandler(
  filename <- function() {
    paste(input$projectname, "_ClusterHeatMap", input$clusterHeatmapDownloadAs,sep="")
  },
  content <- function(file) {
    ggsave(file, clusterHeatmapFunc(), width = input$clusterHeatmapDownloadWidth,
           height = input$clusterHeatmapDownloadHeight, units = "cm", dpi = 300)
  },
  contentType = "image"
)

#VLN plot output and download handler
vlnPlotFunc = reactive({
  validate(
    need(length(input$genesToPlotVln) > 0, message = "Select at least one gene")
  )
  
  pbmc = myValues$finalData$pbmc
  
  VlnPlot(object = pbmc, features = input$genesToPlotVln, log = input$ylog)
})

output$VlnMarkersPlot = renderPlot({
  if(input$plotVlns < 1)
    return()
  
  isolate({
    vlnPlotFunc()
  })
})

output$downloadVlnPlot <- downloadHandler(
  filename <- function() {
    paste(input$projectname, "_VlnPlot", input$vlnDownloadAs,sep="")
  },
  content <- function(file) {
    validate(
      need(length(input$genesToPlotVln) > 0, message = "Select at least one gene")
    )
    ggsave(file, vlnPlotFunc(), width = input$vlnDownloadWidth,
           height = input$vlnDownloadHeight, units = "cm", dpi = 300)
  },
  contentType = "image"
)


#Feature Markers plot output and download handler
featurePlotFunc = reactive({
  validate(
    need(length(input$genesToFeaturePlot) > 0, message = "Select at least one gene")
  )
  
  pbmc = myValues$finalData$pbmc
  
  validate(
    need(!is.null(pbmc@reductions[[input$reducUseFeature]]), message = paste(input$reducUseFeature, "was not computed!, choose another reduction"))
  )
  
  if(input$sctransformOption == 'defaultPath')
    FeaturePlot(object = pbmc, features = input$genesToFeaturePlot, cols = c("gray88", "blue"),reduction = input$reducUseFeature, pt.size = 2)
  else
    FeaturePlot(object = pbmc, features = input$genesToFeaturePlot, cols = c("gray88", "blue"),reduction = input$reducUseFeature, pt.size = 2)
})

output$FeatureMarkersPlot = renderPlot({
  if(input$plotFeatureMarkers < 1)
    return()
  
  isolate({
    featurePlotFunc()
  })
})


output$downloadFeaturePlot <- downloadHandler(
  filename <- function() {
    paste(input$projectname, "_FeatureMarkersPLot", input$featureDownloadAs,sep="")
  },
  content <- function(file) {
    if (length(input$genesToFeaturePlot) == 0){
      session$sendCustomMessage(type = 'alert', message = "Select at least one gene and plot before downloading the plot.")
    }
    req(input$genesToFeaturePlot)
    ggsave(file, featurePlotFunc(), width = input$featureDownloadWidth,
           height = input$featureDownloadHeight, units = "cm", dpi = 300)
  },
  contentType = "image"
)


observe({
  if(input$viewCellBrowser > 0)
  {
    withProgress(message = "Generating UCSC Cell Browser data",{
      
      shiny::setProgress(value = 0.4, detail = "this might take long for big datasets ...")
      
      folderuuid = UUIDgenerate()
      
      folderpath = paste0(tempdir(),"/pbmcellbrowser-",folderuuid)
      foldercbpath = paste0(getwd(),"/www/pbmcellbrowsercb-",folderuuid)
      
      tryCatch({
        js$addStatusIcon("findMarkersTab","loading")
        isolate({
          pbmc = myValues$finalData$pbmc
        })
        
        if(input$sctransformOption == 'defaultPath')
        {
          ExportToCellbrowser(pbmc, dir= folderpath, cb.dir=foldercbpath, reductions = "umap", markers.file = myValues$clusterFilePath)
        }
        else
        {
          ExportToCellbrowser(pbmc, dir= folderpath, cb.dir=foldercbpath, reductions = "umap", markers.file = myValues$clusterFilePath)
        }
        js$addStatusIcon("findMarkersTab","done")
        
        myValues$wwwcbpath = paste0(session$clientData$url_pathname,"pbmcellbrowsercb-",folderuuid,"/index.html?ds=",pbmc@project.name)
        
      }, error = function(e) {
        print(e)
      })
      
      
      myValues$cellBrowserLinkExists = T
      
      
    })
  }
})

output$cellBrowserLinkExists <-
  reactive({
    if(!is.null(myValues$cellBrowserLinkExists))
      return(myValues$cellBrowserLinkExists)
    
    FALSE
  })
outputOptions(output, 'cellBrowserLinkExists', suspendWhenHidden=FALSE)

output$cellbrowserlink <- renderUI({
  column(12,
         hr(),
         a('Launch Cellbrowser ',href = myValues$wwwcbpath, target = "_blank", class = "btn button button-3d button-pill button-action")
  )
})
