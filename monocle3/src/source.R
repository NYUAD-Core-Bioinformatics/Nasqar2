mon.run <- function(data, gene, id) {
  

  print(gene)
  
  print(id)
  # Set identity
  Idents(data) <- id

  # Set resolution
  res <- 0.8
  
  #Inc process
  incProgress(amount = 0.1)
  
  if (DefaultAssay(data) != "integrated") {
  # Normalise and PCA on non-integrated data
  ##data <- NormalizeData(data, verbose = FALSE)
  ##data <- FindVariableFeatures(data, selection.method = "vst", nfeatures = 2000, verbose = FALSE)
  ##data <- ScaleData(data, verbose = FALSE)
  ##data <- RunPCA(data, features = VariableFeatures(object = data), verbose = FALSE)
  }
  
  
  ##data <- ScaleData(data, verbose = FALSE)
  ##data <- RunPCA(data)
  ##data <- RunUMAP(data, dims = 1:30, n.neighbors = 60)
  ##data <- FindNeighbors(data, dims = 1:30)
  ##data <- FindClusters(data)
  
  #data <- FindNeighbors(data, reduction = 'pca', dims = 1:30, k.param = 60)
  #data <- FindNeighbors(data, dims = 1:30)
  #data <- FindClusters(data, resolution = res, verbose = FALSE)
  #data <- RunUMAP(data, dims = 1:30, n.neighbors = 60)
  
  # Create dimension plot
  dim <- DimPlot(data, reduction = "umap", label = TRUE, group.by = id)

  #Inc process
  incProgress(amount = 0.1)
  
  # Create cell set for clustering
  #cds <- as.CellDataSet(data)
  cds <- suppressWarnings(as.cell_data_set(data))
  
 
  # Cluster cells with Leiden clustering
  #cds <- cluster_cells(cds)
  cds <- cluster_cells(cds, cluster_methods = 'leiden', reduction_method = 'UMAP')

  print('done...')
  #Inc process
  incProgress(amount = 0.1)
  
  #sc.sub <- subset(as.Seurat(cds, assay = NULL), monocle3_partitions == 1)
  
  #cds <- as.cell_data_set(sc.sub)

  #Inc process
  incProgress(amount = 0.1)
  
 

  # Trajectory analysis
  cds <- learn_graph(cds)
  


  # Set progenitor
  #max.progenitor <- which.max(unlist(FetchData(sc.sub, gene)))
  #max.progenitor <- which.max(unlist(FetchData(data, gene)))
  ##max.progenitor <- colnames(sc.sub)[max.progenitor]
  ##cds <- order_cells(cds, root_cells = max.progenitor)
  max.progenitor <- which.max(unlist(FetchData(data, gene)))
  max.progenitor <- colnames(data)[max.progenitor]
  cds <- order_cells(cds, root_cells = max.progenitor)
  

  # Convert back to Seurat assayy
  #sc.sub <- as.Seurat(cds, assay = NULL)
  #sc.sub <- as.Seurat(cds, assay = 'integrated')
  

  #Manually add pseudotime due to error
  #sc.sub <- AddMetaData(sc.sub, metadata=cds@principal_graph_aux@listData$UMAP$pseudotime, col.name = "monocle3_pseudotime")
  #data <- AddMetaData(data, metadata=cds@principal_graph_aux@listData$UMAP$pseudotime, col.name = "monocle3_pseudotime")
  
  #Inc process
  incProgress(amount = 0.1)
  
  # Add pseudotimes back to seurat object
  #data$monocle3_clusters <- sc.sub$monocle3_clusters
  #data$monocle3_partitions <- sc.sub$monocle3_partitions
  #data$monocle3_pseudotime <- sc.sub$monocle3_pseudotime
  #data$monocle3_pseudotime[is.na(data$monocle3_pseudotime)] <- 0 
  data <- AddMetaData(data, metadata=cds@clusters$UMAP$clusters, col.name = "monocle3_clusters")
  data <- AddMetaData(data, metadata=cds@clusters$UMAP$partitions, col.name = "monocle3_partitions")
  data <- AddMetaData(data, metadata=cds@principal_graph_aux@listData$UMAP$pseudotime, col.name = "monocle3_pseudotime")
  data$monocle3_pseudotime[is.na(data$monocle3_pseudotime)] <- 0 

  
  
  #print('integrated')
  #data[['integrated']] <- sc.sub[['integrated']]
  
  #print('integrated data')

  #Inc process
  incProgress(amount = 0.1)
  
  # Create plots
  p1 <- plot_cells(cds, label_groups_by_cluster = FALSE, label_leaves = FALSE, label_branch_points = FALSE)
  p2 <- plot_cells(cds, color_cells_by = 'pseudotime', label_cell_groups = FALSE, label_leaves = FALSE, label_branch_points = FALSE)
  p3 <- plot_cells(cds, color_cells_by = 'pseudotime', label_cell_groups = FALSE, label_leaves = FALSE, label_branch_points = FALSE, show_trajectory_graph = FALSE)
  #p4 <- FeaturePlot(sc.sub, feature = 'monocle3_pseudotime')
  p4 <- FeaturePlot(data, feature = 'monocle3_pseudotime')

  #Inc process
  incProgress(amount = 0.1)
  
  # Find differentially expressed genes
  cds <-  estimate_size_factors(cds)
  
  # Set gene names
  #cds@rowRanges@elementMetadata@listData[['gene_short_name']] <- rownames(data[['integrated']])
  
  #Inc process
  incProgress(amount = 0.1)
  
  # Create table of DE genes
  cds_dif_test <- graph_test(cds, neighbor_graph = 'principal_graph', cores = 4)
  
  #Set table for output
  t1 <- cds_dif_test
  
  #Select significant genes
  cds_dif_test <-  cds_dif_test[cds_dif_test$q_value < 0.05,]
  
  #Inc process
  incProgress(amount = 0.1)
  
  # Get top genes
  gene.list <-  cds_dif_test %>% arrange(desc(morans_test_statistic), desc(-q_value)) %>% rownames()
  top.genes <- gene.list[1:40]
  
  # Select top genes
  top <- cds_dif_test[rownames(cds_dif_test) %in% top.genes,]
  
  
  str(AggregateExpression(data, assays = "RNA", features = top.genes))
  print('top')
  print(top)
  # Count mean expression of top genes
  summ <- AggregateExpression(data, assays = "RNA", features = top.genes) %>% as.data.frame()
  summ$all <- rowMeans(summ)
  
  # Edit top genes to remove those with 0 expression
  top.genes <- row.names(summ)[summ$all > 0]
  
  # Cluster cells (based on pseudotime) for heatmap
  data$monocle3_pseudotime[data$monocle3_pseudotime == "Inf"] <- 0 
  kk <- Mclust(-data$monocle3_pseudotime, 5, modelNames = 'E')

  #Inc process
  incProgress(amount = 0.1)
  
  # Cluster genes with k-means clustering
  #p5 <- plotMarkerHeat(data@assays$RNA@data[,order(data$monocle3_pseudotime, decreasing = FALSE)], 
  #	kk$classification[order(data$monocle3_pseudotime, decreasing = TRUE)], 
  #	top.genes, averageCells = 10^1, clusterGenesK = 5, clusterGenes = TRUE, 
  #	gap = FALSE, outs = TRUE, plotheat = TRUE) # fontsize = 20)
  
  p5 <- plotMarkerHeat(data@assays$RNA$data[,order(data$monocle3_pseudotime, decreasing = FALSE)], 
                       kk$classification[order(data$monocle3_pseudotime, decreasing = TRUE)], 
                       top.genes, averageCells = 10^1, clusterGenesK = 5, clusterGenes = TRUE, 
                       gap = FALSE, outs = TRUE, plotheat = TRUE) # fontsize = 20)

  # Create output list
  out <- list(dim=dim, p1=p1, p2=p2, p3=p3, p4=p4, p5=p5, t1=t1)
  
return(out)
  
}
