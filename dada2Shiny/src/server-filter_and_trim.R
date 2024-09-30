reactiveInputData <- eventReactive(input$runDADA2, {
    qc_done(FALSE)
    #    shinyjs::hide(selector = "a[data-value=\"qualityprofile_tab\"]")
    shinyjs::hide(selector = "a[data-value=\"errorRatesTab\"]")
    # shinyjs::hide(selector = "a[data-value=\"filter_and_trim_tab\"]")
    shinyjs::hide(selector = "a[data-value=\"margePairedReadsTab\"]")
    shinyjs::hide(selector = "a[data-value=\"trackReadsTab\"]")
    shinyjs::hide(selector = "a[data-value=\"taxanomyTab\"]")
    shinyjs::hide(selector = "a[data-value=\"alphaDiversityTab\"]")




    js$addStatusIcon("filter_and_trim_tab", "loading")
    req(input$sel_sample_qualityprofile_tab)
    path <- my_values$base_dir
    sample.names <- row.names(my_values$samples_df)

    # print(my_values$samples_df)
    fnFs <- file.path(path, my_values$samples_df[, "FASTQ_Fs"])

    filtFs <- file.path(path, "filtered", paste0(sample.names, "_F_filt.fastq.gz"))
    print("dir1")
    sapply(fnFs, function(fasqfile) {
        print(file.exists(fasqfile))
    })
    names(filtFs) <- sample.names

    if (input$seq_type == "paired") {
        fnRs <- file.path(path, my_values$samples_df[, "FASTQ_Rs"])
        filtRs <- file.path(path, "filtered", paste0(sample.names, "_R_filt.fastq.gz"))
        print("dir2")
        sapply(fnRs, function(fasqfile) {
            print(file.exists(fasqfile))
        })

        names(filtRs) <- sample.names
    }
    # print(fnFs)
    # print(fnRs)







    withProgress(message = "Running DADA2 , please wait", {
        shiny::setProgress(value = 0.1, detail = "...filterAndTrim")

        # disabling multithread as it is runs in reactive context
        if (input$seq_type == "paired") {
            out <- filterAndTrim(fnFs, filtFs, fnRs, filtRs,
                truncLen = c(input$truncLen_fwd, input$truncLen_rev),
                maxN = 0, maxEE = c(input$maxEE_fwd, input$maxEE_rev), truncQ = 2, rm.phix = TRUE,
                compress = TRUE, multithread = FALSE
            )
        } else {
            out <- filterAndTrim(fnFs, filtFs,
                truncLen = c(input$truncLen_fwd),
                maxN = 0, maxEE = c(input$maxEE_fwd), truncQ = 2, rm.phix = TRUE,
                compress = TRUE, multithread = FALSE
            )
        }

        rownames(out) <- sample.names

        shiny::setProgress(value = 0.2, detail = "...learnErrors Fr")
        errF <- learnErrors(filtFs, multithread = TRUE)

        if (input$seq_type == "paired") {
            shiny::setProgress(value = 0.3, detail = "...learnErrors Rr")
            errR <- learnErrors(filtRs, multithread = TRUE)
        }

        shiny::setProgress(value = 0.4, detail = "...dadafs")
        dadaFs <- dada(filtFs, err = errF, multithread = TRUE)

        if (input$seq_type == "paired") {
            shiny::setProgress(value = 0.5, detail = "...dadaRs")
            dadaRs <- dada(filtRs, err = errR, multithread = TRUE)
        }

        if (input$seq_type == "paired") {
            shiny::setProgress(value = 0.6, detail = "...mergePairs")
            mergers <- mergePairs(dadaFs, filtFs, dadaRs, filtRs, verbose = TRUE)
            # Inspect the merger data.frame from the first sample
            print("mergers")



            # print(names(head(mergers)))
            updateSelectInput(session, "selSample4margePairedReadsTab", choices = names(mergers))
        }
        shiny::setProgress(value = 0.7, detail = "...makeSequenceTable")

        if (input$seq_type == "paired") {
            seqtab <- makeSequenceTable(mergers)
        } else {
            seqtab <- makeSequenceTable(dadaFs)
        }
        dim(seqtab)

        # Inspect distribution of sequence lengths
        print("table")
        seqtabTable <- table(nchar(getSequences(seqtab)))

        shiny::setProgress(value = 0.8, detail = "...makeSequenceTable")
        # remove chimeras from the sequence table:
        seqtab.nochim <- removeBimeraDenovo(seqtab, method = "consensus", multithread = TRUE, verbose = TRUE)
        dim(seqtab.nochim)

        sum(seqtab.nochim) / sum(seqtab)

        getN <- function(x) sum(getUniques(x))

        if (input$seq_type == "paired") {
            track <- cbind(out, sapply(dadaFs, getN), sapply(dadaRs, getN), sapply(mergers, getN), rowSums(seqtab.nochim))
            # If processing a single sample, remove the sapply calls: e.g. replace sapply(dadaFs, getN) with getN(dadaFs)
            colnames(track) <- c("input", "filtered", "denoisedF", "denoisedR", "merged", "nonchim")
            rownames(track) <- sample.names
        } else {
            track <- cbind(out, sapply(dadaFs, getN), rowSums(seqtab.nochim))
            # If processing a single sample, remove the sapply calls: e.g. replace sapply(dadaFs, getN) with getN(dadaFs)
            colnames(track) <- c("input", "filtered", "denoisedF", "nonchim")
            rownames(track) <- sample.names
        }
        # updateSelectInput(session, "selSample4trackReadsTab", choices = sample.names)


        print("track")
        # print(head(track))

        print("taxa")


        # taxa <- assignTaxonomy(seqtab.nochim, "./www/taxonomy/silva_nr99_v138.1_train_set.fa.gz", multithread = TRUE)
        # print(taxa)
        # print("taxa second")
        # taxa <- addSpecies(taxa, "./www/taxonomy/silva_species_assignment_v138.1.fa.gz")
        # print(taxa)
        # taxa.print <- taxa # Removing sequence rownames for display only
        # rownames(taxa.print) <- NULL


        # print(head(taxa.print))


        # samples.out <- rownames(seqtab.nochim)
        # print('samples.out')
        # print(samples.out)
        # subject <- sapply(strsplit(samples.out, "D"), '[', 1)
        # gender <- substr(subject,1,1)
        # subject <- substr(subject,2,999)
        # # day <- as.integer(sapply(strsplit(samples.out, "D"), '[', 2))
        # day <- as.integer(sapply(strsplit(sapply(strsplit(samples.out, "D"), '[', 2), '_'), '[', 1))
        # samdf <- data.frame(Subject=subject, Gender=gender, Day=day)
        # samdf$When <- "Early"
        # samdf$When[samdf$Day>100] <- "Late"
        # rownames(samdf) <- samples.out

        # print("samples.out")
        # print(samdf)

        # ps <- phyloseq(otu_table(seqtab.nochim, taxa_are_rows=FALSE),
        #        sample_data(samdf),
        #        tax_table(taxa))
        # ps <- prune_samples(sample_names(ps) != "Mock", ps) # Remove mock sample


        # dna <- Biostrings::DNAStringSet(taxa_names(ps))
        # names(dna) <- taxa_names(ps)
        # ps <- merge_phyloseq(ps, dna)
        # taxa_names(ps) <- paste0("ASV", seq(ntaxa(ps)))
        # print(ps)
        # # Transform data to proportions as appropriate for Bray-Curtis distances
        # ps.prop <- transform_sample_counts(ps, function(otu) otu/sum(otu))
        # ord.nmds.bray <- ordinate(ps.prop, method="NMDS", distance="bray")


        # top20 <- names(sort(taxa_sums(ps), decreasing=TRUE))[1:20]
        # ps.top20 <- transform_sample_counts(ps, function(OTU) OTU/sum(OTU))
        # ps.top20 <- prune_taxa(top20, ps.top20)

        print("qc done")


        qc_done(TRUE)

        shiny::setProgress(value = 1.0, detail = "...done")
    })

    shinyjs::show(selector = "a[data-value=\"errorRatesTab\"]")
    if (input$seq_type == "paired") {
        shinyjs::show(selector = "a[data-value=\"margePairedReadsTab\"]")
        js$addStatusIcon("margePairedReadsTab", "done")
    }

    shinyjs::show(selector = "a[data-value=\"taxanomyTab\"]")
    shinyjs::show(selector = "a[data-value=\"trackReadsTab\"]")
    shinyjs::show(selector = "a[data-value=\"filter_and_trim_tab\"]")
    # shinyjs::show(selector = "a[data-value=\"taxanomyTab\"]")



    js$addStatusIcon("filter_and_trim_tab", "done")
    js$addStatusIcon("errorRatesTab", "done")
    js$addStatusIcon("trackReadsTab", "done")
    js$addStatusIcon("taxanomyTab", "next")
    





    if (input$seq_type == "paired") {
        return(list(sample.names=sample.names,dadaFs=dadaFs,dadaRs=dadaRs,mergers=mergers, out = out, errF = errF, errR = errR, mergers = mergers, seqtabTable = seqtabTable, track = track, seqtab.nochim = seqtab.nochim))
    } else {
        return(list(sample.names=sample.names,dadaFs=dadaFs,dadaRs=dadaRs, out = out, errF = errF, seqtabTable = seqtabTable, track = track, seqtab.nochim = seqtab.nochim))
    }
})

output$dada2object_ready <- reactive({
    return(!is.null(reactiveInputData()))
})
outputOptions(output, "dada2object_ready", suspendWhenHidden = FALSE)






output$filterAndTrim_output_table <- DT::renderDataTable(
    {
        # print(filtFs)

        data <- reactiveInputData()

        # output_table_wth_samples <- cbind(Sample = rownames(data$out), data$out)
        output_table_wth_samples <- cbind(Sample = data$sample.names, data$out)
        
        colnames(output_table_wth_samples) <- c('Sample', "Fragments before Quality trimming", "Fragments after Quality trimming")
        output_table_wth_samples
    },
    rownames = FALSE, 
    options = list(scrollX = TRUE, pageLength = 10)
)
