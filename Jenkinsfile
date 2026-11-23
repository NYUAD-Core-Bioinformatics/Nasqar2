// Nasqar-CI: build the image, run it on the build host and check it.
//
// Site-specific values are not kept in this file. Define them once as
// environment variables in Jenkins (Manage Jenkins > System > Global
// properties > Environment variables); the first stage stops the build and
// lists any that are missing.
//
//   NASQAR_BUILD_HOST                  name of the build host the container is reached on, e.g. build.example.org

pipeline {
  agent any

  options {
    disableConcurrentBuilds()
    ws('/workspace/jobs')
  }

  environment {
    REPO_URL    = 'https://github.com/NYUAD-Core-Bioinformatics/Nasqar2'
    REPO_BRANCH = 'main'
    APP_IMAGE   = 'nasqarall'
    APP_NAME    = 'nasqar'
    APP_PORT    = '8181'
    APP_HOST    = "${env.NASQAR_BUILD_HOST}"
  }

  stages {

    stage('Check configuration') {
      steps {
        script {
          def required = ['NASQAR_BUILD_HOST']
          def missing = required.findAll { !env[it] }
          if (missing) {
            error "Not set in Jenkins (Manage Jenkins > System > Global properties > Environment variables): ${missing.join(', ')}"
          }
        }
      }
    }

    stage('Cleanup Workspace') {
      steps {
        echo "=== Cleaning /workspace/jobs to avoid @2 directories ==="
        deleteDir()
        // The previous container keeps serving until the new image is built.
        // Only dangling images and old build cache are removed, so the slow
        // Dockerfile layers (conda environment, BSgenome, dbSNP, example data)
        // are reused when only application sources changed.
        sh '''
          docker image prune -f || true
          docker builder prune -f --filter until=336h || true
        '''
      }
    }

    stage('Build Docker Image') {
      steps {
        sh '''#!/bin/bash
          set -eux
          echo "=== Cloning Nasqar2 repo ==="
          rm -rf Nasqar2 || true
          git clone --depth 1 --branch "${REPO_BRANCH}" "${REPO_URL}" Nasqar2

          cd Nasqar2
          git log -1 --oneline

          # The Dockerfile is self-contained: it downloads the BSgenome and
          # dbSNP packages from Bioconductor and the example data and SILVA
          # references through common/fetch-app-data.sh. Nothing is extracted
          # or patched here.
        '''
        // Downloads from conda, CRAN, Bioconductor and GitHub fail now and
        // then; a retry resumes from the last cached layer.
        retry(3) {
          sh '''#!/bin/bash
            set -eux
            cd Nasqar2
            echo "=== Building image ==="
            docker build -f Dockerfile -t "${APP_IMAGE}:latest" .
          '''
        }
      }
    }

    stage('Run Docker Container') {
      steps {
        sh '''#!/bin/bash
          set -eux
          echo "=== Replacing container ==="
          docker rm -f "${APP_NAME}" || true
          docker run -d --name "${APP_NAME}" -p "${APP_PORT}:80" "${APP_IMAGE}:latest"
          docker ps -a | grep "${APP_NAME}"
          docker images
        '''
      }
    }

    stage('Health Check URLs') {
      steps {
        sh '''#!/bin/bash
          set -euo pipefail

          BASE="http://${APP_HOST}:${APP_PORT}"
          URLS=(
            "${BASE}/"
            "${BASE}/GeneCountMerger/"
            "${BASE}/mergeFPKMs/"
            "${BASE}/DEBrowser/"
            "${BASE}/deseq2shiny/"
            "${BASE}/tsar_nasqar/"
            "${BASE}/ClusterProfShinyGSEA/"
            "${BASE}/ClusterProfShinyORA/"
            "${BASE}/DADA2Shiny/"
            "${BASE}/animalcules/"
            "${BASE}/ATACseqQCShniy/"
            "${BASE}/SeuratV5Shiny/"
            "${BASE}/monocle3/"
            "${BASE}/VariantAnnotationShiny/"
          )

          # The applications load their packages at startup, some for over a
          # minute, so each URL is retried for up to 5 minutes.
          failed=0
          for url in "${URLS[@]}"; do
            echo "=== Checking $url ==="
            code=000
            for attempt in $(seq 1 30); do
              code=$(curl -s -o /dev/null --max-time 20 -w "%{http_code}" "$url" || true)
              [ "$code" = "200" ] && break
              sleep 10
            done
            if [ "$code" = "200" ]; then
              echo "SUCCESS: $url returned 200 OK"
            else
              echo "ERROR: $url returned $code"
              failed=1
            fi
          done

          echo "=== supervisord programs ==="
          docker exec "${APP_NAME}" supervisorctl status || failed=1

          exit $failed
        '''
      }
    }

    stage('Validate R Packages') {
      steps {
        sh '''#!/bin/bash
          set -euo pipefail
          echo "=== Validating required R packages inside container ==="

          docker exec "${APP_NAME}" Rscript -e 'pkgs <- c("smplot2", "GOplot", "wordcloud2", "genesorteR", "dqshiny", "BPCells", "SeuratWrappers", "monocle3", "shinythemes", "VariantAnnotation", "TxDb.Hsapiens.UCSC.hg19.knownGene", "TxDb.Hsapiens.UCSC.hg38.knownGene", "org.Hs.eg.db", "BSgenome.Hsapiens.UCSC.hg19", "BSgenome.Hsapiens.UCSC.hg38", "SNPlocs.Hsapiens.dbSNP144.GRCh37", "SNPlocs.Hsapiens.dbSNP144.GRCh38"); ok <- vapply(pkgs, requireNamespace, logical(1), quietly = TRUE); for (p in pkgs) cat(if (ok[[p]]) "OK:" else "FAILED:", p, fill = TRUE); if (!all(ok)) quit(status = 1)'
        '''
      }
    }

    stage('Validate App Data') {
      steps {
        sh '''#!/bin/bash
          set -euo pipefail
          echo "=== Validating example data and references inside container ==="

          docker exec "${APP_NAME}" bash -c '
            status=0
            for d in \
              deseq2shiny/src/www/exampleData \
              dada2Shiny/src/www/exampleData \
              ClusterProfShinyORA/src/www/exampleData \
              ClusterProfShinyGSEA/src/www/exampleData \
              tsar_nasqar/src/www/exampleData \
              mergeFPKMs/src/www/exampleData \
              ATACseqQCShniy/src/www/exampleData; do
              if [ -n "$(ls -A "/srv/shiny-server/$d" 2>/dev/null)" ]; then
                echo "OK: $d"
              else
                echo "FAILED: $d is missing or empty"; status=1
              fi
            done
            for f in \
              dada2Shiny/src/www/taxonomy/silva_nr99_v138.1_train_set.fa.gz \
              dada2Shiny/src/www/taxonomy/silva_species_assignment_v138.1.fa.gz; do
              if [ -s "/srv/shiny-server/$f" ]; then
                echo "OK: $f"
              else
                echo "FAILED: $f is missing or empty"; status=1
              fi
            done
            exit $status'
        '''
      }
    }

  }

  post {
    always {
      echo "Pipeline completed (image build + Docker run + health checks)."
    }
    failure {
      echo "❌ Build or checks failed — investigate logs."
      sh '''
        docker exec "${APP_NAME}" supervisorctl status || true
        docker logs --tail 200 "${APP_NAME}" || true
      '''
    }
    success {
      echo "✅ All health checks passed successfully!"
    }
  }
}
