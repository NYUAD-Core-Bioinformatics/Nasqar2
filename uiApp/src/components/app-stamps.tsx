import React from 'react';
import { Card, CardContent, Typography, Box, Grid } from '@mui/material';

interface AppStampProps {
  title: string;
  href: string;
}

function AppStamp({ title, href }: AppStampProps) {
  return (
    <Card
      sx={{
        width: '100%',
        minHeight: 60,
        display: 'flex',
        alignItems: 'center',
        justifyContent: 'center',
        cursor: 'pointer',
        transition: 'all 0.2s ease-in-out',
        '&:hover': {
          transform: 'translateY(-2px)',
          boxShadow: 3,
        },
      }}
      component="a"
      href={href}
    >
      <CardContent sx={{ textAlign: 'center', p: '8px !important', width: '100%' }}>
        <Typography variant="body2" component="div" sx={{ fontSize: '0.875rem', fontWeight: 500 }}>
          {title}
        </Typography>
      </CardContent>
    </Card>
  );
}

export function AppStampsGrid() {
  const categories = [
    {
      name: 'Bulk RNA Seq',
      apps: [
        { title: 'DEseq2',         href: '/applications/rna_seq/bulk_rna/deseq2' },
        { title: 'STARTApp',       href: '/applications/rna_seq/bulk_rna/startapp' },
        { title: 'DeBrowser',      href: '/applications/rna_seq/bulk_rna/debrowser' },
        { title: 'Enrichment', href: '/applications/rna_seq/bulk_rna/enrichment' }
      ]
    },
    {
      name: 'Single Cell',
      apps: [
        { title: 'Monocle3', href: '/applications/rna_seq/single_cell/monocle3' },
        { title: 'Seurat V5', href: '/applications/rna_seq/single_cell/seuartV5' },
      ]
    },
    {
      name: 'Metagenomic',
      apps: [
        { title: 'DADA2', href: '/applications/meta_genomics/dada2' },
        { title: 'Animalcules', href: '/applications/meta_genomics/animalcules' },
      ]
    },
    {
      name: 'Epigenetic',
      apps: [
        { title: 'AtacseqQC', href: '/applications/epigenetics/bulk/atacseq_qc' },
      ]
    },
    {
      name: 'Data Preprocessing & QC',
      apps: [
        { title: 'GeneCountMerger', href: '/applications/data_preprocessing_and_qc/gene_count_merger' },
        { title: 'Merge FPKMS', href: '/applications/data_preprocessing_and_qc/merge_fpkms' },
      ]
    },
  ];

  return (
    <Box sx={{ flexGrow: 1, p: 2 }}>
      <Grid container spacing={3}>
        {categories.map((category, categoryIndex) => (
          <Grid item xs={12} sm={6} md={4} key={categoryIndex}>
            <Box 
              sx={{ 
                border: '1px solid',
                borderColor: 'divider',
                borderRadius: 2,
                overflow: 'hidden',
                height: '100%',
                display: 'flex',
                flexDirection: 'column',
              }}
            >
              <Box 
                sx={{ 
                  p: 2, 
                  backgroundColor: 'grey.100',
                  borderBottom: 1,
                  borderColor: 'divider',
                }}
              >
                <Typography variant="h6" sx={{ fontWeight: 600, fontSize: '1rem', color: 'text.primary' }}>
                  {category.name}
                </Typography>
              </Box>
              <Box sx={{ p: 2, flexGrow: 1 }}>
                <Grid container spacing={1.5}>
                  {category.apps.map((app, appIndex) => (
                    <Grid item xs={12} sm={6} key={appIndex}>
                      <AppStamp title={app.title} href={app.href} />
                    </Grid>
                  ))}
                </Grid>
              </Box>
            </Box>
          </Grid>
        ))}
      </Grid>
    </Box>
  );
}
