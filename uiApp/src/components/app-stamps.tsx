import React from 'react';
import { Card, CardContent, Typography, Box, Grid } from '@mui/material';

interface AppStampProps {
  title: string;
  href: string;
  // position in the group's workflow; apps without one are alternatives
  step?: number;
}

function AppStamp({ title, href, step }: AppStampProps) {
  const basePath = process.env.NEXT_PUBLIC_NASQAR_BASE_PATH || '';

  return (
    <Card
      sx={{
        width: '100%',
        minHeight: 48,
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
      href={`${basePath}${href}`}
    >
      <CardContent sx={{ display: 'flex', alignItems: 'center', justifyContent: 'flex-start', gap: 1.25, p: '8px 14px !important', width: '100%' }}>
        {step !== undefined && (
          <Box
            component="span"
            aria-label={`Step ${step}`}
            sx={{
              flex: '0 0 auto',
              width: 22,
              height: 22,
              borderRadius: '50%',
              backgroundColor: 'primary.main',
              color: 'primary.contrastText',
              fontSize: '0.75rem',
              fontWeight: 600,
              lineHeight: '22px',
              textAlign: 'center',
            }}
          >
            {step}
          </Box>
        )}
        <Typography variant="body2" component="div" sx={{ fontSize: '0.875rem', fontWeight: 500, textAlign: 'center' }}>
          {title}
        </Typography>
      </CardContent>
    </Card>
  );
}

export function AppStampsGrid() {
  // Groups follow the order of an analysis, and so do the apps inside each
  // group: a numbered app takes the output of the one before it. Apps without
  // a number are alternatives to the numbered ones.
  const categories: { name: string; note?: string; apps: AppStampProps[] }[] = [
    {
      name: 'Data Preprocessing & QC',
      note: 'Start here to turn per-sample files into one table.',
      apps: [
        { title: 'Gene Count Merger', href: '/applications/data_preprocessing_and_qc/gene_count_merger' },
        { title: 'Merge FPKMs', href: '/applications/data_preprocessing_and_qc/merge_fpkms' },
      ]
    },
    {
      name: 'Bulk RNA-seq',
      note: 'START and DEBrowser are alternatives to DESeq2.',
      apps: [
        { title: 'DESeq2',     href: '/applications/rna_seq/bulk_rna/deseq2', step: 1 },
        { title: 'Enrichment', href: '/applications/rna_seq/bulk_rna/enrichment', step: 2 },
        { title: 'START',      href: '/applications/rna_seq/bulk_rna/startapp' },
        { title: 'DEBrowser',  href: '/applications/rna_seq/bulk_rna/debrowser' },
      ]
    },
    {
      name: 'Single Cell',
      apps: [
        { title: 'Seurat v5', href: '/applications/rna_seq/single_cell/seuartV5', step: 1 },
        { title: 'Monocle 3', href: '/applications/rna_seq/single_cell/monocle3', step: 2 },
      ]
    },
    {
      name: 'Metagenomics',
      apps: [
        { title: 'DADA2', href: '/applications/meta_genomics/dada2', step: 1 },
        { title: 'Animalcules', href: '/applications/meta_genomics/animalcules', step: 2 },
      ]
    },
    {
      name: 'Epigenetics',
      apps: [
        { title: 'ATACseqQC', href: '/applications/epigenetics/bulk/atacseq_qc' },
      ]
    },
    {
      name: 'Variant Annotation',
      apps: [
        { title: 'Variant Annotation', href: '/applications/variant_annotation/variant_annotation' },
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
                {category.note && (
                  <Typography variant="body2" sx={{ fontSize: '0.8rem', color: 'text.secondary', mt: 0.5 }}>
                    {category.note}
                  </Typography>
                )}
              </Box>
              <Box sx={{ p: 2, flexGrow: 1 }}>
                {/* one app per row: with its step number the list reads top to bottom */}
                <Grid container spacing={1.5}>
                  {category.apps.map((app, appIndex) => (
                    <Grid item xs={12} key={appIndex}>
                      <AppStamp title={app.title} href={app.href} step={app.step} />
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
