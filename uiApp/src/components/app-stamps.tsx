import React from 'react';
import { Card, CardContent, Typography, Box, Grid } from '@mui/material';
import {
  Science,
  Biotech,
  Dns,
  Analytics,
  Transform,
  CallMerge,
  Search,
  Assessment,
  BubbleChart,
  ScatterPlot,
  SettingsInputAntenna,
  BiotechOutlined
} from '@mui/icons-material';

interface AppStampProps {
  title: string;
  href: string;
  icon: React.ReactNode;
}

function AppStamp({ title, href, icon }: AppStampProps) {
  return (
    <Card
      sx={{
        width: 140,
        height: 120,
        display: 'flex',
        flexDirection: 'column',
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
      <CardContent sx={{ textAlign: 'center', p: 1 }}>
        <Box sx={{ mb: 1, color: 'primary.main' }}>
          {icon}
        </Box>
        <Typography variant="body2" component="div" sx={{ fontSize: '0.75rem', fontWeight: 500 }}>
          {title}
        </Typography>
      </CardContent>
    </Card>
  );
}

export function AppStampsGrid() {
  const apps = [
    // Data Preprocessing & QC
    { title: 'Gene Count Merger', href: '/applications/data_preprocessing_and_qc/gene_count_merger', icon: <CallMerge sx={{ fontSize: 32 }} /> },
    { title: 'Merge FPKMS', href: '/applications/data_preprocessing_and_qc/merge_fpkms', icon: <Transform sx={{ fontSize: 32 }} /> },

    // RNA Seq - Bulk RNA
    { title: 'Debrowser', href: '/applications/rna_seq/bulk_rna/debrowser', icon: <Analytics sx={{ fontSize: 32 }} /> },
    { title: 'DEseq2', href: '/applications/rna_seq/bulk_rna/deseq2', icon: <Assessment sx={{ fontSize: 32 }} /> },
    { title: 'STARTApp', href: '/applications/rna_seq/bulk_rna/startapp', icon: <Science sx={{ fontSize: 32 }} /> },
    { title: 'Enrichment', href: '/applications/rna_seq/bulk_rna/enrichment', icon: <Search sx={{ fontSize: 32 }} /> },

    // RNA Seq - Single Cell
    { title: 'Monocle3', href: '/applications/rna_seq/single_cell/monocle3', icon: <BubbleChart sx={{ fontSize: 32 }} /> },
    { title: 'Seuart V5', href: '/applications/rna_seq/single_cell/seuartV5', icon: <ScatterPlot sx={{ fontSize: 32 }} /> },

    // Meta Genomics
    { title: 'DADA2', href: '/applications/meta_genomics/dada2', icon: <Dns sx={{ fontSize: 32 }} /> },
    { title: 'Animalcules', href: '/applications/meta_genomics/animalcules', icon: <Biotech sx={{ fontSize: 32 }} /> },

    // Epigenetics
    { title: 'Atacseq QC', href: '/applications/epigenetics/bulk/atacseq_qc', icon: <SettingsInputAntenna sx={{ fontSize: 32 }} /> },
  ];

  return (
    <Box sx={{ flexGrow: 1, p: 2 }}>
      <Grid container spacing={2} justifyContent="center">
        {apps.map((app, index) => (
          <Grid item key={index}>
            <AppStamp title={app.title} href={app.href} icon={app.icon} />
          </Grid>
        ))}
      </Grid>
    </Box>
  );
}
