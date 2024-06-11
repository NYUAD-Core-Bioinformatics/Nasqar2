import * as React from 'react';
import { styled } from '@mui/material/styles';
import Grid from '@mui/material/Grid';
import Paper from '@mui/material/Paper';
import Box from '@mui/material/Box';
import Image from 'next/image';
import Typography from '@mui/material/Typography';

const Item = styled(Paper)(({ theme }) => ({
  backgroundColor: theme.palette.mode === 'dark' ? '#1A2027' : '#fff',
  ...theme.typography.body2,
  padding: theme.spacing(1),
  textAlign: 'center',
  color: theme.palette.text.secondary,
}));

export default function MyGrid(){

    return (
<Grid container rowSpacing={1} columnSpacing={{ xs: 1, sm: 2, md: 3 }}>
  <Grid item xs={6}>
    <Item><Image src="/deseq2_vst.png" width="800" height="800" alt="hello" /></Item>
    <Typography variant="caption" gutterBottom>
    Variance Stabilizing Transformation (VST): After DESeq2 analysis, the VST is utilized for data
    quality evaluation through sample clustering and visualization. 
      </Typography>
  </Grid>
  
  <Grid item xs={6}>
    <Item><Image src="/deseq2_dea2.png" width="800" height="800" alt="hello" /></Item>
    <Typography variant="caption" gutterBottom>
    DE results: MA plot illustrating differential gene expression analysis results obtained from DESeq2. 
    The Y-axis represents the log fold change in gene expression between conditions, 
    while the X-axis represents the average expression level across conditions. 
    

      </Typography>
  </Grid>
  <Grid item xs={6}>
    <Item><Image src="/deseq2_GE_boxplot2.png" width="800" height="800" alt="hello" /></Item>
    <Typography variant="caption" gutterBottom>
    Gene BoxPlot: Gene expression box plot illustrates the distribution of expression levels if a gene across conditions post-DESeq2 analysis. 


      </Typography>
  </Grid>
  
  
  <Grid item xs={6}>
    <Item><Image src="/deseq2_GE_heatmap2.png" width="800" height="800" alt="hello" /></Item>
    <Typography variant="caption" gutterBottom>
    Heatmap: Heatmap can be used to visualize the overall patterns of gene expression across samples or conditions following DESeq2 analysis.
      </Typography>
  </Grid>


  <Grid item xs={6}>
    <Item><Image src="/deseq2_volcano2.png" width="800" height="800" alt="hello" /></Item>
    <Typography variant="caption" gutterBottom>
    Volcano plot:A volcano plot is used to visualize the results of differential expression analysis by plotting the log fold change versus the statistical significance for each gene..
      </Typography>
  </Grid>


  <Grid item xs={6}>
    <Item><Image src="/deseq2_venn1.png" width="800" height="800" alt="hello" /></Item>
    <Typography variant="caption" gutterBottom>
    Venn digaram: To visualize the overlap between different sets of
    differentially expressed genes (DEGs) across multiple conditions can provide insights
    into shared and condition-specific gene expression changes. .
      </Typography>
  </Grid>
</Grid>)
}


