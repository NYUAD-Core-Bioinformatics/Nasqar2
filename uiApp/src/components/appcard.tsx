
import Card from '@mui/material/Card';
import CardContent from '@mui/material/CardContent';
import CardMedia from '@mui/material/CardMedia';
import Typography from '@mui/material/Typography';
import { Button, CardActionArea, CardActions } from '@mui/material';



import * as React from 'react';
import { styled } from '@mui/material/styles';
import Box from '@mui/material/Box';
import Paper from '@mui/material/Paper';
import Grid from '@mui/material/Grid';

const Item = styled(Paper)(({ theme }) => ({
  backgroundColor: theme.palette.mode === 'dark' ? '#1A2027' : '#fff',
  ...theme.typography.body2,
  padding: theme.spacing(1),
  textAlign: 'center',
  color: theme.palette.text.secondary,
}));



function MultiActionAreaCardDeseq2() {
  return (
    <Card sx={{ maxWidth: 345 }}>
      <CardActionArea>
        <CardMedia
          component="img"
          height="140"
          image="/deseq2_dea2.png"
          alt="green iguana"
        />
        <CardContent>
          <Typography gutterBottom variant="h5" component="div">
            DESseq2 Shniy
          </Typography>
          <Typography variant="body2" color="text.secondary">
            DESeq2, an R package for
            “Differential gene expression analysis based on the negative
            binomial distribution”.

          </Typography>
        </CardContent>
      </CardActionArea>
      <CardActions>
        <Button size="small" color="primary">
          Launch
        </Button>
      </CardActions>
    </Card>
  );
}


function MultiActionAreaCardDeBrowser() {
  return (
    <Card sx={{ maxWidth: 345 }}>
      <CardActionArea>
        <CardMedia
          component="img"
          height="140"
          image="/deseq2_dea2.png"
          alt="green iguana"
        />
        <CardContent>
          <Typography gutterBottom variant="h5" component="div">
            DEBrowser Shiny
          </Typography>
          {/* <Typography variant="body2" color="text.secondary">
              DESEQ2
            </Typography> */}
        </CardContent>
      </CardActionArea>
      <CardActions>
        <Button size="small" color="primary">
          Launch
        </Button>
      </CardActions>
    </Card>
  );
}


function MultiActionAreaCardStartApp() {
  return (
    <Card sx={{ maxWidth: 345 }}>
      <CardActionArea>
        <CardMedia
          component="img"
          height="140"
          image="/deseq2_dea2.png"
          alt="green iguana"
        />
        <CardContent>
          <Typography gutterBottom variant="h5" component="div">
            STARTApp
          </Typography>
          <Typography variant="body2" color="text.secondary">
            DESEQ2
          </Typography>
        </CardContent>
      </CardActionArea>
      <CardActions>
        <Button size="small" color="primary">
          Launch
        </Button>
      </CardActions>
    </Card>
  );
}





export default function BasicGrid() {
  return (
    <Box sx={{ flexGrow: 1 }}>
      <Grid container spacing={2}>

        <Grid item xs={6}>
          <Item><MultiActionAreaCardDeseq2 /></Item>
        </Grid>
        <Grid item xs={6}>
          <Item><MultiActionAreaCardDeBrowser /></Item>
        </Grid>
        <Grid item xs={6}>
          <Item><MultiActionAreaCardStartApp /></Item>
        </Grid>
      </Grid>
    </Box>
  );
}


export  function KrisCard() {
  return (
    <Card sx={{ width: 345, height:480}}>
        <CardActionArea  href='https://as.nyu.edu/faculty/kristin-gunsalus.html' target="_blank">
      <CardMedia
        sx={{ height: 340 }}
        image="/kris.jpeg"
        title="green iguana"
      />
      <CardContent>
        <Typography gutterBottom variant="h5" component="div">
        Kristin Gunsalus
        </Typography>
        <Typography variant="body2" color="text.secondary">
        Director of Bioinformatics, NYUAD CGSB
        </Typography>
      </CardContent>
      </CardActionArea>
     
    </Card>
  );
}


export  function NizarCard() {
  return (
    <Card sx={{ width: 345, height:480}}>
        <CardActionArea  href='https://nyuad.nyu.edu/en/research/faculty-labs-and-projects/nyuad-cgsb/cgsb-cores/bioinformatics-core/nizar-drou.html' target="_blank">
      <CardMedia
        sx={{ height: 340 }}
        image="/nizar.jpeg"
        title="green iguana"
      />
      <CardContent>
        <Typography gutterBottom variant="h5" component="div">
        Nizar Drou
        </Typography>
        <Typography variant="body2" color="text.secondary">
        Core Bioinformatics Lead Developer
        </Typography>
      </CardContent>
      </CardActionArea>
     
    </Card>
  );
}

export  function ArshadCard() {
  return (
    <Card sx={{ width: 345, height:480}}>
        <CardActionArea  href='https://nyuad.nyu.edu/en/research/faculty-labs-and-projects/nyuad-cgsb/cgsb-cores/bioinformatics-core/muhammad-arshad.html' target="_blank">
      <CardMedia
        sx={{ height: 345 }}
        image="/arshad.jpeg"
        title="green iguana"
      />
      <CardContent>
        <Typography gutterBottom variant="h5" component="div">
        Muhammad Arshad
        </Typography>
        <Typography variant="body2" color="text.secondary">
        Research Scientist
        </Typography>
      </CardContent>
      </CardActionArea>
     
    </Card>
  );
}

export  function JayaramCard() {
  return (
    <Card sx={{ width: 345, height:480}}>
        <CardActionArea  href='https://nyuad.nyu.edu/en/research/faculty-labs-and-projects/nyuad-cgsb/cgsb-cores/bioinformatics-core/jayaram-radhakrishnan.html' target="_blank">
      <CardMedia
        sx={{ height: 340 }}
        image="/jayaram.jpeg"
        title="green iguana"
      />
      <CardContent>
        <Typography gutterBottom variant="h5" component="div">
        Jayaram Radhakrishnan
        </Typography>
        <Typography variant="body2" color="text.secondary">
        Bioinformatics Infrastructure Engineer
        </Typography>
      </CardContent>
      </CardActionArea>
     
    </Card>
  );
}


export  function SaldiCard() {
  return (
    <Card sx={{ width: 345, height:480}}>
        <CardActionArea href=""  target="_blank">
      <CardMedia
        sx={{ height: 340 }}
        image="/Giuseppe.jpeg"
        title="green iguana"
      />
      <CardContent>
        <Typography gutterBottom variant="h5" component="div">
        Giuseppe-Antonio Saldi
        </Typography>
        <Typography variant="body2" color="text.secondary">
        Data Computational Scientist
        </Typography>
      </CardContent>
      </CardActionArea>
     
    </Card>
  );
}


export  function NabilCard() {
  return (
    <Card sx={{ width: 345, height:480}}>
        <CardActionArea  href='https://nyuad.nyu.edu/en/research/faculty-labs-and-projects/nyuad-cgsb/cgsb-cores/bioinformatics-core/nizar-drou.html' target="_blank">
      <CardMedia
        sx={{ height: 340 }}
        image="/nabil.jpeg"
        title="green iguana"
      />
      <CardContent>
        <Typography gutterBottom variant="h5" component="div">
        Nabil Rahiman
        </Typography>
        <Typography variant="body2" color="text.secondary">
        Bioiformatics Full Stack Developer
        </Typography>
      </CardContent>
      </CardActionArea>
     
    </Card>
  );
}

