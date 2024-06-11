// Example from https://beta.reactjs.org/learn





import Image from 'next/image'


{/* Import CSS modules */}










import { useState } from 'react'
import styles from './counters.module.css'

export  function ImageSample(props) {


  return (
    <div>
    <div  style={{
          marginTop:"30px",
          display: "flex",
          justifyContent: "center",
        }}>
    <Image src={props.path} width="800" height="800" alt="" />
    </div>
    </div>
  )
}

export function ImageSample1(props) {


  return (
    <div>
    <div  style={{
          marginTop:"30px",
          display: "flex",
          justifyContent: "center",
        }}>
    <Image src={props.path} width="600" height="600" alt="" />
    </div>
    </div>
  )
}

export default function MyApp(props) {
  return <ImageSample  path={props.path}/>
}