// Example from https://beta.reactjs.org/learn

import Image from 'next/image'
import React, { useState } from 'react'
import styles from './counters.module.css'

// Modal overlay styles
const modalOverlayStyle = {
  position: 'fixed' as const,
  top: 0,
  left: 0,
  right: 0,
  bottom: 0,
  backgroundColor: 'rgba(0, 0, 0, 0.9)',
  display: 'flex',
  justifyContent: 'center',
  alignItems: 'center',
  zIndex: 9999,
  cursor: 'pointer',
  padding: '20px',
}

const modalImageContainerStyle = {
  position: 'relative' as const,
  maxWidth: '95vw',
  maxHeight: '95vh',
  cursor: 'default',
}

const closeButtonStyle = {
  position: 'absolute' as const,
  top: '-40px',
  right: '0px',
  background: 'transparent',
  border: 'none',
  color: 'white',
  fontSize: '32px',
  cursor: 'pointer',
  padding: '8px 16px',
  fontWeight: 'bold',
}

const imageContainerStyle = {
  cursor: 'pointer',
  transition: 'transform 0.2s ease, box-shadow 0.2s ease',
  borderRadius: '8px',
  overflow: 'hidden',
}

export function ImageSample(props) {
  const [isModalOpen, setIsModalOpen] = useState(false)

  const openModal = () => setIsModalOpen(true)
  const closeModal = () => setIsModalOpen(false)

  // Handle ESC key to close modal
  React.useEffect(() => {
    const handleEsc = (e: KeyboardEvent) => {
      if (e.key === 'Escape' && isModalOpen) {
        setIsModalOpen(false)
      }
    }
    if (isModalOpen) {
      window.addEventListener('keydown', handleEsc)
      return () => window.removeEventListener('keydown', handleEsc)
    }
  }, [isModalOpen])

  return (
    <>
      <div style={{
        marginTop: "30px",
        display: "flex",
        justifyContent: "center",
        width: "100%",
      }}>
        <div 
          style={{...imageContainerStyle, width: "100%"}}
          onClick={openModal}
          onMouseEnter={(e) => {
            e.currentTarget.style.transform = 'scale(1.02)'
            e.currentTarget.style.boxShadow = '0 8px 16px rgba(0,0,0,0.2)'
          }}
          onMouseLeave={(e) => {
            e.currentTarget.style.transform = 'scale(1)'
            e.currentTarget.style.boxShadow = 'none'
          }}
          title="Click to enlarge"
        >
          <Image 
            src={props.path} 
            width="1600" 
            height="1600" 
            alt="" 
            style={{
              width: "100%",
              height: "auto",
            }}
          />
        </div>
      </div>

      {isModalOpen && (
        <div style={modalOverlayStyle} onClick={closeModal}>
          <div style={modalImageContainerStyle} onClick={(e) => e.stopPropagation()}>
            <button style={closeButtonStyle} onClick={closeModal} title="Close (Esc)">
              ✕
            </button>
            <Image 
              src={props.path} 
              width="1600" 
              height="1600" 
              alt="" 
              style={{ 
                maxWidth: '95vw', 
                maxHeight: '95vh', 
                width: 'auto', 
                height: 'auto',
                objectFit: 'contain'
              }}
            />
          </div>
        </div>
      )}
    </>
  )
}

export function ImageSample1(props) {
  const [isModalOpen, setIsModalOpen] = useState(false)

  const openModal = () => setIsModalOpen(true)
  const closeModal = () => setIsModalOpen(false)

  // Handle ESC key to close modal
  React.useEffect(() => {
    const handleEsc = (e: KeyboardEvent) => {
      if (e.key === 'Escape' && isModalOpen) {
        setIsModalOpen(false)
      }
    }
    if (isModalOpen) {
      window.addEventListener('keydown', handleEsc)
      return () => window.removeEventListener('keydown', handleEsc)
    }
  }, [isModalOpen])

  return (
    <>
      <div style={{
        marginTop: "30px",
        display: "flex",
        justifyContent: "center",
        width: "100%",
      }}>
        <div 
          style={{...imageContainerStyle, width: "100%"}}
          onClick={openModal}
          onMouseEnter={(e) => {
            e.currentTarget.style.transform = 'scale(1.02)'
            e.currentTarget.style.boxShadow = '0 8px 16px rgba(0,0,0,0.2)'
          }}
          onMouseLeave={(e) => {
            e.currentTarget.style.transform = 'scale(1)'
            e.currentTarget.style.boxShadow = 'none'
          }}
          title="Click to enlarge"
        >
          <Image 
            src={props.path} 
            width="1600" 
            height="1600" 
            alt="" 
            style={{
              width: "100%",
              height: "auto",
            }}
          />
        </div>
      </div>

      {isModalOpen && (
        <div style={modalOverlayStyle} onClick={closeModal}>
          <div style={modalImageContainerStyle} onClick={(e) => e.stopPropagation()}>
            <button style={closeButtonStyle} onClick={closeModal} title="Close (Esc)">
              ✕
            </button>
            <Image 
              src={props.path} 
              width="1600" 
              height="1600" 
              alt="" 
              style={{ 
                maxWidth: '95vw', 
                maxHeight: '95vh', 
                width: 'auto', 
                height: 'auto',
                objectFit: 'contain'
              }}
            />
          </div>
        </div>
      )}
    </>
  )
}

export default function MyApp(props) {
  return <ImageSample path={props.path} />
}