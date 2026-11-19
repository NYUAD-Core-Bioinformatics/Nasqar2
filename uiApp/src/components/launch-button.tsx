import React from 'react'

const style: React.CSSProperties = {
  display: 'inline-flex',
  alignItems: 'center',
  justifyContent: 'center',
  minHeight: '40px',
  padding: '8px 18px',
  marginTop: '1rem',
  backgroundColor: '#2563eb',
  color: '#ffffff',
  fontWeight: 600,
  fontSize: '14px',
  textDecoration: 'none',
  borderRadius: '6px',
}

// One launch button for every application page.
export function LaunchButton({ path, children }: { path: string; children: React.ReactNode }) {
  const base = process.env.NEXT_PUBLIC_NASQAR_BASE_PATH || ''
  return (
    <a href={`${base}${path}`} target="_blank" rel="noreferrer" style={style}>
      {children}
    </a>
  )
}
