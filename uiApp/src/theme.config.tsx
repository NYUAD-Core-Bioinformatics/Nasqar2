import React from 'react'
import { DocsThemeConfig } from 'nextra-theme-docs'
import { useConfig } from 'nextra-theme-docs'


const config: DocsThemeConfig = {
  // logo: <span>Nasqar2</span>,
  logo: (
    <>
      <img src={'/CGSB-Logo.png'} width="60" />
      {/* <svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 206 246" fill="none">
        <circle cx="40" cy="40" r="40" fill="currentColor"/>
        <circle cx="40" cy="206" r="40" fill="currentColor"/>
        <circle cx="166" cy="120" r="40" fill="currentColor"/>
      </svg> */}
      <span style={{ marginLeft: '.4em', fontWeight: 800 }}>
      NASQAR2 : Nucleic Acid SeQuence Analysis Resource
      </span>
    </>
  ),
  // i18n: [
  //   { locale: 'en', text: 'English' },
  //   { locale: 'zh', text: '中文' },
  //   { locale: 'jp', text: '日本語'},
  //   { locale: 'pt', text: 'Português' },
  //   { locale: 'it', text: 'Italian' },
  //   { locale: 'tr', text: 'Türkçe' },
  //   { locale: 'es', text: 'Español' },
  //   { locale: 'fr', text: 'Français' },
  //   { locale: 'kr', text: '한국어' },
  //   { locale: `ca`, text: `Català` },
  //   { locale: 'fi', text: 'Finnish' },
  //   { locale: 'ru', text: 'Русский' },
  //   { locale: 'de', text: 'Deutsch' },
  // ],
  
  themeSwitch: {
    useOptions() {
      return {
        light: 'Light',
        dark: 'Dark',
        system: 'System'
      }
    }
  },
  project: {
    link: 'https://github.com/nabilr/nasqar2app',
  },
  chat: {
    link: 'https://discord.com',
  },
  docsRepositoryBase: 'https://github.com/nabilr/nasqar2app/tree/main',
  footer: {
    text: 'Nextra Docs Template',
  },


  darkMode: true,
  nextThemes: {
    defaultTheme: 'light',
    forcedTheme: 'light',

  }


  
}

export default config
