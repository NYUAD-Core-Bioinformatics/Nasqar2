const withNextra = require('nextra')({
  theme: 'nextra-theme-docs',
  themeConfig: './theme.config.tsx',

})

const basePath = process.env.NASQAR_UI_BASE_PATH || ''

if (basePath && (!basePath.startsWith('/') || basePath.endsWith('/'))) {
  throw new Error('NASQAR_UI_BASE_PATH must start with / and must not end with /')
}


/**
 * @type {import('next').NextConfig}
 */
const nextConfig = {
	//output: "standalone",
	output: "export",
  basePath,
  assetPrefix: basePath || undefined,
  env: {
    NEXT_PUBLIC_NASQAR_BASE_PATH: basePath,
  },
  images: {
    unoptimized: true,
  },
 
  // Optional: Change links `/me` -> `/me/` and emit `/me.html` -> `/me/index.html`
  // trailingSlash: true,
 
  // Optional: Prevent automatic `/me` -> `/me/`, instead preserve `href`
  // skipTrailingSlashRedirect: true,
 
  // Optional: Change the output directory `out` -> `dist`
   
  
}

//module.exports = nextConfig
module.exports = {...withNextra(), ...nextConfig}
