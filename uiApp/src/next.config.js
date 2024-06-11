const withNextra = require('nextra')({
  theme: 'nextra-theme-docs',
  themeConfig: './theme.config.tsx',

})


/**
 * @type {import('next').NextConfig}
 */
const nextConfig = {
	//output: "standalone",
	output: "export",
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
