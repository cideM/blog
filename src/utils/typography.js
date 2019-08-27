import Typography from 'typography'

const typography = new Typography({
  baseFontSize: '16px',
  baseLineHeight: 1.4,
  googleFonts: [
    {
      name: 'IBM Plex Sans',
      styles: ['400', '500', '400i'],
    },
    {
      name: 'IBM Plex Mono',
      styles: ['400'],
    },
  ],
  bodyFontFamily: ['IBM Plex Sans'],
  headerFontFamily: ['IBM Plex Sans'],
})

export default typography

const rhythm = typography.rhythm
const scale = typography.scale

export { rhythm, scale }
