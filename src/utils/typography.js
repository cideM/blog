import Typography from 'typography'

const typography = new Typography({
  baseFontSize: '18px',
  baseLineHeight: 1.4,
  googleFonts: [
    {
      name: 'Oswald',
      styles: ['400', '500'],
    },
    {
      name: 'Vollkorn',
      styles: ['400', '400i', '700'],
    },
  ],
  bodyFontFamily: ['Vollkorn'],
  headerFontFamily: ['Oswald'],
})

export default typography

const rhythm = typography.rhythm
const scale = typography.scale

export { rhythm, scale }
