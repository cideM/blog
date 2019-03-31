import Typography from 'typography'

const typography = new Typography({
  baseFontSize: '18px',
  baseLineHeight: 1.4,
  googleFonts: [
    {
      name: 'Roboto',
      styles: ['400', '400i', '700'],
    },
    {
      name: 'Roboto Mono',
      styles: ['400', '400i', '700'],
    },
  ],
  bodyFontFamily: ['Roboto'],
  headerFontFamily: ['Roboto'],
})

export default typography

const rhythm = typography.rhythm
const scale = typography.scale

export { rhythm, scale }
