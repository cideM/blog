import Typography from 'typography'

const typography = new Typography({
  baseFontSize: '18px',
  baseLineHeight: 1.4,
  googleFonts: [
    {
      name: 'Roboto',
      styles: ['400', '500', '400i'],
    },
    {
      name: 'Roboto Mono',
      styles: ['400'],
    },
  ],
  bodyFontFamily: ['Roboto'],
  headerFontFamily: ['Roboto'],
})

export default typography

const rhythm = typography.rhythm
const scale = typography.scale

export { rhythm, scale }
