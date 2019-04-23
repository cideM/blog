import Typography from 'typography'

const typography = new Typography({
  baseFontSize: '18px',
  baseLineHeight: 1.4,
  googleFonts: [
    {
      name: 'Roboto Mono',
      styles: ['400'],
    },
    {
      name: 'Lora',
      styles: ['400', '400i', '700'],
    },
    {
      name: 'Quicksand',
      styles: ['400'],
    },
  ],
  bodyFontFamily: ['Lora'],
  headerFontFamily: ['Quicksand'],
})

export default typography

const rhythm = typography.rhythm
const scale = typography.scale

export { rhythm, scale }
