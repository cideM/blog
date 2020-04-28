import React from 'react'
import Link from './Link'
import Anchor from './Anchor.js'
import styled, { ThemeProvider, createGlobalStyle } from 'styled-components'
import Padding from './Padding'
import Bio from './Bio.js'
import { rhythm, scale } from '../utils/typography'

const GlobalStyle = createGlobalStyle`
  body {
    color: ${props => props.theme.colors.text.normal};
    background: ${props => props.theme.colors.background};
  }
`

const theme = {
  colors: {
    brand: '#7da3be',
    background: '#ffffff',
    text: {
      dimmed: '#ADADAD',
    },
  },
  gradients: {
    a: `linear-gradient(120deg, #84fab0 0%, #8fd3f4 100%);`,
  },
}

const RootHeader = styled.h2`
  height: ${rhythm(6)};
  margin: 0;
  font-weight: 500;
  display: flex;
  align-items: center;
`

const PostHeader = styled.h3`
  height: ${rhythm(3)};
  font-weight: 500;
  margin: 0;
  display: flex;
  align-items: center;
  justify-content: center;
`

const RootLink = ({ children }) => (
  <Link
    style={{
      boxShadow: `none`,
      textDecoration: `none`,
      color: `inherit`,
    }}
    to={`/`}
  >
    {children}
  </Link>
)

const Header = ({ isRootPath, children }) =>
  isRootPath ? (
    <div
      css={`
        display: flex;
        align-items: center;
        justify-content: space-between;
      `}
    >
      <RootHeader>
        <Padding>
          <RootLink>{children}</RootLink>
        </Padding>
      </RootHeader>
      <Bio />
    </div>
  ) : (
    <PostHeader>
      <Padding>
        <RootLink>{children}</RootLink>
      </Padding>
    </PostHeader>
  )

class Layout extends React.Component {
  render() {
    const { location, title, children } = this.props
    const rootPath = `${__PATH_PREFIX__}/`

    return (
      <ThemeProvider theme={theme}>
        <div
          css={`
            margin-left: auto;
            margin-right: auto;
            max-width: ${rhythm(34)};
          `}
        >
          <GlobalStyle />
          <Header isRootPath={location.pathname === rootPath}>{title}</Header>
          <Padding>{children}</Padding>
          <footer
            css={`
              text-align: center;
              display: flex;
              align-items: center;
              justify-content: center;
              height: ${rhythm(2)};
            `}
          >
            <p
              css={`
                margin: 0;
                color: ${props => props.theme.colors.text.dimmed};
              `}
            >
              Built with <Anchor href="https://www.gatsbyjs.org">Gatsby</Anchor>
            </p>
          </footer>
        </div>
      </ThemeProvider>
    )
  }
}

export default Layout
