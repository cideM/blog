import React from 'react'
import { StaticQuery, graphql } from 'gatsby'
import Anchor from './Anchor'
import styled, { css } from 'styled-components'
import Image from 'gatsby-image'

import { rhythm } from '../utils/typography'

function Bio() {
  return (
    <StaticQuery
      query={bioQuery}
      render={data => {
        const { author, social } = data.site.siteMetadata
        return (
          <div
            css={`
              display: flex;
              align-items: center;
              margin-bottom: ${rhythm(1)};
            `}
          >
            <div
              css={`
                text-align: right;
                display: flex;
                flex-direction: column;
              `}
            >
              haskell rust javascript
              <Anchor href={`https://github.com/cideM`}>GitHub</Anchor>
            </div>
            <Image
              fixed={data.avatar.childImageSharp.fixed}
              alt={author}
              css={`
                margin-left: ${rhythm(0.5)};
                width: 50px;
                height: 50px;
                margin-bottom: 0;
                border-radius: 10000px;
              `}
            />
          </div>
        )
      }}
    />
  )
}

const bioQuery = graphql`
  query BioQuery {
    avatar: file(absolutePath: { regex: "/profile-pic.jpg/" }) {
      childImageSharp {
        fixed(width: 50, height: 50, quality: 95) {
          ...GatsbyImageSharpFixed
        }
      }
    }
    site {
      siteMetadata {
        author
        social {
          twitter
        }
      }
    }
  }
`

export default Bio
