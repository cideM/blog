import React from 'react'
import { StaticQuery, graphql } from 'gatsby'
import Anchor from './Anchor'
import styled, { css } from 'styled-components'

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
            `}
          >
            <div
              css={`
                text-align: right;
                display: flex;
                flex-direction: column;
              `}
            >
              <Anchor href={`https://github.com/cideM`}>GitHub</Anchor>
            </div>
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
