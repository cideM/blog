import React from 'react'
import styled, { withTheme } from 'styled-components'
import { Link } from 'gatsby'

const LinkInternal = props => (
  <Link
    css={`
      color: inherit;
      text-decoration: underline;
      &:hover {
        text-decoration: underline;
      }
    `}
    {...props}
  >
    {props.children}
  </Link>
)

export default withTheme(LinkInternal)
