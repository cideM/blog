import React from 'react'
import styled, { withTheme } from 'styled-components'
import { Link } from 'gatsby'

const LinkInternal = props => (
  <Link
    css={`
      color: ${props => props.theme.colors.brand};
      text-decoration: none;
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
