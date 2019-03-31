import React from 'react'
import styled from 'styled-components'

const Anchor = styled.a`
  color: ${props => props.theme.colors.brand};
  text-decoration: none;
  &:hover {
    text-decoration: underline;
  }
`

export default Anchor
