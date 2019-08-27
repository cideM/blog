import React from 'react'
import { graphql } from 'gatsby'
import Link from '../components/Link'
import Anchor from '../components/Anchor'
import Padding from '../components/Padding'
import Layout from '../components/Layout'
import SEO from '../components/seo'
import styled, { css } from 'styled-components'
import { rhythm, scale } from '../utils/typography'

const hrStyle = css`
  margin-bottom: rhythm(1);
  background: ${props => props.theme.gradients.a};
  height: ${rhythm(1 / 8)};
`

const Post = styled.div`
  & hr {
    ${hrStyle}
  }

  & code {
    font-family: IBM Plex Mono, monospace;
    ${scale(-0.25)};
  }

  & h2 {
    margin-bottom: ${rhythm(0.5)};
    ${scale(0)};
    margin-top: 0;
  }

  & pre {
    min-width: 100%;
    margin: 0;
    float: left;
    padding-top: ${rhythm(0.5)};
    padding-bottom: ${rhythm(0.5)};
  }

  & p {
    margin-bottom: ${rhythm(1)};
  }

  & .gatsby-highlight {
    overflow: auto;
    margin-bottom: ${rhythm(1)};
  }

  a {
    color: inherit;
    text-decoration: underline;
    &:hover {
      text-decoration: underline;
    }
  }

  & blockquote {
    p {
      margin: 0;
    }

    font-style: italic;
    margin: 0;
    padding: ${rhythm(1 / 4)};
    padding-left: ${rhythm(1)};
    margin-bottom: ${rhythm(1)};
    position: relative;

    &::before {
      position: absolute;
      content: '';
      top: 0;
      bottom: 0;
      left: 0;
      width: ${rhythm(1 / 3)};
      background: ${props => props.theme.gradients.a};
    }
  }
`

const HR = styled.hr`
  margin-bottom: rhythm(1);
  background: ${props => props.theme.gradients.a};
  height: ${rhythm(1 / 8)};
`

class BlogPostTemplate extends React.Component {
  render() {
    const post = this.props.data.markdownRemark
    const siteTitle = this.props.data.site.siteMetadata.title
    const { previous, next } = this.props.pageContext

    return (
      <Layout location={this.props.location} title={siteTitle}>
        <SEO title={post.frontmatter.title} description={post.excerpt} />
        <h1
          css={`
            margin-bottom: ${rhythm(1 / 4)};
            text-align: center;
          `}
        >
          {post.frontmatter.title}
        </h1>
        <p
          css={`
            text-align: center;
          `}
        >
          {post.frontmatter.date}
        </p>
        <Post dangerouslySetInnerHTML={{ __html: post.html }} />
        <HR />
        <p
          css={`
            font-style: italic;
            text-align: center;
          `}
        >
          Found a mistake? Got feedback? Please raise an issue on{' '}
          <Anchor href="https://github.com/cideM/blog/issues">GitHub</Anchor>!
        </p>
      </Layout>
    )
  }
}

export default BlogPostTemplate

export const pageQuery = graphql`
  query BlogPostBySlug($slug: String!) {
    site {
      siteMetadata {
        title
        author
      }
    }
    markdownRemark(fields: { slug: { eq: $slug } }) {
      id
      excerpt(pruneLength: 160)
      html
      frontmatter {
        title
        date(formatString: "MMMM DD, YYYY")
      }
    }
  }
`
