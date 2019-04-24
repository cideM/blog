import React from 'react'
import { graphql } from 'gatsby'
import Link from '../components/Link'
import Anchor from '../components/Anchor'
import Bio from '../components/Bio'
import Layout from '../components/Layout'
import SEO from '../components/seo'
import styled from 'styled-components'
import { rhythm, scale } from '../utils/typography'

const Post = styled.div`
  & code {
    font-family: Roboto Mono, monospace;
  }

  & h2 {
    margin-bottom: ${rhythm(0.5)};
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
            margin-top: ${rhythm(1.5)};
            margin-bottom: 0;
          `}
        >
          {post.frontmatter.title}
        </h1>
        <p
          css={`
            margin-top: ${rhythm(1 / 2)};
            margin-bottom: ${rhythm(1)};
          `}
        >
          {post.frontmatter.date}
        </p>
        <Post dangerouslySetInnerHTML={{ __html: post.html }} />
        <hr
          style={{
            marginBottom: rhythm(1),
          }}
        />
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
