import React from 'react'
import { graphql } from 'gatsby'
import Link from '../components/Link.js'

import Layout from '../components/Layout'
import SEO from '../components/seo'
import { rhythm } from '../utils/typography'

class BlogIndex extends React.Component {
  render() {
    const { data } = this.props
    const siteTitle = data.site.siteMetadata.title
    const posts = data.allMarkdownRemark.edges

    return (
      <Layout location={this.props.location} title={siteTitle}>
        <SEO
          title="All posts"
          keywords={[
            `blog`,
            `gatsby`,
            `javascript`,
            `react`,
            `haskell`,
            `rust`,
            `vimscript`,
            `neovim`,
          ]}
        />
        {posts.map(({ node }) => {
          const title = node.frontmatter.title || node.fields.slug
          return (
            <div key={node.fields.slug} css={`
              margin-bottom: ${rhythm(2)};
            `}>
              <h3
                css={`
                  margin: 0;
                `}
              >
                <Link
                  style={{ boxShadow: `none`, textDecoration: `none` }}
                  to={node.fields.slug}
                >
                  {title}
                </Link>
              </h3>
              <div
                css={`
                  margin-bottom: ${rhythm(1 / 4)};
                  margin-top: ${rhythm(1 / 4)};
                `}
              >
                <small>{node.frontmatter.date}</small>
              </div>
              <p dangerouslySetInnerHTML={{ __html: node.excerpt }} />
            </div>
          )
        })}
      </Layout>
    )
  }
}

export default BlogIndex

export const pageQuery = graphql`
  query {
    site {
      siteMetadata {
        title
      }
    }
    allMarkdownRemark(
      sort: { fields: [frontmatter___date], order: DESC }
      filter: { frontmatter: { publish: { eq: true } } }
    ) {
      edges {
        node {
          excerpt(pruneLength: 140)
          fields {
            slug
          }
          frontmatter {
            date(formatString: "MMMM DD, YYYY")
            title
          }
        }
      }
    }
  }
`
