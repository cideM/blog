const fs = require('fs')
const path = require('path')
const util = require('util')
const matter = require('gray-matter')
const remark = require('remark')
const emoji = require('remark-emoji')
const recommended = require('remark-preset-lint-recommended')
const toHtml = require('remark-html')
const report = require('vfile-reporter')
const Mustache = require('mustache')

const readdir = util.promisify(fs.readdir)

const outputPath = path.join(__dirname, '..', 'public')
const blogContentPath = path.join(__dirname, '..', 'content', 'blog')

// TODO: Copy assets for post
// Add link to github
// Back button for post
// Syntax highlighting
// Night mode?
const indexTemplate = fs.readFileSync('index.mst', 'utf-8')

const run = async () => {
  const blogPosts = await readdir(blogContentPath)

  await Promise.all(
    // A post is a directory with at least an index.md. The name of the
    // directory is the name of the post (not the title!)
    blogPosts.map(async postName => {
      const post = fs.readFileSync(
        path.resolve(blogContentPath, postName, 'index.md'),
        'utf-8'
      )
      // Extract front matter
      const {
        content,
        data: { title, date },
      } = matter(post)

      try {
        // Markdown -> HTML
        const html = await remark()
          .use(recommended)
          .use(emoji)
          .use(toHtml)
          .process(content)

        return {
          title,
          date,
          filename: postName,
          html,
        }
      } catch (error) {
        report(error)
      }
    })
  )
    .then(slugs => {
      // TODO: Copy assets
      // Write one .html file per markdown post
      slugs.forEach(async ({ filename, html }) => {
        fs.mkdirSync(path.join(outputPath, 'posts', filename), {
          recursive: true,
        })
        fs.writeFileSync(
          path.join(outputPath, 'posts', filename, 'index.html'),
          html
        )
      })

      return slugs
    })
    .then(slugs => {
      // Render index.html with links to posts
      const rendered = Mustache.render(indexTemplate, { slugs })
      fs.writeFileSync(path.join(outputPath, 'index.html'), rendered)
    })
    .catch(err => console.error(err))
}

run()
