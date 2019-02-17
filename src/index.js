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

const template = `
<!DOCTYPE html>
<html lang="en">
<head>
	<meta charset="utf-8">
	<title>HTML5 boilerplate – all you really need…</title>
	<!--[if IE]>
		<script src="http://html5shiv.googlecode.com/svn/trunk/html5.js"></script>
	<![endif]-->
</head>

<body id="home">

  <h1>HTML5 boilerplate</h1>
  
  <ol>
    {{#slugs}}
    <li><a href="posts/{{filename}}/index.html">{{date}} {{title}}</a></li>
    {{/slugs}}
  </ol>
</body>
</html>

`

const run = async () => {
  const blogPosts = await readdir(blogContentPath)

  await Promise.all(
    // A post is a directory with at least an index.md
    blogPosts.map(async dirName => {
      const fileContent = fs.readFileSync(
        path.resolve(blogContentPath, dirName, 'index.md'),
        'utf-8'
      )
      const {
        content,
        data: { title, date },
      } = matter(fileContent)

      try {
        const html = await remark()
          .use(recommended)
          .use(emoji)
          .use(toHtml)
          .process(content)

        return {
          title,
          date,
          filename: dirName,
          html,
        }
      } catch (error) {
        report(error)
      }
    })
  )
    .then(slugs => {
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
      const rendered = Mustache.render(template, { slugs })
      fs.writeFileSync(path.join(outputPath, 'index.html'), rendered)
    })
    .catch(err => console.error(err))
}

run()
