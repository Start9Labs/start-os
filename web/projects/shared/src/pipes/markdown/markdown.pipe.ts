import { Pipe, PipeTransform } from '@angular/core'
import { marked } from 'marked'
import * as DOMPurify from 'dompurify'

@Pipe({
  name: 'markdown',
})
export class MarkdownPipe implements PipeTransform {
  transform(value: string): string {
    if (value && value.length > 0) {
      // convert markdown to html
      const html = marked(value)
      // sanitize html
      const sanitized = DOMPurify.sanitize(html)
      // parse html to find all links
      let parser = new DOMParser()
      const doc = parser.parseFromString(sanitized, 'text/html')
      const links = Array.from(doc.getElementsByTagName('a'))
      // add target="_blank" to every link
      links.forEach(link => {
        link.setAttribute('target', '_blank')
      })
      // return new html string
      return doc.documentElement.innerHTML
    }
    return value
  }
}
