import { Pipe, PipeTransform } from '@angular/core'
import { marked } from 'marked'
import * as DOMPurify from 'dompurify'

@Pipe({
  name: 'markdown',
})
export class MarkdownPipe implements PipeTransform {
  transform (value: any): any {
    if (value && value.length > 0) {
      const html = marked(value)
      const sanitized = DOMPurify.sanitize(html)
      return sanitized
    }
    return value
  }
}
