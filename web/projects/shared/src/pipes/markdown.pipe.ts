import { Pipe, PipeTransform } from '@angular/core'
import { marked } from 'marked'

@Pipe({
  name: 'markdown',
})
export class MarkdownPipe implements PipeTransform {
  transform(value: string): string {
    return value?.length ? marked(value) : ''
  }
}
