import { Pipe, PipeTransform } from '@angular/core'
import { marked } from 'marked'

@Pipe({
  name: 'markdown',
})
export class MarkdownPipe implements PipeTransform {
  transform(value: string | null): string {
    return value?.length
      ? (marked.parse(value, { async: false }) as string)
      : ''
  }
}
