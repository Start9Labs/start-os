import { NgModule } from '@angular/core'
import { MarkdownPipe } from './markdown.pipe'

@NgModule({
  declarations: [MarkdownPipe],
  exports: [MarkdownPipe],
})
export class MarkdownPipeModule {}
