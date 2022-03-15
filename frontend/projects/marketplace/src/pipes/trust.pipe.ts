import { Pipe, PipeTransform } from '@angular/core'
import { DomSanitizer, SafeResourceUrl } from '@angular/platform-browser'

@Pipe({
  name: 'trust',
})
export class TrustPipe implements PipeTransform {
  constructor(public readonly sanitizer: DomSanitizer) {}

  transform(base64Icon: string): SafeResourceUrl {
    return this.sanitizer.bypassSecurityTrustResourceUrl(base64Icon)
  }
}
