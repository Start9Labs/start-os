import { Pipe, PipeTransform } from '@angular/core'
import { DomSanitizer, SafeResourceUrl } from '@angular/platform-browser'

@Pipe({
  name: 'trustUrl',
})
export class TrustUrlPipe implements PipeTransform {
  constructor(private readonly sanitizer: DomSanitizer) {}

  transform(base64Icon: string): SafeResourceUrl {
    return this.sanitizer.bypassSecurityTrustResourceUrl(base64Icon)
  }
}
