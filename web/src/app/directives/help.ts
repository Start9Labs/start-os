import { Directive, inject, TemplateRef } from '@angular/core'
import { HelpService } from 'src/app/services/help.service'

@Directive({
  selector: 'ng-template[help]',
})
export class Help {
  constructor() {
    inject(HelpService).content.set(inject(TemplateRef))
  }
}
