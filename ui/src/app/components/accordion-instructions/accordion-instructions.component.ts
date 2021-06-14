import {
  Component,
  Input,
  OnInit,
} from '@angular/core'

import { AppInstalledFull }     from '../../models/app-types'

@Component({
   selector   : 'app-accordion-instructions',
   templateUrl: './accordion-instructions.component.html',
   styleUrls  : ['./accordion-instructions.component.scss'],
 })
export class AccordionInstructionsComponent implements OnInit {

  isOpen                = true
  @Input() public app: AppInstalledFull = {} as any

  constructor() { }

  async ngOnInit() {}

  private toggleAccordion(): void {
    this.isOpen = !this.isOpen
  }

}
