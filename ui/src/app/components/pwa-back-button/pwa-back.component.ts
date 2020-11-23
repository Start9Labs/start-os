import { Component } from '@angular/core'
import { PwaBackService } from 'src/app/services/pwa-back.service'
@Component({
  selector: 'pwa-back-button',
  templateUrl: './pwa-back.component.html',
  styleUrls: ['./pwa-back.component.scss'],
})
export class PwaBackComponent {
  constructor (
    private readonly pwaBack: PwaBackService,
  ) {  }

  navigateBack () {
    return this.pwaBack.back()
  }
}
