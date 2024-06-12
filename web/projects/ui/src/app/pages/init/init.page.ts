import { Component, inject } from '@angular/core'
import { InitService } from 'src/app/pages/init/init.service'

@Component({
  selector: 'init-page',
  templateUrl: 'init.page.html',
  styleUrls: ['init.page.scss'],
})
export class InitPage {
  readonly progress$ = inject(InitService)
}
