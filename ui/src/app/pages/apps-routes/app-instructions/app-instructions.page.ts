import { Component } from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { BehaviorSubject } from 'rxjs'
import { AppInstalledFull } from 'src/app/models/app-types'
import { ModelPreload } from 'src/app/models/model-preload'
import { markAsLoadingDuring$ } from 'src/app/services/loader.service'
import { peekProperties } from 'src/app/util/property-subject.util'

@Component({
  selector: 'app-instructions',
  templateUrl: './app-instructions.page.html',
  styleUrls: ['./app-instructions.page.scss'],
})
export class AppInstructionsPage {
  $loading$ = new BehaviorSubject(true)
  error = ''
  app: AppInstalledFull = { } as any
  appId: string
  instructions: any

  constructor (
    private readonly route: ActivatedRoute,
    private readonly preload: ModelPreload,
  ) { }

  async ngOnInit () {
    this.appId = this.route.snapshot.paramMap.get('appId') as string

    markAsLoadingDuring$(this.$loading$, this.preload.appFull(this.appId)).subscribe({
      next: app => this.app = peekProperties(app),
      error: e => {
        console.error(e)
        this.error = e.message
      },
    })
  }
}
