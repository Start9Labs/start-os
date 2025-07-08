import { Component, inject } from '@angular/core'
import { takeUntilDestroyed } from '@angular/core/rxjs-interop'
import { i18nService } from '@start9labs/shared'
import { PatchDB } from 'patch-db-client'
import { merge } from 'rxjs'
import { PatchDataService } from './services/patch-data.service'
import { DataModel } from './services/patch-db/data-model'
import { PatchMonitorService } from './services/patch-monitor.service'

@Component({
  selector: 'app-root',
  template: `
    <tui-root tuiTheme="dark">
      <router-outlet />
      <toast-container />
    </tui-root>
  `,
  styles: `
    :host {
      display: block;
      height: 100%;
    }

    tui-root {
      height: 100%;
      font-family: 'Proxima Nova', system-ui;
    }
  `,
  standalone: false,
})
export class AppComponent {
  private readonly i18n = inject(i18nService)

  readonly subscription = merge(
    inject(PatchDataService),
    inject(PatchMonitorService),
  )
    .pipe(takeUntilDestroyed())
    .subscribe()

  readonly ui = inject<PatchDB<DataModel>>(PatchDB)
    .watch$('ui', 'language')
    .pipe(takeUntilDestroyed())
    .subscribe(language => {
      this.i18n.setLanguage(language || 'english')
    })
}
