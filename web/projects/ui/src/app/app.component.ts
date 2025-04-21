import { Component, inject } from '@angular/core'
import { takeUntilDestroyed } from '@angular/core/rxjs-interop'
import { Title } from '@angular/platform-browser'
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
      font-family: 'Open Sans', sans-serif;
    }
  `,
})
export class AppComponent {
  private readonly title = inject(Title)
  private readonly i18n = inject(i18nService)

  readonly subscription = merge(
    inject(PatchDataService),
    inject(PatchMonitorService),
  )
    .pipe(takeUntilDestroyed())
    .subscribe()

  readonly ui = inject<PatchDB<DataModel>>(PatchDB)
    .watch$('ui')
    .pipe(takeUntilDestroyed())
    .subscribe(({ name, language }) => {
      this.title.setTitle(name || 'StartOS')
      this.i18n.setLanguage(language || 'english')
    })
}
