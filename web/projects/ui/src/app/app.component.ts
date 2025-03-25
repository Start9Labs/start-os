import { Component, inject, OnInit } from '@angular/core'
import { takeUntilDestroyed } from '@angular/core/rxjs-interop'
import { Title } from '@angular/platform-browser'
import { PatchDB } from 'patch-db-client'
import { combineLatest, map, merge, startWith } from 'rxjs'
import { i18nService } from 'src/app/i18n/i18n.service'
import { ConnectionService } from './services/connection.service'
import { PatchDataService } from './services/patch-data.service'
import { DataModel } from './services/patch-db/data-model'
import { PatchMonitorService } from './services/patch-monitor.service'

@Component({
  selector: 'app-root',
  templateUrl: 'app.component.html',
  styleUrls: ['app.component.scss'],
})
export class AppComponent implements OnInit {
  private readonly title = inject(Title)
  private readonly i18n = inject(i18nService)
  private readonly patch = inject<PatchDB<DataModel>>(PatchDB)

  readonly subscription = merge(
    inject(PatchDataService),
    inject(PatchMonitorService),
  )
    .pipe(takeUntilDestroyed())
    .subscribe()

  readonly offline$ = combineLatest([
    inject(ConnectionService),
    this.patch
      .watch$('serverInfo', 'statusInfo')
      .pipe(startWith({ restarting: false, shuttingDown: false })),
  ]).pipe(
    map(
      ([connected, { restarting, shuttingDown }]) =>
        connected && (restarting || shuttingDown),
    ),
    startWith(true),
  )

  ngOnInit() {
    this.patch.watch$('ui').subscribe(({ name, language }) => {
      this.title.setTitle(name || 'StartOS')
      this.i18n.setLanguage(language)
    })
  }
}
