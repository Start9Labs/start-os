import { Component, inject, OnInit } from '@angular/core'
import { takeUntilDestroyed } from '@angular/core/rxjs-interop'
import { Title } from '@angular/platform-browser'
import { PatchDB } from 'patch-db-client'
import { combineLatest, map, merge, startWith } from 'rxjs'
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

  async ngOnInit() {
    this.patch
      .watch$('ui', 'name')
      .subscribe(name => this.title.setTitle(name || 'StartOS'))
  }
}
