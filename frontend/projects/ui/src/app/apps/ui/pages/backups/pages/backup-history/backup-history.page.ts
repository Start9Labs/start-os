import { Component } from '@angular/core'
import { Pipe, PipeTransform } from '@angular/core'
import { ErrorService, LoadingService } from '@start9labs/shared'
import { TuiDialogService } from '@taiga-ui/core'
import { BackupReport, BackupRun } from 'src/app/services/api/api.types'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { BehaviorSubject } from 'rxjs'
import { BackupReportComponent } from '../../../../modals/backup-report/backup-report.component'
import { PolymorpheusComponent } from '@tinkoff/ng-polymorpheus'

@Component({
  selector: 'backup-history',
  templateUrl: './backup-history.page.html',
  styleUrls: ['./backup-history.page.scss'],
})
export class BackupHistoryPage {
  selected: Record<string, boolean> = {}
  runs: BackupRun[] = []
  loading$ = new BehaviorSubject(true)

  constructor(
    private readonly dialogs: TuiDialogService,
    private readonly loader: LoadingService,
    private readonly errorService: ErrorService,
    private readonly api: ApiService,
  ) {}

  async ngOnInit() {
    try {
      this.runs = await this.api.getBackupRuns({})
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      this.loading$.next(false)
    }
  }

  get empty() {
    return this.count === 0
  }

  get count() {
    return Object.keys(this.selected).length
  }

  presentModalReport(run: BackupRun) {
    this.dialogs
      .open(new PolymorpheusComponent(BackupReportComponent), {
        label: 'Backup Report',
        data: {
          report: run.report,
          timestamp: run['completed-at'],
        },
      })
      .subscribe()
  }

  async toggleChecked(id: string) {
    if (this.selected[id]) {
      delete this.selected[id]
    } else {
      this.selected[id] = true
    }
  }

  async toggleAll(runs: BackupRun[]) {
    if (this.empty) {
      runs.forEach(r => (this.selected[r.id] = true))
    } else {
      this.selected = {}
    }
  }

  async deleteSelected(): Promise<void> {
    const ids = Object.keys(this.selected)
    const loader = this.loader.open('Deleting...').subscribe()

    try {
      await this.api.deleteBackupRuns({ ids })
      this.selected = {}
      this.runs = this.runs.filter(r => !ids.includes(r.id))
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }
}

@Pipe({
  name: 'duration',
})
export class DurationPipe implements PipeTransform {
  transform(start: string, finish: string): number {
    const diffMs = new Date(finish).valueOf() - new Date(start).valueOf()
    return diffMs / 100
  }
}

@Pipe({
  name: 'hasError',
})
export class HasErrorPipe implements PipeTransform {
  transform(report: BackupReport): boolean {
    const osErr = !!report.server.error
    const pkgErr = !!Object.values(report.packages).find(pkg => pkg.error)
    return osErr || pkgErr
  }
}
