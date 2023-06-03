import { Component } from '@angular/core'
import { Pipe, PipeTransform } from '@angular/core'
import { BackupReport, BackupRun } from 'src/app/services/api/api.types'
import { LoadingController, ModalController } from '@ionic/angular'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { ErrorToastService } from '@start9labs/shared'
import { BehaviorSubject } from 'rxjs'
import { BackupReportPage } from 'src/app/apps/ui/modals/backup-report/backup-report.page'

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
    private readonly modalCtrl: ModalController,
    private readonly loadingCtrl: LoadingController,
    private readonly errToast: ErrorToastService,
    private readonly api: ApiService,
  ) {}

  async ngOnInit() {
    try {
      this.runs = await this.api.getBackupRuns({})
    } catch (e: any) {
      this.errToast.present(e)
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

  async presentModalReport(run: BackupRun) {
    const modal = await this.modalCtrl.create({
      component: BackupReportPage,
      componentProps: {
        report: run.report,
        timestamp: run['completed-at'],
      },
    })
    await modal.present()
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

    const loader = await this.loadingCtrl.create({
      message: 'Deleting...',
    })
    await loader.present()

    try {
      await this.api.deleteBackupRuns({ ids })
      this.selected = {}
      this.runs = this.runs.filter(r => !ids.includes(r.id))
    } catch (e: any) {
      this.errToast.present(e)
    } finally {
      loader.dismiss()
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
