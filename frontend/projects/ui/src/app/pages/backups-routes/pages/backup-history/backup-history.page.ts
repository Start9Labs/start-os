import { Component } from '@angular/core'
import { Pipe, PipeTransform } from '@angular/core'
import { BackupReport, BackupRun } from 'src/app/services/api/api.types'
import {
  AlertController,
  LoadingController,
  ModalController,
} from '@ionic/angular'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { ErrorToastService } from '@start9labs/shared'
import { catchError, defer, Subject } from 'rxjs'
import { BackupReportPage } from 'src/app/modals/backup-report/backup-report.page'

@Component({
  selector: 'backup-history',
  templateUrl: './backup-history.page.html',
  styleUrls: ['./backup-history.page.scss'],
})
export class BackupHistoryPage {
  readonly runs$ = defer(() => this.api.getBackupRuns({})).pipe(
    catchError(e => this.errToast.present(e)),
  )

  constructor(
    private readonly modalCtrl: ModalController,
    private readonly alertCtrl: AlertController,
    private readonly loadingCtrl: LoadingController,
    private readonly errToast: ErrorToastService,
    private readonly api: ApiService,
  ) {}

  async presentAlertDelete(id: string, index: number) {
    const alert = await this.alertCtrl.create({
      header: 'Confirm',
      message: 'Delete backup record? This actions cannot be undone.',
      buttons: [
        {
          text: 'Cancel',
          role: 'cancel',
        },
        {
          text: 'Delete',
          handler: () => {
            this.delete(id, index)
          },
          cssClass: 'enter-click',
        },
      ],
    })
    await alert.present()
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

  async delete(id: string, index: number): Promise<void> {
    const loader = await this.loadingCtrl.create({
      message: 'Removing...',
    })
    await loader.present()

    try {
      await this.api.removeBackupTarget({ id })
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
