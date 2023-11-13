import { Component, Input } from '@angular/core'
import { ModalController } from '@ionic/angular'
import { BackupReport } from 'src/app/services/api/api.types'

@Component({
  selector: 'backup-report',
  templateUrl: './backup-report.page.html',
  styleUrls: ['./backup-report.page.scss'],
})
export class BackupReportPage {
  @Input() report!: BackupReport
  @Input() timestamp!: string

  system!: {
    result: string
    icon: 'remove' | 'remove-circle-outline' | 'checkmark'
    color: 'dark' | 'danger' | 'success'
  }

  constructor(private readonly modalCtrl: ModalController) {}

  ngOnInit() {
    if (!this.report.server.attempted) {
      this.system = {
        result: 'Not Attempted',
        icon: 'remove',
        color: 'dark',
      }
    } else if (this.report.server.error) {
      this.system = {
        result: `Failed: ${this.report.server.error}`,
        icon: 'remove-circle-outline',
        color: 'danger',
      }
    } else {
      this.system = {
        result: 'Succeeded',
        icon: 'checkmark',
        color: 'success',
      }
    }
  }

  async dismiss() {
    return this.modalCtrl.dismiss(true)
  }
}
