import { Component, Input } from '@angular/core'
import { ModalController, NavController } from '@ionic/angular'
import {
  ApiService,
  DiskBackupTarget,
  DiskInfo,
} from 'src/app/services/api/api.service'
import { ErrorToastService } from '@start9labs/shared'
import { StateService } from 'src/app/services/state.service'
import { PasswordPage } from '../../modals/password/password.page'

@Component({
  selector: 'app-transfer',
  templateUrl: 'transfer.page.html',
  styleUrls: ['transfer.page.scss'],
})
export class TransferPage {
  loading = true
  drives: DiskInfo[] = []

  constructor(
    private readonly apiService: ApiService,
    private readonly navCtrl: NavController,
    private readonly errToastService: ErrorToastService,
    private readonly stateService: StateService,
  ) {}

  async ngOnInit() {
    await this.getDrives()
  }

  async refresh() {
    this.loading = true
    await this.getDrives()
  }

  async getDrives() {
    try {
      const disks = await this.apiService.getDrives()
      this.drives = disks.filter(d => d.partitions.length && d.guid)
    } catch (e: any) {
      this.errToastService.present(e)
    } finally {
      this.loading = false
    }
  }

  async select(target: DiskInfo) {
    const { logicalname, guid } = target

    if (!logicalname) return

    this.stateService.recoverySource = {
      type: 'migrate',
      guid: guid!,
    }
    this.navCtrl.navigateForward(`/embassy`, {
      queryParams: { action: 'transfer' },
    })
  }
}
