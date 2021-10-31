import { Component } from '@angular/core'
import { ModalController, NavController } from '@ionic/angular'
import { ApiService, PartitionInfo } from 'src/app/services/api/api.service'
import { ErrorToastService } from 'src/app/services/error-toast.service'
import { StateService } from 'src/app/services/state.service'
import { PasswordPage } from '../password/password.page'
import { ProdKeyModal } from '../prod-key-modal/prod-key-modal.page'

@Component({
  selector: 'app-recover',
  templateUrl: 'recover.page.html',
  styleUrls: ['recover.page.scss'],
})
export class RecoverPage {
  passwords = { }
  prodKeys = { }
  recoveryPartitions: { partition: PartitionInfo, model: string, vendor: string }[] = []
  selectedPartition: PartitionInfo = null
  loading = true

  constructor (
    private readonly apiService: ApiService,
    private readonly navCtrl: NavController,
    private readonly modalController: ModalController,
    readonly stateService: StateService,
    private readonly errorToastService: ErrorToastService,
  ) { }

  async ngOnInit () {
    await this.getPartitions()
  }

  async refresh () {
    this.recoveryPartitions = []
    this.selectedPartition = null
    this.loading = true
    await this.getPartitions()
  }

  async getPartitions () {
    try {
      let drives = await this.apiService.getDrives()

      this.recoveryPartitions = drives.map(d => d.partitions.map(p => ({ partition: p, vendor: d.vendor, model: d.model})).filter(p => p.partition['embassy-os']?.full)).flat()
      // if theres no product key, only show 0.2s
      if (!this.stateService.hasProductKey) {
        this.recoveryPartitions = this.recoveryPartitions.filter(p => p.partition['embassy-os']?.version.startsWith('0.2'))
      }
    } catch (e) {
      this.errorToastService.present(`${e.message}: ${e.data}`)
    } finally {
      this.loading = false
    }
  }

  async choosePartition (partition: PartitionInfo) {
    if (this.selectedPartition?.logicalname === partition.logicalname) {
      this.selectedPartition = null
      return
    } else {
      this.selectedPartition = partition
    }

    if ((partition['embassy-os'].version.startsWith('0.2') && this.stateService.hasProductKey) || this.passwords[partition.logicalname] || this.prodKeys[partition.logicalname]) return

    if (this.stateService.hasProductKey) {
      const modal = await this.modalController.create({
        component: PasswordPage,
        componentProps: {
          recoveryPartition: this.selectedPartition,
        },
        cssClass: 'alertlike-modal',
      })
      modal.onDidDismiss().then(async ret => {
        if (!ret.data) {
          this.selectedPartition = null
        } else if (ret.data.password) {
          this.passwords[partition.logicalname] = ret.data.password
        }

      })
      await modal.present()
    } else {
      const modal = await this.modalController.create({
        component: ProdKeyModal,
        componentProps: {
          recoveryPartition: this.selectedPartition,
        },
        cssClass: 'alertlike-modal',
      })
      modal.onDidDismiss().then(async ret => {
        if (!ret.data) {
          this.selectedPartition = null
        } else if (ret.data.productKey) {
          this.prodKeys[partition.logicalname] = ret.data.productKey
        }

      })
      await modal.present()
    }
  }

  async selectRecoveryPartition () {
    this.stateService.recoveryPartition = this.selectedPartition
    const pw = this.passwords[this.selectedPartition.logicalname]
    if (pw) {
      this.stateService.recoveryPassword = pw
    }
    await this.navCtrl.navigateForward(`/embassy`)
  }
}
