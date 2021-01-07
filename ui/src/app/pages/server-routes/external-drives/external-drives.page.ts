import { Component } from '@angular/core'
import { pauseFor } from 'src/app/util/misc.util'
import { ApiService } from 'src/app/services/api/api.service'
import { DiskInfo } from 'src/app/models/server-model'
import { markAsLoadingDuring$, markAsLoadingDuringAsync, markAsLoadingDuringP } from 'src/app/services/loader.service'
import { BehaviorSubject } from 'rxjs'
import { AlertController } from '@ionic/angular'

type Ejectable<T> = T & { $ejecting$: BehaviorSubject<boolean> }

@Component({
  selector: 'external-drives',
  templateUrl: './external-drives.page.html',
  styleUrls: ['./external-drives.page.scss'],
})
export class ExternalDrivesPage {
  disks: Ejectable<DiskInfo>[] = []
  $loading$ = new BehaviorSubject(false)

  constructor (
    private readonly apiService: ApiService,
    private readonly alertCtrl: AlertController,
  ) { }

  ngOnInit () {
    markAsLoadingDuringP(this.$loading$, this.fetchDisks())
  }

  async doRefresh (event: any) {
    await Promise.all([
      this.fetchDisks(),
      pauseFor(600),
    ])
    event.target.complete()
  }

  async fetchDisks () {
    return this.apiService.getExternalDisks().then(ds => {
      this.disks = ds.map(d => ({ ...d, $ejecting$: new BehaviorSubject(false)})).sort( (a, b) => a.logicalname < b.logicalname ? -1 : 1 )
    })
  }

  async ejectDisk (diskIndex: number) {
    const d = this.disks[diskIndex]
    markAsLoadingDuringP(d.$ejecting$, this.apiService.ejectExternalDisk(d.logicalname))
      .then(() => this.disks.splice(diskIndex, 1))
      .catch((e: Error) => {
        this.alertError(`Could not eject ${d.logicalname}: ${e.message}`)
      })
  }

  async alertError (desc: string) {
    const alert = await this.alertCtrl.create({
      backdropDismiss: true,
      message: desc,
      cssClass: 'alert-error-message',
    })
    await alert.present()
  }
}

