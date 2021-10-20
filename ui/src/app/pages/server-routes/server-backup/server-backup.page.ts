import { Component } from '@angular/core'
import { ModalController, NavController } from '@ionic/angular'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { GenericInputComponent } from 'src/app/modals/generic-input/generic-input.component'
import { MappedPartitionInfo } from 'src/app/util/misc.util'
import { PatchDbService } from 'src/app/services/patch-db/patch-db.service'
import { PackageDataEntry, PackageMainStatus, ServerStatus } from 'src/app/services/patch-db/data-model'
import { Subscription } from 'rxjs'
import { take } from 'rxjs/operators'

@Component({
  selector: 'server-backup',
  templateUrl: './server-backup.page.html',
  styleUrls: ['./server-backup.page.scss'],
})
export class ServerBackupPage {
  backingUp = false
  pkgs: PkgInfo[] = []
  subs: Subscription[]

  constructor (
    private readonly navCtrl: NavController,
    private readonly modalCtrl: ModalController,
    private readonly embassyApi: ApiService,
    private readonly patch: PatchDbService,
  ) { }

  ngOnInit () {
    this.subs = [
      this.patch.watch$('server-info', 'status').subscribe(status => {
        if (status === ServerStatus.BackingUp) {
          this.backingUp = true
          this.subscribeToBackup()
        } else {
          this.backingUp = false
          this.pkgs.forEach(pkg => pkg.sub.unsubscribe())
        }
      }),
    ]
  }

  ngOnDestroy () {
    this.subs.forEach(sub => sub.unsubscribe())
    this.pkgs.forEach(pkg => pkg.sub.unsubscribe())
  }

  back () {
    this.navCtrl.back()
  }

  async presentModal (partition: MappedPartitionInfo): Promise<void> {
    let message: string
    if (partition.hasBackup) {
      message = 'Enter your master password to decrypt this drive and update its backup. Depending on how much data was added or changed, this could be very fast, or it could take a while.'
    } else {
      message = 'Enter your master password to create an encrypted backup of your Embassy and all its installed services. Since this is a fresh backup, it could take a while. Future backups will likely be much faster.'
    }

    const m = await this.modalCtrl.create({
      componentProps: {
        title: 'Create Backup',
        message,
        label: 'Password',
        placeholder: 'Enter password',
        useMask: true,
        buttonText: 'Create Backup',
        loadingText: 'Beginning backup...',
        submitFn: async (password: string) => await this.create(partition.logicalname, password),
      },
      cssClass: 'alertlike-modal',
      component: GenericInputComponent,
    })

    await m.present()
  }

  private async create (logicalname: string, password: string): Promise<void> {
    await this.embassyApi.createBackup({ logicalname, password })
  }

  private subscribeToBackup () {
    this.patch.watch$('package-data')
    .pipe(
      take(1),
    )
    .subscribe(pkgs => {
      const pkgArr = Object.values(pkgs)
      const activeIndex = pkgArr.findIndex(pkg => pkg.installed.status.main.status === PackageMainStatus.BackingUp)

      this.pkgs = pkgArr.map((pkg, i) => {
        const pkgInfo = {
          entry: pkg,
          active: i === activeIndex,
          complete: i < activeIndex,
          sub: null,
        }
        return pkgInfo
      })

      // subscribe to pkg
      this.pkgs.forEach(pkg => {
        pkg.sub = this.patch.watch$('package-data', pkg.entry.manifest.id, 'installed', 'status', 'main', 'status').subscribe(status => {
          if (status === PackageMainStatus.BackingUp) {
            pkg.active = true
          } else if (pkg.active) {
            pkg.active = false
            pkg.complete = true
          }
        })
      })
    })
  }
}

interface PkgInfo {
  entry: PackageDataEntry,
  active: boolean
  complete: boolean,
  sub: Subscription,
}
