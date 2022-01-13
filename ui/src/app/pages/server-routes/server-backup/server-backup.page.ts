import { Component } from '@angular/core'
import { LoadingController, ModalController, NavController } from '@ionic/angular'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { GenericInputComponent, GenericInputOptions } from 'src/app/modals/generic-input/generic-input.component'
import { PatchDbService } from 'src/app/services/patch-db/patch-db.service'
import { PackageDataEntry, PackageMainStatus, ServerStatus } from 'src/app/services/patch-db/data-model'
import { Subscription } from 'rxjs'
import { take } from 'rxjs/operators'
import { MappedBackupTarget } from 'src/app/util/misc.util'
import * as argon2 from '@start9labs/argon2'
import { CifsBackupTarget, DiskBackupTarget } from 'src/app/services/api/api.types'

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
    private readonly loadingCtrl: LoadingController,
    private readonly modalCtrl: ModalController,
    private readonly embassyApi: ApiService,
    private readonly patch: PatchDbService,
    private readonly navCtrl: NavController,
  ) { }

  ngOnInit () {
    this.subs = [
      this.patch.watch$('server-info', 'status')
        .pipe()
        .subscribe(status => {
          if (status === ServerStatus.BackingUp) {
            if (!this.backingUp) {
              this.backingUp = true
              this.subscribeToBackup()
            }
          } else {
            if (this.backingUp) {
              this.backingUp = false
              this.pkgs.forEach(pkg => pkg.sub.unsubscribe())
              this.navCtrl.navigateRoot('/embassy')
            }
          }
        }),
    ]
  }

  ngOnDestroy () {
    this.subs.forEach(sub => sub.unsubscribe())
    this.pkgs.forEach(pkg => pkg.sub.unsubscribe())
  }

  async presentModalPassword (target: MappedBackupTarget<CifsBackupTarget | DiskBackupTarget>): Promise<void> {
    let message = 'Enter your master password to create an encrypted backup of your Embassy and all its services.'
    if (!target.hasValidBackup) {
      message = message + ' Since this is a fresh backup, it could take a while. Future backups will likely be much faster.'
    }

    const options: GenericInputOptions = {
      title: 'Master Password Needed',
      message,
      label: 'Master Password',
      placeholder: 'Enter master password',
      useMask: true,
      buttonText: 'Create Backup',
      submitFn: (password: string) => this.test(target, password),
    }

    const m = await this.modalCtrl.create({
      component: GenericInputComponent,
      componentProps: { options },
      cssClass: 'alertlike-modal',
    })

    await m.present()
  }

  private async test (target: MappedBackupTarget<CifsBackupTarget | DiskBackupTarget>, password: string, oldPassword?: string): Promise<void> {
    const passwordHash = this.patch.getData()['server-info']['password-hash']
    argon2.verify(passwordHash, password)

    if (!target.hasValidBackup) {
      await this.createBackup(target.id, password)
    } else {
      try {
        argon2.verify(target.entry['embassy-os']['password-hash'], oldPassword || password)
        await this.createBackup(target.id, password)
      } catch (e) {
        if (oldPassword) {
          throw e
        } else {
          setTimeout(() => this.presentModalOldPassword(target, password), 500)
        }
      }
    }
  }

  private async presentModalOldPassword (target: MappedBackupTarget<CifsBackupTarget | DiskBackupTarget>, password: string): Promise<void> {
    const options: GenericInputOptions = {
      title: 'Original Password Needed',
      message: 'This backup was created with a different password. Enter the ORIGINAL password that was used to encrypt this backup.',
      label: 'Original Password',
      placeholder: 'Enter original password',
      useMask: true,
      buttonText: 'Create Backup',
      submitFn: (oldPassword: string) => this.test(target, password, oldPassword),
    }

    const m = await this.modalCtrl.create({
      component: GenericInputComponent,
      componentProps: { options },
      cssClass: 'alertlike-modal',
    })

    await m.present()
  }

  private async createBackup (id: string, password: string, oldPassword?: string): Promise<void> {
    const loader = await this.loadingCtrl.create({
      spinner: 'lines',
      message: 'Beginning backup...',
      cssClass: 'loader',
    })
    await loader.present()

    try {
      await this.embassyApi.createBackup({
        'target-id': id,
        'old-password': oldPassword || null,
        password,
      })
    } finally {
      loader.dismiss()
    }
  }

  private subscribeToBackup () {
    this.patch.watch$('package-data')
    .pipe(
      take(1),
    )
    .subscribe(pkgs => {
      const pkgArr = Object.keys(pkgs).sort().map(key => pkgs[key])
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
