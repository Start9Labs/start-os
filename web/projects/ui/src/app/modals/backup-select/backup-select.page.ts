import { Component } from '@angular/core'
import { ModalController } from '@ionic/angular'
import { map, take } from 'rxjs/operators'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { PatchDB } from 'patch-db-client'
import { firstValueFrom } from 'rxjs'
import { getManifest } from 'src/app/util/get-package-data'

@Component({
  selector: 'backup-select',
  templateUrl: './backup-select.page.html',
  styleUrls: ['./backup-select.page.scss'],
})
export class BackupSelectPage {
  hasSelection = false
  selectAll = false
  pkgs: {
    id: string
    title: string
    icon: string
    disabled: boolean
    checked: boolean
  }[] = []

  constructor(
    private readonly modalCtrl: ModalController,
    private readonly patch: PatchDB<DataModel>,
  ) {}

  async ngOnInit() {
    this.pkgs = await firstValueFrom(
      this.patch.watch$('packageData').pipe(
        map(pkgs => {
          return Object.values(pkgs)
            .map(pkg => {
              const { id, title } = getManifest(pkg)
              return {
                id,
                title,
                icon: pkg.icon,
                disabled: pkg.stateInfo.state !== 'installed',
                checked: false,
              }
            })
            .sort((a, b) =>
              b.title.toLowerCase() > a.title.toLowerCase() ? -1 : 1,
            )
        }),
      ),
    )
  }

  dismiss(success = false) {
    if (success) {
      const ids = this.pkgs.filter(p => p.checked).map(p => p.id)
      this.modalCtrl.dismiss(ids)
    } else {
      this.modalCtrl.dismiss()
    }
  }

  handleChange() {
    this.hasSelection = this.pkgs.some(p => p.checked)
  }

  toggleSelectAll() {
    this.pkgs.forEach(pkg => (pkg.checked = this.selectAll))
    this.selectAll = !this.selectAll
  }
}
