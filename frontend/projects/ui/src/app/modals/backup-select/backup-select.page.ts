import { Component } from '@angular/core'
import { ModalController } from '@ionic/angular'
import { map, take } from 'rxjs/operators'
import { DataModel, PackageState } from 'src/app/services/patch-db/data-model'
import { PatchDB } from 'patch-db-client'

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

  ngOnInit() {
    this.patch
      .watch$('package-data')
      .pipe(
        map(pkgs => {
          return Object.values(pkgs).map(pkg => {
            const { id, title } = pkg.manifest
            return {
              id,
              title,
              icon: pkg['static-files'].icon,
              disabled: pkg.state !== PackageState.Installed,
              checked: pkg.state === PackageState.Installed,
            }
          })
        }),
        take(1),
      )
      .subscribe(pkgs => (this.pkgs = pkgs))
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
