import { Component, Input } from '@angular/core'
import { ModalController } from '@ionic/angular'
import { map } from 'rxjs/operators'
import { DataModel, PackageState } from 'src/app/services/patch-db/data-model'
import { PatchDB } from 'patch-db-client'
import { firstValueFrom } from 'rxjs'

@Component({
  selector: 'backup-select',
  templateUrl: './backup-select.page.html',
  styleUrls: ['./backup-select.page.scss'],
})
export class BackupSelectPage {
  @Input() btnText!: string
  @Input() selectedIds: string[] = []

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
      this.patch.watch$('package-data').pipe(
        map(pkgs => {
          return Object.values(pkgs)
            .map(pkg => {
              const { id, title } = pkg.manifest
              return {
                id,
                title,
                icon: pkg.icon,
                disabled: pkg.state !== PackageState.Installed,
                checked: this.selectedIds.includes(id),
              }
            })
            .sort((a, b) =>
              b.title.toLowerCase() > a.title.toLowerCase() ? -1 : 1,
            )
        }),
      ),
    )
  }

  dismiss() {
    this.modalCtrl.dismiss()
  }

  async done() {
    const pkgIds = this.pkgs.filter(p => p.checked).map(p => p.id)
    this.modalCtrl.dismiss(pkgIds)
  }

  handleChange() {
    this.hasSelection = this.pkgs.some(p => p.checked)
  }

  toggleSelectAll() {
    this.pkgs.forEach(pkg => (pkg.checked = this.selectAll))
    this.selectAll = !this.selectAll
  }
}
