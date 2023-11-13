import { Component, Inject, Input } from '@angular/core'
import { PatchDB } from 'patch-db-client'
import { firstValueFrom, map } from 'rxjs'
import { DataModel, PackageState } from 'src/app/services/patch-db/data-model'
import { POLYMORPHEUS_CONTEXT } from '@tinkoff/ng-polymorpheus'
import { TuiDialogContext } from '@taiga-ui/core'

@Component({
  selector: 'backup-select',
  templateUrl: './backup-select.page.html',
  styleUrls: ['./backup-select.page.scss'],
})
export class BackupSelectPage {
  @Input() selectedIds: string[] = []

  hasSelection = false
  pkgs: {
    id: string
    title: string
    icon: string
    disabled: boolean
    checked: boolean
  }[] = []

  constructor(
    @Inject(POLYMORPHEUS_CONTEXT)
    private readonly context: TuiDialogContext<string[], { btnText: string }>,
    private readonly patch: PatchDB<DataModel>,
  ) {}

  get btnText(): string {
    return this.context.data.btnText
  }

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

  done() {
    this.context.completeWith(this.pkgs.filter(p => p.checked).map(p => p.id))
  }

  handleChange() {
    this.hasSelection = this.pkgs.some(p => p.checked)
  }

  toggleSelectAll() {
    this.pkgs.forEach(pkg => (pkg.checked = !this.hasSelection))
    this.hasSelection = !this.hasSelection
  }
}
