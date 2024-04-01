import { CommonModule } from '@angular/common'
import { Component, inject } from '@angular/core'
import { FormsModule } from '@angular/forms'
import { TuiForModule } from '@taiga-ui/cdk'
import {
  TuiDialogContext,
  TuiDialogOptions,
  TuiGroupModule,
  TuiLoaderModule,
} from '@taiga-ui/core'
import { TuiButtonModule } from '@taiga-ui/experimental'
import { TuiCheckboxBlockModule } from '@taiga-ui/kit'
import {
  POLYMORPHEUS_CONTEXT,
  PolymorpheusComponent,
} from '@tinkoff/ng-polymorpheus'
import { PatchDB } from 'patch-db-client'
import { firstValueFrom, map } from 'rxjs'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { getManifest } from 'src/app/util/get-package-data'

interface Package {
  id: string
  title: string
  icon: string
  disabled: boolean
  checked: boolean
}

@Component({
  template: `
    <div tuiGroup orientation="vertical">
      <tui-checkbox-block
        *ngFor="let pkg of pkgs; else: loading; empty: blank"
        [disabled]="pkg.disabled"
        [(ngModel)]="pkg.checked"
        (ngModelChange)="handleChange()"
      >
        <div class="g-action">
          <img class="icon" alt="" [src]="pkg.icon" />
          {{ pkg.title }}
        </div>
      </tui-checkbox-block>
      <ng-template #loading><tui-loader></tui-loader></ng-template>
      <ng-template #blank>No services installed!</ng-template>
    </div>
    <footer class="g-buttons">
      <button tuiButton appearance="flat" (click)="toggleSelectAll()">
        Toggle all
      </button>
      <button tuiButton [disabled]="!hasSelection" (click)="done()">
        {{ context.data.btnText || 'Done' }}
      </button>
    </footer>
  `,
  styles: [
    `
      :host {
        display: flex;
        flex-direction: column;
        margin-top: 1.5rem;
      }

      .icon {
        width: 2.5rem;
        border-radius: 100%;
      }
    `,
  ],
  standalone: true,
  imports: [
    CommonModule,
    FormsModule,
    TuiForModule,
    TuiButtonModule,
    TuiGroupModule,
    TuiCheckboxBlockModule,
    TuiLoaderModule,
  ],
})
export class BackupsBackupModal {
  private readonly patch = inject(PatchDB<DataModel>)
  readonly context =
    inject<TuiDialogContext<string[], { btnText: string }>>(
      POLYMORPHEUS_CONTEXT,
    )

  hasSelection = false

  pkgs: readonly Package[] | null = null

  async ngOnInit() {
    this.pkgs = await firstValueFrom(
      this.patch.watch$('packageData').pipe(
        map(pkgs =>
          Object.values(pkgs)
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
            ),
        ),
      ),
    )
  }

  done() {
    this.context.completeWith(
      this.pkgs?.filter(p => p.checked).map(p => p.id) || [],
    )
  }

  handleChange() {
    this.hasSelection = !!this.pkgs?.some(p => p.checked)
  }

  toggleSelectAll() {
    this.pkgs?.forEach(p => (p.checked = !this.hasSelection && !p.disabled))
    this.hasSelection = !this.hasSelection
  }
}

export const BACKUP = new PolymorpheusComponent(BackupsBackupModal)

export const BACKUP_OPTIONS: Partial<TuiDialogOptions<unknown>> = {
  label: 'Select Services to Back Up',
}
