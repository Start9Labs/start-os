import { Component, inject } from '@angular/core'
import { FormsModule } from '@angular/forms'
import {
  TuiButton,
  TuiDialogContext,
  TuiDialogOptions,
  TuiGroup,
  TuiLoader,
} from '@taiga-ui/core'
import { TuiBlock, TuiCheckbox } from '@taiga-ui/kit'
import {
  POLYMORPHEUS_CONTEXT,
  PolymorpheusComponent,
} from '@taiga-ui/polymorpheus'
import { PatchDB } from 'patch-db-client'
import { firstValueFrom, map } from 'rxjs'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { getManifest } from 'src/app/utils/get-package-data'

interface Package {
  id: string
  title: string
  icon: string
  disabled: boolean
  checked: boolean
}

@Component({
  template: `
    <div tuiGroup orientation="vertical" [collapsed]="true">
      @if (pkgs) {
        @for (pkg of pkgs; track $index) {
          <label tuiBlock>
            <img class="icon" alt="" [src]="pkg.icon" />
            {{ pkg.title }}
            <input
              type="checkbox"
              tuiCheckbox
              [disabled]="pkg.disabled"
              [(ngModel)]="pkg.checked"
              (ngModelChange)="handleChange()"
            />
          </label>
        } @empty {
          No services installed!
        }
      } @else {
        <tui-loader />
      }
    </div>
    <footer class="g-buttons">
      <button tuiButton appearance="flat-grayscale" (click)="toggleSelectAll()">
        Toggle all
      </button>
      <button tuiButton [disabled]="!hasSelection" (click)="done()">
        {{ context.data?.btnText || 'Done' }}
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
  imports: [FormsModule, TuiButton, TuiGroup, TuiLoader, TuiBlock, TuiCheckbox],
})
export class BackupsBackupModal {
  private readonly patch = inject<PatchDB<DataModel>>(PatchDB)
  readonly context =
    inject<TuiDialogContext<string[], { btnText: string } | undefined>>(
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
