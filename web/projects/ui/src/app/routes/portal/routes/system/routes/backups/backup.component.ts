import { Component, inject } from '@angular/core'
import { FormsModule } from '@angular/forms'
import * as argon2 from '@start9labs/argon2'
import {
  DialogService,
  ErrorService,
  i18nPipe,
  LoadingService,
} from '@start9labs/shared'
import { TuiButton, TuiGroup, TuiLoader, TuiTitle } from '@taiga-ui/core'
import { TuiBlock, TuiCheckbox } from '@taiga-ui/kit'
import { injectContext, PolymorpheusComponent } from '@taiga-ui/polymorpheus'
import { PatchDB } from 'patch-db-client'
import { firstValueFrom, map } from 'rxjs'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { getManifest } from 'src/app/utils/get-package-data'
import { getServerInfo } from 'src/app/utils/get-server-info'
import { verifyPassword } from 'src/app/utils/verify-password'
import { BackupService } from './backup.service'
import { BackupContext } from './backup.types'

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
          <label tuiBlock="m">
            <img alt="" [src]="pkg.icon" />
            <span tuiTitle>{{ pkg.title }}</span>
            <input
              type="checkbox"
              tuiCheckbox
              [disabled]="pkg.disabled"
              [(ngModel)]="pkg.checked"
              (ngModelChange)="handleChange()"
            />
          </label>
        } @empty {
          {{ 'No services installed' | i18n }}
        }
      } @else {
        <tui-loader />
      }
    </div>
    <footer class="g-buttons">
      <button tuiButton appearance="flat-grayscale" (click)="toggleSelectAll()">
        {{ 'Toggle all' | i18n }}
      </button>
      <button tuiButton [disabled]="!hasSelection" (click)="done()">
        {{ 'Done' | i18n }}
      </button>
    </footer>
  `,
  styles: [
    `
      [tuiGroup] {
        width: 100%;
        margin: 1.5rem 0 0;
      }

      [tuiBlock] {
        align-items: center;
      }

      img {
        width: 2.5rem;
        border-radius: 100%;
      }
    `,
  ],
  standalone: true,
  imports: [
    FormsModule,
    TuiButton,
    TuiGroup,
    TuiLoader,
    TuiBlock,
    TuiCheckbox,
    TuiTitle,
    i18nPipe,
  ],
})
export class BackupsBackupComponent {
  private readonly dialog = inject(DialogService)
  private readonly loader = inject(LoadingService)
  private readonly errorService = inject(ErrorService)
  private readonly api = inject(ApiService)
  private readonly patch = inject<PatchDB<DataModel>>(PatchDB)
  private readonly service = inject(BackupService)

  readonly context = injectContext<BackupContext>()

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

  async done() {
    const { passwordHash, id } = await getServerInfo(this.patch)
    const { entry } = this.context.data

    this.dialog
      .openPrompt<string>({
        label: 'Master Password Needed',
        data: {
          message: 'Enter your master password to encrypt this backup.',
          label: 'Master Password',
          placeholder: 'Enter master password',
          useMask: true,
          buttonText: 'Create Backup',
        },
      })
      .pipe(verifyPassword(passwordHash, e => this.errorService.handleError(e)))
      .subscribe(async password => {
        // first time backup
        if (!this.service.hasThisBackup(entry, id)) {
          this.createBackup(password)
          // existing backup
        } else {
          try {
            argon2.verify(entry.startOs[id]?.passwordHash!, password)
            await this.createBackup(password)
          } catch {
            this.oldPassword(password)
          }
        }
      })
  }

  handleChange() {
    this.hasSelection = !!this.pkgs?.some(p => p.checked)
  }

  toggleSelectAll() {
    this.pkgs?.forEach(p => (p.checked = !this.hasSelection && !p.disabled))
    this.hasSelection = !this.hasSelection
  }

  private async oldPassword(password: string) {
    const { id } = await getServerInfo(this.patch)
    const { passwordHash = '' } = this.context.data.entry.startOs[id] || {}

    this.dialog
      .openPrompt<string>({
        label: 'Original Password Needed',
        data: {
          message:
            'This backup was created with a different password. Enter the original password that was used to encrypt this backup.',
          label: 'Original Password',
          placeholder: 'Enter original password',
          useMask: true,
          buttonText: 'Create Backup',
        },
      })
      .pipe(verifyPassword(passwordHash, e => this.errorService.handleError(e)))
      .subscribe(oldPassword => this.createBackup(password, oldPassword))
  }

  private async createBackup(
    password: string,
    oldPassword: string | null = null,
  ) {
    const loader = this.loader.open('Beginning backup').subscribe()
    const packageIds = this.pkgs?.filter(p => p.checked).map(p => p.id) || []
    const params = {
      targetId: this.context.data.id,
      packageIds,
      oldPassword,
      password,
    }

    try {
      await this.api.createBackup(params)
      this.context.$implicit.complete()
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }
}

export const BACKUP = new PolymorpheusComponent(BackupsBackupComponent)
