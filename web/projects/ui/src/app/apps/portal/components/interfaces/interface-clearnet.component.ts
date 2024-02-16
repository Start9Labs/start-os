import { NgIf } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  inject,
  Input,
} from '@angular/core'
import { ErrorService, LoadingService } from '@start9labs/shared'
import { TuiDialogOptions, TuiDialogService } from '@taiga-ui/core'
import { TuiButtonModule } from '@taiga-ui/experimental'
import { TUI_PROMPT } from '@taiga-ui/kit'
import { filter } from 'rxjs'
import {
  FormComponent,
  FormContext,
} from 'src/app/apps/portal/components/form.component'
import {
  getClearnetSpec,
  REMOVE,
} from 'src/app/apps/portal/components/interfaces/interface.utils'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { FormDialogService } from 'src/app/services/form-dialog.service'
import { DomainInfo, NetworkInfo } from 'src/app/services/patch-db/data-model'
import { getClearnetAddress } from 'src/app/util/clearnetAddress'
import { InterfaceComponent } from './interface.component'
import { InterfacesComponent } from './interfaces.component'

type ClearnetForm = {
  domain: string
  subdomain: string | null
}

@Component({
  standalone: true,
  selector: 'app-interface-clearnet',
  template: `
    <em>
      Add clearnet to expose this interface to the public Internet.
      <a
        href="https://docs.start9.com/latest/user-manual/interface-addresses#clearnet"
        target="_blank"
        rel="noreferrer"
      >
        <strong>View instructions</strong>
      </a>
    </em>
    <ng-container
      *ngIf="interfaces.addressInfo.domainInfo as domainInfo; else noClearnet"
    >
      <app-interface
        label="Clearnet"
        [hostname]="getClearnet(domainInfo)"
        [isUi]="interfaces.isUi"
      />
      <div [style.display]="'flex'" [style.gap.rem]="1">
        <button tuiButton size="s" (click)="add()">Update</button>
        <button tuiButton size="s" appearance="danger-solid" (click)="remove()">
          Remove
        </button>
      </div>
    </ng-container>
    <ng-template #noClearnet>
      <button
        tuiButton
        iconLeft="tuiIconPlus"
        [style.align-self]="'flex-start'"
        (click)="add()"
      >
        Add Clearnet
      </button>
    </ng-template>
  `,
  imports: [InterfaceComponent, NgIf, TuiButtonModule],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class InterfaceClearnetComponent {
  private readonly formDialog = inject(FormDialogService)
  private readonly loader = inject(LoadingService)
  private readonly errorService = inject(ErrorService)
  private readonly api = inject(ApiService)
  private readonly dialogs = inject(TuiDialogService)
  readonly interfaces = inject(InterfacesComponent)

  @Input({ required: true }) network!: NetworkInfo

  getClearnet(clearnet: DomainInfo): string {
    return getClearnetAddress('https', clearnet)
  }

  async add() {
    const { domainInfo } = this.interfaces.addressInfo
    const { domain = '', subdomain = '' } = domainInfo || {}
    const options: Partial<TuiDialogOptions<FormContext<ClearnetForm>>> = {
      label: 'Select Domain/Subdomain',
      data: {
        value: { domain, subdomain },
        spec: await getClearnetSpec(this.network),
        buttons: [
          {
            text: 'Manage domains',
            link: 'portal/system/settings/domains',
          },
          {
            text: 'Save',
            handler: async value => this.save(value),
          },
        ],
      },
    }
    this.formDialog.open(FormComponent, options)
  }

  remove() {
    this.dialogs
      .open(TUI_PROMPT, REMOVE)
      .pipe(filter(Boolean))
      .subscribe(async () => {
        const loader = this.loader.open('Removing...').subscribe()

        try {
          if (this.interfaces.packageContext) {
            await this.api.setInterfaceClearnetAddress({
              ...this.interfaces.packageContext,
              domainInfo: null,
            })
          } else {
            await this.api.setServerClearnetAddress({ domainInfo: null })
          }
        } catch (e: any) {
          this.errorService.handleError(e)
        } finally {
          loader.unsubscribe()
        }
      })
  }

  private async save(domainInfo: ClearnetForm): Promise<boolean> {
    const loader = this.loader.open('Saving...').subscribe()

    try {
      if (this.interfaces.packageContext) {
        await this.api.setInterfaceClearnetAddress({
          ...this.interfaces.packageContext,
          domainInfo,
        })
      } else {
        await this.api.setServerClearnetAddress({ domainInfo })
      }
      return true
    } catch (e: any) {
      this.errorService.handleError(e)
      return false
    } finally {
      loader.unsubscribe()
    }
  }
}
