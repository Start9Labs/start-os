import { NgForOf, NgIf } from '@angular/common'
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
import { NetworkInfo } from 'src/app/services/patch-db/data-model'
import { InterfaceAddressComponent } from './interface-addresses.component'
import { InterfaceComponent } from './interface.component'

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
    @for (
      address of interface.serviceInterface.addresses.clearnet;
      track $index
    ) {
      <app-interface-address
        [label]="address.label"
        [address]="address.url"
        [isMasked]="interface.serviceInterface.masked"
        [isUi]="interface.serviceInterface.type === 'ui'"
      />
    } @empty {
      <button
        tuiButton
        iconLeft="tuiIconPlus"
        [style.align-self]="'flex-start'"
        (click)="add()"
      >
        Add Address
      </button>
    }
  `,
  imports: [NgForOf, InterfaceAddressComponent, NgIf, TuiButtonModule],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class InterfaceClearnetComponent {
  private readonly formDialog = inject(FormDialogService)
  private readonly loader = inject(LoadingService)
  private readonly errorService = inject(ErrorService)
  private readonly api = inject(ApiService)
  private readonly dialogs = inject(TuiDialogService)
  readonly interface = inject(InterfaceComponent)

  @Input({ required: true }) network!: NetworkInfo

  async add() {
    const options: Partial<TuiDialogOptions<FormContext<ClearnetForm>>> = {
      label: 'Select Domain/Subdomain',
      data: {
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
          if (this.interface.packageContext) {
            await this.api.setInterfaceClearnetAddress({
              ...this.interface.packageContext,
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
      if (this.interface.packageContext) {
        await this.api.setInterfaceClearnetAddress({
          ...this.interface.packageContext,
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
