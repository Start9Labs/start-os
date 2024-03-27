import {
  ChangeDetectionStrategy,
  Component,
  inject,
  Injectable,
} from '@angular/core'
import { FormsModule } from '@angular/forms'
import { TuiAlertService, TuiDialogService } from '@taiga-ui/core'
import * as argon2 from '@start9labs/argon2'
import { ErrorService, LoadingService } from '@start9labs/shared'
import { TUI_PROMPT, TuiCheckboxLabeledModule } from '@taiga-ui/kit'
import { PolymorpheusComponent } from '@tinkoff/ng-polymorpheus'
import { PatchDB } from 'patch-db-client'
import { filter, firstValueFrom, from, take } from 'rxjs'
import { switchMap } from 'rxjs/operators'
import { FormComponent } from 'src/app/apps/portal/components/form.component'
import { PROMPT } from 'src/app/apps/portal/modals/prompt.component'
import { ProxyService } from 'src/app/services/proxy.service'
import { configBuilderToSpec } from 'src/app/util/configBuilderToSpec'
import { getServerInfo } from 'src/app/util/get-server-info'
import { FormDialogService } from 'src/app/services/form-dialog.service'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { ApiService } from 'src/app/services/api/embassy-api.service'

import { passwordSpec, PasswordSpec, SettingBtn } from './settings.types'
import { ConfigService } from 'src/app/services/config.service'

@Injectable({ providedIn: 'root' })
export class SettingsService {
  private readonly alerts = inject(TuiAlertService)
  private readonly dialogs = inject(TuiDialogService)
  private readonly proxyService = inject(ProxyService)
  private readonly loader = inject(LoadingService)
  private readonly errorService = inject(ErrorService)
  private readonly formDialog = inject(FormDialogService)
  private readonly patch = inject(PatchDB<DataModel>)
  private readonly api = inject(ApiService)
  private readonly isTor = inject(ConfigService).isTor()

  wipe = false

  readonly settings: Record<string, readonly SettingBtn[]> = {
    General: [
      {
        title: 'Email',
        description:
          'Connect to an external SMTP server to send yourself emails',
        icon: 'tuiIconMail',
        routerLink: 'email',
      },
      {
        title: 'Change Master Password',
        description: `Change your StartOS master password`,
        icon: 'tuiIconKey',
        action: () => this.promptNewPassword(),
      },
    ],
    Network: [
      {
        title: 'Domains',
        description: 'Manage domains for clearnet connectivity',
        icon: 'tuiIconGlobe',
        routerLink: 'domains',
      },
      {
        title: 'Proxies',
        description: 'Manage proxies for inbound and outbound connections',
        icon: 'tuiIconShuffle',
        routerLink: 'proxies',
      },
      {
        title: 'Router Config',
        description: 'Connect or configure your router for clearnet',
        icon: 'tuiIconRadio',
        routerLink: 'router',
      },
      {
        title: 'WiFi',
        description: 'Add or remove WiFi networks',
        icon: 'tuiIconWifi',
        routerLink: 'wifi',
      },
      {
        title: 'Reset Tor',
        description: `May help resolve Tor connectivity issues`,
        icon: 'tuiIconRefreshCw',
        action: () => this.promptResetTor(),
      },
    ],
    'StartOS UI': [
      {
        title: 'Browser Tab Title',
        description: `Customize the display name of your browser tab`,
        icon: 'tuiIconTag',
        action: () => this.setBrowserTab(),
      },
      {
        title: 'Web Addresses',
        description: 'View and manage web addresses for accessing this UI',
        icon: 'tuiIconMonitor',
        routerLink: 'ui',
      },
    ],
    'Privacy and Security': [
      {
        title: 'Outbound Proxy',
        description: 'Proxy outbound traffic from the StartOS main process',
        icon: 'tuiIconShield',
        action: () => this.setOutboundProxy(),
      },
      {
        title: 'SSH',
        description:
          'Manage your SSH keys to access your server from the command line',
        icon: 'tuiIconTerminal',
        routerLink: 'ssh',
      },
      {
        title: 'Active Sessions',
        description: 'View and manage device access',
        icon: 'tuiIconClock',
        routerLink: 'sessions',
      },
    ],
  }

  private async setOutboundProxy(): Promise<void> {
    const proxy = await firstValueFrom(
      this.patch.watch$('serverInfo', 'network', 'outboundProxy'),
    )
    await this.proxyService.presentModalSetOutboundProxy(proxy)
  }

  private promptResetTor() {
    this.wipe = false
    this.dialogs
      .open(TUI_PROMPT, {
        label: this.isTor ? 'Warning' : 'Confirm',
        data: {
          content: new PolymorpheusComponent(WipeComponent),
          yes: 'Reset',
          no: 'Cancel',
        },
      })
      .pipe(filter(Boolean))
      .subscribe(() => this.resetTor(this.wipe))
  }

  private async resetTor(wipeState: boolean) {
    const loader = this.loader.open('Resetting Tor...').subscribe()

    try {
      await this.api.resetTor({
        wipeState: wipeState,
        reason: 'User triggered',
      })
      this.alerts.open('Tor reset in progress').subscribe()
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }

  private async setBrowserTab(): Promise<void> {
    this.patch
      .watch$('ui', 'name')
      .pipe(
        switchMap(initialValue =>
          this.dialogs.open<string>(PROMPT, {
            label: 'Browser Tab Title',
            data: {
              message: `This value will be displayed as the title of your browser tab.`,
              label: 'Device Name',
              placeholder: 'StartOS',
              required: false,
              buttonText: 'Save',
              initialValue,
            },
          }),
        ),
        take(1),
      )
      .subscribe(async name => {
        const loader = this.loader.open('Saving...').subscribe()

        try {
          await this.api.setDbValue<string | null>(['name'], name || null)
        } finally {
          loader.unsubscribe()
        }
      })
  }

  private promptNewPassword() {
    this.dialogs
      .open(TUI_PROMPT, {
        label: 'Warning',
        size: 's',
        data: {
          content:
            'You will still need your current password to decrypt existing backups!',
          yes: 'Continue',
          no: 'Cancel',
        },
      })
      .pipe(
        filter(Boolean),
        switchMap(() => from(configBuilderToSpec(passwordSpec))),
      )
      .subscribe(spec => {
        this.formDialog.open(FormComponent, {
          label: 'Change Master Password',
          data: {
            spec,
            buttons: [
              {
                text: 'Save',
                handler: (value: PasswordSpec) => this.resetPassword(value),
              },
            ],
          },
        })
      })
  }

  private async resetPassword(value: PasswordSpec): Promise<boolean> {
    let err = ''

    if (value.newPassword1 !== value.newPassword2) {
      err = 'New passwords do not match'
    } else if (value.newPassword1.length < 12) {
      err = 'New password must be 12 characters or greater'
    } else if (value.newPassword1.length > 64) {
      err = 'New password must be less than 65 characters'
    }

    // confirm current password is correct
    const { passwordHash } = await getServerInfo(this.patch)
    try {
      argon2.verify(passwordHash, value.currentPassword)
    } catch (e) {
      err = 'Current password is invalid'
    }

    if (err) {
      this.errorService.handleError(err)
      return false
    }

    const loader = this.loader.open('Saving...').subscribe()

    try {
      await this.api.resetPassword({
        oldPassword: value.currentPassword,
        newPassword: value.newPassword1,
      })

      this.alerts.open('Password changed!').subscribe()

      return true
    } catch (e: any) {
      this.errorService.handleError(e)
      return false
    } finally {
      loader.unsubscribe()
    }
  }
}

@Component({
  standalone: true,
  template: `
    <p>
      @if (isTor) {
        You are currently connected over Tor. If you reset the Tor daemon, you
        will lose connectivity until it comes back online.
      } @else {
        Reset Tor?
      }
    </p>
    <p>
      Optionally wipe state to forcibly acquire new guard nodes. It is
      recommended to try without wiping state first.
    </p>
    <tui-checkbox-labeled size="l" [(ngModel)]="service.wipe">
      Wipe state
    </tui-checkbox-labeled>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [TuiCheckboxLabeledModule, FormsModule],
})
class WipeComponent {
  readonly isTor = inject(ConfigService).isTor()
  readonly service = inject(SettingsService)
}
