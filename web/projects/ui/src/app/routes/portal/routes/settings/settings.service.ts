import {
  ChangeDetectionStrategy,
  Component,
  inject,
  Injectable,
} from '@angular/core'
import { FormsModule } from '@angular/forms'
import {
  TuiAlertService,
  TuiDialogOptions,
  TuiDialogService,
  TuiLabel,
} from '@taiga-ui/core'
import * as argon2 from '@start9labs/argon2'
import { ErrorService, LoadingService } from '@start9labs/shared'
import { TuiConfirmData, TUI_CONFIRM, TuiCheckbox } from '@taiga-ui/kit'
import { PolymorpheusComponent } from '@taiga-ui/polymorpheus'
import { PatchDB } from 'patch-db-client'
import { filter, firstValueFrom, from, take } from 'rxjs'
import { switchMap } from 'rxjs/operators'
import { FormComponent } from 'src/app/routes/portal/components/form.component'
import { PROMPT } from 'src/app/routes/portal/modals/prompt.component'
import { AuthService } from 'src/app/services/auth.service'
import { configBuilderToSpec } from 'src/app/utils/configBuilderToSpec'
import { getServerInfo } from 'src/app/utils/get-server-info'
import { FormDialogService } from 'src/app/services/form-dialog.service'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { ApiService } from 'src/app/services/api/embassy-api.service'

import { passwordSpec, PasswordSpec, SettingBtn } from './settings.types'
import { ConfigService } from 'src/app/services/config.service'

@Injectable({ providedIn: 'root' })
export class SettingsService {
  private readonly alerts = inject(TuiAlertService)
  private readonly dialogs = inject(TuiDialogService)
  private readonly loader = inject(LoadingService)
  private readonly errorService = inject(ErrorService)
  private readonly formDialog = inject(FormDialogService)
  private readonly patch = inject<PatchDB<DataModel>>(PatchDB)
  private readonly api = inject(ApiService)
  private readonly auth = inject(AuthService)
  private readonly isTor = inject(ConfigService).isTor()

  wipe = false

  readonly settings: Record<string, readonly SettingBtn[]> = {
    General: [
      {
        title: 'Email',
        description:
          'Connect to an external SMTP server to send yourself emails',
        icon: '@tui.mail',
        routerLink: 'email',
      },
      {
        title: 'Change Master Password',
        description: `Change your StartOS master password`,
        icon: '@tui.key',
        action: () => this.promptNewPassword(),
      },
    ],
    Network: [
      // {
      //   title: 'Domains',
      //   description: 'Manage domains for clearnet connectivity',
      //   icon: '@tui.globe',
      //   routerLink: 'domains',
      // },
      // {
      //   title: 'Proxies',
      //   description: 'Manage proxies for inbound and outbound connections',
      //   icon: '@tui.shuffle',
      //   routerLink: 'proxies',
      // },
      // {
      //   title: 'Router Config',
      //   description: 'Connect or configure your router for clearnet',
      //   icon: '@tui.radio',
      //   routerLink: 'router',
      // },
      {
        title: 'WiFi',
        description: 'Add or remove WiFi networks',
        icon: '@tui.wifi',
        routerLink: 'wifi',
      },
      {
        title: 'Reset Tor',
        description: `May help resolve Tor connectivity issues`,
        icon: '@tui.refresh-cw',
        action: () => this.promptResetTor(),
      },
    ],
    'StartOS UI': [
      {
        title: 'Browser Tab Title',
        description: `Customize the display name of your browser tab`,
        icon: '@tui.tag',
        action: () => this.setBrowserTab(),
      },
      {
        title: 'Web Addresses',
        description: 'View and manage web addresses for accessing this UI',
        icon: '@tui.monitor',
        routerLink: 'ui',
      },
    ],
    'Privacy and Security': [
      // {
      //   title: 'Outbound Proxy',
      //   description: 'Proxy outbound traffic from the StartOS main process',
      //   icon: '@tui.shield',
      //   action: () => this.setOutboundProxy(),
      // },
      {
        title: 'SSH',
        description:
          'Manage your SSH keys to access your server from the command line',
        icon: '@tui.terminal',
        routerLink: 'ssh',
      },
      {
        title: 'Active Sessions',
        description: 'View and manage device access',
        icon: '@tui.clock',
        routerLink: 'sessions',
      },
    ],
    Power: [
      {
        title: 'Restart',
        icon: '@tui.refresh-cw',
        description: 'Restart Start OS server',
        action: () => this.promptPower('Restart'),
      },
      {
        title: 'Shutdown',
        icon: '@tui.power',
        description: 'Turn Start OS server off',
        action: () => this.promptPower('Shutdown'),
      },
      {
        title: 'Logout',
        icon: '@tui.log-out',
        description: 'Log off from Start OS',
        action: () => {
          this.api.logout({}).catch(e => console.error('Failed to log out', e))
          this.auth.setUnverified()
        },
      },
    ],
  }

  // private async setOutboundProxy(): Promise<void> {
  //   const proxy = await firstValueFrom(
  //     this.patch.watch$('serverInfo', 'network', 'outboundProxy'),
  //   )
  //   await this.proxyService.presentModalSetOutboundProxy(proxy)
  // }

  private promptResetTor() {
    this.wipe = false
    this.dialogs
      .open(TUI_CONFIRM, {
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

  private async promptPower(action: 'Restart' | 'Shutdown') {
    this.dialogs
      .open(TUI_CONFIRM, getOptions(action))
      .pipe(filter(Boolean))
      .subscribe(async () => {
        const loader = this.loader.open(`Beginning ${action}...`).subscribe()

        try {
          await this.api[
            action === 'Restart' ? 'restartServer' : 'shutdownServer'
          ]({})
        } catch (e: any) {
          this.errorService.handleError(e)
        } finally {
          loader.unsubscribe()
        }
      })
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
      .open(TUI_CONFIRM, {
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
    <label tuiLabel>
      <input type="checkbox" tuiCheckbox [(ngModel)]="service.wipe" />
      Wipe state
    </label>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [TuiLabel, FormsModule, TuiCheckbox],
})
class WipeComponent {
  readonly isTor = inject(ConfigService).isTor()
  readonly service = inject(SettingsService)
}

function getOptions(
  operation: 'Restart' | 'Shutdown',
): Partial<TuiDialogOptions<TuiConfirmData>> {
  return operation === 'Restart'
    ? {
        label: 'Restart',
        size: 's',
        data: {
          content:
            'Are you sure you want to restart your server? It can take several minutes to come back online.',
          yes: 'Restart',
          no: 'Cancel',
        },
      }
    : {
        label: 'Warning',
        size: 's',
        data: {
          content:
            'Are you sure you want to power down your server? This can take several minutes, and your server will not come back online automatically. To power on again, You will need to physically unplug your server and plug it back in',
          yes: 'Shutdown',
          no: 'Cancel',
        },
      }
}
