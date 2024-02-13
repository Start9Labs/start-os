import { inject, Injectable } from '@angular/core'
import { TuiAlertService, TuiDialogService } from '@taiga-ui/core'
import * as argon2 from '@start9labs/argon2'
import { ErrorService, LoadingService } from '@start9labs/shared'
import { TUI_PROMPT } from '@taiga-ui/kit'
import { PatchDB } from 'patch-db-client'
import { filter, from, take } from 'rxjs'
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
      {
        title: 'Experimental Features',
        description: 'Try out new and potentially unstable new features',
        icon: 'tuiIconThermometer',
        routerLink: 'experimental',
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
    ],
    'User Interface': [
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
        routerLink: 'interfaces',
      },
    ],
    'Privacy and Security': [
      {
        title: 'Outbound Proxy',
        description: 'Proxy outbound traffic from the StartOS main process',
        icon: 'tuiIconShield',
        action: () => this.proxyService.presentModalSetOutboundProxy(),
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
    const { 'password-hash': passwordHash } = await getServerInfo(this.patch)
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
        'old-password': value.currentPassword,
        'new-password': value.newPassword1,
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
