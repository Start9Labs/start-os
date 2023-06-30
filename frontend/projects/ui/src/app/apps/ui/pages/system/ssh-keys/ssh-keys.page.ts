import { Component } from '@angular/core'
import { ErrorService, LoadingService } from '@start9labs/shared'
import { TuiDialogService } from '@taiga-ui/core'
import { BehaviorSubject, filter, take } from 'rxjs'
import { SSHKey } from 'src/app/services/api/api.types'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { PROMPT } from 'src/app/apps/ui/modals/prompt/prompt.component'
import { TUI_PROMPT } from '@taiga-ui/kit'

@Component({
  selector: 'ssh-keys',
  templateUrl: 'ssh-keys.page.html',
  styleUrls: ['ssh-keys.page.scss'],
})
export class SSHKeysPage {
  readonly docsUrl = 'https://docs.start9.com/latest/user-manual/ssh'
  sshKeys: SSHKey[] = []
  loading$ = new BehaviorSubject(true)

  constructor(
    private readonly loader: LoadingService,
    private readonly dialogs: TuiDialogService,
    private readonly errorService: ErrorService,
    private readonly embassyApi: ApiService,
  ) {}

  async ngOnInit() {
    await this.getKeys()
  }

  async getKeys(): Promise<void> {
    try {
      this.sshKeys = await this.embassyApi.getSshKeys({})
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      this.loading$.next(false)
    }
  }

  async presentModalAdd() {
    this.dialogs
      .open<string>(PROMPT, {
        label: 'SSH Key',
        data: {
          message:
            'Enter the SSH public key you would like to authorize for root access to your Embassy.',
        },
      })
      .pipe(take(1))
      .subscribe(pk => this.add(pk))
  }

  presentAlertDelete(key: SSHKey, i: number) {
    this.dialogs
      .open(TUI_PROMPT, {
        label: 'Confirm',
        size: 's',
        data: {
          content: 'Delete key? This action cannot be undone.',
          yes: 'Delete',
          no: 'Cancel',
        },
      })
      .pipe(filter(Boolean))
      .subscribe(() => this.delete(key, i))
  }

  private async add(pubkey: string): Promise<void> {
    const loader = this.loader.open('Saving...').subscribe()

    try {
      const key = await this.embassyApi.addSshKey({ key: pubkey })
      this.sshKeys.push(key)
    } finally {
      loader.unsubscribe()
    }
  }

  private async delete(key: SSHKey, i: number): Promise<void> {
    const loader = this.loader.open('Deleting...').subscribe()

    try {
      await this.embassyApi.deleteSshKey({ fingerprint: key.fingerprint })
      this.sshKeys.splice(i, 1)
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }
}
