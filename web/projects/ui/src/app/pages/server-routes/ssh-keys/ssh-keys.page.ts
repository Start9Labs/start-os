import { ChangeDetectorRef, Component } from '@angular/core'
import { ErrorService, LoadingService } from '@start9labs/shared'
import { TuiDialogOptions, TuiDialogService } from '@taiga-ui/core'
import { TUI_PROMPT, TuiPromptData } from '@taiga-ui/kit'
import { filter } from 'rxjs'
import { take } from 'rxjs/operators'
import { PROMPT } from 'src/app/modals/prompt.component'
import { SSHKey } from 'src/app/services/api/api.types'
import { ApiService } from 'src/app/services/api/embassy-api.service'

@Component({
  selector: 'ssh-keys',
  templateUrl: 'ssh-keys.page.html',
  styleUrls: ['ssh-keys.page.scss'],
})
export class SSHKeysPage {
  loading = true
  sshKeys: SSHKey[] = []
  readonly docsUrl = 'https://docs.start9.com/0.3.5.x/user-manual/ssh'

  constructor(
    private readonly cdr: ChangeDetectorRef,
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
      this.loading = false
    }
  }

  add() {
    this.dialogs
      .open<string>(PROMPT, ADD_OPTIONS)
      .pipe(take(1))
      .subscribe(async key => {
        const loader = this.loader.open('Saving...').subscribe()

        try {
          this.sshKeys?.push(await this.embassyApi.addSshKey({ key }))
        } finally {
          loader.unsubscribe()
          this.cdr.markForCheck()
        }
      })
  }

  delete(key: SSHKey) {
    this.dialogs
      .open(TUI_PROMPT, DELETE_OPTIONS)
      .pipe(filter(Boolean))
      .subscribe(async () => {
        const loader = this.loader.open('Deleting...').subscribe()

        try {
          await this.embassyApi.deleteSshKey({ fingerprint: key.fingerprint })
          this.sshKeys?.splice(this.sshKeys?.indexOf(key), 1)
        } catch (e: any) {
          this.errorService.handleError(e)
        } finally {
          loader.unsubscribe()
          this.cdr.markForCheck()
        }
      })
  }
}

const ADD_OPTIONS: Partial<TuiDialogOptions<{ message: string }>> = {
  label: 'SSH Key',
  data: {
    message:
      'Enter the SSH public key you would like to authorize for root access to your StartOS Server.',
  },
}

const DELETE_OPTIONS: Partial<TuiDialogOptions<TuiPromptData>> = {
  label: 'Confirm',
  size: 's',
  data: {
    content: 'Delete key? This action cannot be undone.',
    yes: 'Delete',
    no: 'Cancel',
  },
}
