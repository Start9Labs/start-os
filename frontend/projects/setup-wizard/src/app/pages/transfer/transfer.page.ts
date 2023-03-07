import { Component } from '@angular/core'
import { NavController } from '@ionic/angular'
import { ApiService } from 'src/app/services/api/api.service'
import { DiskInfo, ErrorService } from '@start9labs/shared'
import { StateService } from 'src/app/services/state.service'
import { TuiDialogService } from '@taiga-ui/core'
import { TUI_PROMPT } from '@taiga-ui/kit'
import { filter } from 'rxjs'

@Component({
  selector: 'app-transfer',
  templateUrl: 'transfer.page.html',
  styleUrls: ['transfer.page.scss'],
})
export class TransferPage {
  loading = true
  drives: DiskInfo[] = []

  constructor(
    private readonly apiService: ApiService,
    private readonly navCtrl: NavController,
    private readonly dialogs: TuiDialogService,
    private readonly errorService: ErrorService,
    private readonly stateService: StateService,
  ) {}

  async ngOnInit() {
    this.stateService.setupType = 'transfer'
    await this.getDrives()
  }

  async refresh() {
    this.loading = true
    await this.getDrives()
  }

  async getDrives() {
    try {
      this.drives = await this.apiService.getDrives()
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      this.loading = false
    }
  }

  select(guid: string) {
    this.dialogs
      .open(TUI_PROMPT, {
        label: 'Warning',
        size: 's',
        data: {
          content:
            'After transferring data from this drive, <b>do not</b> attempt to boot into it again as a Start9 Server. This may result in services malfunctioning, data corruption, or loss of funds.',
          yes: 'Continue',
          no: 'Cancel',
        },
      })
      .pipe(filter(Boolean))
      .subscribe(() => {
        this.stateService.recoverySource = {
          type: 'migrate',
          guid,
        }
        this.navCtrl.navigateForward(`/storage`)
      })
  }
}
