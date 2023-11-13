import { Component } from '@angular/core'
import { NavController } from '@ionic/angular'
import { DiskInfo, ErrorService, LoadingService } from '@start9labs/shared'
import { TuiDialogService } from '@taiga-ui/core'
import { ApiService } from 'src/app/services/api/api.service'
import { StateService } from 'src/app/services/state.service'
import { PASSWORD, PasswordPage } from 'src/app/modals/password/password.page'

@Component({
  selector: 'app-attach',
  templateUrl: 'attach.page.html',
  styleUrls: ['attach.page.scss'],
})
export class AttachPage {
  loading = true
  drives: DiskInfo[] = []

  constructor(
    private readonly apiService: ApiService,
    private readonly navCtrl: NavController,
    private readonly errorService: ErrorService,
    private readonly stateService: StateService,
    private readonly dialogs: TuiDialogService,
    private readonly loader: LoadingService,
  ) {}

  async ngOnInit() {
    this.stateService.setupType = 'attach'
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
      .open<string>(PASSWORD, {
        label: 'Set Password',
        size: 's',
        data: { storageDrive: true },
      })
      .subscribe(password => {
        this.attachDrive(guid, password)
      })
  }

  private async attachDrive(guid: string, password: string) {
    const loader = this.loader.open('Connecting to drive...').subscribe()

    try {
      await this.stateService.importDrive(guid, password)
      await this.navCtrl.navigateForward(`/loading`)
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }
}
