import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { ToastController } from '@ionic/angular'
import { copyToClipboard } from '@start9labs/shared'
import { T } from '@start9labs/start-sdk'
import { StandardActionsService } from 'src/app/services/standard-actions.service'

@Component({
  selector: 'app-show-error',
  templateUrl: 'app-show-error.component.html',
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class AppShowErrorComponent {
  @Input()
  manifest!: T.Manifest

  @Input()
  error!: T.MainStatus & { main: 'error' }

  constructor(
    private readonly toastCtrl: ToastController,
    private readonly standardActionsService: StandardActionsService,
  ) {}

  async copy(text: string): Promise<void> {
    const success = await copyToClipboard(text)
    const message = success
      ? 'Copied to clipboard!'
      : 'Failed to copy to clipboard.'

    const toast = await this.toastCtrl.create({
      header: message,
      position: 'bottom',
      duration: 1000,
    })
    await toast.present()
  }

  async rebuild() {
    return this.standardActionsService.rebuild(this.manifest.id)
  }

  async tryUninstall() {
    return this.standardActionsService.tryUninstall(this.manifest)
  }
}
