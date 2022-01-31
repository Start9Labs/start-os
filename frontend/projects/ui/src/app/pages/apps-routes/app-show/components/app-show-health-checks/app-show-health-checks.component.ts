import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { AlertController } from '@ionic/angular'
import {
  HealthResult,
  PackageDataEntry,
} from 'src/app/services/patch-db/data-model'

@Component({
  selector: 'app-show-health-checks',
  templateUrl: './app-show-health-checks.component.html',
  styleUrls: ['./app-show-health-checks.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class AppShowHealthChecksComponent {
  @Input()
  pkg: PackageDataEntry

  @Input()
  connectionFailure = false

  HealthResult = HealthResult

  constructor(private readonly alertCtrl: AlertController) {}

  isLoading(result: HealthResult): boolean {
    return result === HealthResult.Starting || result === HealthResult.Loading
  }

  isReady(result: HealthResult): boolean {
    return result !== HealthResult.Failure && result !== HealthResult.Loading
  }

  async presentAlertDescription(id: string) {
    const health = this.pkg.manifest['health-checks'][id]

    const alert = await this.alertCtrl.create({
      header: 'Health Check',
      subHeader: health.name,
      message: health.description,
      buttons: [
        {
          text: `OK`,
          handler: () => {
            alert.dismiss()
          },
          cssClass: 'enter-click',
        },
      ],
    })
    await alert.present()
  }

  asIsOrder() {
    return 0
  }
}
