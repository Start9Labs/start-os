import { Component, Input, OnInit } from '@angular/core'
import { NavigationExtras } from '@angular/router'
import { AlertController, NavController } from '@ionic/angular'
import { BehaviorSubject, Observable } from 'rxjs'
import { AppStatus } from 'src/app/models/app-model'
import { AppDependency, BaseApp, DependencyViolationSeverity, getInstalledViolationSeverity, getViolationSeverity, isInstalling, isMisconfigured, isMissing, isNotRunning, isVersionMismatch } from 'src/app/models/app-types'
import { Recommendation } from '../../recommendation-button/recommendation-button.component'

@Component({
  selector: 'installed-dependency-item',
  templateUrl: './installed-dependency-item.component.html',
  styleUrls: ['./installed-dependency-item.component.scss'],
})
export class InstalledDependencyItemComponent implements OnInit {
  @Input() dep: AppDependency
  @Input() hostApp: BaseApp
  @Input() $loading$: BehaviorSubject<boolean>

  isLoading$: Observable<boolean>
  color: string
  installing = false
  badgeStyle: string
  violationSeverity: DependencyViolationSeverity
  statusText: string
  actionText: string
  action: () => Promise<any>

  constructor (private readonly navCtrl: NavController, private readonly alertCtrl: AlertController) { }

  ngOnInit () {
    this.violationSeverity = getInstalledViolationSeverity(this.dep)

    const { color, statusText, installing, actionText, action } = this.getValues()

    this.color = color
    this.statusText = statusText
    this.installing = installing
    this.actionText = actionText
    this.action = action
    this.badgeStyle = `background: radial-gradient(var(--ion-color-${this.color}) 40%, transparent)`
  }

  isDanger () {
    // installed dep violations are either REQUIRED or NONE, by getInstalledViolationSeverity above.
    return [DependencyViolationSeverity.REQUIRED].includes(this.violationSeverity)
  }

  getValues (): { color: string, statusText: string, installing: boolean, actionText: string, action: () => Promise<any> } {
    if (isInstalling(this.dep)) return { color: 'primary', statusText: 'Installing', installing: true, actionText: undefined, action: () => this.view() }
    if (!this.isDanger()) return { color: 'success', statusText: 'Satisfied', installing: false, actionText: 'View', action: () => this.view() }

    if (isMissing(this.dep))   return { color: 'warning', statusText: 'Not Installed', installing: false, actionText: 'Install', action: () => this.install() }
    if (isVersionMismatch(this.dep)) return { color: 'warning', statusText: 'Incompatible Version Installed', installing: false, actionText: 'Update', action: () => this.install() }
    if (isMisconfigured(this.dep)) return { color: 'warning', statusText: 'Incompatible Config', installing: false, actionText: 'Configure', action: () => this.configure() }
    if (isNotRunning(this.dep)) return { color: 'warning', statusText: 'Not Running', installing: false, actionText: 'View', action: () => this.view() }
    return { color: 'success', statusText: 'Satisfied', installing: false, actionText: 'View', action: () => this.view() }
  }

  async view () {
    return this.navCtrl.navigateForward(`/services/installed/${this.dep.id}`)
  }

  async install () {
    const verb = 'requires'
    const description = `${this.hostApp.title} ${verb} an install of ${this.dep.title} satisfying ${this.dep.versionSpec}.`

    const whyDependency = this.dep.description

    const installationRecommendation: Recommendation = {
      iconURL: this.hostApp.iconURL,
      appId: this.hostApp.id,
      description,
      title: this.hostApp.title,
      versionSpec: this.dep.versionSpec,
      whyDependency,
    }
    const navigationExtras: NavigationExtras = {
      state: { installationRecommendation },
    }

    return this.navCtrl.navigateForward(`/services/marketplace/${this.dep.id}`, navigationExtras)
  }

  async configure () {
    if (this.dep.violation.name !== 'incompatible-config') return
    const configViolationDesc = this.dep.violation.ruleViolations

    const configViolationFormatted =
      `<ul>${configViolationDesc.map(d => `<li>${d}</li>`).join('\n')}</ul>`

    const configRecommendation: Recommendation = {
      iconURL: this.hostApp.iconURL,
      appId: this.hostApp.id,
      description: configViolationFormatted,
      title: this.hostApp.title,
    }
    const navigationExtras: NavigationExtras = {
      state: { configRecommendation },
    }

    return this.navCtrl.navigateForward(`/services/installed/${this.dep.id}/config`, navigationExtras)
  }

  async presentAlertDescription() {
    const description = `<p>${this.dep.description}<\p>`

    const alert = await this.alertCtrl.create({
      backdropDismiss: true,
      message: description,
    })
    await alert.present()
  }
}
