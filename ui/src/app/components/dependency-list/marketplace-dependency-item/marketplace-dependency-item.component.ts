import { Component, Input, OnInit } from '@angular/core'
import { NavigationExtras } from '@angular/router'
import { NavController } from '@ionic/angular'
import { BehaviorSubject, Observable } from 'rxjs'
import { AppDependency, BaseApp, DependencyViolationSeverity, getViolationSeverity, isOptional, isMissing, isInstalling, isRecommended, isVersionMismatch } from 'src/app/models/app-types'
import { Recommendation } from '../../recommendation-button/recommendation-button.component'

@Component({
  selector: 'marketplace-dependency-item',
  templateUrl: './marketplace-dependency-item.component.html',
  styleUrls: ['./marketplace-dependency-item.component.scss'],
})
export class MarketplaceDependencyItemComponent implements OnInit {
  @Input() dep: AppDependency
  @Input() hostApp: BaseApp
  @Input() $loading$: BehaviorSubject<boolean>

  presentAlertDescription = false

  isLoading$: Observable<boolean>
  color: string
  installing = false
  recommended = false
  badgeStyle: string
  violationSeverity: DependencyViolationSeverity
  statusText: string
  actionText: 'View' | 'Get'

  descriptionText: string

  constructor (
    private readonly navCtrl: NavController,
  ) { }

  ngOnInit () {
    this.violationSeverity = getViolationSeverity(this.dep)
    if (isOptional(this.dep)) throw new Error('Do not display optional deps, satisfied or otherwise, on the AAL')

    const { actionText, color, statusText, installing } = this.getValues()

    this.color = color
    this.statusText = statusText
    this.installing = installing
    this.recommended = isRecommended(this.dep)
    this.actionText = actionText
    this.badgeStyle = `background: radial-gradient(var(--ion-color-${this.color}) 40%, transparent)`
    this.descriptionText = `<p>${this.dep.description}<\p>`
    if (this.recommended) {
      this.descriptionText = this.descriptionText + `<p>This service is not required: ${this.dep.optional}<\p>`
    }
  }

  isDanger (): boolean {
    return [DependencyViolationSeverity.REQUIRED, DependencyViolationSeverity.RECOMMENDED].includes(this.violationSeverity)
  }

  getValues (): { color: string, statusText: string, installing: boolean, actionText: 'View' | 'Get' } {
    if (isInstalling(this.dep)) return { color: 'primary', statusText: 'Installing', installing: true, actionText: undefined }
    if (!this.isDanger()) return { color: 'success', statusText: 'Satisfied', installing: false, actionText: 'View' }
    if (isMissing(this.dep)) return { color: 'warning', statusText: 'Not Installed', installing: false, actionText: 'Get' }
    if (isVersionMismatch(this.dep)) return { color: 'warning', statusText: 'Incompatible Version Installed', installing: false, actionText: 'Get' }
    return { color: 'success', statusText: 'Satisfied', installing: false, actionText: 'View' }
  }

  async toInstall () {
    if (this.actionText === 'View') return this.navCtrl.navigateForward(`/services/marketplace/${this.dep.id}`)

    const verb = this.violationSeverity === DependencyViolationSeverity.REQUIRED ? 'requires' : 'recommends'
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

}
