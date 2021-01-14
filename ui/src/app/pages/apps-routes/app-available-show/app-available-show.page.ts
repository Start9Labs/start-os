import { Component, HostListener, NgZone } from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { AppAvailableFull, AppAvailableVersionSpecificInfo, AppDependency } from 'src/app/models/app-types'
import { ApiService } from 'src/app/services/api/api.service'
import { AlertController, ModalController, NavController, PopoverController } from '@ionic/angular'
import { markAsLoadingDuring$ } from 'src/app/services/loader.service'
import { BehaviorSubject, from, merge, Observable, of, Subscription } from 'rxjs'
import { catchError, concatMap, filter, switchMap, tap } from 'rxjs/operators'
import { Recommendation } from 'src/app/components/recommendation-button/recommendation-button.component'
import { wizardModal } from 'src/app/components/install-wizard/install-wizard.component'
import { WizardBaker } from 'src/app/components/install-wizard/prebaked-wizards'
import { AppModel } from 'src/app/models/app-model'
import { initPropertySubject, peekProperties, PropertySubject } from 'src/app/util/property-subject.util'
import { Cleanup } from 'src/app/util/cleanup'
import { InformationPopoverComponent } from 'src/app/components/information-popover/information-popover.component'
import { pauseFor } from 'src/app/util/misc.util'
import { AppReleaseNotesPage } from 'src/app/modals/app-release-notes/app-release-notes.page'
import { Emver } from 'src/app/services/emver.service'
import { displayEmver } from 'src/app/pipes/emver.pipe'

@Component({
  selector: 'app-available-show',
  templateUrl: './app-available-show.page.html',
  styleUrls: ['./app-available-show.page.scss'],
})
export class AppAvailableShowPage extends Cleanup {
  $loading$ = new BehaviorSubject(true)

  // When a new version is selected
  $newVersionLoading$ = new BehaviorSubject(false)
  // When dependencies are refreshing
  $dependenciesLoading$ = new BehaviorSubject(false)

  $error$ = new BehaviorSubject(undefined)
  $app$: PropertySubject<AppAvailableFull> = { } as any
  appId: string

  openRecommendation = false
  recommendation: Recommendation | null = null

  serviceDependencyDefintion = '<span style="font-style: italic">Service Dependencies</span> are other services that this service recommends or requires in order to run.'

  showMoreReleaseNotes = false

  constructor (
    private readonly route: ActivatedRoute,
    private readonly apiService: ApiService,
    private readonly alertCtrl: AlertController,
    private readonly zone: NgZone,
    private readonly modalCtrl: ModalController,
    private readonly wizardBaker: WizardBaker,
    private readonly navCtrl: NavController,
    private readonly appModel: AppModel,
    private readonly popoverController: PopoverController,
    private readonly emver: Emver,
  ) {
    super()
  }

  async ngOnInit () {
    this.appId = this.route.snapshot.paramMap.get('appId') as string

    this.cleanup(
      // new version always includes dependencies, but not vice versa
      this.$newVersionLoading$.subscribe(this.$dependenciesLoading$),
      markAsLoadingDuring$(this.$loading$,
        from(this.apiService.getAvailableApp(this.appId)).pipe(
          tap(app => this.$app$ = initPropertySubject(app)),
          concatMap(() => this.fetchRecommendation()),
        ),
      ).pipe(
        concatMap(() => this.syncWhenDependencyInstalls()), //must be final in stack
        catchError(e => of(this.setError(e))),
      ).subscribe(),
    )
  }

  ionViewDidEnter () {
    markAsLoadingDuring$(this.$dependenciesLoading$, this.syncVersionSpecificInfo()).subscribe({
      error: e => this.setError(e),
    })
  }

  async presentPopover (information: string, ev: any) {
    const popover = await this.popoverController.create({
      component: InformationPopoverComponent,
      event: ev,
      translucent: false,
      showBackdrop: true,
      backdropDismiss: true,
      componentProps: {
        information,
      },
    })
    return await popover.present()
  }

  syncVersionSpecificInfo (versionSpec?: string): Observable<any> {
    if (!this.$app$.versionViewing) return of({ })
    const specToFetch = versionSpec || `=${this.$app$.versionViewing.getValue()}`
    return from(this.apiService.getAvailableAppVersionSpecificInfo(this.appId, specToFetch)).pipe(
      tap(versionInfo => this.mergeInfo(versionInfo)),
    )
  }

  private mergeInfo (versionSpecificInfo: AppAvailableVersionSpecificInfo) {
    this.zone.run(() => {
      Object.entries(versionSpecificInfo).forEach( ([k, v]) => {
        if (!this.$app$[k]) this.$app$[k] = new BehaviorSubject(undefined)
        if (v !== this.$app$[k].getValue()) this.$app$[k].next(v)
      })
    })
  }

  async presentAlertVersions () {
    const app = peekProperties(this.$app$)
    const alert = await this.alertCtrl.create({
      header: 'Versions',
      backdropDismiss: false,
      inputs: app.versions.sort((a, b) => -1 * this.emver.compare(a, b)).map(v => {
        return { name: v, // for CSS
          type: 'radio',
          label: displayEmver(v), // appearance on screen
          value: v, // literal SEM version value
          checked: app.versionViewing === v,
        }
      }),
      buttons: [
        {
          text: 'Cancel',
          role: 'cancel',
        }, {
          text: 'Ok',
          handler: (version: string) => {
            const previousVersion = this.$app$.versionViewing.getValue()
            this.$app$.versionViewing.next(version)
            markAsLoadingDuring$(
              this.$newVersionLoading$, this.syncVersionSpecificInfo(`=${version}`),
            )
            .subscribe({
              error: e => {
                this.setError(e)
                this.$app$.versionViewing.next(previousVersion)
              },
            })
          },
        },
      ],
    })

    await alert.present()
  }

  async install () {
    const app = peekProperties(this.$app$)
    const { cancelled } = await wizardModal(
      this.modalCtrl,
      this.wizardBaker.install({
        id: app.id,
        title: app.title,
        version: app.versionViewing,
        serviceRequirements: app.serviceRequirements,
        installWarning: app.installWarning,
      }),
    )
    if (cancelled) return
    this.navCtrl.back()
  }

  async update (action: 'update' | 'downgrade') {
    const app = peekProperties(this.$app$)

    const value = {
      id: app.id,
      title: app.title,
      version: app.versionViewing,
      serviceRequirements: app.serviceRequirements,
      installWarning: app.installWarning,
    }

    switch (action) {
      case 'update':
        return wizardModal(
          this.modalCtrl,
          this.wizardBaker.update(value),
        ).then(({ cancelled }) => cancelled || this.navCtrl.back())
      case 'downgrade':
        return wizardModal(
          this.modalCtrl,
          this.wizardBaker.downgrade(value),
        ).then(({ cancelled }) => cancelled || this.navCtrl.back())
    }
  }

  async presentModalReleaseNotes () {
    const { releaseNotes, versionViewing } = peekProperties(this.$app$)

    const modal = await this.modalCtrl.create({
      component: AppReleaseNotesPage,
      componentProps: {
        releaseNotes,
        version: versionViewing,
      },
    })

    await modal.present()
  }

  private fetchRecommendation (): Observable<any> {
    this.recommendation = history.state && history.state.installationRecommendation

    if (this.recommendation) {
      return from(this.syncVersionSpecificInfo(this.recommendation.versionSpec))
    } else {
      return of({ })
    }
  }

  private syncWhenDependencyInstalls (): Observable<void> {
    return this.$app$.serviceRequirements.pipe(
      filter(deps => !!deps),
      switchMap(deps => this.appModel.watchForInstallations(deps)),
      concatMap(() => markAsLoadingDuring$(this.$dependenciesLoading$, this.syncVersionSpecificInfo())),
      catchError(e => of(console.error(e))),
    )
  }

  private setError (e: Error) {
    console.error(e)
    this.$error$.next(e.message)
  }
}
