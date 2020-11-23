import { Component, Input, OnInit, QueryList, ViewChild, ViewChildren } from '@angular/core'
import { IonContent, IonSlides, ModalController } from '@ionic/angular'
import { BehaviorSubject, combineLatest, Subscription } from 'rxjs'
import { map } from 'rxjs/operators'
import { Cleanup } from 'src/app/util/cleanup'
import { capitalizeFirstLetter } from 'src/app/util/misc.util'
import { CompleteComponent } from './complete/complete.component'
import { DependenciesComponent } from './dependencies/dependencies.component'
import { DependentsComponent } from './dependents/dependents.component'
import { Colorable, Loadable } from './loadable'
import { WizardAction } from './wizard-types'

@Component({
  selector: 'install-wizard',
  templateUrl: './install-wizard.component.html',
  styleUrls: ['./install-wizard.component.scss'],
})
export class InstallWizardComponent extends Cleanup implements OnInit {
  @Input() params: {
    // defines the slideshow in the html
    slideDefinitions: SlideDefinition[]
    toolbar: TopbarParams
  }

  // containers
  @ViewChild(IonContent) contentContainer: IonContent
  @ViewChild(IonSlides) slideContainer: IonSlides

  //don't use this, use slideComponents instead.
  @ViewChildren('components')
  public slideComponentsQL: QueryList<Loadable & Colorable>

  //don't use this, use currentSlide instead.
  slideIndex = 0

  get slideComponents (): (Loadable & Colorable)[] {
    return this.slideComponentsQL.toArray()
  }

  get currentSlide (): (Loadable & Colorable) {
    return this.slideComponents[this.slideIndex]
  }

  get currentSlideDef (): SlideDefinition {
    return this.params.slideDefinitions[this.slideIndex]
  }

  $anythingLoading$: BehaviorSubject<boolean> = new BehaviorSubject(true)
  $currentColor$: BehaviorSubject<string> = new BehaviorSubject('medium')
  $error$ = new BehaviorSubject(undefined)

  constructor (private readonly modalController: ModalController) { super() }
  ngOnInit () { }

  ngAfterViewInit () {
    this.currentSlide.load()
    this.slideContainer.update()
    this.slideContainer.lockSwipes(true)
  }

  ionViewDidEnter () {
    this.cleanup(
      combineLatest(this.slideComponents.map(component => component.$loading$)).pipe(
        map(loadings => !loadings.every(p => !p)),
      ).subscribe(this.$anythingLoading$),
      combineLatest(this.slideComponents.map(component => component.$color$)).pipe(
        map(colors => colors[this.slideIndex]),
      ).subscribe(this.$currentColor$),
    )
  }

  finished = (info: { error?: Error, cancelled?: true, final?: true }) => {
    if (info.cancelled) this.currentSlide.$cancel$.next()
    if (info.final || info.cancelled) return this.modalController.dismiss(info)
    if (info.error) return this.$error$.next(capitalizeFirstLetter(info.error.message))

    this.slide()
  }

  private async slide () {
    if (this.slideComponents[this.slideIndex + 1] === undefined) { return this.finished({ final: true }) }
    this.slideIndex += 1
    await this.slideContainer.lockSwipes(false)
    await Promise.all([this.contentContainer.scrollToTop(), this.slideContainer.slideNext()])
    await this.slideContainer.lockSwipes(true)
    this.currentSlide.load()
  }
}

export interface SlideCommon {
  selector: string
  cancelButton: {
    // indicates the existence of a cancel button, and whether to have text or an icon 'x' by default.
    afterLoading?: { text?: string },
    whileLoading?: { text?: string }
  }
  nextButton?: string,
  finishButton?: string
}

export type SlideDefinition = SlideCommon & (
  {
    selector: 'dependencies',
    params: DependenciesComponent['params']
  } | {
    selector: 'dependents',
    params: DependentsComponent['params']
  } | {
    selector: 'complete',
    params: CompleteComponent['params']
  }
)

export type TopbarParams = { action: WizardAction, title: string, version: string }

export async function wizardModal (
  modalController: ModalController, params: InstallWizardComponent['params'],
): Promise<{ cancelled?: true, final?: true, modal: HTMLIonModalElement }> {
  const modal = await modalController.create({
    backdropDismiss: false,
    cssClass: 'wizard-modal',
    component: InstallWizardComponent,
    componentProps: { params },
  })

  await modal.present()
  return modal.onWillDismiss().then(({ data }) => ({ ...data, modal }))
}
