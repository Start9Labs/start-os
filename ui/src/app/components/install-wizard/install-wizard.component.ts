import { Component, Input, NgZone, QueryList, ViewChild, ViewChildren } from '@angular/core'
import { IonContent, IonSlides, ModalController } from '@ionic/angular'
import { BehaviorSubject } from 'rxjs'
import { capitalizeFirstLetter, pauseFor } from 'src/app/util/misc.util'
import { CompleteComponent } from './complete/complete.component'
import { DependentsComponent } from './dependents/dependents.component'
import { AlertComponent } from './alert/alert.component'
import { NotesComponent } from './notes/notes.component'
import { Loadable } from './loadable'
import { WizardAction } from './wizard-types'

@Component({
  selector: 'install-wizard',
  templateUrl: './install-wizard.component.html',
  styleUrls: ['./install-wizard.component.scss'],
})
export class InstallWizardComponent {
  @Input() params: {
    // defines each slide along with bottom bar
    slideDefinitions: SlideDefinition[]
    toolbar: TopbarParams
  }

  // content container so we can scroll to top between slide transitions
  @ViewChild(IonContent) contentContainer: IonContent
  // slide container gives us hook into ion-slide, allowing for slide transitions
  @ViewChild(IonSlides) slideContainer: IonSlides

  //a slide component gives us hook into a slide. Allows us to call load when slide comes into view
  @ViewChildren('components')
  slideComponentsQL: QueryList<Loadable>
  get slideComponents (): Loadable[] { return this.slideComponentsQL.toArray() }

  private slideIndex = 0
  get currentSlide (): Loadable {
    return this.slideComponents[this.slideIndex]
  }
  get currentBottomBar (): SlideDefinition['bottomBar'] {
    return this.params.slideDefinitions[this.slideIndex].bottomBar
  }

  initializing$ = new BehaviorSubject(true)
  error$ = new BehaviorSubject(undefined)

  constructor (
    private readonly modalController: ModalController,
    private readonly zone: NgZone,
  ) { }

  ngAfterViewInit () {
    this.currentSlide.load()
    this.slideContainer.update()
    this.slideContainer.lockSwipes(true)
  }

  ionViewDidEnter () {
    this.initializing$.next(false)
  }

  // process bottom bar buttons
  private transition = (info: { next: any } | { error: Error } | { cancelled: true } | { final: true }) => {
    const i = info as { next?: any, error?: Error, cancelled?: true, final?: true }
    if (i.cancelled) this.currentSlide.cancel$.next()
    if (i.final || i.cancelled) return this.modalController.dismiss(i)
    if (i.error) return this.error$.next(capitalizeFirstLetter(i.error.message))

    this.moveToNextSlide(i.next)
  }

  // bottom bar button callbacks. Pass this into components if they need to trigger slide transitions independent of the bottom bar clicks
  transitions = {
    next: (prevResult: any) => this.transition({ next: prevResult || this.currentSlide.result }),
    cancel: () => this.transition({ cancelled: true }),
    final: () => this.transition({ final: true }),
    error: (e: Error) => this.transition({ error: e }),
  }

  private async moveToNextSlide (prevResult?: any) {
    if (this.slideComponents[this.slideIndex + 1] === undefined) { return this.transition({ final: true }) }
    this.zone.run(async () => {
      this.slideComponents[this.slideIndex + 1].load(prevResult)
      await pauseFor(50) // give the load ^ opportunity to propogate into slide before sliding it into view
      this.slideIndex += 1
      await this.slideContainer.lockSwipes(false)
      await this.contentContainer.scrollToTop()
      await this.slideContainer.slideNext(500)
      await this.slideContainer.lockSwipes(true)
    })
  }
}

export interface SlideDefinition {
  slide:
    { selector: 'dependents', params: DependentsComponent['params'] } |
    { selector: 'complete', params: CompleteComponent['params'] } |
    { selector: 'alert', params: AlertComponent['params'] } |
    { selector: 'notes', params: NotesComponent['params'] }
  bottomBar: {
    cancel: {
      // indicates the existence of a cancel button, and whether to have text or an icon 'x' by default.
      afterLoading?: { text?: string },
      whileLoading?: { text?: string }
    }
    // indicates the existence of next or finish buttons (should only have one)
    next?: string
    finish?: string
  }
}

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
