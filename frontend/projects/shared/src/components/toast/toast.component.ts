import {
  AfterViewInit,
  ChangeDetectionStrategy,
  Component,
  ContentChildren,
  ElementRef,
  EventEmitter,
  Input,
  OnDestroy,
  Output,
  QueryList,
  ViewChild,
} from '@angular/core'
import { IonicSafeString, ToastController, ToastOptions } from '@ionic/angular'
import { OverlayEventDetail } from '@ionic/core'
import { ToastButtonDirective } from './toast-button.directive'

@Component({
  selector: 'toast',
  template: `
    <div #message><ng-content></ng-content></div>
    <ng-content select="[toastButton]"></ng-content>
  `,
  styles: [':host { display: none !important; }'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class ToastComponent<T> implements AfterViewInit, OnDestroy {
  @Output()
  readonly dismiss = new EventEmitter<OverlayEventDetail<T>>()

  @Input()
  header = ''

  @Input()
  duration = 0

  @Input()
  position: 'top' | 'bottom' | 'middle' = 'bottom'

  @ViewChild('message', { static: true })
  private readonly content?: ElementRef<HTMLElement>

  @ContentChildren(ToastButtonDirective)
  private readonly buttons: QueryList<ToastButtonDirective> = new QueryList()

  private toast?: HTMLIonToastElement

  constructor(
    private readonly elementRef: ElementRef<HTMLElement>,
    private readonly controller: ToastController,
  ) {}

  get cssClass(): string[] {
    return Array.from(this.elementRef.nativeElement.classList)
  }

  get message(): IonicSafeString {
    return new IonicSafeString(this.content?.nativeElement.innerHTML || '')
  }

  async ngAfterViewInit() {
    this.toast = await this.controller.create(this.getOptions())
    this.toast.onDidDismiss().then(event => {
      this.dismiss.emit(event)
    })

    await this.toast.present()
  }

  async ngOnDestroy() {
    await this.toast?.dismiss()
  }

  private getOptions(): ToastOptions {
    const { header, message, duration, position, cssClass, buttons } = this
    return {
      header,
      message,
      duration,
      position,
      cssClass,
      buttons: buttons.toArray(),
    }
  }
}
