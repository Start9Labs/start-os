import {
  Directive,
  ElementRef,
  EventEmitter,
  HostBinding,
  HostListener,
  Output,
} from '@angular/core'
import { DomSanitizer } from '@angular/platform-browser'

@Directive({
  selector: '[appDnd]',
})
export class DragNDropDirective {
  @Output() onFileDropped: EventEmitter<any> = new EventEmitter()

  @HostBinding('style.background') private background = 'rgba(24, 24, 24, 0.5)'

  constructor(el: ElementRef, private sanitizer: DomSanitizer) {}

  @HostListener('dragover', ['$event']) public onDragOver(evt: DragEvent) {
    evt.preventDefault()
    evt.stopPropagation()
    this.background = '#6a937b3c'
  }

  @HostListener('dragleave', ['$event']) public onDragLeave(evt: DragEvent) {
    evt.preventDefault()
    evt.stopPropagation()
    this.background = 'rgba(24, 24, 24, 0.5)'
  }

  @HostListener('drop', ['$event']) public onDrop(evt: DragEvent) {
    evt.preventDefault()
    evt.stopPropagation()
    this.background = ' rgba(24, 24, 24, 0.5)'
    this.onFileDropped.emit(evt)
  }
}
