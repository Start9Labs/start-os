import {
  ChangeDetectionStrategy,
  Component,
  ElementRef,
  HostListener,
  Input,
  ViewChild,
} from '@angular/core'

@Component({
  selector: 'widget-card',
  templateUrl: './widget-card.component.html',
  styleUrls: ['./widget-card.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class WidgetCardComponent {
  @Input() title: string = ''
  @Input() icon: string = ''
  @Input() color: string = ''
  @Input() description: string = ''
  @Input() link: string = ''
  @Input() containerDimensions: any
  @ViewChild('outerWrapper') outerWrapper: ElementRef<HTMLElement> =
    {} as ElementRef<HTMLElement>
  @ViewChild('innerWrapper') innerWrapper: ElementRef<HTMLElement> =
    {} as ElementRef<HTMLElement>
  @HostListener('window:resize', ['$event'])
  onResize() {
    this.resize()
  }
  maxHeight = 0
  maxWidth = 0
  innerTransform = ''
  outerWidth: any
  outerHeight: any

  constructor() {}

  ngAfterViewInit() {
    this.maxHeight = (<HTMLElement> (
      this.innerWrapper.nativeElement
    )).getBoundingClientRect().height
    this.maxWidth = (<HTMLElement> (
      this.innerWrapper.nativeElement
    )).getBoundingClientRect().width
    this.resize()
  }

  resize() {
    const height = this.containerDimensions.height
    const width = this.containerDimensions.width
    const isMax = width >= this.maxWidth && height >= this.maxHeight
    const scale = Math.min(width / this.maxWidth, height / this.maxHeight)
    this.innerTransform = isMax ? '' : 'scale(' + scale + ')'
    this.outerWidth = isMax ? '' : this.maxWidth * scale
    this.outerHeight = isMax ? '' : this.maxHeight * scale
  }
}

interface Dimension {
  height: number
  width: number
}
