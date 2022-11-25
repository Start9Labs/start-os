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
  @Input() cardDetails!: Card
  @Input() containerDimensions!: Dimension
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

export interface Dimension {
  height: number
  width: number
}

export interface Card {
  title: string
  icon: string
  color: string
  description: string
  link: string
  qp?: Record<string, string>
}
