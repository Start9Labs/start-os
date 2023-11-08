import {
  ChangeDetectionStrategy,
  Component,
  ElementRef,
  HostListener,
  ViewChild,
} from '@angular/core'
import { Card, Dimension } from './widget-card/widget-card.component'

@Component({
  selector: 'widget-list',
  templateUrl: './widget-list.component.html',
  styleUrls: ['./widget-list.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class WidgetListComponent {
  @ViewChild('gridContent')
  gridContent: ElementRef<HTMLElement> = {} as ElementRef<HTMLElement>
  @HostListener('window:resize', ['$event'])
  onResize() {
    this.setContainerDimensions()
  }

  containerDimensions: Dimension = {} as Dimension

  ngAfterViewInit() {
    this.setContainerDimensions()
  }

  setContainerDimensions() {
    this.containerDimensions.height = (<HTMLElement> (
      this.gridContent.nativeElement
    )).getBoundingClientRect().height
    this.containerDimensions.width = (<HTMLElement> (
      this.gridContent.nativeElement
    )).getBoundingClientRect().width
  }

  cards: Card[] = [
    {
      title: 'Server Info',
      icon: 'information-circle-outline',
      color: 'var(--alt-green)',
      description: 'View information about your server',
      link: '/system/specs',
    },
    {
      title: 'Browse',
      icon: 'storefront-outline',
      color: 'var(--alt-purple)',
      description: 'Browse for services to install',
      link: '/marketplace',
      qp: { back: 'true' },
    },
    {
      title: 'Create Backup',
      icon: 'duplicate-outline',
      color: 'var(--alt-blue)',
      description: 'Back up StartOS and service data',
      link: '/system/backup',
    },
    {
      title: 'Monitor',
      icon: 'pulse-outline',
      color: 'var(--alt-orange)',
      description: `View your system resource usage`,
      link: '/system/metrics',
    },
    {
      title: 'User Manual',
      icon: 'map-outline',
      color: 'var(--alt-yellow)',
      description: 'Discover what StartOS can do',
      link: 'https://docs.start9.com/0.3.5.x/user-manual/index',
    },
    {
      title: 'Contact Support',
      icon: 'chatbubbles-outline',
      color: 'var(--alt-red)',
      description: 'Get help from the Start9 community',
      link: 'https://start9.com/contact',
    },
  ]
}
