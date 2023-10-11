import {
  ChangeDetectionStrategy,
  Component,
  ElementRef,
  HostListener,
  ViewChild,
} from '@angular/core'
import { Card, Dimension } from '../widget-card/widget-card.component'

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
      title: 'Visit the Marketplace',
      icon: 'storefront-outline',
      color: 'var(--alt-blue)',
      description: 'Shop for your favorite open source services',
      link: '/marketplace',
      qp: { back: 'true' },
    },
    {
      title: 'Root CA',
      icon: 'ribbon-outline',
      color: 'var(--alt-orange)',
      description: `Download and trust your server's root certificate authority`,
      link: '/system/root-ca',
    },
    {
      title: 'Create Backup',
      icon: 'duplicate-outline',
      color: 'var(--alt-purple)',
      description: 'Back up StartOS and service data',
      link: '/system/backup',
    },
    {
      title: 'Server Info',
      icon: 'information-circle-outline',
      color: 'var(--alt-green)',
      description: 'View basic information about your server',
      link: '/system/specs',
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
      description: 'Get help from the Start9 team and community',
      link: 'https://start9.com/contact',
    },
  ]
}
