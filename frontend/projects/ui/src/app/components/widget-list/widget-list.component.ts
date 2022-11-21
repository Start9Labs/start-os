import { ChangeDetectionStrategy, Component, Input } from '@angular/core'

@Component({
  selector: 'widget-list',
  templateUrl: './widget-list.component.html',
  styleUrls: ['./widget-list.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class WidgetListComponent {
  constructor() {}

  cards: Card[] = [
    {
      title: 'Visit the Marketplace',
      icon: 'storefront-outline',
      color: 'var(--alt-blue)',
      description: 'Shop for your favorite open source services',
      link: '/marketplace/browse',
    },
    {
      title: 'LAN Setup',
      icon: 'home-outline',
      color: 'var(--alt-orange)',
      description:
        'Install your Embassy certificate for a secure local connection',
      link: '/settings/lan',
    },
    {
      title: 'Create Backup',
      icon: 'duplicate-outline',
      color: 'var(--alt-purple)',
      description: 'Back up your Embassy and service data',
      link: '/settings/backup',
    },
    {
      title: 'Embassy Info',
      icon: 'information-circle-outline',
      color: 'var(--alt-green)',
      description: 'View basic information about your Embassy',
      link: '/settings/specs',
    },
    {
      title: 'User Manual',
      icon: 'map-outline',
      color: 'var(--alt-yellow)',
      description: 'Discover what your Embassy can do',
      link: 'https://docs.start9.com/latest/user-manual/index',
    },
    {
      title: 'Contact Support',
      icon: 'chatbubbles-outline',
      color: 'var(--alt-red)',
      description: 'Get help from the Start9 team and community',
      link: 'https://docs.start9.com/latest/support/contact',
    },
  ]
}

interface Card {
  title: string
  icon: string
  color: string
  description: string
  link: string
}
