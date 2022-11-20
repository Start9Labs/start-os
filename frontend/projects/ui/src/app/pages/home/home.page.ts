import { Component } from '@angular/core'

@Component({
  selector: 'home',
  templateUrl: 'home.page.html',
  styleUrls: ['home.page.scss'],
})
export class HomePage {
  constructor() {}

  cards: Card[] = [
    {
      title: 'Visit the Marketplace',
      icon: 'storefront-outline',
      color: 'var(--alt-blue)',
      description: 'Start shopping for your favorite services',
      link: '/marketplace/browse',
    },
    {
      title: 'Get Support',
      icon: 'help-circle-outline',
      color: 'var(--alt-red)',
      description: 'Contact our all start customer support team',
      link: 'https://docs.start9.com/latest/support/contact',
    },
    {
      title: 'User Manual',
      icon: 'information-circle-outline',
      color: 'var(--alt-yellow)',
      description: 'Discover what your Embassy can do',
      link: 'https://docs.start9.com/latest/user-manual/index',
    },
    {
      title: 'View Details',
      icon: 'cube-outline',
      color: 'var(--alt-green)',
      description: 'See information about your Embassy',
      link: '/settings/specs',
    },
    {
      title: 'Create a backup',
      icon: 'duplicate-outline',
      color: 'var(--alt-purple)',
      description: 'Backup your device information and service data',
      link: '/settings/backup',
    },
    {
      title: 'Setup LAN',
      icon: 'lock-closed-outline',
      color: 'var(--alt-orange)',
      description: 'Install your certificate for a secure local connection',
      link: '/settings/lan',
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
