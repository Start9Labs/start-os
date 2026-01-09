import { DdnsSection } from 'src/app/services/api/types'

// Mock 1: Disabled
export const ddnsDisabled: DdnsSection = {
  type: 'service',
  name: 'wan',
  options: {
    enabled: '0',
    service_name: 'start9',
  },
  lists: {},
}

// Mock 2: Start9 enabled (no additional fields needed)
export const ddnsStart9: DdnsSection = {
  type: 'service',
  name: 'wan',
  options: {
    enabled: '1',
    service_name: 'start9',
    ip_source: 'network',
    ip_network: 'wan',
  },
  lists: {},
}

// Mock 3: DynDNS enabled
export const ddnsDyndns: DdnsSection = {
  type: 'service',
  name: 'wan',
  options: {
    enabled: '1',
    service_name: 'dyndns.org',
    username: 'myuser',
    password: 'mypassword',
    domain: 'myhost.dyndns.org',
    lookup_host: 'myhost.dyndns.org',
    ip_source: 'network',
    ip_network: 'wan',
  },
  lists: {},
}

// Mock 4: Cloudflare enabled
export const ddnsCloudflare: DdnsSection = {
  type: 'service',
  name: 'wan',
  options: {
    enabled: '1',
    service_name: 'cloudflare.com-v4',
    username: 'zone_id_here',
    password: 'api_token_here',
    domain: 'home.example.com',
    lookup_host: 'home.example.com',
    ip_source: 'network',
    ip_network: 'wan',
  },
  lists: {},
}

// Mock 5: DuckDNS enabled
export const ddnsDuckdns: DdnsSection = {
  type: 'service',
  name: 'wan',
  options: {
    enabled: '1',
    service_name: 'duckdns.org',
    password: 'my-duckdns-token',
    domain: 'myhome.duckdns.org',
    lookup_host: 'myhome.duckdns.org',
    ip_source: 'network',
    ip_network: 'wan',
  },
  lists: {},
}
