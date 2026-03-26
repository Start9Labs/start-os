/**
 * IANA timezone → POSIX TZ string mapping.
 *
 * The POSIX TZ string encodes the UTC offset and DST transition rules so that
 * the router can convert UTC ↔ local time without the full zoneinfo database.
 * OpenWrt writes this string to /etc/TZ via `/etc/init.d/system reload`.
 *
 * Sorted by standard UTC offset (west → east) for dropdown display.
 * Based on LuCI's timezone list — covers all major IANA zones.
 */
export interface TimezoneEntry {
  /** IANA timezone name, e.g. "America/New_York" */
  iana: string
  /** POSIX TZ string, e.g. "EST5EDT,M3.2.0,M11.1.0" */
  posix: string
  /** Human-readable label for UI display */
  label: string
  /** Standard UTC offset in minutes (negative = west of UTC) */
  offsetMin: number
}

/** Format an offset in minutes as "UTC±HH:MM". */
export function formatOffset(offsetMin: number): string {
  if (offsetMin === 0) return 'UTC'
  const sign = offsetMin >= 0 ? '+' : '\u2212'
  const abs = Math.abs(offsetMin)
  const h = Math.floor(abs / 60)
  const m = abs % 60
  return `UTC${sign}${String(h).padStart(2, '0')}:${String(m).padStart(2, '0')}`
}

export const TIMEZONES: TimezoneEntry[] = [
  // UTC-10
  {
    iana: 'America/Adak',
    posix: 'HAST10HADT,M3.2.0,M11.1.0',
    label: 'Hawaii-Aleutian',
    offsetMin: -600,
  },
  {
    iana: 'Pacific/Honolulu',
    posix: 'HST10',
    label: 'Hawaii',
    offsetMin: -600,
  },

  // UTC-09
  {
    iana: 'America/Anchorage',
    posix: 'AKST9AKDT,M3.2.0,M11.1.0',
    label: 'Alaska',
    offsetMin: -540,
  },

  // UTC-08
  {
    iana: 'America/Los_Angeles',
    posix: 'PST8PDT,M3.2.0,M11.1.0',
    label: 'US Pacific',
    offsetMin: -480,
  },
  {
    iana: 'America/Vancouver',
    posix: 'PST8PDT,M3.2.0,M11.1.0',
    label: 'Vancouver',
    offsetMin: -480,
  },
  {
    iana: 'America/Tijuana',
    posix: 'PST8PDT,M3.2.0,M11.1.0',
    label: 'Tijuana',
    offsetMin: -480,
  },

  // UTC-07
  {
    iana: 'America/Phoenix',
    posix: 'MST7',
    label: 'Arizona',
    offsetMin: -420,
  },
  {
    iana: 'America/Denver',
    posix: 'MST7MDT,M3.2.0,M11.1.0',
    label: 'US Mountain',
    offsetMin: -420,
  },
  {
    iana: 'America/Edmonton',
    posix: 'MST7MDT,M3.2.0,M11.1.0',
    label: 'Edmonton',
    offsetMin: -420,
  },

  // UTC-06
  {
    iana: 'America/Chicago',
    posix: 'CST6CDT,M3.2.0,M11.1.0',
    label: 'US Central',
    offsetMin: -360,
  },
  {
    iana: 'America/Winnipeg',
    posix: 'CST6CDT,M3.2.0,M11.1.0',
    label: 'Winnipeg',
    offsetMin: -360,
  },
  {
    iana: 'America/Mexico_City',
    posix: 'CST6',
    label: 'Mexico City',
    offsetMin: -360,
  },

  // UTC-05
  {
    iana: 'America/New_York',
    posix: 'EST5EDT,M3.2.0,M11.1.0',
    label: 'US Eastern',
    offsetMin: -300,
  },
  {
    iana: 'America/Toronto',
    posix: 'EST5EDT,M3.2.0,M11.1.0',
    label: 'Toronto',
    offsetMin: -300,
  },
  {
    iana: 'America/Havana',
    posix: 'CST5CDT,M3.2.0/0,M11.1.0/1',
    label: 'Havana',
    offsetMin: -300,
  },
  {
    iana: 'America/Bogota',
    posix: '<-05>5',
    label: 'Bogota',
    offsetMin: -300,
  },
  {
    iana: 'America/Lima',
    posix: '<-05>5',
    label: 'Lima',
    offsetMin: -300,
  },

  // UTC-04
  {
    iana: 'America/Caracas',
    posix: '<-04>4',
    label: 'Caracas',
    offsetMin: -240,
  },
  {
    iana: 'America/Halifax',
    posix: 'AST4ADT,M3.2.0,M11.1.0',
    label: 'Atlantic',
    offsetMin: -240,
  },
  {
    iana: 'America/Santiago',
    posix: '<-04>4<-03>,M9.1.6/24,M4.1.6/24',
    label: 'Santiago',
    offsetMin: -240,
  },

  // UTC-03:30
  {
    iana: 'America/St_Johns',
    posix: 'NST3:30NDT,M3.2.0,M11.1.0',
    label: 'Newfoundland',
    offsetMin: -210,
  },

  // UTC-03
  {
    iana: 'America/Sao_Paulo',
    posix: '<-03>3',
    label: 'São Paulo',
    offsetMin: -180,
  },
  {
    iana: 'America/Argentina/Buenos_Aires',
    posix: '<-03>3',
    label: 'Buenos Aires',
    offsetMin: -180,
  },

  // UTC+00
  {
    iana: 'UTC',
    posix: 'UTC0',
    label: 'UTC',
    offsetMin: 0,
  },
  {
    iana: 'Atlantic/Reykjavik',
    posix: 'GMT0',
    label: 'Reykjavik',
    offsetMin: 0,
  },
  {
    iana: 'Europe/London',
    posix: 'GMT0BST,M3.5.0/1,M10.5.0',
    label: 'London',
    offsetMin: 0,
  },
  {
    iana: 'Europe/Dublin',
    posix: 'IST-1GMT0,M10.5.0,M3.5.0/1',
    label: 'Dublin',
    offsetMin: 0,
  },
  {
    iana: 'Europe/Lisbon',
    posix: 'WET0WEST,M3.5.0/1,M10.5.0',
    label: 'Lisbon',
    offsetMin: 0,
  },

  // UTC+01
  {
    iana: 'Africa/Casablanca',
    posix: '<+01>-1',
    label: 'Casablanca',
    offsetMin: 60,
  },
  {
    iana: 'Africa/Lagos',
    posix: 'WAT-1',
    label: 'Lagos',
    offsetMin: 60,
  },
  {
    iana: 'Europe/Paris',
    posix: 'CET-1CEST,M3.5.0,M10.5.0/3',
    label: 'Paris',
    offsetMin: 60,
  },
  {
    iana: 'Europe/Berlin',
    posix: 'CET-1CEST,M3.5.0,M10.5.0/3',
    label: 'Berlin',
    offsetMin: 60,
  },
  {
    iana: 'Europe/Amsterdam',
    posix: 'CET-1CEST,M3.5.0,M10.5.0/3',
    label: 'Amsterdam',
    offsetMin: 60,
  },
  {
    iana: 'Europe/Brussels',
    posix: 'CET-1CEST,M3.5.0,M10.5.0/3',
    label: 'Brussels',
    offsetMin: 60,
  },
  {
    iana: 'Europe/Madrid',
    posix: 'CET-1CEST,M3.5.0,M10.5.0/3',
    label: 'Madrid',
    offsetMin: 60,
  },
  {
    iana: 'Europe/Rome',
    posix: 'CET-1CEST,M3.5.0,M10.5.0/3',
    label: 'Rome',
    offsetMin: 60,
  },
  {
    iana: 'Europe/Zurich',
    posix: 'CET-1CEST,M3.5.0,M10.5.0/3',
    label: 'Zurich',
    offsetMin: 60,
  },
  {
    iana: 'Europe/Stockholm',
    posix: 'CET-1CEST,M3.5.0,M10.5.0/3',
    label: 'Stockholm',
    offsetMin: 60,
  },
  {
    iana: 'Europe/Oslo',
    posix: 'CET-1CEST,M3.5.0,M10.5.0/3',
    label: 'Oslo',
    offsetMin: 60,
  },
  {
    iana: 'Europe/Copenhagen',
    posix: 'CET-1CEST,M3.5.0,M10.5.0/3',
    label: 'Copenhagen',
    offsetMin: 60,
  },
  {
    iana: 'Europe/Warsaw',
    posix: 'CET-1CEST,M3.5.0,M10.5.0/3',
    label: 'Warsaw',
    offsetMin: 60,
  },
  {
    iana: 'Europe/Vienna',
    posix: 'CET-1CEST,M3.5.0,M10.5.0/3',
    label: 'Vienna',
    offsetMin: 60,
  },
  {
    iana: 'Europe/Prague',
    posix: 'CET-1CEST,M3.5.0,M10.5.0/3',
    label: 'Prague',
    offsetMin: 60,
  },
  {
    iana: 'Europe/Budapest',
    posix: 'CET-1CEST,M3.5.0,M10.5.0/3',
    label: 'Budapest',
    offsetMin: 60,
  },

  // UTC+02
  {
    iana: 'Africa/Cairo',
    posix: 'EET-2EEST,M4.5.5/0,M10.5.4/24',
    label: 'Cairo',
    offsetMin: 120,
  },
  {
    iana: 'Africa/Johannesburg',
    posix: 'SAST-2',
    label: 'Johannesburg',
    offsetMin: 120,
  },
  {
    iana: 'Asia/Jerusalem',
    posix: 'IST-2IDT,M3.4.4/26,M10.5.0',
    label: 'Jerusalem',
    offsetMin: 120,
  },
  {
    iana: 'Europe/Helsinki',
    posix: 'EET-2EEST,M3.5.0/3,M10.5.0/4',
    label: 'Helsinki',
    offsetMin: 120,
  },
  {
    iana: 'Europe/Athens',
    posix: 'EET-2EEST,M3.5.0/3,M10.5.0/4',
    label: 'Athens',
    offsetMin: 120,
  },
  {
    iana: 'Europe/Bucharest',
    posix: 'EET-2EEST,M3.5.0/3,M10.5.0/4',
    label: 'Bucharest',
    offsetMin: 120,
  },
  {
    iana: 'Europe/Kyiv',
    posix: 'EET-2EEST,M3.5.0/3,M10.5.0/4',
    label: 'Kyiv',
    offsetMin: 120,
  },

  // UTC+03
  {
    iana: 'Africa/Nairobi',
    posix: 'EAT-3',
    label: 'Nairobi',
    offsetMin: 180,
  },
  {
    iana: 'Asia/Riyadh',
    posix: '<+03>-3',
    label: 'Riyadh',
    offsetMin: 180,
  },
  {
    iana: 'Europe/Istanbul',
    posix: '<+03>-3',
    label: 'Istanbul',
    offsetMin: 180,
  },
  {
    iana: 'Europe/Moscow',
    posix: 'MSK-3',
    label: 'Moscow',
    offsetMin: 180,
  },

  // UTC+03:30
  {
    iana: 'Asia/Tehran',
    posix: '<+0330>-3:30<+0430>,J79/24,J263/24',
    label: 'Tehran',
    offsetMin: 210,
  },

  // UTC+04
  {
    iana: 'Asia/Dubai',
    posix: '<+04>-4',
    label: 'Dubai',
    offsetMin: 240,
  },

  // UTC+05
  {
    iana: 'Asia/Karachi',
    posix: 'PKT-5',
    label: 'Karachi',
    offsetMin: 300,
  },

  // UTC+05:30
  {
    iana: 'Asia/Kolkata',
    posix: 'IST-5:30',
    label: 'Kolkata',
    offsetMin: 330,
  },
  {
    iana: 'Asia/Colombo',
    posix: '<+0530>-5:30',
    label: 'Colombo',
    offsetMin: 330,
  },

  // UTC+05:45
  {
    iana: 'Asia/Kathmandu',
    posix: '<+0545>-5:45',
    label: 'Kathmandu',
    offsetMin: 345,
  },

  // UTC+06
  {
    iana: 'Asia/Dhaka',
    posix: '<+06>-6',
    label: 'Dhaka',
    offsetMin: 360,
  },

  // UTC+07
  {
    iana: 'Asia/Bangkok',
    posix: '<+07>-7',
    label: 'Bangkok',
    offsetMin: 420,
  },
  {
    iana: 'Asia/Jakarta',
    posix: 'WIB-7',
    label: 'Jakarta',
    offsetMin: 420,
  },
  {
    iana: 'Asia/Ho_Chi_Minh',
    posix: '<+07>-7',
    label: 'Ho Chi Minh',
    offsetMin: 420,
  },

  // UTC+08
  {
    iana: 'Asia/Singapore',
    posix: '<+08>-8',
    label: 'Singapore',
    offsetMin: 480,
  },
  {
    iana: 'Asia/Kuala_Lumpur',
    posix: '<+08>-8',
    label: 'Kuala Lumpur',
    offsetMin: 480,
  },
  {
    iana: 'Asia/Shanghai',
    posix: 'CST-8',
    label: 'China',
    offsetMin: 480,
  },
  {
    iana: 'Asia/Taipei',
    posix: 'CST-8',
    label: 'Taipei',
    offsetMin: 480,
  },
  {
    iana: 'Asia/Hong_Kong',
    posix: 'HKT-8',
    label: 'Hong Kong',
    offsetMin: 480,
  },
  {
    iana: 'Asia/Manila',
    posix: 'PHT-8',
    label: 'Manila',
    offsetMin: 480,
  },
  {
    iana: 'Australia/Perth',
    posix: 'AWST-8',
    label: 'Perth',
    offsetMin: 480,
  },

  // UTC+09
  {
    iana: 'Asia/Seoul',
    posix: 'KST-9',
    label: 'Seoul',
    offsetMin: 540,
  },
  {
    iana: 'Asia/Tokyo',
    posix: 'JST-9',
    label: 'Tokyo',
    offsetMin: 540,
  },

  // UTC+09:30
  {
    iana: 'Australia/Darwin',
    posix: 'ACST-9:30',
    label: 'Darwin',
    offsetMin: 570,
  },
  {
    iana: 'Australia/Adelaide',
    posix: 'ACST-9:30ACDT,M10.1.0,M4.1.0/3',
    label: 'Adelaide',
    offsetMin: 570,
  },

  // UTC+10
  {
    iana: 'Australia/Brisbane',
    posix: 'AEST-10',
    label: 'Brisbane',
    offsetMin: 600,
  },
  {
    iana: 'Australia/Sydney',
    posix: 'AEST-10AEDT,M10.1.0,M4.1.0/3',
    label: 'Sydney',
    offsetMin: 600,
  },
  {
    iana: 'Australia/Melbourne',
    posix: 'AEST-10AEDT,M10.1.0,M4.1.0/3',
    label: 'Melbourne',
    offsetMin: 600,
  },
  {
    iana: 'Australia/Hobart',
    posix: 'AEST-10AEDT,M10.1.0,M4.1.0/3',
    label: 'Hobart',
    offsetMin: 600,
  },
  {
    iana: 'Pacific/Guam',
    posix: 'ChST-10',
    label: 'Guam',
    offsetMin: 600,
  },

  // UTC+12
  {
    iana: 'Pacific/Auckland',
    posix: 'NZST-12NZDT,M9.5.0,M4.1.0/3',
    label: 'Auckland',
    offsetMin: 720,
  },
  {
    iana: 'Pacific/Fiji',
    posix: '<+12>-12',
    label: 'Fiji',
    offsetMin: 720,
  },
]

/** Common IANA aliases that map to entries in TIMEZONES. */
const IANA_ALIASES: Record<string, string> = {
  'Etc/UTC': 'UTC',
  'Etc/GMT': 'UTC',
  'Asia/Calcutta': 'Asia/Kolkata',
  'Asia/Saigon': 'Asia/Ho_Chi_Minh',
  'Pacific/Samoa': 'Pacific/Honolulu',
}

/** Look up a POSIX TZ string from an IANA timezone name. */
export function getPosixTz(iana: string): string | undefined {
  return (
    TIMEZONES.find(tz => tz.iana === iana)?.posix ??
    TIMEZONES.find(tz => tz.iana === IANA_ALIASES[iana])?.posix
  )
}

/** Resolve an IANA name (or alias) to the canonical IANA name in TIMEZONES. */
export function resolveTimezone(iana: string): string {
  if (TIMEZONES.some(tz => tz.iana === iana)) return iana
  return IANA_ALIASES[iana] ?? iana
}

/** Get the browser's IANA timezone. */
export function getBrowserTimezone(): string {
  return Intl.DateTimeFormat().resolvedOptions().timeZone
}
