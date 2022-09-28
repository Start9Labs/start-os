module.exports = {
  '**/*.{js,ts,html,md,json}': 'prettier --write',
  '*.ts': 'tslint --fix',
  'projects/ui/**/*.ts': () => 'npm run check:ui',
  'projects/shared/**/*.ts': () => 'npm run check:shared',
  'projects/diagnostic-ui/**/*.ts': () => 'npm run check:dui',
  'projects/setup-wizard/**/*.ts': () => 'npm run check:setup',
}
