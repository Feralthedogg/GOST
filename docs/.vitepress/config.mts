import { defineConfig } from 'vitepress'

const base = process.env.GOST_DOCS_BASE ?? '/'

export default defineConfig({
  title: 'Gost',
  description: 'Official documentation for the Gost language, compiler, runtime, and tooling',
  base,
  cleanUrls: true,
  lastUpdated: true,
  markdown: {
    languageAlias: {
      gs: 'go'
    }
  },
  themeConfig: {
    logo: '/logo.png',
    nav: [
      { text: 'Home', link: '/' },
      { text: 'Getting Started', link: '/getting-started' },
      { text: 'Language', link: '/language-guide' },
      { text: 'Stdlib', link: '/stdlib-reference' },
      { text: 'Internals', link: '/compiler-architecture' }
    ],
    sidebar: [
      {
        text: 'Start',
        items: [
          { text: 'Docs Home', link: '/' },
          { text: 'Getting Started', link: '/getting-started' },
          { text: 'Gost by Example', link: '/gost-by-example' },
          { text: 'CLI Reference', link: '/cli-reference' }
        ]
      },
      {
        text: 'Language',
        collapsed: false,
        items: [
          { text: 'Language Guide', link: '/language-guide' },
          { text: 'Language Reference', link: '/language-reference' },
          { text: 'Ownership Cheat Sheet', link: '/ownership-cheatsheet' },
          { text: 'Owner/Alias Model', link: '/owner_alias_model' }
        ]
      },
      {
        text: 'Practical Guides',
        collapsed: false,
        items: [
          { text: 'Modules and Packages', link: '/modules-and-packages' },
          { text: 'Concurrency and Ownership', link: '/concurrency-ownership-practice' },
          { text: 'FFI Guide', link: '/ffi-guide' },
          { text: 'Testing Guide', link: '/testing-guide' }
        ]
      },
      {
        text: 'Reference',
        collapsed: false,
        items: [
          { text: 'Standard Library', link: '/stdlib-reference' },
          { text: 'Intrinsics Reference', link: '/intrinsics-reference' },
          { text: 'Diagnostics', link: '/diagnostics' }
        ]
      },
      {
        text: 'Compiler Internals',
        collapsed: true,
        items: [
          { text: 'Compiler Architecture', link: '/compiler-architecture' }
        ]
      }
    ],
    socialLinks: [
      { icon: 'github', link: 'https://github.com/Feralthedogg/GOST' }
    ],
    search: {
      provider: 'local'
    },
    editLink: {
      pattern: 'https://github.com/Feralthedogg/GOST/edit/main/docs/:path',
      text: 'Edit this page on GitHub'
    },
    outline: {
      level: [2, 3]
    },
    footer: {
      message: 'Released under the MIT License.',
      copyright: 'Copyright 2026-present Gost contributors'
    }
  }
})
