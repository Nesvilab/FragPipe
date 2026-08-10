import {themes as prismThemes} from 'prism-react-renderer';
import type {Config} from '@docusaurus/types';
import type * as Preset from '@docusaurus/preset-classic';

// This runs in Node.js - Don't use client-side code here (browser APIs, JSX...)

const config: Config = {
  title: 'FragPipe',
  tagline: 'A complete proteomics pipeline with the MSFragger search engine at heart',
  // favicon: 'img/favicon.ico',

  // Future flags, see https://docusaurus.io/docs/api/docusaurus-config#future
  future: {
    v4: true, // Improve compatibility with the upcoming Docusaurus v4
    // use --no-minify flag in build script to disable minification
    // to avoid issues with Tailwind CSS v4, see https://github.com/facebook/docusaurus/discussions/11325
    // Note: This flag below is not needed if you use the `--no-minify` flag in the build script
    // v4: {
    //   useCssCascadeLayers: false, // Disable CSS cascade layers for compatibility with Tailwind CSS v4, https://github.com/facebook/docusaurus/discussions/11325
    // }
  },

  plugins: [
    function tailwindPlugin(context, options) {
      return {
        name: "tailwind-plugin",
        configurePostCss(postcssOptions) {
          postcssOptions.plugins = [require("@tailwindcss/postcss")];
          return postcssOptions;
        },
      };
    },
  ],

  // Set the production url of your site here
  url: 'https://fragpipe.nesvilab.org',
  // Set the /<baseUrl>/ pathname under which your site is served
  // For GitHub pages deployment, it is often '/<projectName>/'
  baseUrl: '/',

  // GitHub pages deployment config.
  // If you aren't using GitHub pages, you don't need these.
  organizationName: 'Nesvilab', // Usually your GitHub org/user name.
  projectName: 'FragPipe', // Usually your repo name.



  // Even if you don't use internationalization, you can use this field to set
  // useful metadata like html lang. For example, if your site is Chinese, you
  // may want to replace "en" with "zh-Hans".
  i18n: {
    defaultLocale: 'en',
    locales: ['en'],
  },

  presets: [
    [
      'classic',
      {
        docs: {
          sidebarPath: './sidebars.ts',
          // // Please change this to your repo.
          // // Remove this to remove the "edit this page" links.
          // editUrl:
          //   'https://github.com/facebook/docusaurus/tree/main/packages/create-docusaurus/templates/shared/',
        },
        blog: {
          showReadingTime: true,
          feedOptions: {
            type: ['rss', 'atom'],
            xslt: true,
          },
          // Please change this to your repo.
          // Remove this to remove the "edit this page" links.
          editUrl:
            'https://github.com/facebook/docusaurus/tree/main/packages/create-docusaurus/templates/shared/',
          // Useful options to enforce blogging best practices
          onInlineTags: 'warn',
          onInlineAuthors: 'warn',
          onUntruncatedBlogPosts: 'warn',
        },
        theme: {
          customCss: './src/css/custom.css',
        },
      } satisfies Preset.Options,
    ],
  ],

  themeConfig: {
    // Replace with your project's social card
    image: 'https://raw.githubusercontent.com/Nesvilab/FragPipe/develop/images/fragpipe-01-bg-white.png.png',
    announcementBar: {
      id: 'announcementBar',
      content: `🎉️ <b><a target="_blank" href="https://github.com/Nesvilab/FragPipe/releases/latest">FragPipe 24.1</a> is out!</b> 🥳️`,
      // backgroundColor: '#fafbfc',
      // textColor: '#091E42',
      // isCloseable: false,
    },
    colorMode: {
        defaultMode: 'light',
        disableSwitch: false,
        // respectPrefersColorScheme: true,
    },
    navbar: {
      title: 'FragPipe',
      // logo: {
      //   alt: 'FragPipe Logo',
      //   src: 'img/logo.svg',
      // },
      items: [
        // {
        //   type: 'docSidebar',
        //   sidebarId: 'tutorialSidebar',
        //   position: 'left',
        //   label: 'Tutorial',
        // },
        // {to: '/blog', label: 'Blog', position: 'left'},
        {
          href: 'https://bsky.app/profile/nesvilab.bsky.social',
          position: 'right',
          className: 'header-social-link header-bluesky-link',
          'aria-label': 'Bluesky',
        },
        {
          href: 'https://x.com/Nesvilab',
          position: 'right',
          className: 'header-social-link header-twitter-link',
          'aria-label': 'X',
        },
        {
          href: 'https://github.com/Nesvilab/FragPipe',
          //label: 'GitHub',
          position: 'right',
          className: 'header-social-link header-github-link',
          'aria-label': 'GitHub repository',
        },
      ],
    },
    footer: {
      style: 'dark',
      // links: [
      //   {
      //     title: 'Docs',
      //     items: [
      //       {
      //         label: 'Tutorial',
      //         //to: '/docs/intro',
      //         to: '/docs/tutorial_fragpipe',
      //       },
      //     ],
      //   },
      //   {
      //     title: 'Community',
      //     items: [
      //       // {
      //       //   label: 'Stack Overflow',
      //       //   href: 'https://stackoverflow.com/questions/tagged/docusaurus',
      //       // },
      //       // {
      //       //   label: 'Discord',
      //       //   href: 'https://discordapp.com/invite/docusaurus',
      //       // },
      //       {
      //         label: 'X',
      //         href: 'https://x.com/nesvilab/',
      //       },
      //     ],
      //   },
      //   {
      //     title: 'More',
      //     items: [
      //       // {
      //       //   label: 'Blog',
      //       //   to: '/blog',
      //       // },
      //       {
      //         label: 'GitHub',
      //         href: 'https://github.com/Nesvilab/FragPipe/',
      //       },
      //     ],
      //   },
      // ],
      copyright: `<a href="https://github.com/Nesvilab/FragPipe">FragPipe</a> is maintained by <a href="https://github.com/Nesvilab">Nesvilab</a>.
      <br />
      Copyright © ${new Date().getFullYear()}. Built with Docusaurus.`,
    },
    prism: {
      theme: prismThemes.github,
      darkTheme: prismThemes.dracula,
    },
  } satisfies Preset.ThemeConfig,
  themes: [
    // ... Your other themes.
    [
      require.resolve("@easyops-cn/docusaurus-search-local"),
      /** @type {import("@easyops-cn/docusaurus-search-local").PluginOptions} */
      ({
        // ... Your options.
        // `hashed` is recommended as long-term-cache of index file is possible.
        hashed: true,
        highlightSearchTermsOnTargetPage: true,
        explicitSearchResultPath: true,
        // For Docs using Chinese, it is recomended to set:
        // language: ["en", "zh"],

        // Customize the keyboard shortcut to focus search bar (default is "mod+k"):
        // searchBarShortcutKeymap: "s", // Use 'S' key
        // searchBarShortcutKeymap: "ctrl+shift+f", // Use Ctrl+Shift+F

        // If you're using `noIndex: true`, set `forceIgnoreNoIndex` to enable local index:
        // forceIgnoreNoIndex: true,
      }),
    ],
  ],
};

export default config;
