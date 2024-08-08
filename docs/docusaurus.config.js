// @ts-check
// Note: type annotations allow type checking and IDEs autocompletion

const lightCodeTheme = require("prism-react-renderer/themes/github");
const darkCodeTheme = require("prism-react-renderer/themes/dracula");
const docsMetadataJson = require("./static/docs-metadata.json");

const customFields = {
  apiSpecDir: "../hydra-node/json-schemas",
  apiSpecUrl: "api.yaml",
};

const editUrl = "https://github.com/cardano-scaling/hydra/tree/master/docs";

/** @type {import('@docusaurus/types').Config} */
const config = {
  title: "Hydra Head protocol documentation",
  url: "https://input-output-hk.github.io",
  baseUrl: "/head-protocol/",
  onBrokenLinks: "warn",
  onBrokenMarkdownLinks: "warn",
  favicon: "img/hydra.png",
  organizationName: "Input Output",
  projectName: "Hydra",
  staticDirectories: ["static", customFields.apiSpecDir],
  customFields,

  scripts: [
    {
      src: "https://plausible.io/js/script.js",
      defer: true,
      "data-domain": "hydra.family",
    },
  ],

  presets: [
    [
      "classic",
      /** @type {import('@docusaurus/preset-classic').Options} */
      ({
        docs: {
          editUrl,
          editLocalizedFiles: true,
          sidebarPath: require.resolve("./sidebars.js"),
          sidebarCollapsible: false,
        },
        blog: {
          path: "adr",
          routeBasePath: "/adr",
          blogTitle: "Architectural Decision Records",
          blogDescription:
            "Lightweight technical documentation for the Hydra node software.",
          blogSidebarTitle: "Architectural Decision Records",
          blogSidebarCount: "ALL",
          sortPosts: "ascending",
          authorsMapPath: "../authors.yaml",
        },
        theme: {
          customCss: require.resolve("./src/css/custom.css"),
        },
      }),
    ],
  ],

  plugins: [
    async function myPlugin(context, options) {
      return {
        name: "docusaurus-tailwindcss",
        configurePostCss(postcssOptions) {
          // Appends TailwindCSS and AutoPrefixer.
          postcssOptions.plugins.push(require("tailwindcss"));
          postcssOptions.plugins.push(require("autoprefixer"));
          return postcssOptions;
        },
      };
    },
    [
      "content-docs",
      /** @type {import('@docusaurus/plugin-content-docs').Options} */
      ({
        id: "standalone",
        path: "standalone",
        routeBasePath: "/",
        editUrl,
        editLocalizedFiles: true,
        sidebarPath: false,
      }),
    ],
    [
      "content-docs",
      /** @type {import('@docusaurus/plugin-content-docs').Options} */
      ({
        id: "use-cases",
        path: "use-cases",
        routeBasePath: "use-cases",
        editUrl,
        editLocalizedFiles: true,
      }),
    ],
    [
      "content-docs",
      /** @type {import('@docusaurus/plugin-content-docs').Options} */
      ({
        id: "topologies",
        path: "topologies",
        routeBasePath: "topologies",
        editUrl,
        editLocalizedFiles: true,
      }),
    ],
    [
      "content-docs",
      /** @type {import('@docusaurus/plugin-content-docs').Options} */
      ({
        id: "benchmarks",
        path: "benchmarks",
        routeBasePath: "benchmarks",
        editLocalizedFiles: true,
      }),
    ],
    [
      "@docusaurus/plugin-client-redirects",
      {
        redirects: [
          // Use cases section re-organized (2023-07-25)
          {
            from: "/use-cases/poker-game",
            to: "/use-cases/other/poker-game",
          },
          {
            from: "/use-cases/nft-auction",
            to: "/use-cases/auctions",
          },
          {
            from: "/use-cases/pay-per-use-api",
            to: "/use-cases/payments/pay-per-use-api",
          },
          {
            from: "/use-cases/inter-wallet-payments",
            to: "/use-cases/payments/inter-wallet-payments",
          },
        ],
      },
    ],
  ],

  themeConfig:
    /** @type {import('@docusaurus/preset-classic').ThemeConfig} */
    ({
      colorMode: {
        defaultMode: "light",
        disableSwitch: true,
        respectPrefersColorScheme: false,
      },
      navbar: {
        title: "Hydra Head Protocol",
        logo: {
          alt: "Hydra Head Protocol",
          src: "img/hydra.png",
          style: { height: 27, marginTop: 2.5 },
          srcDark: "img/hydra-white.png",
        },
        items: [
          {
            to: "/docs",
            label: "User manual",
            position: "right",
          },
          {
            to: "/docs/faqs",
            label: "FAQ",
            position: "right",
          },
        ],
      },
      footer: {
        style: "dark",
        links: [
          {
            title: "Contributing",
            items: [
              {
                label: "Coding Standards",
                to: "https://github.com/cardano-scaling/hydra/wiki/Coding-Standards",
              },
              {
                label: "Architectural Decision",
                to: "/adr",
              },
              {
                label: "Testing Strategy",
                to: "https://github.com/cardano-scaling/hydra/wiki/Testing-Strategy",
              },
            ],
          },
          {
            title: "Community",
            items: [
              {
                label: "Discord",
                href: "https://discord.gg/Qq5vNTg9PT",
              },
              {
                label: "Github",
                href: "https://github.com/cardano-scaling/hydra/discussions",
              },
              {
                label: "Stack Exchange",
                href: "https://cardano.stackexchange.com/questions/tagged/hydra",
              },
            ],
          },
          {
            title: "More",
            items: [
              {
                label: "Haskell Packages",
                to: "/docs/dev/haskell-packages",
              },
              {
                label: "Monthly reports",
                to: "https://cardano-scaling.github.io/website/monthly",
              },
              {
                label: "Logbook",
                to: "https://github.com/cardano-scaling/hydra/wiki/Logbook",
              },
            ],
          },
          {
            title: "Legal",
            items: [
              {
                label: "Terms & Conditions",
                to: "https://static.iohk.io/terms/iohktermsandconditions.pdf",
              },
              {
                label: "Privacy Policy",
                to: "https://static.iohk.io/terms/iog-privacy-policy.pdf",
              },
              {
                label: "Contributors",
                to: "https://github.com/cardano-scaling/hydra/graphs/contributors",
              },
            ],
          },
        ],
        copyright: `© 2024`,
      },
      prism: {
        theme: lightCodeTheme,
        darkTheme: darkCodeTheme,
        additionalLanguages: ["haskell"],
      },
      algolia: {
        appId: "YZTAF8IOVB",
        apiKey: "ad133fe3b0b40974c26853abc9cad2ab",
        indexName: "hydra-family",
        searchPagePath: "search",
        contextualSearch: true,
      },
    }),

  markdown: {
    mermaid: true,
  },

  themes: ["@docusaurus/theme-mermaid"],
};

module.exports = config;
