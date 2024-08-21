import { Configuration } from "@alephium/cli";

// Settings are usually for configuring
export type Settings = {};

const defaultSettings: Settings = {};

const configuration: Configuration<Settings> = {
  networks: {
    devnet: {
      nodeUrl: "http://localhost:22973",
      privateKeys: [
        "a642942e67258589cd2b1822c631506632db5a12aabcf413604e785300d762a5", // group 0
      ],
      settings: defaultSettings,
      networkId: 4,
    },

    testnet: {
      nodeUrl:
        (process.env.NODE_URL as string) ??
        "https://wallet-v20.testnet.alephium.org",
      privateKeys:
        process.env.PRIVATE_KEYS === undefined
          ? []
          : process.env.PRIVATE_KEYS.split(","),
      settings: defaultSettings,
    },

    mainnet: {
      nodeUrl:
        (process.env.NODE_URL as string) ??
        "https://wallet-v20.mainnet.alephium.org",
      privateKeys:
        process.env.PRIVATE_KEYS === undefined
          ? []
          : process.env.PRIVATE_KEYS.split(","),
      settings: defaultSettings,
    },
  },
};

export default configuration;
