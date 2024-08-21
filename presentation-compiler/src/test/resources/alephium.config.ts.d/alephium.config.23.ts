import type { Configuration } from "@alephium/cli";
import { PRICE_PER_MS, ONE_MONTH_IN_MS } from "./constants";

const settings = {
	orderManager: {
		initialFields: {
			price: PRICE_PER_MS,
			minDuration: ONE_MONTH_IN_MS,
			maxDuration: ONE_MONTH_IN_MS * 12n,
		},
	},
};

export type Settings = typeof settings;

export default {
	artifactDir: "./artifacts",

	networks: {
		devnet: {
			nodeUrl: "http://127.0.0.1:22973",
			// here we could configure which address groups to deploy the contract
			privateKeys: [
				"a642942e67258589cd2b1822c631506632db5a12aabcf413604e785300d762a5",
			],
			settings: {
				...settings,
				// devnet overrides
			},
		},

		testnet: {
			nodeUrl: process.env.TESTNET_NODE_URL as string,
			privateKeys:
				process.env.PRIVATE_KEYS === undefined
					? []
					: process.env.PRIVATE_KEYS.split(","),
			settings: {
				...settings,
				// testnet overrides
			},
		},

		mainnet: {
			nodeUrl: process.env.MAINNET_NODE_URL as string,
			privateKeys:
				process.env.PRIVATE_KEYS === undefined
					? []
					: process.env.PRIVATE_KEYS.split(","),
			settings: {
				...settings,
				// mainnet overrides
			},
		},
	},
} satisfies Configuration<Settings>;
