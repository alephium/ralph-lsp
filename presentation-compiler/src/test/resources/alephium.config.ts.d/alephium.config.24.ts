import { Configuration } from '@alephium/cli'
import { Address, HexString } from '@alephium/web3'

export type Settings = {
  beneficiaryAsset: HexString
  beneficiaryAssetAmount: bigint
  auctioneer: Address
  auctionEndTime: number
}

const configuration: Configuration<Settings> = {
  networks: {
    devnet: {
      nodeUrl: 'http://localhost:22973',
      // here we could configure which address groups to deploy the contract
      privateKeys: ['a642942e67258589cd2b1822c631506632db5a12aabcf413604e785300d762a5'],
      settings: undefined as unknown as Settings
    },

    testnet: {
      nodeUrl: process.env.NODE_URL as string,
      privateKeys: process.env.PRIVATE_KEYS === undefined ? [] : process.env.PRIVATE_KEYS.split(','),
      settings: undefined as unknown as Settings
    },

    mainnet: {
      nodeUrl: process.env.NODE_URL as string,
      privateKeys: process.env.PRIVATE_KEYS === undefined ? [] : process.env.PRIVATE_KEYS.split(','),
      settings: undefined as unknown as Settings
    }
  }
}

export default configuration
