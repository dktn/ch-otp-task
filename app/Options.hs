module Options where

import           Protolude           hiding (option)

import           Options.Applicative

import           Config
import           Utils


optionsParser :: Parser Options
optionsParser = subparser (
    command "slave"
        ( info
            ( helper <*> (
                (SlaveOptions .: HostConfig)
                    <$> argument str (metavar "HOST" <> help "Host name")
                    <*> argument str (metavar "PORT" <> help "Port number")
                )
            )
            ( progDesc "Runs slave process" )
        )
    <>
    command "master"
        ( info
            ( helper <*> (
                (\h p sd wd s -> MasterOptions (HostConfig h p) (MasterConfig sd wd s))
                    <$> argument str (metavar "HOST" <> help "Host name")
                    <*> argument str (metavar "PORT" <> help "Port number")
                    <*> option auto
                        ( long "send-for"
                        <> short 's'
                        <> metavar "SECONDS"
                        <> help "Time to send messages" )
                    <*> option auto
                        ( long "wait-for"
                        <> short 'w'
                        <> metavar "SECONDS"
                        <> help "Time to wait for the result" )
                    <*> option auto
                        ( long "with-seed"
                        <> short 'r'
                        <> metavar "SEED"
                        <> showDefault
                        <> value 0
                        <> help "Seed for random numbers generator" )
                )
            )
            ( progDesc "Runs master process" )
        )
    )
