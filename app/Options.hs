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
                (\h p s w r d b t -> MasterOptions (HostConfig h p) (MasterConfig s w r d b t))
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
                    <*> option auto
                        ( long "msg-delay"
                        <> short 'd'
                        <> metavar "MICROSECONDS"
                        <> showDefault
                        <> value 100
                        <> help "Delay between sending messages" )
                    <*> option auto
                        ( long "buffer-size"
                        <> short 'b'
                        <> metavar "NUMBER"
                        <> showDefault
                        <> value 5000
                        <> help "Size of messages buffer" )
                    <*> option auto
                        ( long "time-to-show"
                        <> short 't'
                        <> metavar "MICROSECONDS"
                        <> showDefault
                        <> value 500000
                        <> help "Time needed to calculate and show result" )
                )
            )
            ( progDesc "Runs master process" )
        )
    )
