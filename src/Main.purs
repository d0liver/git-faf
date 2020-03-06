module Main where

import Prelude

import Data.Maybe (Maybe)
import Data.Posix (Gid, Uid)
import Data.Posix.Signal (Signal)
import Effect (Effect)
import Effect.Class.Console (log)
import Foreign.Object (Object)
import Node.Buffer as Buff
import Node.ChildProcess (Exit(..), StdIOBehaviour, defaultExecSyncOptions, defaultSpawnOptions, execSync, inherit, onExit, spawn)
import Node.Encoding (Encoding(..))
import Node.Process (exit)
import Options.Applicative.Builder (forwardOptions, header, info, progDesc)
import Options.Applicative.Extra (execParser, helper)

execSync_ :: String
 -> { cwd :: Maybe String
    , env :: Maybe (Object String)
    , gid :: Maybe Gid
    , input :: Maybe String
    , killSignal :: Maybe Signal
    , maxBuffer :: Maybe Int
    , stdio :: Array (Maybe StdIOBehaviour)
    , timeout :: Maybe Number
    , uid :: Maybe Uid
    }
    -> Effect Unit
execSync_ cmd opts = execSync cmd opts *> pure unit

bailIf :: Effect Unit -> Exit -> Effect Unit
bailIf e = case _ of
  Normally c
    | c /= 0 -> (exit c)
    | otherwise -> e
  _ -> exit 1

push :: Effect Unit -> Effect Unit
push c = do
  log ">> git push"
  proc <- spawn "git" [ "push" ] (defaultSpawnOptions { stdio = inherit })
  onExit proc (bailIf c)

delete :: Effect Unit
delete = do
  n <- branchName
  h <- headCommit
  log (">> git checkout " <> h) *> execSync_ ("git checkout " <> h) defaultExecSyncOptions
  log (">> git branch -D " <> n) *> execSync_ ("git branch -D " <> n) defaultExecSyncOptions

headCommit :: Effect String
headCommit = execSync "git rev-parse HEAD" defaultExecSyncOptions >>= Buff.toString UTF8

branchName :: Effect String
branchName =
  execSync "git rev-parse --abbrev-ref HEAD" defaultExecSyncOptions
  >>= Buff.toString UTF8

main :: Effect Unit
main = do
  opts <-
    execParser
      $ info helper
          ( header "git faf"
              <> progDesc "'Fire and forget' to push a branch and immediately delete it locally."
              <> forwardOptions
          )
  push (delete)
